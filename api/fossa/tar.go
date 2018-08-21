package fossa

import (
	"archive/tar"
	"compress/gzip"
	"crypto/md5"
	"encoding/base64"
	"encoding/hex"
	"encoding/json"
	"io"
	"io/ioutil"
	"mime/multipart"
	"net/http"
	"net/url"
	"os"
	"path/filepath"
	"strings"

	"github.com/fossas/fossa-cli/log"
)

var SignedURLAPI = "/api/components/signed_url"

type SignedURL struct {
	SignedURL string
}

type TarballSpec struct {
	PackageSpec string `json:"packageSpec"`
	Revision    string `json:"revision"`
}

// UploadTarball archives, compresses, and uploads a specified directory. It
// uses the directory name as the project name and the MD5 of the uploaded
// tarball as the revision name. It returns the locator of the uploaded tarball.
//
// In order to upload the tarball, we need the revision name. In order to get
// the revision name, we need to compute the tarball's MD5 hash. In order to
// compute the MD5 hash, we need to see every byte of the final tarball.
//
// To do this, we actually stream through the tarball _twice_: once to create
// the compressed tarball (and writing it to disk) while simultaneously
// calculating its hash, and again to perform the upload.
//
// The alternative is to hold the entire tarball in memory while we upload.
// Since this will be running within CI machines, this is probably not a good
// idea. (See https://circleci.com/docs/2.0/configuration-reference/#resource_class
// for an example of our memory constraints.)
func UploadTarball(dir string) (Locator, error) {
	_, err := os.Stat(dir)
	if err != nil {
		return Locator{}, err
	}

	// Run first pass: tarball creation and hashing.
	tmp, hash, err := CreateTarball(dir)
	if err != nil {
		return Locator{}, err
	}

	// Get signed URL for uploading.
	q := url.Values{}
	q.Add("packageSpec", filepath.Base(dir))
	q.Add("revision", hex.EncodeToString(hash))
	var url SignedURL
	_, err = GetJSON(SignedURLAPI+"?"+q.Encode(), &url)
	if err != nil {
		return Locator{}, err
	}

	// Run second pass: multi-part uploading.
	r, w := io.Pipe()
	// In parallel, stream temporary file to POST.
	m := multipart.NewWriter(w)
	go func() {
		defer w.Close()
		defer tmp.Close()

		data, err := m.CreateFormFile("file", filepath.Base(dir))
		if err != nil {
			log.Logger.Fatalf("Unable to upload: %s", err.Error())
		}

		_, err = io.Copy(data, tmp)
		if err != nil {
			log.Logger.Fatalf("Unable to upload: %s", err.Error())
		}

		meta, err := m.CreateFormField(filepath.Base(dir))
		if err != nil {
			log.Logger.Fatalf("Unable to upload: %s", err.Error())
		}
		metadata := TarballSpec{
			PackageSpec: filepath.Base(dir),
			Revision:    base64.StdEncoding.EncodeToString(hash),
		}
		marshalled, err := json.Marshal(metadata)
		if err != nil {
			log.Logger.Fatalf("Unable to upload: %s", err.Error())
		}
		_, err = meta.Write(marshalled)
		if err != nil {
			log.Logger.Fatalf("Unable to upload: %s", err.Error())
		}

		err = m.Close()
		if err != nil {
			log.Logger.Fatalf("Unable to upload: %s", err.Error())
		}
	}()

	res, err := http.Post("", m.FormDataContentType(), r)
	if err != nil {
		return Locator{}, err
	}
	defer res.Body.Close()

	body, err := ioutil.ReadAll(res.Body)
	if err != nil {
		return Locator{}, err
	}
	log.Logger.Info("%#v", body)

	return Locator{}, nil
}

// CreateTarball archives and compresses a directory's contents to a temporary
// file while simultaneously computing its MD5 hash.
func CreateTarball(dir string) (*os.File, []byte, error) {
	tmp, err := ioutil.TempFile("", "fossa-tar-"+dir+"-")
	if err != nil {
		return nil, nil, err
	}
	defer tmp.Close()

	h := md5.New()

	m := io.MultiWriter(h, tmp)

	g := gzip.NewWriter(m)
	defer g.Close()

	t := tar.NewWriter(g)
	defer t.Close()

	err = filepath.Walk(dir, func(filename string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}

		// Potential future space optimization: don't upload VCS history.
		// if info.IsDir() && (info.Name() == ".git" ||
		// 	info.Name() == ".svn" ||
		// 	info.Name() == ".hg" ||
		// 	info.Name() == ".bzr") {
		// 	return filepath.SkipDir
		// }

		header, err := tar.FileInfoHeader(info, info.Name())
		if err != nil {
			return err
		}
		header.Name = strings.TrimPrefix(filename, string(filepath.Separator))
		header.Name = strings.TrimPrefix(filename, dir)
		header.Name = strings.TrimPrefix(filename, string(filepath.Separator))

		err = t.WriteHeader(header)
		if err != nil {
			return err
		}

		// Exit early for directories, symlinks, etc.
		if !info.Mode().IsRegular() {
			return nil
		}

		file, err := os.Open(filename)
		if err != nil {
			return err
		}
		defer file.Close()

		_, err = io.Copy(t, file)
		if err != nil {
			return err
		}
		// Close again to force a disk flush. Closing an *os.File is undefined, but
		// safe in practice. See https://github.com/golang/go/issues/20705.
		file.Close()

		return nil
	})
	if err != nil {
		return nil, nil, err
	}

	return tmp, h.Sum(nil), nil
}
