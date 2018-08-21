package fossa

import (
	"archive/tar"
	"compress/gzip"
	"crypto/md5"
	"encoding/hex"
	"io"
	"io/ioutil"
	"net/http"
	"net/url"
	"os"
	"path/filepath"
	"strings"

	"github.com/fossas/fossa-cli/log"
)

var (
	SignedURLAPI       = "/api/components/signed_url"
	ComponentsBuildAPI = "/api/components/build"
)

type SignedURL struct {
	SignedURL string
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
	p, err := filepath.Abs(dir)
	name := filepath.Base(p)
	if err != nil {
		return Locator{}, err
	}
	_, err = os.Stat(p)
	if err != nil {
		return Locator{}, err
	}

	// Run first pass: tarball creation and hashing.
	tarball, hash, err := CreateTarball(p)
	if err != nil {
		return Locator{}, err
	}
	info, err := tarball.Stat()
	if err != nil {
		return Locator{}, err
	}

	// Get signed URL for uploading.
	revision := hex.EncodeToString(hash)
	q := url.Values{}
	q.Add("packageSpec", name)
	q.Add("revision", revision)
	var signed SignedURL
	_, err = GetJSON(SignedURLAPI+"?"+q.Encode(), &signed)
	if err != nil {
		return Locator{}, err
	}

	// Run second pass: multi-part uploading.
	r, w := io.Pipe()
	// In parallel, stream temporary file to PUT.
	go func() {
		defer w.Close()
		defer tarball.Close()
		tarball.Seek(0, 0)
		_, err = io.Copy(w, tarball)
		if err != nil {
			log.Logger.Fatalf("Unable to upload: %s", err.Error())
		}
	}()

	req, err := http.NewRequest(http.MethodPut, signed.SignedURL, r)
	if err != nil {
		return Locator{}, err
	}
	req.Header.Set("Content-Type", "binary/octet-stream")
	req.ContentLength = info.Size()
	req.GetBody = func() (io.ReadCloser, error) {
		return r, nil
	}
	log.Logger.Debugf("req: %#v", req)
	res, err := http.DefaultClient.Do(req)
	if err != nil {
		return Locator{}, err
	}
	defer res.Body.Close()

	body, err := ioutil.ReadAll(res.Body)
	if err != nil {
		return Locator{}, err
	}
	log.Logger.Debugf("%#v", string(body))

	// Queue the component build.
	build := url.Values{}
	build.Add("archives[0][packageSpec]", name)
	build.Add("archives[0][revision]", revision)
	log.Logger.Debugf("queueing build: %#v", build)
	res, err = http.PostForm(mustParse(ComponentsBuildAPI).String(), build)
	if err != nil {
		return Locator{}, err
	}
	log.Logger.Debugf("%#v", string(body))

	return Locator{
		Fetcher:  "archive",
		Project:  name,
		Revision: revision,
	}, nil
}

// CreateTarball archives and compresses a directory's contents to a temporary
// file while simultaneously computing its MD5 hash. The caller is responsible
// for closing the file handle.
func CreateTarball(dir string) (*os.File, []byte, error) {
	tmp, err := ioutil.TempFile("", "fossa-tar-"+filepath.Base(dir)+"-")
	if err != nil {
		return nil, nil, err
	}
	defer tmp.Sync()

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
