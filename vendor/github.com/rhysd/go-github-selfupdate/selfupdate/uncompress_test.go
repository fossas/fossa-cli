package selfupdate

import (
	"bytes"
	"io/ioutil"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestCompressionNotRequired(t *testing.T) {
	buf := []byte{'a', 'b', 'c'}
	want := bytes.NewReader(buf)
	r, err := UncompressCommand(want, "https://github.com/foo/bar/releases/download/v1.2.3/foo", "foo")
	if err != nil {
		t.Fatal(err)
	}
	have, err := ioutil.ReadAll(r)
	if err != nil {
		t.Fatal(err)
	}
	for i, b := range have {
		if buf[i] != b {
			t.Error(i, "th elem is not the same as wanted. want", buf[i], "but got", b)
		}
	}
}

func getArchiveFileExt(file string) string {
	if strings.HasSuffix(file, ".tar.gz") {
		return ".tar.gz"
	}
	if strings.HasSuffix(file, ".tar.xz") {
		return ".tar.xz"
	}
	return filepath.Ext(file)
}

func TestUncompress(t *testing.T) {
	for _, n := range []string{
		"testdata/foo.zip",
		"testdata/single-file.zip",
		"testdata/single-file.gz",
		"testdata/single-file.gzip",
		"testdata/foo.tar.gz",
		"testdata/foo.tar.xz",
		"testdata/single-file.xz",
	} {
		t.Run(n, func(t *testing.T) {
			f, err := os.Open(n)
			if err != nil {
				t.Fatal(err)
			}

			ext := getArchiveFileExt(n)
			url := "https://github.com/foo/bar/releases/download/v1.2.3/bar" + ext
			r, err := UncompressCommand(f, url, "bar")
			if err != nil {
				t.Fatal(err)
			}

			bytes, err := ioutil.ReadAll(r)
			if err != nil {
				t.Fatal(err)
			}
			s := string(bytes)
			if s != "this is test\n" {
				t.Fatal("Uncompressing zip failed into unexpected content", s)
			}
		})
	}
}

func TestUncompressInvalidArchive(t *testing.T) {
	for _, a := range []struct {
		name string
		msg  string
	}{
		{"testdata/invalid.zip", "not a valid zip file"},
		{"testdata/invalid.gz", "Failed to uncompress gzip file"},
		{"testdata/invalid-tar.tar.gz", "Failed to unarchive .tar file"},
		{"testdata/invalid-gzip.tar.gz", "Failed to uncompress .tar.gz file"},
		{"testdata/invalid.xz", "Failed to uncompress xzip file"},
		{"testdata/invalid-tar.tar.xz", "Failed to unarchive .tar file"},
		{"testdata/invalid-xz.tar.xz", "Failed to uncompress .tar.xz file"},
	} {
		f, err := os.Open(a.name)
		if err != nil {
			t.Fatal(err)
		}

		ext := getArchiveFileExt(a.name)
		url := "https://github.com/foo/bar/releases/download/v1.2.3/bar" + ext
		_, err = UncompressCommand(f, url, "bar")
		if err == nil {
			t.Fatal("Error should be raised")
		}
		if !strings.Contains(err.Error(), a.msg) {
			t.Fatal("Unexpected error:", err)
		}
	}
}

func TestTargetNotFound(t *testing.T) {
	for _, tc := range []struct {
		name string
		msg  string
	}{
		{"testdata/empty.zip", "command is not found"},
		{"testdata/bar-not-found.zip", "command is not found"},
		{"testdata/bar-not-found.gzip", "does not match to command"},
		{"testdata/empty.tar.gz", "command is not found"},
		{"testdata/bar-not-found.tar.gz", "command is not found"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			f, err := os.Open(tc.name)
			if err != nil {
				t.Fatal(err)
			}
			ext := getArchiveFileExt(tc.name)
			url := "https://github.com/foo/bar/releases/download/v1.2.3/bar" + ext
			_, err = UncompressCommand(f, url, "bar")
			if err == nil {
				t.Fatal("Error should be raised for")
			}
			if !strings.Contains(err.Error(), tc.msg) {
				t.Fatal("Unexpected error:", err)
			}
		})
	}
}
