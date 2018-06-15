package builderutil

import (
	"bufio"
	"crypto/md5" // nolint: gas
	"crypto/sha1"
	"crypto/sha256"
	"encoding/hex"
	"io"
	"os"

	"github.com/fossas/fossa-cli/module"
)

// GetHashes computes hexadecimal checksums of a variety of types for a given file path
func GetHashes(path string) (module.Hashes, error) {
	hashes := module.Hashes{}

	f, err := os.Open(path)
	if err != nil {
		return hashes, err
	}
	defer f.Close()

	md5 := md5.New() // nolint: gas
	sha1 := sha1.New()
	sha256 := sha256.New()

	// Read the file once and write to a multiplexer
	// so we can calculate all hashes simultaneously
	pagesize := os.Getpagesize()
	reader := bufio.NewReaderSize(f, pagesize)
	multiWriter := io.MultiWriter(md5, sha1, sha256)
	if _, err := io.Copy(multiWriter, reader); err != nil {
		panic(err.Error())
	}

	hashes.MD5 = hex.EncodeToString(md5.Sum(nil))
	hashes.SHA1 = hex.EncodeToString(sha1.Sum(nil))
	hashes.SHA256 = hex.EncodeToString(sha256.Sum(nil))

	return hashes, err
}
