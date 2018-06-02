package builderutil

import (
	"crypto/md5" // #nosec
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

	sha1Hash := sha1.New()
	if _, err := io.Copy(sha1Hash, f); err != nil {
		return hashes, err
	}
	hashes.SHA1 = hex.EncodeToString(sha1Hash.Sum(nil))

	md5Hash := md5.New() // #nosec
	if _, err := io.Copy(md5Hash, f); err != nil {
		return hashes, err
	}
	hashes.MD5 = hex.EncodeToString(md5Hash.Sum(nil))

	sha256Hash := sha256.New()
	if _, err := io.Copy(sha256Hash, f); err != nil {
		return hashes, err
	}
	hashes.SHA256 = hex.EncodeToString(sha256Hash.Sum(nil))

	return hashes, err
}
