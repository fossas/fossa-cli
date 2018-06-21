package module

// Hashes contains hexadecimal checksums of code libraries brought in by running
// a Build.
type Hashes struct {
	SHA1   string
	SHA256 string
	MD5    string
}
