package main

import (
	"crypto/aes"
	"crypto/cipher"
	"crypto/ecdsa"
	"crypto/ed25519"
	"crypto/elliptic"
	"crypto/hmac"
	"crypto/md5"
	"crypto/rand"
	"crypto/rsa"
	"crypto/sha1"
	"crypto/sha256"
	"crypto/sha512"
	"fmt"
	"io"

	"golang.org/x/crypto/argon2"
	"golang.org/x/crypto/bcrypt"
	"golang.org/x/crypto/blake2b"
	"golang.org/x/crypto/chacha20poly1305"
	"golang.org/x/crypto/curve25519"
	"golang.org/x/crypto/hkdf"
	"golang.org/x/crypto/scrypt"
)

// ─── AES-256-GCM (FIPS Approved) ─────────────────────────────────────
func EncryptAESGCM(key, plaintext, nonce []byte) ([]byte, error) {
	block, err := aes.NewCipher(key)
	if err != nil {
		return nil, err
	}
	aesGCM, err := cipher.NewGCM(block)
	if err != nil {
		return nil, err
	}
	return aesGCM.Seal(nil, nonce, plaintext, nil), nil
}

// ─── ChaCha20-Poly1305 (NOT FIPS Approved) ───────────────────────────
func EncryptChaCha(key, plaintext, nonce []byte) ([]byte, error) {
	aead, err := chacha20poly1305.New(key)
	if err != nil {
		return nil, err
	}
	return aead.Seal(nil, nonce, plaintext, nil), nil
}

// ─── RSA (FIPS Approved) ─────────────────────────────────────────────
func GenerateRSAKey() (*rsa.PrivateKey, error) {
	return rsa.GenerateKey(rand.Reader, 2048)
}

// ─── ECDSA P-256 (FIPS Approved) ─────────────────────────────────────
func GenerateECDSAKey() (*ecdsa.PrivateKey, error) {
	return ecdsa.GenerateKey(elliptic.P256(), rand.Reader)
}

// ─── ECDSA P-384 (FIPS Approved) ─────────────────────────────────────
func GenerateECDSAP384Key() (*ecdsa.PrivateKey, error) {
	return ecdsa.GenerateKey(elliptic.P384(), rand.Reader)
}

// ─── Ed25519 (FIPS Approved for signatures) ──────────────────────────
func GenerateEd25519Key() (ed25519.PublicKey, ed25519.PrivateKey, error) {
	return ed25519.GenerateKey(rand.Reader)
}

// ─── X25519 key exchange (NOT FIPS Approved) ─────────────────────────
func X25519KeyExchange(privateKey, peerPublic []byte) ([]byte, error) {
	return curve25519.X25519(privateKey, peerPublic)
}

// ─── Hash functions ──────────────────────────────────────────────────
func HashSHA256(data []byte) []byte {
	h := sha256.New()
	h.Write(data)
	return h.Sum(nil)
}

func HashSHA512(data []byte) []byte {
	h := sha512.New()
	h.Write(data)
	return h.Sum(nil)
}

func HashSHA1Legacy(data []byte) []byte {
	h := sha1.New()
	h.Write(data)
	return h.Sum(nil)
}

func HashMD5Legacy(data []byte) []byte {
	h := md5.New()
	h.Write(data)
	return h.Sum(nil)
}

func HashBLAKE2b(data []byte) []byte {
	h, _ := blake2b.New256(nil)
	h.Write(data)
	return h.Sum(nil)
}

// ─── HMAC (FIPS Approved) ────────────────────────────────────────────
func ComputeHMAC(key, message []byte) []byte {
	mac := hmac.New(sha256.New, key)
	mac.Write(message)
	return mac.Sum(nil)
}

// ─── Password hashing ───────────────────────────────────────────────
func HashPasswordBcrypt(password string) (string, error) {
	hash, err := bcrypt.GenerateFromPassword([]byte(password), bcrypt.DefaultCost)
	return string(hash), err
}

func HashPasswordArgon2(password, salt []byte) []byte {
	return argon2.IDKey(password, salt, 1, 64*1024, 4, 32)
}

func HashPasswordScrypt(password, salt []byte) ([]byte, error) {
	return scrypt.Key(password, salt, 1<<15, 8, 1, 32)
}

// ─── Key derivation (FIPS Approved) ──────────────────────────────────
func DeriveKeyHKDF(secret, salt, info []byte) ([]byte, error) {
	hkdfReader := hkdf.New(sha256.New, secret, salt, info)
	key := make([]byte, 32)
	_, err := io.ReadFull(hkdfReader, key)
	return key, err
}

func main() {
	fmt.Println("Go API Server with crypto")
}
