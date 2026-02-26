<?php

use phpseclib3\Crypt\AES;
use phpseclib3\Crypt\RSA;

// --- AES-256-GCM encryption (FIPS Approved) ---
function encrypt_aes_gcm(string $key, string $plaintext): array {
    $iv = openssl_random_pseudo_bytes(12);
    $tag = '';
    $ciphertext = openssl_encrypt($plaintext, 'aes-256-gcm', $key, OPENSSL_RAW_DATA, $iv, $tag);
    return ['iv' => $iv, 'data' => $ciphertext, 'tag' => $tag];
}

// --- AES-256-CBC encryption (FIPS Approved) ---
function encrypt_aes_cbc(string $key, string $plaintext): string {
    $iv = openssl_random_pseudo_bytes(16);
    return openssl_encrypt($plaintext, 'aes-256-cbc', $key, OPENSSL_RAW_DATA, $iv);
}

// --- 3DES (FIPS Deprecated) ---
function encrypt_3des(string $key, string $plaintext): string {
    $iv = openssl_random_pseudo_bytes(8);
    return openssl_encrypt($plaintext, 'des-ede3-cbc', $key, OPENSSL_RAW_DATA, $iv);
}

// --- SHA-256 hash (FIPS Approved) ---
function hash_sha256(string $data): string {
    return hash('sha256', $data);
}

// --- SHA-1 hash (FIPS Deprecated) ---
function hash_sha1(string $data): string {
    return hash('sha1', $data);
}

// --- MD5 hash (NOT FIPS Approved) ---
function hash_md5(string $data): string {
    return hash('md5', $data);
}

// --- HMAC-SHA256 (FIPS Approved) ---
function hmac_sha256(string $key, string $data): string {
    return hash_hmac('sha256', $data, $key);
}

// --- RSA key generation (FIPS Approved) ---
function generate_rsa_key(): OpenSSLAsymmetricKey {
    return openssl_pkey_new([
        'private_key_bits' => 2048,
        'private_key_type' => OPENSSL_KEYTYPE_RSA,
    ]);
}

// --- Sodium: Ed25519 signing (FIPS Approved) ---
function generate_signing_keypair(): string {
    return sodium_crypto_sign_keypair();
}

// --- Sodium: X25519 key exchange (NOT FIPS Approved) ---
function generate_box_keypair(): string {
    return sodium_crypto_box_keypair();
}

// --- Sodium: ChaCha20-Poly1305 (NOT FIPS Approved) ---
function encrypt_chacha(string $key, string $nonce, string $plaintext, string $ad): string {
    return sodium_crypto_aead_chacha20poly1305_encrypt($plaintext, $ad, $nonce, $key);
}

// --- Sodium: BLAKE2b (NOT FIPS Approved) ---
function generic_hash(string $data): string {
    return sodium_crypto_generichash($data);
}

// --- Sodium: Argon2 password hashing (NOT FIPS Approved) ---
function hash_password(string $password): string {
    return sodium_crypto_pwhash(
        32, $password, random_bytes(16),
        SODIUM_CRYPTO_PWHASH_OPSLIMIT_INTERACTIVE,
        SODIUM_CRYPTO_PWHASH_MEMLIMIT_INTERACTIVE
    );
}

// --- bcrypt password hashing (NOT FIPS Approved) ---
function hash_password_bcrypt(string $password): string {
    return password_hash($password, PASSWORD_BCRYPT);
}
