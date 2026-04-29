defmodule PhoenixApp.Crypto do
  @moduledoc """
  Cryptographic utilities for the Phoenix application.
  """

  # --- AES-256-GCM encryption (FIPS Approved) ---
  def encrypt_aes_gcm(key, plaintext) do
    iv = :crypto.strong_rand_bytes(12)
    {ciphertext, tag} = :crypto.crypto_one_time(:aes_256_gcm, key, iv, plaintext)
    {iv, ciphertext, tag}
  end

  # --- AES-128-GCM encryption (FIPS Approved) ---
  def encrypt_aes_128_gcm(key, plaintext) do
    iv = :crypto.strong_rand_bytes(12)
    {ciphertext, tag} = :crypto.crypto_one_time(:aes_128_gcm, key, iv, plaintext)
    {iv, ciphertext, tag}
  end

  # --- ChaCha20-Poly1305 (NOT FIPS Approved) ---
  def encrypt_chacha(key, plaintext) do
    iv = :crypto.strong_rand_bytes(12)
    :crypto.crypto_one_time(:chacha20_poly1305, key, iv, plaintext)
  end

  # --- 3DES (FIPS Deprecated) ---
  def encrypt_3des(key, plaintext) do
    iv = :crypto.strong_rand_bytes(8)
    :crypto.block_encrypt(:des_ede3_cbc, key, iv, plaintext)
  end

  # --- SHA-256 hash (FIPS Approved) ---
  def hash_sha256(data) do
    :crypto.hash(:sha256, data)
  end

  # --- SHA-512 hash (FIPS Approved) ---
  def hash_sha512(data) do
    :crypto.hash(:sha512, data)
  end

  # --- SHA-1 hash (FIPS Deprecated) ---
  def hash_sha1(data) do
    :crypto.hash(:sha, data)
  end

  # --- MD5 hash (NOT FIPS Approved) ---
  def hash_md5(data) do
    :crypto.hash(:md5, data)
  end

  # --- HMAC-SHA256 (FIPS Approved) ---
  def hmac_sha256(key, data) do
    :crypto.mac(:hmac, :sha256, key, data)
  end

  # --- HMAC-SHA512 (FIPS Approved) ---
  def hmac_sha512(key, data) do
    :crypto.mac(:hmac, :sha512, key, data)
  end

  # --- RSA key generation (FIPS Approved) ---
  def generate_rsa_key do
    :crypto.generate_key(:rsa, {2048, 65537})
  end

  # --- ECDH key generation (FIPS Approved) ---
  def generate_ecdh_key do
    :crypto.generate_key(:ecdh, :secp256r1)
  end

  # --- bcrypt password hashing (NOT FIPS Approved) ---
  def hash_password_bcrypt(password) do
    Bcrypt.hash_pwd_salt(password)
  end

  def verify_password_bcrypt(password, hash) do
    Bcrypt.verify_pass(password, hash)
  end

  # --- Argon2 password hashing (NOT FIPS Approved) ---
  def hash_password_argon2(password) do
    Argon2.hash_pwd_salt(password)
  end

  # --- PBKDF2 (FIPS Approved) ---
  def hash_password_pbkdf2(password) do
    Pbkdf2.hash_pwd_salt(password)
  end
end
