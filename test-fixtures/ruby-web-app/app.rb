require 'openssl'
require 'digest'
require 'bcrypt'

module CryptoUtils
  # --- AES-256-GCM encryption (FIPS Approved) ---
  def self.encrypt_aes_gcm(key, plaintext)
    cipher = OpenSSL::Cipher.new('aes-256-gcm')
    cipher.encrypt
    iv = cipher.random_iv
    cipher.key = key
    encrypted = cipher.update(plaintext) + cipher.final
    tag = cipher.auth_tag
    { iv: iv, data: encrypted, tag: tag }
  end

  # --- AES-128-CBC encryption (FIPS Approved) ---
  def self.encrypt_aes_cbc(key, plaintext)
    cipher = OpenSSL::Cipher.new('aes-128-cbc')
    cipher.encrypt
    cipher.key = key
    iv = cipher.random_iv
    cipher.update(plaintext) + cipher.final
  end

  # --- ChaCha20-Poly1305 (NOT FIPS Approved) ---
  def self.encrypt_chacha(key, plaintext)
    cipher = OpenSSL::Cipher.new('chacha20-poly1305')
    cipher.encrypt
    cipher.key = key
    iv = cipher.random_iv
    cipher.update(plaintext) + cipher.final
  end

  # --- 3DES (FIPS Deprecated) ---
  def self.encrypt_3des(key, plaintext)
    cipher = OpenSSL::Cipher.new('des-ede3-cbc')
    cipher.encrypt
    cipher.key = key
    cipher.update(plaintext) + cipher.final
  end

  # --- SHA-256 hash (FIPS Approved) ---
  def self.hash_sha256(data)
    Digest::SHA256.hexdigest(data)
  end

  # --- SHA-512 hash (FIPS Approved) ---
  def self.hash_sha512(data)
    Digest::SHA512.hexdigest(data)
  end

  # --- SHA-1 hash (FIPS Deprecated) ---
  def self.hash_sha1(data)
    Digest::SHA1.hexdigest(data)
  end

  # --- MD5 hash (NOT FIPS Approved) ---
  def self.hash_md5(data)
    Digest::MD5.hexdigest(data)
  end

  # --- HMAC-SHA256 (FIPS Approved) ---
  def self.hmac_sha256(key, data)
    OpenSSL::HMAC.hexdigest('SHA256', key, data)
  end

  # --- RSA key generation (FIPS Approved) ---
  def self.generate_rsa_key
    OpenSSL::PKey::RSA.generate(2048)
  end

  # --- ECDSA P-256 (FIPS Approved) ---
  def self.generate_ec_key
    OpenSSL::PKey::EC.new('prime256v1')
  end

  # --- PBKDF2 (FIPS Approved) ---
  def self.derive_key_pbkdf2(password, salt)
    OpenSSL::PKCS5.pbkdf2_hmac(password, salt, 600_000, 32, 'SHA256')
  end

  # --- bcrypt password hashing (NOT FIPS Approved) ---
  def self.hash_password(password)
    BCrypt::Password.create(password)
  end
end
