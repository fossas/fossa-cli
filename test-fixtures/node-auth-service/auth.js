const crypto = require('crypto');
const bcrypt = require('bcrypt');

// ─── AES-256-GCM encryption (FIPS Approved) ─────────────────────────
function encryptAesGcm(key, plaintext) {
  const iv = crypto.randomBytes(12);
  const cipher = crypto.createCipheriv('aes-256-gcm', key, iv);
  const encrypted = Buffer.concat([cipher.update(plaintext), cipher.final()]);
  return { iv, encrypted, tag: cipher.getAuthTag() };
}

// ─── AES-128-CBC encryption (FIPS Approved) ─────────────────────────
function encryptAesCbc(key, plaintext) {
  const iv = crypto.randomBytes(16);
  const cipher = crypto.createCipheriv('aes-128-cbc', key, iv);
  return Buffer.concat([iv, cipher.update(plaintext), cipher.final()]);
}

// ─── ChaCha20-Poly1305 (NOT FIPS Approved) ──────────────────────────
function encryptChaCha(key, plaintext) {
  const iv = crypto.randomBytes(12);
  const cipher = crypto.createCipheriv('chacha20-poly1305', key, iv, { authTagLength: 16 });
  return Buffer.concat([iv, cipher.update(plaintext), cipher.final(), cipher.getAuthTag()]);
}

// ─── 3DES (FIPS Deprecated) ─────────────────────────────────────────
function encryptTripleDes(key, plaintext) {
  const iv = crypto.randomBytes(8);
  const cipher = crypto.createCipheriv('des-ede3-cbc', key, iv);
  return Buffer.concat([iv, cipher.update(plaintext), cipher.final()]);
}

// ─── SHA-256 hash (FIPS Approved) ───────────────────────────────────
function hashSha256(data) {
  return crypto.createHash('sha256').update(data).digest('hex');
}

// ─── SHA-1 hash (FIPS Deprecated) ───────────────────────────────────
function hashSha1(data) {
  return crypto.createHash('sha1').update(data).digest('hex');
}

// ─── MD5 hash (NOT FIPS Approved) ───────────────────────────────────
function hashMd5(data) {
  return crypto.createHash('md5').update(data).digest('hex');
}

// ─── HMAC-SHA256 (FIPS Approved) ────────────────────────────────────
function hmacSha256(key, data) {
  return crypto.createHmac('sha256', key).update(data).digest('hex');
}

// ─── HMAC-SHA1 (FIPS Deprecated) ────────────────────────────────────
function hmacSha1(key, data) {
  return crypto.createHmac('sha1', key).update(data).digest('hex');
}

// ─── RSA key generation (FIPS Approved) ─────────────────────────────
function generateRsaKeyPair() {
  return crypto.generateKeyPairSync('rsa', {
    modulusLength: 2048,
    publicKeyEncoding: { type: 'pkcs1', format: 'pem' },
    privateKeyEncoding: { type: 'pkcs1', format: 'pem' },
  });
}

// ─── Ed25519 key generation (FIPS Approved) ─────────────────────────
function generateEd25519KeyPair() {
  return crypto.generateKeyPairSync('ed25519', {
    publicKeyEncoding: { type: 'spki', format: 'pem' },
    privateKeyEncoding: { type: 'pkcs8', format: 'pem' },
  });
}

// ─── X25519 key generation (NOT FIPS Approved) ──────────────────────
function generateX25519KeyPair() {
  return crypto.generateKeyPairSync('x25519', {
    publicKeyEncoding: { type: 'spki', format: 'pem' },
    privateKeyEncoding: { type: 'pkcs8', format: 'pem' },
  });
}

// ─── PBKDF2 (FIPS Approved) ─────────────────────────────────────────
function deriveKeyPbkdf2(password, salt) {
  return crypto.pbkdf2Sync(password, salt, 600000, 32, 'sha256');
}

// ─── scrypt (NOT FIPS Approved) ─────────────────────────────────────
function deriveKeyScrypt(password, salt) {
  return crypto.scryptSync(password, salt, 32);
}

// ─── bcrypt (NOT FIPS Approved) ─────────────────────────────────────
async function hashPasswordBcrypt(password) {
  return bcrypt.hash(password, 10);
}

module.exports = {
  encryptAesGcm, encryptAesCbc, encryptChaCha, encryptTripleDes,
  hashSha256, hashSha1, hashMd5, hmacSha256, hmacSha1,
  generateRsaKeyPair, generateEd25519KeyPair, generateX25519KeyPair,
  deriveKeyPbkdf2, deriveKeyScrypt, hashPasswordBcrypt,
};
