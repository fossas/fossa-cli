"""Example Python web app with various crypto usage patterns."""
import hashlib
import hmac
import os

import bcrypt
from cryptography.hazmat.primitives.ciphers import Cipher, algorithms, modes
from cryptography.hazmat.primitives.asymmetric import rsa, ec, ed25519
from cryptography.hazmat.primitives import hashes, serialization
from cryptography.hazmat.primitives.kdf.hkdf import HKDF
from cryptography.hazmat.primitives.kdf.pbkdf2 import PBKDF2HMAC
from cryptography.hazmat.primitives.kdf.scrypt import Scrypt


# ─── AES-256-GCM encryption (FIPS Approved) ─────────────────────────
def encrypt_data(key: bytes, plaintext: bytes) -> tuple[bytes, bytes, bytes]:
    iv = os.urandom(12)
    cipher = Cipher(algorithms.AES(key), modes.GCM(iv))
    encryptor = cipher.encryptor()
    ciphertext = encryptor.update(plaintext) + encryptor.finalize()
    return iv, ciphertext, encryptor.tag


# ─── AES-CBC encryption (FIPS Approved) ──────────────────────────────
def encrypt_legacy(key: bytes, plaintext: bytes) -> tuple[bytes, bytes]:
    iv = os.urandom(16)
    cipher = Cipher(algorithms.AES(key), modes.CBC(iv))
    encryptor = cipher.encryptor()
    ciphertext = encryptor.update(plaintext) + encryptor.finalize()
    return iv, ciphertext


# ─── ChaCha20 encryption (NOT FIPS Approved) ────────────────────────
def encrypt_fast(key: bytes, plaintext: bytes) -> bytes:
    nonce = os.urandom(16)
    cipher = Cipher(algorithms.ChaCha20(key, nonce), mode=None)
    encryptor = cipher.encryptor()
    return nonce + encryptor.update(plaintext) + encryptor.finalize()


# ─── RSA key generation (FIPS Approved) ──────────────────────────────
def generate_rsa_keypair():
    private_key = rsa.generate_private_key(
        public_exponent=65537,
        key_size=2048,
    )
    return private_key


# ─── ECDSA with P-256 (FIPS Approved) ───────────────────────────────
def generate_ecdsa_keypair():
    private_key = ec.generate_private_key(ec.SECP256R1())
    return private_key


# ─── Ed25519 signatures (FIPS Approved) ──────────────────────────────
def generate_ed25519_keypair():
    private_key = ed25519.Ed25519PrivateKey.generate()
    return private_key


# ─── Hash functions ──────────────────────────────────────────────────
def hash_sha256(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()

def hash_sha1_legacy(data: bytes) -> str:
    """SHA-1 is deprecated but still used in legacy systems."""
    return hashlib.sha1(data).hexdigest()

def hash_md5_checksum(data: bytes) -> str:
    """MD5 is NOT FIPS approved, used only for non-security checksums."""
    return hashlib.md5(data).hexdigest()

def hash_blake2(data: bytes) -> str:
    """BLAKE2 is NOT FIPS approved."""
    return hashlib.blake2b(data).hexdigest()


# ─── HMAC (FIPS Approved) ───────────────────────────────────────────
def create_hmac(key: bytes, message: bytes) -> str:
    return hmac.new(key, message, hashlib.sha256).hexdigest()


# ─── Password hashing ───────────────────────────────────────────────
def hash_password_bcrypt(password: str) -> bytes:
    """bcrypt is NOT FIPS approved."""
    return bcrypt.hashpw(password.encode(), bcrypt.gensalt())


# ─── Key derivation ─────────────────────────────────────────────────
def derive_key_hkdf(master_key: bytes, info: bytes) -> bytes:
    """HKDF is FIPS Approved."""
    hkdf = HKDF(
        algorithm=hashes.SHA256(),
        length=32,
        salt=None,
        info=info,
    )
    return hkdf.derive(master_key)

def derive_key_pbkdf2(password: bytes, salt: bytes) -> bytes:
    """PBKDF2 is FIPS Approved."""
    kdf = PBKDF2HMAC(
        algorithm=hashes.SHA256(),
        length=32,
        salt=salt,
        iterations=600000,
    )
    return kdf.derive(password)

def derive_key_scrypt(password: bytes, salt: bytes) -> bytes:
    """scrypt is NOT FIPS Approved."""
    kdf = Scrypt(salt=salt, length=32, n=2**14, r=8, p=1)
    return kdf.derive(password)
