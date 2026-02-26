using System;
using System.Security.Cryptography;
using System.Text;

namespace CryptoService
{
    public class CryptoHelper
    {
        // --- AES encryption (FIPS Approved) ---
        public static byte[] EncryptAes(byte[] key, byte[] plaintext)
        {
            using var aes = Aes.Create();
            aes.Key = key;
            aes.GenerateIV();
            return aes.EncryptCbc(plaintext, aes.IV);
        }

        // --- AES-GCM (FIPS Approved) ---
        public static byte[] EncryptAesGcm(byte[] key, byte[] plaintext, byte[] nonce)
        {
            var ciphertext = new byte[plaintext.Length];
            var tag = new byte[16];
            using var gcm = new AesGcm(key, 16);
            gcm.Encrypt(nonce, plaintext, ciphertext, tag);
            return ciphertext;
        }

        // --- 3DES (FIPS Deprecated) ---
        public static byte[] EncryptTripleDes(byte[] key, byte[] plaintext)
        {
            using var tdes = TripleDES.Create();
            tdes.Key = key;
            return tdes.EncryptCbc(plaintext, tdes.IV);
        }

        // --- SHA-256 hash (FIPS Approved) ---
        public static byte[] HashSha256(byte[] data)
        {
            using var sha = SHA256.Create();
            return sha.ComputeHash(data);
        }

        // --- SHA-512 hash (FIPS Approved) ---
        public static byte[] HashSha512(byte[] data)
        {
            using var sha = SHA512.Create();
            return sha.ComputeHash(data);
        }

        // --- SHA-1 hash (FIPS Deprecated) ---
        public static byte[] HashSha1(byte[] data)
        {
            using var sha = SHA1.Create();
            return sha.ComputeHash(data);
        }

        // --- MD5 hash (NOT FIPS Approved) ---
        public static byte[] HashMd5(byte[] data)
        {
            using var md5 = MD5.Create();
            return md5.ComputeHash(data);
        }

        // --- HMAC-SHA256 (FIPS Approved) ---
        public static byte[] HmacSha256(byte[] key, byte[] data)
        {
            using var hmac = new HMACSHA256(key);
            return hmac.ComputeHash(data);
        }

        // --- RSA key generation (FIPS Approved) ---
        public static RSA GenerateRsaKey()
        {
            return RSA.Create(2048);
        }

        // --- ECDSA (FIPS Approved) ---
        public static ECDsa GenerateEcKey()
        {
            return ECDsa.Create(ECCurve.NamedCurves.nistP256);
        }

        // --- ECDH (FIPS Approved) ---
        public static ECDiffieHellman GenerateEcdhKey()
        {
            return ECDiffieHellman.Create(ECCurve.NamedCurves.nistP256);
        }

        // --- PBKDF2 (FIPS Approved) ---
        public static byte[] DeriveKeyPbkdf2(string password, byte[] salt)
        {
            using var pbkdf2 = new Rfc2898DeriveBytes(password, salt, 600000, HashAlgorithmName.SHA256);
            return pbkdf2.GetBytes(32);
        }
    }
}
