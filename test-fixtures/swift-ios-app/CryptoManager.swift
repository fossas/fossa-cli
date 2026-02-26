import CryptoKit
import CommonCrypto
import Foundation

struct CryptoManager {

    // --- AES-GCM encryption (FIPS Approved) ---
    static func encryptAesGcm(key: SymmetricKey, plaintext: Data) throws -> AES.GCM.SealedBox {
        return try AES.GCM.seal(plaintext, using: key)
    }

    static func decryptAesGcm(key: SymmetricKey, sealedBox: AES.GCM.SealedBox) throws -> Data {
        return try AES.GCM.open(sealedBox, using: key)
    }

    // --- ChaCha20-Poly1305 (NOT FIPS Approved) ---
    static func encryptChaCha(key: SymmetricKey, plaintext: Data) throws -> ChaChaPoly.SealedBox {
        return try ChaChaPoly.seal(plaintext, using: key)
    }

    // --- SHA-256 hash (FIPS Approved) ---
    static func hashSha256(data: Data) -> SHA256Digest {
        return SHA256.hash(data: data)
    }

    // --- SHA-384 hash (FIPS Approved) ---
    static func hashSha384(data: Data) -> SHA384Digest {
        return SHA384.hash(data: data)
    }

    // --- SHA-512 hash (FIPS Approved) ---
    static func hashSha512(data: Data) -> SHA512Digest {
        return SHA512.hash(data: data)
    }

    // --- SHA-1 hash (FIPS Deprecated) ---
    static func hashSha1(data: Data) -> Insecure.SHA1.Digest {
        return Insecure.SHA1.hash(data: data)
    }

    // --- MD5 hash (NOT FIPS Approved) ---
    static func hashMd5(data: Data) -> Insecure.MD5.Digest {
        return Insecure.MD5.hash(data: data)
    }

    // --- HMAC-SHA256 (FIPS Approved) ---
    static func hmacSha256(key: SymmetricKey, data: Data) -> HMAC<SHA256>.MAC {
        return HMAC<SHA256>.authenticationCode(for: data, using: key)
    }

    // --- ECDSA P-256 signing (FIPS Approved) ---
    static func signP256(data: Data) throws -> (P256.Signing.PrivateKey, P256.Signing.ECDSASignature) {
        let privateKey = P256.Signing.PrivateKey()
        let signature = try privateKey.signature(for: data)
        return (privateKey, signature)
    }

    // --- ECDSA P-384 signing (FIPS Approved) ---
    static func signP384(data: Data) throws -> P384.Signing.ECDSASignature {
        let privateKey = P384.Signing.PrivateKey()
        return try privateKey.signature(for: data)
    }

    // --- Ed25519 / Curve25519 signing (FIPS Approved) ---
    static func signEd25519(data: Data) throws -> Data {
        let privateKey = Curve25519.Signing.PrivateKey()
        return try privateKey.signature(for: data)
    }

    // --- X25519 key agreement (NOT FIPS Approved) ---
    static func keyAgreement(privateKey: Curve25519.KeyAgreement.PrivateKey, publicKey: Curve25519.KeyAgreement.PublicKey) throws -> SharedSecret {
        return try privateKey.sharedSecretFromKeyAgreement(with: publicKey)
    }

    // --- ECDH P-256 key agreement (FIPS Approved) ---
    static func ecdhP256(privateKey: P256.KeyAgreement.PrivateKey, publicKey: P256.KeyAgreement.PublicKey) throws -> SharedSecret {
        return try privateKey.sharedSecretFromKeyAgreement(with: publicKey)
    }

    // --- PBKDF2 via CommonCrypto (FIPS Approved) ---
    static func deriveKeyPbkdf2(password: String, salt: Data) -> Data {
        var derivedKey = Data(count: 32)
        derivedKey.withUnsafeMutableBytes { derivedBytes in
            salt.withUnsafeBytes { saltBytes in
                CCKeyDerivationPBKDF(
                    CCPBKDFAlgorithm(kCCPBKDF2),
                    password, password.utf8.count,
                    saltBytes.baseAddress!, salt.count,
                    CCPseudoRandomAlgorithm(kCCPRFHmacAlgSHA256),
                    600_000,
                    derivedBytes.baseAddress!, 32
                )
            }
        }
        return derivedKey
    }
}
