package com.example;

import javax.crypto.Cipher;
import javax.crypto.KeyAgreement;
import javax.crypto.Mac;
import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.GCMParameterSpec;
import javax.crypto.spec.PBEKeySpec;
import javax.crypto.spec.SecretKeySpec;
import java.security.KeyPairGenerator;
import java.security.MessageDigest;
import java.security.SecureRandom;

/**
 * Example Java microservice demonstrating various crypto patterns.
 */
public class CryptoService {

    // ─── AES-GCM encryption (FIPS Approved) ─────────────────────────
    public byte[] encryptAesGcm(byte[] key, byte[] plaintext) throws Exception {
        Cipher cipher = Cipher.getInstance("AES/GCM/NoPadding");
        byte[] iv = new byte[12];
        new SecureRandom().nextBytes(iv);
        GCMParameterSpec spec = new GCMParameterSpec(128, iv);
        cipher.init(Cipher.ENCRYPT_MODE, new SecretKeySpec(key, "AES"), spec);
        return cipher.doFinal(plaintext);
    }

    // ─── AES-ECB encryption (FIPS Deprecated) ───────────────────────
    public byte[] encryptAesEcb(byte[] key, byte[] plaintext) throws Exception {
        Cipher cipher = Cipher.getInstance("AES/ECB/PKCS5Padding");
        cipher.init(Cipher.ENCRYPT_MODE, new SecretKeySpec(key, "AES"));
        return cipher.doFinal(plaintext);
    }

    // ─── DESede / 3DES (FIPS Deprecated) ────────────────────────────
    public byte[] encryptTripleDes(byte[] key, byte[] plaintext) throws Exception {
        Cipher cipher = Cipher.getInstance("DESede/CBC/PKCS5Padding");
        cipher.init(Cipher.ENCRYPT_MODE, new SecretKeySpec(key, "DESede"));
        return cipher.doFinal(plaintext);
    }

    // ─── RSA key pair (FIPS Approved) ───────────────────────────────
    public void generateRsaKeyPair() throws Exception {
        KeyPairGenerator kpg = KeyPairGenerator.getInstance("RSA");
        kpg.initialize(2048);
        kpg.generateKeyPair();
    }

    // ─── ECDSA key pair (FIPS Approved) ─────────────────────────────
    public void generateEcKeyPair() throws Exception {
        KeyPairGenerator kpg = KeyPairGenerator.getInstance("EC");
        kpg.initialize(256);
        kpg.generateKeyPair();
    }

    // ─── Ed25519 key pair (FIPS Approved) ───────────────────────────
    public void generateEd25519KeyPair() throws Exception {
        KeyPairGenerator kpg = KeyPairGenerator.getInstance("Ed25519");
        kpg.generateKeyPair();
    }

    // ─── DH key agreement (FIPS Approved) ───────────────────────────
    public void performDhKeyAgreement() throws Exception {
        KeyAgreement ka = KeyAgreement.getInstance("DH");
    }

    // ─── X25519 key agreement (NOT FIPS Approved) ───────────────────
    public void performX25519KeyAgreement() throws Exception {
        KeyAgreement ka = KeyAgreement.getInstance("X25519");
    }

    // ─── SHA-256 hash (FIPS Approved) ───────────────────────────────
    public byte[] hashSha256(byte[] data) throws Exception {
        MessageDigest md = MessageDigest.getInstance("SHA-256");
        return md.digest(data);
    }

    // ─── SHA-1 hash (FIPS Deprecated) ───────────────────────────────
    public byte[] hashSha1(byte[] data) throws Exception {
        MessageDigest md = MessageDigest.getInstance("SHA-1");
        return md.digest(data);
    }

    // ─── MD5 hash (NOT FIPS Approved) ───────────────────────────────
    public byte[] hashMd5(byte[] data) throws Exception {
        MessageDigest md = MessageDigest.getInstance("MD5");
        return md.digest(data);
    }

    // ─── HMAC-SHA256 (FIPS Approved) ────────────────────────────────
    public byte[] hmacSha256(byte[] key, byte[] data) throws Exception {
        Mac mac = Mac.getInstance("HmacSHA256");
        mac.init(new SecretKeySpec(key, "HmacSHA256"));
        return mac.doFinal(data);
    }

    // ─── PBKDF2 (FIPS Approved) ─────────────────────────────────────
    public byte[] deriveKeyPbkdf2(char[] password, byte[] salt) throws Exception {
        SecretKeyFactory factory = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA256");
        PBEKeySpec spec = new PBEKeySpec(password, salt, 600000, 256);
        return factory.generateSecret(spec).getEncoded();
    }
}
