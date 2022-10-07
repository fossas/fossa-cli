## Custom SSL Certificate with `fossa-cli`

`fossa-cli` uses the path provided in the following environment variable to infer root certificate store:

1) `SSL_CERT_FILE`
2) `SSL_CERT_DIR`
3) `SYSTEM_CERTIFICATE_PATH` (only for linux)

If none of these environment variables are provided, fossa-cli uses a system store to retrieve signed certificates.

#### Linux

`fossa-cli` looks for certificate store in the following paths:

- `/etc/ssl/certs/`  
- `/system/etc/security/cacerts/`
- `/usr/local/share/certs/`
- `/etc/ssl/cert.pem`

#### macOS

`fossa-cli` creates a certificate store by finding all signed certificates.  

We use following command to retrieve all signed certificates: `security find-certificate -pa /System/Library/Keychains/SystemRootCertificates.keychain /Library/Keychains/System.keychain`


#### Windows

`fossa-cli` retrieves certificates store by performing,

- [CertOpenSystemStoreW](https://docs.microsoft.com/en-us/windows/win32/api/wincrypt/nf-wincrypt-certopensystemstorew)

From which, [CertEnumCertificatesInStore](https://docs.microsoft.com/en-us/windows/win32/api/wincrypt/nf-wincrypt-certenumcertificatesinstore) perform until exhaustion to retrieve all signed certificates. 

## Recommendations

_We recommended that you leverage the operating system's certificate store instead of using environment variables._

## Examples

In Windows:
```
$Env:SSL_CERT_FILE = "\path\to\rootCa"
fossa analyze
```

In Linux:
```
SSL_CERT_FILE=path/to/rootCa fossa analyze
```

In Osx:
```
SSL_CERT_FILE=path/to/rootCa fossa analyze
```
