## Experimental Scanner

Usage:

```bash
fossa container analyze redis:alpine --experimental-scanner --output
```

## Current Limitation

As this functionality is in active development, following is not yet supported with experimental scanner. 
It is expected that these functionalities will be supported in future. 

For backwards compatibility: 
- Dpkg Scanning (for debian files)
- Rpm Scanning
- Image sourced from OCI Registry
- Image sourced from private OCI Registry
- Wider support for OSs.

New Functionalities:
- Application Dependencies (python, nodejs projects etc.)
- Container Scanning in windows
- Improved runtime performance (time spent to analyze image)
- Path Exclusions (via configuration file)

### What's new in Experimental Scanner

With Experimental Scanner, FOSSA can support following:

- Origin Paths (in FOSSA's dependency view) for alpine dependencies
- Better Logging and Diagnostics Supports from FOSSA support team
- Improved Runtime performance.

### Docker Engine Supports

`fossa-cli` can analyze docker image from docker engine api. This requires docker engine (daemon) to
be running and accessible to `fossa-cli`.

To retrieve docker image from docker engine api, 

1) We perform `/_ping` to check if docker engine api is accessible

```bash
# Equivalent curl command is:
curl --unix-socket /var/run/docker.sock -X GET "http://localhost/v1.28/_ping"
```

2) We perform `/images/<IMAGE>/json` to see if we can identify image's total size in bytes. This also throws 404 if image is not present

```bash
# Equivalent curl command is:
curl --unix-socket /var/run/docker.sock -X GET "http://localhost/v1.28/images/redis:alpine/json"
```

3) We perform `/images/<IMAGE>/get` to retrieve raw image in tarball format, which we write to temp file in system temp folder. 

```bash
# Equivalent curl command is:
curl --unix-socket /var/run/docker.sock -X GET "http://localhost/v1.28/images/redis:alpine/get" > img.tar
```

For more information on docker engine API, refer to [docs](https://docs.docker.com/engine/api/v1.28/#). 

To ensure docker engine source works, please ensure image is locally accessible: perform `docker pull IMAGE`, if it is not locally present.

