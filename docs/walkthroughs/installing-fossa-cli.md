# Installing Fossa CLI

## Using Installation script

FOSSA CLI provides an install script that downloads the latest release from GitHub Releases for your computer's architecture. You can see the source code and flags at [`install-latest.sh`](../../install-latest.sh) for Mac and Linux or [`install-latest.ps1`](../../install-latest.ps1) for Windows.

**NOTE:** You may need to add the downloaded executable to your `$PATH`. The installer script will output the installed path of the executable. You can also use `-b` to pick the installation directory when using `install-latest.sh` (see [the `install-latest.sh` source code](../../install-latest.sh) for details).

### Linux or macOS

```bash
# Install latest version
curl -H 'Cache-Control: no-cache' https://raw.githubusercontent.com/fossas/fossa-cli/master/install-latest.sh | bash

# Installs cli version v3.1.1 instead of latest
curl -H 'Cache-Control: no-cache' https://raw.githubusercontent.com/fossas/fossa-cli/master/install-latest.sh | bash -s -- v3.1.1

# Installs cli version v3.1.1 instead of latest at current working directory
curl -H 'Cache-Control: no-cache' https://raw.githubusercontent.com/fossas/fossa-cli/master/install-latest.sh | bash -s -- -b . v3.1.1

# Installs cli version v3.1.1 instead of latest at current working directory in debug mode
curl -H 'Cache-Control: no-cache' https://raw.githubusercontent.com/fossas/fossa-cli/master/install-latest.sh | bash -s -- -b . -d v3.1.1
```

### Windows

```powershell
Set-ExecutionPolicy Bypass -Scope Process -Force; iex  ((New-Object System.Net.WebClient).DownloadString('https://raw.githubusercontent.com/fossas/fossa-cli/master/install-latest.ps1'))
```
<!-- markdown-link-check-disable-next-line -->
Alternatively, install using [Scoop](https://scoop.sh/):

```
scoop install fossa
```

Likewise with `scoop` you can install a specific version of CLI:

```powershell
scoop install fossa@3.2.9
```

## Installing CLI 1.x using Installation script

You can install FOSSA CLI 1.x with installation script for macOS or 64-bit Linux using:

```bash
curl -H 'Cache-Control: no-cache' https://raw.githubusercontent.com/fossas/fossa-cli/master/install-v1.sh | bash
```

And for windows:

```powershell
Set-ExecutionPolicy Bypass -Scope Process -Force; iex  ((New-Object System.Net.WebClient).DownloadString('https://raw.githubusercontent.com/fossas/fossa-cli/master/install-v1.ps1'))
```

> As of now, there is no development work being done on CLI 1.x. Likewise, support for CLI 1.x is deprecated. If there is a defect with CLI 1.x, we will not make patches to CLI 1.x anymore but instead will ask you to migrate to 3.x, and make necessary patches to CLI 3.x. It is recommended that you migrate to CLI 3.x. Please read the [migration guide](./../differences-from-v1.md) for more details.

## Installing manually with Github Releases

1) To download FOSSA CLI manually using GITHUB releases, identify release of interest by going to [releases](https://github.com/fossas/fossa-cli/releases/)

Example: https://github.com/fossas/fossa-cli/releases/tag/v3.2.17

2) Identify release artifact for your system's OS and ARCH by looking at release [assets](https://github.com/fossas/fossa-cli/releases/tag/v3.2.17)

Example: https://github.com/fossas/fossa-cli/releases/download/v3.2.17/fossa_3.2.17_darwin_amd64.zip

3) Identify release checksum to verify the release artifact

Example: https://github.com/fossas/fossa-cli/releases/download/v3.2.17/fossa_3.2.17_darwin_amd64.zip.sha256

4) Download release artifact, and release artifact checksum

Example:
```bash
wget https://github.com/fossas/fossa-cli/releases/download/v3.2.17/fossa_3.2.17_darwin_amd64.zip
wget https://github.com/fossas/fossa-cli/releases/download/v3.2.17/fossa_3.2.17_darwin_amd64.zip.sha256
```

5) Confirm sanity of the release artifact by comparing sha256 checksum

```bash
sha256sum --binary fossa_3.2.17_darwin_amd64.zip
```

```bash
cat fossa_3.2.17_darwin_amd64.zip.sha256
```

You can also compare output using diff:
```bash
diff <(sha256sum --binary fossa_3.2.17_darwin_amd64.zip) <(cat fossa_3.2.17_darwin_amd64.zip.sha256)
```

6) Extract and copy the binary to `$PATH`

```bash
unzip fossa_3.2.17_darwin_amd64.zip
mv fossa /usr/local/bin/fossa
```

7) Confirm the fossa version using `--version`

```bash
fossa --version
```



