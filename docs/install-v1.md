## Installing CLI 1.x

You can install FOSSA CLI 1.x with installation script for macOS or 64-bit Linux using:

```bash
curl -H 'Cache-Control: no-cache' https://raw.githubusercontent.com/fossas/fossa-cli/master/install-v1.sh | bash
```

And for windows:

```powershell
Set-ExecutionPolicy Bypass -Scope Process -Force; iex  ((New-Object System.Net.WebClient).DownloadString('https://raw.githubusercontent.com/fossas/fossa-cli/master/install-v1.ps1'))
```

It is recommended that you migrate to CLI 3.x. Please read the [migration guide](./differences-from-v1.md) for more details.