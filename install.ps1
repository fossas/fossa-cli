#Requires -Version 5

<#
    .SYNOPSIS
    Download and install the latest available v1 FOSSA release from GitHub.
#>

[CmdletBinding()]
Param()

$OldEAP = $ErrorActionPreference #Preserve the original value
$ErrorActionPreference = "Stop"
$release_v1_upperbound="v1.1.10"
$releaseVersionSemver = $release_v1_upperbound.TrimStart("v");
$github = "https://github.com"
$extractDir = "$env:ALLUSERSPROFILE\fossa-cli"

Write-host "`n"
Write-Host "-------------------"
Write-Host "Deprecation Warning"
Write-Host "-------------------"
Write-host "`n"
Write-Host "You are installing FOSSA CLI v1, which is no longer in active"
Write-Host "development. FOSSA will not address new defects found in CLI v1."
Write-host "`n"
Write-Host "Please upgrade to the latest FOSSA CLI."
Write-host "`n"
Write-Host "Please upgrade to FOSSA CLI v3 by using install-latest script:"
Write-Host "--------------------------------------------------------------"
Write-Host "    Set-ExecutionPolicy Bypass -Scope Process -Force; iex  ((New-Object System.Net.WebClient).DownloadString('https://raw.githubusercontent.com/fossas/fossa-cli/master/install-latest.ps1'))"
Write-host "`n"
Write-Host "Migration guide for FOSSA CLI v3:"
Write-Host "---------------------------------"
Write-Host "    https://github.com/fossas/fossa-cli/blob/master/docs/differences-from-v1.md#how-to-upgrade-to-fossa-3x"
Write-host "`n"

[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.SecurityProtocolType]::Tls12

$downloadPath = "fossas/fossa-cli/releases/download/$($release_v1_upperbound)/fossa-cli_$($releaseVersionSemver)_windows_amd64.zip"
$downloadUri = "$github/$downloadPath"
Write-Verbose "Downloading from: $downloadUri"

$TempDir = Join-Path ([System.IO.Path]::GetTempPath()) "fossa-cli"
if (![System.IO.Directory]::Exists($TempDir)) {[void][System.IO.Directory]::CreateDirectory($TempDir)}

$zipFile = "$TempDir\fossa-cli.zip"

(New-Object System.Net.WebClient).DownloadFile($downloadUri, $zipFile)

Expand-Archive -Path $zipFile -DestinationPath $extractDir -Force

$ErrorActionPreference = $OldEAP

$fossa = "$extractDir\fossa.exe"

Write-Host "Installed fossa-cli at: $fossa"
Write-Host "Get started by running: fossa.exe --help"

Write-Output $fossa
