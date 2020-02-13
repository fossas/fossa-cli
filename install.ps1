#Requires -Version 5

<#
    .SYNOPSIS
    Download and install the latest available FOSSA release from GitHub.
#>

[CmdletBinding()]
Param()

$OldEAP = $ErrorActionPreference #Preserve the original value
$ErrorActionPreference = "Stop"

$github = "https://github.com"
$latestUri = "$github/fossas/spectrometer/releases/latest"
$extractDir = "$env:ALLUSERSPROFILE\hscli"

Write-Verbose "Looking up latest release..."

[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.SecurityProtocolType]::Tls12

$releasePage = Invoke-RestMethod $latestUri

if ($releasePage -inotmatch 'href=\"(.*?releases\/download\/.*?windows.*?)\"')
{
    throw "Did not find latest Windows release at $latestUri"
}

$downloadUri = "$github/$($Matches[1])"
Write-Verbose "Downloading from: $downloadUri"

$TempDir = Join-Path ([System.IO.Path]::GetTempPath()) "hscli"
if (![System.IO.Directory]::Exists($TempDir)) {[void][System.IO.Directory]::CreateDirectory($TempDir)}

$zipFile = "$TempDir\hscli.zip"

(New-Object System.Net.WebClient).DownloadFile($downloadUri, $zipFile)

Expand-Archive -Path $zipFile -DestinationPath $extractDir -Force

$ErrorActionPreference = $OldEAP

$hscli = "$extractDir\hscli.exe"

Write-Host "Installed fossa-hscli at: hscli"
Write-Host "Get started by running: hscli.exe --help"

Write-Output $hscli