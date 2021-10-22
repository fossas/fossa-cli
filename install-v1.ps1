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
$github = "https://github.com"
$latestUri = "$github/fossas/fossa-cli/releases/$release_v1_upperbound"
$extractDir = "$env:ALLUSERSPROFILE\fossa-cli"

Write-Verbose "Looking up latest v1 release..."

[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.SecurityProtocolType]::Tls12

$releasePage = Invoke-RestMethod $latestUri

if ($releasePage -inotmatch 'href=\"(.*?releases\/download\/.*?windows.*?)\"')
{
    throw "Did not find latest Windows release at $latestUri"
}

$downloadUri = "$github/$($Matches[1])"
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