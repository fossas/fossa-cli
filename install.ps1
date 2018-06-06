#Requires -Version 5

<#
    .SYNOPSIS
    Executes the latest available FOSSA release from GitHub.
    
    .PARAMETER ApiKey
    The FOSSA API key to be used for the current Powershell Session.

    .PARAMETER TempDir
    The directory used for FOSSA download and extraction.
#>

[CmdletBinding()]
Param(
    $TempDir = $env:TEMP,
    [Parameter(Mandatory=$true)]
    $ApiKey
)

$ErrorActionPreference = "Stop"

$github = "https://github.com"
$latestUri = "$github/fossas/fossa-cli/releases/latest"
$extractDir = "$env:ALLUSERSPROFILE\fossa-cli"

[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.SecurityProtocolType]::Tls12

$releasePage = Invoke-RestMethod $latestUri

if ($releasePage -inotmatch 'href=\"(.*?releases\/download\/.*?windows.*?)\"')
{
    throw "Did not find latest Windows release at $latestUri"
}

$downloadUri = "$github/$($Matches[1])"
Write-Verbose "Download from: $downloadUri"

$zipFile = "$TempDir\fossa-cli.zip"

(New-Object System.Net.WebClient).DownloadFile($downloadUri, $zipFile)

Expand-Archive -Path $zipFile -DestinationPath $extractDir -Force

$ErrorActionPreference = "Continue"

$env:FOSSA_API_KEY = $ApiKey

& "$TempDir\fossa-cli\fossa.exe"
