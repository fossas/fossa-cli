#Requires -Version 5

<#
    .SYNOPSIS
    Download and install the latest available FOSSA release from GitHub.
#>

[CmdletBinding()]
Param()

$OldEAP = $ErrorActionPreference #Preserve the original value
$ErrorActionPreference = "Stop"

# Set to default if null
if ($env:FOSSA_RELEASE)
{
    if ($env:FOSSA_RELEASE -inotmatch '^v\d.\d+.\d+$')
    {
        throw "FOSSA_RELEASE must be in the format of v2.x.x (e.g.: 'v2.0.1')"
    }
    $releaseTag = "$env:FOSSA_RELEASE"
}
else 
{
    $releaseTag = "latest"
}

$github = "https://github.com"
$latestUri = "$github/fossas/spectrometer/releases/$releaseTag"
$userExtractDir = "$env:LOCALAPPDATA\fossa-cli"
$allUsersExtractDir = "$env:PROGRAMFILES\fossa-cli"

if ($env:FOSSA_ALL_USERS)
{
    $currentPrincipal = New-Object Security.Principal.WindowsPrincipal([Security.Principal.WindowsIdentity]::GetCurrent())
    if (-not $currentPrincipal.IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator))
    {
        throw "Cannot install for all users without admin privleges.  Please run powershell as administrator to install for all users."
    }
    $extractDir = "$allUsersExtractDir"
}
else 
{
    $extractDir = "$userExtractDir"
}

Write-Verbose "Looking up release ($releaseTag)..."

[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.SecurityProtocolType]::Tls12

$releasePage = Invoke-RestMethod $latestUri

if ($releasePage -inotmatch 'href=\"(.*?releases\/download\/.*?windows.*?)\"')
{
    throw "Did not find Windows release at $latestUri"
}

$downloadUri = "$github/$($Matches[1])"
Write-Verbose "Downloading from: $downloadUri"

$TempDir = Join-Path ([System.IO.Path]::GetTempPath()) "fossa"
if (![System.IO.Directory]::Exists($TempDir)) {[void][System.IO.Directory]::CreateDirectory($TempDir)}

$zipFile = "$TempDir\fossa.zip"

(New-Object System.Net.WebClient).DownloadFile($downloadUri, $zipFile)

Expand-Archive -Path $zipFile -DestinationPath $extractDir -Force

$ErrorActionPreference = $OldEAP

$fossa = "$extractDir\fossa.exe"
$env:Path += ";$extractDir"
Write-Host "The fossa-cli installation directory has been added to the PATH for this session."

Write-Host "Installed fossa at: $fossa"
Write-Host "Get started by running: fossa.exe --help"

Write-Host "Running fossa.exe --version"
# Doesn't run without '&', seems to tell PS to treat the output as a command
& $fossa --version