#Requires -Version 5

<#
    .SYNOPSIS
    Download and install the latest available FOSSA release from GitHub.
#>

[CmdletBinding()]
Param()

$OldEAP = $ErrorActionPreference #Preserve the original value
$ErrorActionPreference = "Stop"

$app = "fossa"

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
$latestUri = "$github/fossas/fossa-cli/releases/$releaseTag"
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

$headers = @{
    'Accept' = 'application/json'
}

$release = Invoke-RestMethod -Uri $latestUri -Method Get -Headers $headers
$releaseVersion = $release.tag_name;
$releaseVersionSemver = $releaseVersion.TrimStart("v");
$downloadUri = "$github/fossas/fossa-cli/releases/download/$releaseVersion/$($app)_$($releaseVersionSemver)_windows_amd64.zip"

Write-Output "Downloading from: $downloadUri"

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
Write-Host ""
Write-Host "------"
Write-Host "Notice"
Write-Host "------"
Write-Host ""
Write-Host "FOSSA collects warnings, errors, and usage data to improve"
Write-Host "the FOSSA CLI and your experience."
Write-Host ""
Write-Host "Read more: https://github.com/fossas/fossa-cli/blob/master/docs/telemetry.md"
Write-Host ""
Write-Host "If you want to prevent any telemetry data from being sent to" 
Write-Host "the server, you can opt out of telemetry by setting"
Write-Host "FOSSA_TELEMETRY_SCOPE environment variable to 'off' in your shell."
Write-Host ""
Write-Host "For example:"
Write-Host "  `$env:FOSSA_TELEMETRY_SCOPE=off"
Write-Host "   fossa analyze"
Write-Host ""
Write-Host ""
Write-Host "Get started by running: fossa.exe --help"
Write-Host "Running fossa.exe --version"

# Doesn't run without '&', seems to tell PS to treat the output as a command
& $fossa --version
