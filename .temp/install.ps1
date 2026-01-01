$ErrorActionPreference = "Stop"
[Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12

$repo = if ($env:FERRET_REPO) { $env:FERRET_REPO } else { "Ferret-Language/Ferret" }
$baseUrl = if ($env:FERRET_RELEASE_BASE) { $env:FERRET_RELEASE_BASE } else { "https://github.com/$repo/releases/latest/download" }
$installDir = if ($env:FERRET_INSTALL_DIR) { $env:FERRET_INSTALL_DIR } else { Join-Path $env:USERPROFILE ".ferret" }

$arch = [System.Runtime.InteropServices.RuntimeInformation]::OSArchitecture.ToString()
switch ($arch) {
    "X64" { $arch = "amd64" }
    "Arm64" { $arch = "arm64" }
    default { throw "Unsupported architecture: $arch" }
}

$archive = "ferret-windows-$arch.zip"
$url = "$baseUrl/$archive"

$tempDir = Join-Path $env:TEMP "ferret-install"
New-Item -ItemType Directory -Force -Path $tempDir | Out-Null
$zipPath = Join-Path $tempDir $archive

Invoke-WebRequest -Uri $url -OutFile $zipPath

Remove-Item -Recurse -Force (Join-Path $installDir "bin") -ErrorAction SilentlyContinue
Remove-Item -Recurse -Force (Join-Path $installDir "libs") -ErrorAction SilentlyContinue
New-Item -ItemType Directory -Force -Path $installDir | Out-Null

Expand-Archive -Path $zipPath -DestinationPath $installDir -Force
$binDir = Join-Path $installDir "bin"

function Add-ToPath {
    param([string]$dir)
    $current = [Environment]::GetEnvironmentVariable("Path", "User")
    if ([string]::IsNullOrWhiteSpace($current)) {
        $current = ""
    }
    $parts = $current -split ';' | Where-Object { $_ -and $_ -ne "" }
    if ($parts -contains $dir) {
        return
    }
    $newPath = ($parts + $dir) -join ';'
    [Environment]::SetEnvironmentVariable("Path", $newPath, "User")
    $env:Path = "$newPath;$env:Path"
}

Add-ToPath $binDir

# Cleanup temp directory
Remove-Item -Recurse -Force $tempDir -ErrorAction SilentlyContinue

Write-Host "Installed to $installDir"
Write-Host "Added to PATH: $binDir"
Write-Host "Restart your terminal to use 'ferret'."
