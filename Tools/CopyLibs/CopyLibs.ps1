<#
.SYNOPSIS
    Copies MinGW DLLs and their dependencies into the project lib directories.

.PARAMETER Arch
    Architecture to process: 32, 64 or All (default).

.PARAMETER MsysRoot
    MSYS2 installation directory (default: c:\msys64).

.PARAMETER NoPause
    Do not wait for Enter at the end.

.EXAMPLE
    .\CopyLibs.ps1            # both 32-bit and 64-bit
    .\CopyLibs.ps1 -Arch 64   # 64-bit only
#>
[CmdletBinding()]
param(
    [ValidateSet('32', '64', 'All')]
    [string]$Arch = 'All',

    [string]$MsysRoot = 'c:\msys64',

    [switch]$NoPause
)

$ErrorActionPreference = 'Stop'

$dllfetch = Join-Path $PSScriptRoot 'dllfetch\dllfetch.ps1'

# Per-architecture settings
#   Mingw  - MinGW bin directory (source of DLLs, also used as the search directory)
#   Lib    - target directory (paths are relative to this script's location)
#   Copy   - DLLs copied from MinGW into Lib before the dependency check
#   Check  - DLLs in Lib whose dependencies are resolved and copied
#   Ignore - DLLs skipped when resolving dependencies
$configs = [ordered]@{
    '32' = @{
        Mingw  = Join-Path $MsysRoot 'mingw32\bin'
        Lib    = Join-Path $PSScriptRoot '..\..\.bin\win32\lib32'
        Copy   = @('libcurl-4.dll', 'libsqlite3-0.dll')
        Check  = @('imagequant.dll', 'libproj-25.dll', 'libgeotiff.dll',
                   'libminizip-ng-1.dll', 'libfreeimage-3.dll',
                   'libcurl-4.dll', 'libsqlite3-0.dll')
        Ignore = @('7z.dll', 'imagequant.dll')
    }
    '64' = @{
        Mingw  = Join-Path $MsysRoot 'mingw64\bin'
        Lib    = Join-Path $PSScriptRoot '..\..\.bin\win64\lib64'
        Copy   = @('libcurl-4.dll', 'libsqlite3-0.dll', 'libgeotiff.dll',
                   'libminizip-ng-1.dll', 'libfreeimage-3.dll')
        Check  = @('libcurl-4.dll', 'libsqlite3-0.dll', 'libgeotiff.dll',
                   'libminizip-ng-1.dll', 'libfreeimage-3.dll')
        Ignore = @('7z.dll', 'imagequant.dll')
    }
}

# Per-architecture results: name -> @{ Error; Missing } (printed at the very end)
$summary = [ordered]@{}

function Copy-ArchLibs {
    param([string]$Name, [hashtable]$Config)

    $mingw = [System.IO.Path]::GetFullPath($Config.Mingw)
    $lib   = [System.IO.Path]::GetFullPath($Config.Lib)

    $missing = [System.Collections.Generic.List[object]]::new()
    $result  = [pscustomobject]@{ Error = $null; Missing = $missing }
    $script:summary[$Name] = $result

    Write-Host "`n===== $Name-bit =====" -ForegroundColor Cyan
    Write-Host "MinGW: $mingw"
    Write-Host "Lib:   $lib"

    if (-not (Test-Path -LiteralPath $mingw -PathType Container)) {
        Write-Warning "MinGW directory not found: $mingw"
        $result.Error = "MinGW directory not found: $mingw"
        return
    }
    if (-not (Test-Path -LiteralPath $lib)) {
        New-Item -ItemType Directory -Path $lib | Out-Null
    }

    # objdump must be resolvable from the matching MinGW bin directory
    $oldPath = $env:PATH
    $env:PATH = "$mingw;$oldPath"
    try {
        foreach ($dll in $Config.Copy) {
            Write-Host "Copying: $dll"
            $src = Join-Path $mingw $dll
            if (Test-Path -LiteralPath $src) {
                Copy-Item -LiteralPath $src -Destination (Join-Path $lib $dll) -Force
            } else {
                Write-Warning "Not found: $src"
                $missing.Add([pscustomobject]@{ Dll = $dll; Source = 'copy list (not found in MinGW)' })
            }
        }

        foreach ($dll in $Config.Check) {
            $file = Join-Path $lib $dll
            if (-not (Test-Path -LiteralPath $file -PathType Leaf)) {
                Write-Warning "File to check not found: $file"
                $missing.Add([pscustomobject]@{ Dll = $dll; Source = 'check list (not found in lib)' })
                continue
            }

            # dllfetch writes progress via Write-Host and emits only the missing DLL names
            $notFound = @(& $dllfetch -File $file `
                                      -Directory $mingw `
                                      -TargetDir $lib `
                                      -Ignore $Config.Ignore)
            foreach ($missingName in $notFound) {
                $missing.Add([pscustomobject]@{ Dll = $missingName; Source = $dll })
            }
        }
    }
    finally {
        $env:PATH = $oldPath
    }
}

if (-not (Test-Path -LiteralPath $dllfetch)) {
    throw "dllfetch.ps1 not found: $dllfetch"
}

foreach ($key in $configs.Keys) {
    if ($Arch -eq 'All' -or $Arch -eq $key) {
        Copy-ArchLibs -Name $key -Config $configs[$key]
    }
}

Write-Host "`n===== Missing DLLs summary =====" -ForegroundColor Cyan
foreach ($key in $summary.Keys) {
    $result = $summary[$key]
    Write-Host "`n$key-bit:" -ForegroundColor Cyan
    if ($result.Error) {
        Write-Host "  skipped: $($result.Error)" -ForegroundColor Red
    }
    elseif ($result.Missing.Count -eq 0) {
        Write-Host "  none" -ForegroundColor Green
    }
    else {
        $result.Missing | Group-Object Dll | Sort-Object Name | ForEach-Object {
            $by = ($_.Group | ForEach-Object { $_.Source } | Sort-Object -Unique) -join ', '
            Write-Host "  - $($_.Name)  (required by: $by)" -ForegroundColor Yellow
        }
    }
}

if (-not $NoPause) {
    Read-Host "`nPress Enter to exit" | Out-Null
}
