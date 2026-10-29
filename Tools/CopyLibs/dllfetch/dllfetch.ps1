<#
.SYNOPSIS
    Checks the DLL dependencies of a file (PowerShell port of dllfetch.py).

.PARAMETER File
    File whose dependencies should be checked.

.PARAMETER Directory
    Directories to search for dependencies (default: root of the system drive).

.PARAMETER TargetDir
    Directory to copy the found dependencies into (optional).

.PARAMETER Ignore
    Dependencies to exclude from the check (optional).

.EXAMPLE
    .\dllfetch.ps1 app.exe -d C:\msys64\mingw64\bin -t .\dist -i libfoo.dll
#>
[CmdletBinding()]
param(
    [Parameter(Mandatory, Position = 0)]
    [string]$File,

    [Alias('d', 'Dir')]
    [string[]]$Directory,

    [Alias('t', 'Target')]
    [string]$TargetDir,

    [Alias('i')]
    [string[]]$Ignore
)

$ErrorActionPreference = 'Stop'

$SystemDll = @(
    'msvcrt.dll', 'kernel32.dll', 'user32.dll', 'oleaut32.dll',
    'ntdll.dll', 'advapi32.dll', 'ws2_32.dll', 'bcrypt.dll',
    'userenv.dll', 'shell32.dll', 'crypt32.dll', 'wldap32.dll',
    'secur32.dll', 'iphlpapi.dll'
)

if (-not (Get-Command objdump -ErrorAction SilentlyContinue)) {
    throw "objdump was not found in PATH. Install binutils (e.g. via MSYS2: pacman -S mingw-w64-x86_64-binutils) and add it to PATH."
}

# Returns the list of non-system DLLs the file depends on
function Get-Imports {
    param([string]$Path)

    Write-Host "checking dependencies for $Path"

    $output = & objdump -p $Path 2>&1
    if ($LASTEXITCODE -ne 0) {
        throw "objdump failed for ${Path}: $output"
    }

    $hits = [System.Collections.Generic.HashSet[string]]::new([System.StringComparer]::OrdinalIgnoreCase)
    foreach ($line in $output) {
        if ("$line" -match 'DLL Name:\s*(\S+)') {
            $name = $Matches[1]
            if ($SystemDll -notcontains $name.ToLower()) {
                [void]$hits.Add($name)
            }
        }
    }
    return @($hits)
}

# Search directories
if (-not $Directory) {
    $Directory = @("$($env:SystemDrive)\")
    Write-Host "No search directories given, searching in $($Directory[0]) (this may take a long time)"
}

if (-not (Test-Path -LiteralPath $File -PathType Leaf)) {
    Write-Host "File does not exist: $File"
    return
}

# Index all *.dll files in the given directories once: name -> list of full paths
# (PowerShell hashtables are case-insensitive by default)
Write-Host "indexing DLLs in: $($Directory -join ', ')"
$index = @{}
foreach ($dir in $Directory) {
    Get-ChildItem -Path $dir -Recurse -File -Filter '*.dll' -ErrorAction SilentlyContinue |
        ForEach-Object {
            if (-not $index.ContainsKey($_.Name)) {
                $index[$_.Name] = [System.Collections.Generic.List[string]]::new()
            }
            if (-not $index[$_.Name].Contains($_.FullName)) {
                $index[$_.Name].Add($_.FullName)
            }
        }
}

# Dependencies: name -> @{ Name; Path; Found }
$deps  = @{}
$queue = [System.Collections.Generic.Queue[string]]::new()

function Add-Dependency {
    param([string]$Name)

    if ($Ignore -contains $Name) {
        Write-Host "skip dependencies check for $Name"
        return
    }
    if (-not $deps.ContainsKey($Name)) {
        $deps[$Name] = [pscustomobject]@{
            Name  = $Name.ToLower()
            Path  = $null
            Found = $false
        }
        $queue.Enqueue($Name)
    }
}

foreach ($name in (Get-Imports -Path $File)) {
    Add-Dependency $name
}

while ($queue.Count -gt 0) {
    $name = $queue.Dequeue()
    $dep  = $deps[$name]

    $paths = @()
    if ($index.ContainsKey($name)) { $paths = @($index[$name]) }

    if ($paths.Count -eq 0) {
        Write-Host "- $($dep.Name) missing"
        continue
    }
    elseif ($paths.Count -eq 1) {
        $dep.Path = $paths[0]
    }
    else {
        Write-Host ""
        Write-Host "$($dep.Name) found in multiple directories"
        Write-Host "please choose one:"
        for ($n = 0; $n -lt $paths.Count; $n++) {
            Write-Host (" [{0}] {1}" -f $n, $paths[$n])
        }
        while ($true) {
            $answer = Read-Host '>'
            $idx = 0
            if ([int]::TryParse($answer, [ref]$idx) -and $idx -ge 0 -and $idx -lt $paths.Count) {
                break
            }
            Write-Host "invalid answer"
        }
        $dep.Path = $paths[$idx]
    }

    $dep.Found = $true
    foreach ($sub in (Get-Imports -Path $dep.Path)) {
        Add-Dependency $sub
    }
}

$found    = @($deps.Values | Where-Object { $_.Found }     | ForEach-Object { $_.Path } | Sort-Object)
$notFound = @($deps.Values | Where-Object { -not $_.Found } | ForEach-Object { $_.Name } | Sort-Object)

Write-Host "`n*** Found dependencies: ***"
if ($found.Count -gt 0) {
    $found | ForEach-Object { Write-Host "+ $_" }
} else {
    Write-Host "None"
}

Write-Host "`n*** Missing dependencies: ***"
if ($notFound.Count -gt 0) {
    $notFound | ForEach-Object { Write-Host "- $_" }
} else {
    Write-Host "None"
}

if ($TargetDir -and $found.Count -gt 0) {
    Write-Host "`nCopying files to $TargetDir"
    if (-not (Test-Path -LiteralPath $TargetDir)) {
        New-Item -ItemType Directory -Path $TargetDir | Out-Null
    }
    foreach ($src in $found) {
        Write-Host "> $(Split-Path $src -Leaf)"
        Copy-Item -LiteralPath $src -Destination $TargetDir -Force
    }
}

Write-Host "`nDone!"
