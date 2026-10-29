#
# UnitsSort.ps1 - keeps unit paths in .dpr/.dproj in sync with the file system
# and sorts the local units in the uses section of the .dpr files.
#
# Usage: powershell -ExecutionPolicy Bypass -File Tools\UnitsSort\UnitsSort.ps1 [-ByName]
# By default the uses section is sorted by unit path; -ByName sorts by unit name.
#

param([switch]$ByName)

$ErrorActionPreference = 'Stop'

# Latin-1 maps bytes 1:1 to chars, so any encoding (UTF-8, BOM, cp1251) round-trips untouched.
$latin1 = [Text.Encoding]::GetEncoding(28591)

# Applies $Edit (text -> text) to a file and saves it only if the text changed.
function Edit-File([string]$File, [scriptblock]$Edit) {
    $old = $latin1.GetString([IO.File]::ReadAllBytes($File))
    $new = & $Edit $old
    if ($new -ceq $old) {
        Write-Host "OK:      $File"
    } else {
        [IO.File]::WriteAllBytes($File, $latin1.GetBytes($new))
        Write-Host "Updated: $File"
    }
}

# Replaces quoted '*.pas' paths with the paths actually found on disk.
function Repair-UnitPaths([string]$Text, [hashtable]$Units) {
    $fix = {
        param($m)
        $paths = $Units[[IO.Path]::GetFileName($m.Value)]
        if ($paths -and $paths -notcontains $m.Value) { $paths[0] } else { $m.Value }
    }.GetNewClosure()
    [regex]::Replace($Text, '(?<=[''"])[^''"\r\n]*\.pas(?=[''"])', $fix)
}

# Moves the local ('in ...') units to the end of the first uses section and sorts them.
function Sort-DprUses([string]$Text) {
    $m = [regex]::Match($Text, '(?is)uses\r?\n(.*?);')
    if (-not $m.Success) { return $Text }

    $block = $m.Groups[1].Value
    $sys = @(); $loc = @()
    foreach ($item in $block -split ',') {
        if ($item -match ' in ' -and $item -notmatch 'sasplanet\.modules') { $loc += $item } else { $sys += $item }
    }
    if (-not $loc) { return $Text }

    [string[]]$keys = if ($ByName) { $loc } else { $loc | ForEach-Object { ($_ -csplit ' in ')[1] } }
    [string[]]$sorted = $loc
    [Array]::Sort($keys, $sorted, [StringComparer]::Ordinal)

    $Text.Replace($block, ((($sys + $sorted) -ne '') -join ','))
}

# Returns all *.pas files under $Base. Dirs starting with '.' or '__' and excluded
# dirs are pruned before descending (Get-ChildItem -Recurse cannot do that).
function Get-PasFiles([string]$Base, [string[]]$Exclude) {
    $stack = New-Object Collections.Generic.Stack[string]
    $stack.Push($Base.TrimEnd('\'))
    while ($stack.Count) {
        $dir = $stack.Pop()
        foreach ($f in [IO.Directory]::GetFiles($dir, '*.pas')) {
            if ($f.EndsWith('.pas')) { $f }
        }
        foreach ($d in [IO.Directory]::GetDirectories($dir)) {
            $name = [IO.Path]::GetFileName($d)
            if ($name.StartsWith('.') -or $name.StartsWith('__')) { continue }
            $rel = $d.Substring($Base.Length) + '\'
            $skip = $false
            foreach ($ex in $Exclude) {
                if ($rel.StartsWith($ex, [StringComparison]::OrdinalIgnoreCase)) { $skip = $true }
            }
            if (-not $skip) { $stack.Push($d) }
        }
    }
}

$root = (Resolve-Path "$PSScriptRoot\..\..").Path.TrimEnd('\') + '\'

$projects = @(
    @{ Dir = '';           Dpr = 'SASPlanet.dpr';      Exclude = 'Test\', 'Benchmark\' },
    @{ Dir = 'Test\';      Dpr = 'SASPlanetTests.dpr'; Exclude = @() },
    @{ Dir = 'Benchmark\'; Dpr = 'BenchmarkCmd.dpr';   Exclude = @() }
)

foreach ($p in $projects) {
    $base = $root + $p.Dir
    Write-Host "Scan file system: $base"

    # name -> relative paths
    $units = @{}
    foreach ($f in Get-PasFiles $base $p.Exclude) {
        $units[[IO.Path]::GetFileName($f)] += @($f.Substring($base.Length))
    }
    if ($units.Count -eq 0) { continue }

    $dpr = $base + $p.Dpr
    Edit-File $dpr { param($t) Sort-DprUses (Repair-UnitPaths $t $units) }

    Get-ChildItem $base -Filter *.dproj -File | ForEach-Object {
        Edit-File $_.FullName { param($t) Repair-UnitPaths $t $units }
    }
}
