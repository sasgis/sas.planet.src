param(
    [string]$Path = ""
)

if (-not $Path) {
    $basePath = if ($PSScriptRoot) { $PSScriptRoot } else { $PWD.Path }
    $Path = Join-Path $basePath "..\..\Src"
}

$Path = (Resolve-Path $Path).Path

$extensions = @("*.pas", "*.dfm", "*.inc", "*.dpr", "*.dproj")

$total     = 0
$converted = 0
$skipped   = 0
$failed    = 0

Write-Host "Processing directory: $Path" -ForegroundColor Cyan
Write-Host "----------------------------------------"

# Get all matching files recursively
$files = Get-ChildItem -Path $Path -Include $extensions -Recurse -File

foreach ($file in $files) {
    $total++
    
    # Relative path for cleaner logging
    $relativeName = $file.FullName.Replace($Path, "").TrimStart("\").TrimStart("/")
    
    try {
        # Read file content (auto-detects encoding)
        $content = [System.IO.File]::ReadAllText($file.FullName)

        # Normalize any line ending to Windows CRLF
        $crlfContent = $content -replace "\r\n|\n|\r", "`r`n"

        # Update file only if changes were made
        if ($content -cne $crlfContent) {
            [System.IO.File]::WriteAllText($file.FullName, $crlfContent)
            Write-Host "[CONVERTED] $relativeName" -ForegroundColor Green
            $converted++
        } else {
            # Write-Host "[SKIPPED]   $relativeName (already CRLF)" -ForegroundColor DarkGray
            $skipped++
        }
    } catch {
        Write-Host "[ERROR]     $relativeName - $($_.Exception.Message)" -ForegroundColor Red
        $failed++
    }
}

Write-Host "`n========================================" -ForegroundColor Cyan
Write-Host "  Total found : $total"
Write-Host "  Converted   : $converted" -ForegroundColor Green
Write-Host "  Skipped     : $skipped"   -ForegroundColor DarkGray
Write-Host "  Errors      : $failed"    -ForegroundColor Red
Write-Host "========================================" -ForegroundColor Cyan
