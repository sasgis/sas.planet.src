#!/usr/bin/env pwsh

# Get the list of staged files (only Added, Copied, Modified)
$stagedFiles = git diff --cached --name-only --diff-filter=ACM
if (-not $stagedFiles) { exit 0 }

$fixedFiles = @()

# Use ISO-8859-1 (28591) for transparent byte-by-byte reading/writing
$encoding = [System.Text.Encoding]::GetEncoding(28591)

foreach ($file in $stagedFiles) {
    # Filter by extension and ensure the file still exists on disk
    if ($file -match '\.(pas|inc|dfm|dpr|dproj)$' -and (Test-Path -LiteralPath $file)) {
        
        # Read file content
        $bytes = [System.IO.File]::ReadAllBytes($file)
        $content = $encoding.GetString($bytes)
        
        # Look for an LF character that is not preceded by a CR character
        if ($content -match "(?<!\r)\n") {
            
            # Replace bare LFs with CRLF
            $fixedContent = $content -replace "(?<!\r)\n", "`r`n"
            
            # Convert back to bytes and overwrite the file on disk
            $outBytes = $encoding.GetBytes($fixedContent)
            [System.IO.File]::WriteAllBytes($file, $outBytes)
            
            # Add to the fixed list (we intentionally DO NOT run git add)
            $fixedFiles += $file
        }
    }
}

# If files were found and fixed, inform the user and abort the commit
if ($fixedFiles.Count -gt 0) {
    Write-Host "WARNING: Incorrect line endings (LF) were found in the following files:" -ForegroundColor Yellow
    foreach ($fixedFile in $fixedFiles) { Write-Host "  - $fixedFile" -ForegroundColor Yellow }
    
    Write-Host "`nINFO: These files have been automatically fixed on your disk (converted to CRLF)." -ForegroundColor Cyan
    Write-Host "ACTION REQUIRED: The commit was aborted. Please review the unstaged changes, add them (git add), and commit again." -ForegroundColor Red
    
    exit 1
}

# All files are fine
exit 0
