## Line Endings Pre-commit Hook

This hook ensures that all Delphi source files use `CRLF` line endings before they are committed.

### Installation

1. Open your local repository root directory.
2. Navigate to the hidden `.git/hooks/` folder.
3. Place both files there:
   * **`pre-commit`** (the shell wrapper script with no file extension).
   * **`pre-commit.ps1`** (the core PowerShell script containing the logic).

### How It Works

1. Every time you run `git commit`, the hook scans your **staged** `.pas`, `.inc`, `.dfm`, `.dpr`, `.dproj` files.
2. If it detects any incorrect line endings (Unix-style `LF`), it will:
   * **Automatically convert** them to `CRLF` directly on your disk.
   * **Abort the commit** to prevent raw changes from slipping through unverified.
3. The modified files will appear in your **Unstaged Changes**.
4. **Action Required:** Review the changes via your Git client or `git diff`, restage them (`git add`), and run your commit again.