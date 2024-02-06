# [dllfetch](https://github.com/zedxxx/dllfetch)

## About
Recursively maps the DLL dependencies of an executable using [`objdump`][objdump], using a breadth-first search. Optionally copies found dependencies into a specified directory.

Since it targets Windows, the script attempts to be as case-insensitive as possible: internally, all dependency names are converted to lowercase. When searching for files, a [regular expression][re] is constructed from the lowercase name with the [re.I][ignore] option, matching filenames with any capitalization. If a file matches, its path is stored with the proper capitalization.

This script was originally written to assist in deploying a dynamically linked [Qt][qt] GUI app developed under Linux and cross-compiled for Windows using [MXE][mxe]. Theoretically, it could work on a Windows machine with Python 3.x, MinGW and binutils, but this was not tested.

## Usage
For a brief usage summary, run the script with the `-h` switch.

At minimum, the script needs to be given one argument: the executable (or DLL, or any file that `objdump` understands) for which the dependencies should be fetched. Two optional arguments can be given:
 - **`-d`** or **`--dir`**: Directory to be searched for dependencies. Defaults to the current working directory. Directories are searched recursively. Multiple directories can be specified as separate parameters (`-d`/`--dir` needs to be given once). Note that the `-d` option will consume as many arguments as possible, so unless you specify `-t` afterwards, make sure to put it after the executable:
 
   ```
   python3 dllfetch.py myprogram.exe                    # OK, will search current directory
   python3 dllfetch.py myprogram.exe -d /path/to/dlls   # OK, will search /path/to/dlls
   python3 dllfetch.py -d /path/to/dlls myprogram.exe   # BAD! myprogram.exe is interpreted as part of the -d option
   python3 dllfetch.py -d /path/to/dlls -t /target/dir myprogram.exe    # OK, the -t option only takes one argument
   ```
   
 - **`-t`** or **`--target-dir`**: Copy found dependencies into this directory (using [`shutil.copy`][copy]). If omitted, no files will be copied.

At the end of the run, the script prints a list of found and missing dependencies. This can be used to verify "acceptable" missing dependencies such as `shell32.dll` which is unlikely to be part of a cross-compiler, but can be reasonably expected to be found on most Windows machines.

[objdump]: http://sourceware.org/binutils/docs/binutils/objdump.html
[re]: http://docs.python.org/3.5/library/re.html
[ignore]: http://docs.python.org/3.5/library/re.html#re.I
[copy]: http://docs.python.org/3.5/library/shutil.html#shutil.copy
[qt]: http://www.qt.io/
[mxe]: http://mxe.cc/
