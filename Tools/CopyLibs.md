1. Install [MSYS2](https://www.msys2.org/) into `c:\msys64` (default location)

2. Update MSYS2:
```  
pacman -Suy
```
3. Install 32-bit packages:
```
pacman -S \
  mingw-w64-i686-bzip2 \
  mingw-w64-i686-curl \
  mingw-w64-i686-binutils \
  mingw-w64-i686-sqlite3 \
  mingw-w64-i686-libpng \
  mingw-w64-i686-libimagequant \
  mingw-w64-i686-libjpeg-turbo \
  mingw-w64-i686-libwebp \
  mingw-w64-i686-libtiff
```

4. Install 64-bit packages:
```
pacman -S \
  mingw-w64-x86_64-7zip \
  mingw-w64-x86_64-curl \
  mingw-w64-x86_64-binutils \
  mingw-w64-x86_64-sqlite3 \
  mingw-w64-x86_64-libpng \
  mingw-w64-x86_64-libimagequant \
  mingw-w64-x86_64-libjpeg-turbo \
  mingw-w64-x86_64-libwebp \
  mingw-w64-x86_64-libtiff \
  mingw-w64-x86_64-libgeotiff \
  mingw-w64-x86_64-proj \
  mingw-w64-x86_64-freeimage \
  mingw-w64-x86_64-minizip-ng
```

these packages are 64-bit only:
```
 - 7zip
 - geotiff
 - proj
 - minizip-ng
```

5. Periodically update packages and check for unsupported packages:
```  
pacman -Suy
pacman -Qm
```