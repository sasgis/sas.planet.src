{*******************************************************************************
 The MIT License (MIT); this license applies to GeographicLib,
 versions 1.12 and later.

 Copyright (c) 2008-2016, Charles Karney

 Permission is hereby granted, free of charge, to any person
 obtaining a copy of this software and associated documentation
 files (the "Software"), to deal in the Software without
 restriction, including without limitation the rights to use, copy,
 modify, merge, publish, distribute, sublicense, and/or sell copies
 of the Software, and to permit persons to whom the Software is
 furnished to do so, subject to the following conditions:

 The above copyright notice and this permission notice shall be
 included in all copies or substantial portions of the Software.

 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 DEALINGS IN THE SOFTWARE.

 http://geographiclib.sourceforge.net/
*******************************************************************************}

unit GeographicLib;

interface

const
  geodesic_dll = 'geodesic.dll';

type
  geod_geodesic = record
    a: Double;
    f: Double;
    f1, e2, ep2, n, b, c2, etol2: Double;
    A3x: array [0..5] of Double;
    C3x: array [0..14] of Double;
    C4x: array [0..20] of Double;
  end;
  geod_geodesic_ptr = ^geod_geodesic;

var
  geod_init: procedure(
    const g: geod_geodesic_ptr;
    const a, f: Double
  ); cdecl;

  geod_direct: procedure(
    const g: geod_geodesic_ptr;
    const lat1, lon1, azi1, s12: Double;
    out lat2, lon2, azi2: Double
  ); cdecl;

  geod_inverse: procedure(
    const g: geod_geodesic_ptr;
    const lat1, lon1, lat2, lon2: Double;
    out s12, azi1, azi2: Double
  ); cdecl;

function init_geodesic_dll(
  const ALibName: string = geodesic_dll;
  const ARaiseExceptions: Boolean = True
): Boolean;

implementation

uses
  Windows,
  SyncObjs,
  Math,
  SysUtils;

var
  gHandle: THandle = 0;
  gLock: TCriticalSection = nil;
  gIsInitialized: Boolean = False;

procedure _safe_internal_init_geodesic;
const
  wgs84_a: Double = 6378137;
  wgs84_f: Double = 1/298.257223563;
var
  VGeod: geod_geodesic;
begin
  geod_init(@VGeod, wgs84_a, wgs84_f);
end;

function init_geodesic_dll(const ALibName: string; const ARaiseExceptions: Boolean): Boolean;

  function GetProcAddr(Name: PAnsiChar): Pointer;
  begin
    Result := GetProcAddress(gHandle, Name);
    if ARaiseExceptions and (Result = nil) then begin
      RaiseLastOSError;
    end;
  end;

begin
  if gIsInitialized then begin
    Result := True;
    Exit;
  end;

  gLock.Acquire;
  try
    if gIsInitialized then begin
      Result := True;
      Exit;
    end;

    if gHandle = 0 then begin
      gHandle := LoadLibrary(PChar(ALibName));
      if ARaiseExceptions and (gHandle = 0) then begin
        RaiseLastOSError;
      end;
    end;

    if gHandle <> 0 then begin
      geod_init := GetProcAddr('geod_init');
      geod_direct := GetProcAddr('geod_direct');
      geod_inverse := GetProcAddr('geod_inverse');
    end;

    gIsInitialized :=
      (gHandle <> 0) and
      (Addr(geod_init) <> nil) and
      (Addr(geod_direct) <> nil) and
      (Addr(geod_inverse) <> nil);

    Result := gIsInitialized;

    if Result then begin
      _safe_internal_init_geodesic;
    end;
  finally
    gLock.Release;
  end;
end;

procedure free_geodesic_dll;
begin
  gLock.Acquire;
  try
    gIsInitialized := False;

    if gHandle <> 0 then begin
      FreeLibrary(gHandle);
      gHandle := 0;
    end;

    geod_init := nil;
    geod_direct := nil;
    geod_inverse := nil;
  finally
    gLock.Release;
  end;
end;

initialization
  gLock := TCriticalSection.Create;

finalization
  free_geodesic_dll;
  FreeAndNil(gLock);

end.
