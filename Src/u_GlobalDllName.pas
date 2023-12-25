{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_GlobalDllName;

{.$DEFINE FORCE_USE_WINXP_DLL}
{.$DEFINE HANDLE_DELAYLOAD_ERRORS} // enable for dll's with delayed import (if any)

interface

uses
  Windows,
  {$IFDEF HANDLE_DELAYLOAD_ERRORS}
  UITypes,
  Dialogs,
  {$ENDIF}
  SysUtils;

type
  TGlobalDllName = record
    Sqlite3: string;
    Curl: string;
    Tiff: string;
    GeoTiff: string;
    Jpeg62: string;
    Png16: string;
    FreeImage: string;
    Geodesic: string;
    MiniZip: string;
    ImageQuant: string;

    procedure Init;
  end;

var
  GDllName: TGlobalDllName;

implementation

{$IFDEF HANDLE_DELAYLOAD_ERRORS}
function _DelayedHandlerHook(dliNotify: dliNotification; pdli: PDelayLoadInfo): Pointer; stdcall;
var
  VText: string;
begin
  VText := '';
  Result := nil;

  case dliNotify of
    dliFailLoadLibrary: begin
      VText := 'Could not load library ' +  pdli.szDll;
    end;
    dliFailGetProcAddress: begin
      if pdli.dlp.fImportByName then begin
        VText := Format('Could not load "%s" from %s', [pdli.dlp.szProcName, pdli.szDll])
      end else begin
        VText := Format('Could not load index %d from %s', [pdli.dlp.dwOrdinal, pdli.szDll]);
      end;
    end;
  else
    Assert(False);
  end;

  if (VText <> '') and (MessageDlg(VText, mtError, mbAbortIgnore, 0) = mrAbort) then begin
    Halt;
  end;
end;
{$ENDIF}

{ TGlobalDllName }

procedure TGlobalDllName.Init;

  function IsWinXP: Boolean;
  begin
    Result := {$IFDEF FORCE_USE_WINXP_DLL} True {$ELSE} Win32MajorVersion = 5 {$ENDIF};
  end;

var
  VPath: string;
  VAppPath: string;
begin
  {$IFDEF HANDLE_DELAYLOAD_ERRORS}
  SetDliFailureHook2(_DelayedHandlerHook);
  {$ENDIF}

  VAppPath := ExtractFilePath(ParamStr(0));

  // Precompiled dll packs can be found here:
  // https://github.com/sasgis/sas.planet.bin/releases

  if IsWinXP then begin
    VPath := VAppPath + 'libxp' + PathDelim;

    if not DirectoryExists(VPath) then begin
      VPath := '';
    end;

    Self.Curl := 'libcurl.dll';
    Self.Tiff := 'libtiff.dll';
    Self.GeoTiff := 'libgeotiff.dll';
    Self.Jpeg62 := 'jpeg62.dll';
    Self.Png16 := 'libpng16.dll';
    Self.FreeImage := 'FreeImage.dll';
    Self.Sqlite3 := 'sqlite3.dll';
    Self.Geodesic := 'geodesic.dll';
    Self.MiniZip := 'libminizip.dll';
    Self.ImageQuant := 'libimagequant.dll';
  end else begin
    VPath := VAppPath + {$IFDEF WIN32} 'lib32' {$ELSE} 'lib64' {$ENDIF} + PathDelim;

    if not DirectoryExists(VPath) then begin
      VPath := '';
    end;

    Self.Curl := 'libcurl-4.dll';
    Self.Tiff := 'libtiff-6.dll';
    Self.GeoTiff := 'libgeotiff.dll';
    Self.Jpeg62 := 'libjpeg-62.dll';
    Self.Png16 := 'libpng16-16.dll';
    Self.FreeImage := 'libfreeimage-3.dll';
    Self.Sqlite3 := 'libsqlite3-0.dll';
    Self.Geodesic := 'libproj-25.dll';
    Self.MiniZip := 'libminizip-ng-1.dll';
    Self.ImageQuant := 'imagequant.dll';
  end;

  if VPath <> '' then begin
    if not SetDllDirectory(PChar(VPath)) then begin
      RaiseLastOSError;
    end;
  end;
end;

end.
