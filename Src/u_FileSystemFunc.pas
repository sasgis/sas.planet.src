{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
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

unit u_FileSystemFunc;

interface

function RelativeToAbsolutePath(const ABasePath, ARelativePath: string): string;
function GetDiskFree(const ADrive: Char): Int64;
function ReplaceIllegalFileNameChars(const AFileName: string): string;
function IsValidFileName(const AFileName: string): Boolean;

implementation

uses
  Windows,
  SysUtils,
  ShLwApi;

function RelativeToAbsolutePath(const ABasePath, ARelativePath: string): string;
begin
  SetLength(Result, MAX_PATH);
  if PathCombine(PChar(Result), PChar(ExtractFilePath(ABasePath)), PChar(ARelativePath)) = nil then begin
    RaiseLastOSError;
  end;
  SetLength(Result, StrLen(PChar(Result)));
end;

function GetDiskFree(const ADrive: Char): Int64;
var
  VFreeBytesAvailableToCaller,
  VTotalNumberOfBytes,
  VTotalNumberOfFreeBytes: TLargeInteger;
begin
  if
    GetDiskFreeSpaceEx(
      PChar(ADrive + ':\'),
      VFreeBytesAvailableToCaller,
      VTotalNumberOfBytes,
      @VTotalNumberOfFreeBytes
    )
  then
    Result := VTotalNumberOfFreeBytes
  else
    Result := -1;
end;

function ReplaceIllegalFileNameChars(const AFileName: string): string;
begin
  Result := AFileName;
  Result := StringReplace(Result, '\', '-', [rfReplaceAll]);
  Result := StringReplace(Result, '/', '-', [rfReplaceAll]);
  Result := StringReplace(Result, ':', '-', [rfReplaceAll]);
  Result := StringReplace(Result, '*', '-', [rfReplaceAll]);
  Result := StringReplace(Result, '?', '-', [rfReplaceAll]);
  Result := StringReplace(Result, '"', '-', [rfReplaceAll]);
  Result := StringReplace(Result, '>', '-', [rfReplaceAll]);
  Result := StringReplace(Result, '<', '-', [rfReplaceAll]);
  Result := StringReplace(Result, '|', '-', [rfReplaceAll]);
end;

function IsValidFileName(const AFileName: string): Boolean;
var
  VHandle: THandle;
  VFileName: string;
begin
  Result := False;

  VFileName := Trim(AFileName);
  if (VFileName = '') or (ExtractFileName(VFileName) = '') then begin
    Exit;
  end;

  if FileExists(VFileName) then begin
    Result := True;
    Exit;
  end;

  VHandle := CreateFile(PChar(VFileName), GENERIC_READ or GENERIC_WRITE, 0, nil,
    CREATE_NEW, FILE_ATTRIBUTE_TEMPORARY or FILE_FLAG_DELETE_ON_CLOSE, 0);

  if VHandle <> INVALID_HANDLE_VALUE then begin
    Result := True;
    CloseHandle(VHandle);
  end;
end;

end.
