{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2015, SAS.Planet development team.                      *}
{* This program is free software: you can redistribute it and/or modify       *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* This program is distributed in the hope that it will be useful,            *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *}
{*                                                                            *}
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

unit u_FileSystemFunc;

interface

{$IF CompilerVersion < 23}
function IsRelativePath(const Path: string): Boolean; inline;
{$IFEND}
function GetFullPath(const ABasePath, ARelativePath: string): string;
function GetDiskFree(const ADrive: Char): Int64;
function ReplaceIllegalFileNameChars(const AFileName: string): string;

implementation

uses
  Windows,
  SysUtils,
  ShLwApi;

{$IF CompilerVersion < 23}
function IsRelativePath(const Path: string): Boolean; inline;
var
  L: Integer;
begin
  L := Length(Path);
  Result := (L > 0) and (Path[1] <> PathDelim)
    {$IFDEF MSWINDOWS}and (L > 1) and (Path[2] <> ':'){$ENDIF MSWINDOWS};
end;
{$IFEND}

function GetFullPath(const ABasePath, ARelativePath: string): string;
begin
  SetLength(Result, MAX_PATH);
  PathCombine(@Result[1], PChar(ExtractFilePath(ABasePath)), PChar(ARelativePath));
  SetLength(Result, LStrLen(PChar(Result)));
  Result := IncludeTrailingPathDelimiter(Result);
end;

function GetDiskFree(const ADrive: Char): Int64;
var
  lpFreeBytesAvailableToCaller,
  lpTotalNumberOfBytes,
  lpTotalNumberOfFreeBytes : TLargeInteger;
begin
  if
    GetDiskFreeSpaceEx(
      PChar(ADrive + ':\'),
      lpFreeBytesAvailableToCaller,
      lpTotalNumberOfBytes,
      @lpTotalNumberOfFreeBytes
    )
  then
    Result := lpTotalNumberOfFreeBytes
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

end.
