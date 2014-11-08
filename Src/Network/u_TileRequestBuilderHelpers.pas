{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
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

unit u_TileRequestBuilderHelpers;

interface

function RandomInt(const X: Integer): Integer;
function MaxInt(const A, B: Integer): Integer;
function MaxExt(const A, B: Extended): Extended;
function MinInt(const A, B: Integer): Integer;
function MinExt(const A, B: Extended): Extended;
function GetUnixTime: Int64;
function StrLength(const Str: AnsiString): Integer;
function GetAfter(const SubStr, Str: AnsiString): AnsiString;
function GetBefore(const SubStr, Str: AnsiString): AnsiString;
function GetBetween(const Str, After, Before: AnsiString): AnsiString;
function SubStrPos(const Str, SubStr: AnsiString; FromPos: Integer): Integer;
function SetHeaderValue(const AHeaders, AName, AValue: AnsiString): AnsiString;
function GetHeaderValue(const AHeaders, AName: AnsiString): AnsiString;
function GetNumberAfter(const ASubStr, AText: AnsiString): AnsiString;
function GetDiv3Path(const ASource: AnsiString): AnsiString;
function SaveToLocalFile(const AFullLocalFilename, AData: AnsiString): Integer;
function Base64EncodeStr(const Data: AnsiString): AnsiString;
function Base64UrlEncodeStr(const Data: AnsiString): AnsiString;
function Base64DecodeStr(const Data: AnsiString): AnsiString;

implementation

uses
  SysUtils,
  Classes,
  Math,
  EDBase64,
  DateUtils,
  RegExpr;

function RandomInt(const X: Integer): Integer;
begin
  Result := Random(X);
end;

function MaxInt(const A, B: Integer): Integer;
begin
  Result := Max(A, B);
end;

function MaxExt(const A, B: Extended): Extended;
begin
  Result := Max(A, B);
end;

function MinInt(const A, B: Integer): Integer;
begin
  Result := Min(A, B);
end;

function MinExt(const A, B: Extended): Extended;
begin
  Result := Min(A, B);
end;

function GetUnixTime: Int64;
begin
  Result := DateTimeToUnix(now);
end;

function StrLength(const Str: AnsiString): Integer;
begin
  Result := Length(Str);
end;

function GetAfter(const SubStr, Str: AnsiString): AnsiString;
begin
  if pos(substr, str) > 0 then begin
    result := copy(str, pos(substr, str) + length(substr), length(str));
  end else begin
    result := '';
  end;
end;

function GetBefore(const SubStr, Str: AnsiString): AnsiString;
begin
  if pos(substr, str) > 0 then begin
    result := copy(str, 1, pos(substr, str) - 1);
  end else begin
    result := '';
  end;
end;

function GetBetween(const Str, After, Before: AnsiString): AnsiString;
begin
  result := GetBefore(Before, GetAfter(After, str));
end;

function SubStrPos(
  const Str, SubStr: AnsiString;
  FromPos: Integer
): Integer; assembler;
asm
  PUSH EDI
  PUSH ESI
  PUSH EBX
  PUSH EAX
  OR EAX,EAX
  JE @@2
  OR EDX,EDX
  JE @@2
  DEC ECX
  JS @@2

  MOV EBX,[EAX-4]
  SUB EBX,ECX
  JLE @@2
  SUB EBX,[EDX-4]
  JL @@2
  INC EBX

  ADD EAX,ECX
  MOV ECX,EBX
  MOV EBX,[EDX-4]
  DEC EBX
  MOV EDI,EAX
  @@1: MOV ESI,EDX
  LODSB
  REPNE SCASB
  JNE @@2
  MOV EAX,ECX
  PUSH EDI
  MOV ECX,EBX
  REPE CMPSB
  POP EDI
  MOV ECX,EAX
  JNE @@1
  LEA EAX,[EDI-1]
  POP EDX
  SUB EAX,EDX
  INC EAX
  JMP @@3
  @@2: POP EAX
  XOR EAX,EAX
  @@3: POP EBX
  POP ESI
  POP EDI
end;

function SetHeaderValue(const AHeaders, AName, AValue: AnsiString): AnsiString;
var
  VRegExpr: TRegExpr;
begin
  if AHeaders <> '' then begin
    VRegExpr := TRegExpr.Create;
    try
      VRegExpr.Expression := '(?i)' + AName + ':(\s+|)(.*?)(\r\n|$)';
      if VRegExpr.Exec(AHeaders) then begin
        Result := StringReplace(AHeaders, VRegExpr.Match[2], AValue, [rfIgnoreCase]);
      end else begin
        Result := AName + ': ' + AValue + #13#10 + AHeaders;
      end;
    finally
      FreeAndNil(VRegExpr);
    end;
  end else begin
    Result := AName + ': ' + AValue + #13#10;
  end;
end;

function GetHeaderValue(const AHeaders, AName: AnsiString): AnsiString;
var
  VRegExpr: TRegExpr;
begin
  if AHeaders <> '' then begin
    VRegExpr := TRegExpr.Create;
    try
      VRegExpr.Expression := '(?i)' + AName + ':(\s+|)(.*?)(\r\n|$)';
      if VRegExpr.Exec(AHeaders) then begin
        Result := VRegExpr.Match[2];
      end else begin
        Result := '';
      end;
    finally
      FreeAndNil(VRegExpr);
    end;
  end else begin
    Result := '';
  end;
end;

function SaveToLocalFile(const AFullLocalFilename, AData: AnsiString): Integer;
var
  VPath: String;
  VStream: TFileStream;
  VSize: Integer;
begin
  try
    VPath := ExtractFilePath(AFullLocalFilename);
    if (not DirectoryExists(VPath)) then begin
      ForceDirectories(VPath);
    end;
    VStream := TFileStream.Create(AFullLocalFilename, fmCreate);
    try
      VSize := Length(AData);
      if VSize > 0 then begin
        VStream.WriteBuffer(AData[1], VSize);
      end;
      Result := VSize;
    finally
      VStream.Free;
    end;
  except
    Result := 0;
  end;
end;

function GetNumberAfter(const ASubStr, AText: AnsiString): AnsiString;
var
  VPos: Integer;
begin
  Result := '';
  VPos := SYstem.Pos(ASubStr, AText);
  if (VPos > 0) then begin
    VPos := VPos + Length(ASubStr);
    while ((VPos <= System.Length(AText)) and (AText[VPos] in ['0', '1'..'9'])) do begin
      Result := Result + AText[VPos];
      Inc(VPos);
    end;
  end;
end;

function GetDiv3Path(const ASource: AnsiString): AnsiString;
var
  i: Integer;
begin
  Result := '';

  if (0 < Length(ASource)) then begin
    for i := Length(ASource) downto 1 do begin
      if (0 = ((Length(ASource) - i) mod 3)) then begin
        Result := '\' + Result;
      end;
      Result := ASource[i] + Result;
    end;
  end;

  if (Length(Result) > 0) then begin
    if ('\' = Result[1]) then begin
      System.Delete(Result, 1, 1);
    end;
  end;

  i := System.Pos('\', Result);
  if (i < 4) then begin
    System.Delete(Result, 1, i);
  end;
end;

function Base64EncodeStr(const Data: AnsiString): AnsiString;
begin
  Result := Base64Encode(Data);
end;

function Base64UrlEncodeStr(const Data: AnsiString): AnsiString;
begin
  Result := Base64UrlEncode(Data);
end;

function Base64DecodeStr(const Data: AnsiString): AnsiString;
begin
  Result := Base64Decode(Data);
end;

end.
