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

unit u_PascalScriptUtils;

interface

uses
  uPSRuntime,
  uPSCompiler;

procedure CompileTimeReg_Utils(const APSComp: TPSPascalCompiler);
procedure ExecTimeReg_Utils(const APSExec: TPSExec);

implementation

uses
  SysUtils,
  Classes,
  ALString,
  DateUtils,
  MD5,
  u_GeoToStrFunc,
  u_StrFunc;

procedure CompileTimeReg_Utils(const APSComp: TPSPascalCompiler);
begin
  // SysUtils
  APSComp.AddDelphiFunction('function IntToHex(Value: Integer; Digits: Integer): String');
  APSComp.AddDelphiFunction('function FileExists(const FileName: String): Boolean');

  // ALString
  APSComp.AddTypeS('TReplaceFlag', '(rfReplaceAll, rfIgnoreCase)');
  APSComp.AddTypeS('TReplaceFlags', 'set of TReplaceFlag');
  APSComp.AddDelphiFunction('function StringReplace(const S, OldPattern, NewPattern: AnsiString; Flags: TReplaceFlags): AnsiString');

  // MD5
  APSComp.AddDelphiFunction('function MD5String(const AStr: AnsiString): String');

  // u_GeoToStrFunc
  APSComp.AddDelphiFunction('function RoundEx(const chislo: Double; const Precision: Integer): String');

  // u_StrFunc
  APSComp.AddDelphiFunction('function GetAfter(SubStr, Str: AnsiString): AnsiString');
  APSComp.AddDelphiFunction('function GetBefore(SubStr, Str: AnsiString): AnsiString');
  APSComp.AddDelphiFunction('function GetBetween(Str, After, Before: AnsiString): AnsiString');

  APSComp.AddDelphiFunction('function SetHeaderValue(AHeaders, AName, AValue: AnsiString): AnsiString');
  APSComp.AddDelphiFunction('function GetHeaderValue(AHeaders, AName: AnsiString): AnsiString');

  // internal routines
  APSComp.AddDelphiFunction('function SubStrPos(const Str, SubStr: AnsiString; FromPos: Integer): Integer');

  APSComp.AddDelphiFunction('function GetNumberAfter(const ASubStr, AText: string): string');
  APSComp.AddDelphiFunction('function GetDiv3Path(const ANumber: string): string');

  APSComp.AddDelphiFunction('function GetUnixTime: Int64');
  APSComp.AddDelphiFunction('function SaveToLocalFile(const AFullLocalFilename: string; const AData: AnsiString): Integer');
end;

function SubStrPos_P(const Str, SubStr: AnsiString; FromPos: Integer): Integer; assembler;
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

function GetNumberAfter_P(const ASubStr, AText: string): string;
var
  VPos: Integer;
begin
  Result := '';
  VPos := System.Pos(ASubStr, AText);
  if (VPos > 0) then begin
    VPos := VPos + Length(ASubStr);
    while ((VPos <= System.Length(AText)) and CharInSet(AText[VPos], ['0', '1'..'9'])) do begin
      Result := Result + AText[VPos];
      Inc(VPos);
    end;
  end;
end;

function GetDiv3Path_P(const ASource: string): string;
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

function GetUnixTime_P: Int64;
begin
  Result := DateTimeToUnix(now);
end;

function SaveToLocalFile_P(const AFullLocalFilename: string; const AData: AnsiString): Integer;
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

function MD5String_P(const AStr: AnsiString): string;
begin
  Result := MD5DigestToStr(MD5Buffer(PAnsiChar(AStr)^, Length(AStr)));
end;

procedure ExecTimeReg_Utils(const APSExec: TPSExec);
begin
  // SysUtils
  APSExec.RegisterDelphiFunction(@IntToHex, 'IntToHex', cdRegister);
  APSExec.RegisterDelphiFunction(@FileExists, 'FileExists', cdRegister);

  // ALString
  APSExec.RegisterDelphiFunction(@ALStringReplace, 'StringReplace', cdRegister);

  // MD5
  APSExec.RegisterDelphiFunction(@MD5String_P, 'MD5String', cdRegister);

  // u_GeoToStrFunc
  APSExec.RegisterDelphiFunction(@RoundEx, 'RoundEx', cdRegister);

  // u_StrFunc
  APSExec.RegisterDelphiFunction(@GetAfter, 'GetAfter', cdRegister);
  APSExec.RegisterDelphiFunction(@GetBefore, 'GetBefore', cdRegister);
  APSExec.RegisterDelphiFunction(@GetBetween, 'GetBetween', cdRegister);

  APSExec.RegisterDelphiFunction(@SetHeaderValue, 'SetHeaderValue', cdRegister);
  APSExec.RegisterDelphiFunction(@GetHeaderValue, 'GetHeaderValue', cdRegister);

  // internal routines
  APSExec.RegisterDelphiFunction(@SubStrPos_P, 'SubStrPos', cdRegister);

  APSExec.RegisterDelphiFunction(@GetNumberAfter_P, 'GetNumberAfter', cdRegister);
  APSExec.RegisterDelphiFunction(@GetDiv3Path_P, 'GetDiv3Path', cdRegister);

  APSExec.RegisterDelphiFunction(@GetUnixTime_P, 'GetUnixTime', cdRegister);
  APSExec.RegisterDelphiFunction(@SaveToLocalFile_P, 'SaveToLocalFile', cdRegister);
end;

end.
