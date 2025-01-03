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

unit u_AnsiStr;

interface

uses
  SysUtils,
  AnsiStrings,
  Alcinoe.StringUtils,
  Alcinoe.StringList;

//---------------- AnsiStrings ------------------------------------------------
type
  TStrLenA = function(const S: PAnsiChar): Cardinal;
  TStrLCopyA = function(Dest: PAnsiChar; const Source: PAnsiChar; MaxLen: Cardinal): PAnsiChar;
  TStrICompA = function(const S1, S2: PAnsiChar): Integer;

  TTextToFloatA = function(Buffer: PAnsiChar; var Value; ValueType: TFloatValue;
    const AFormatSettings: TFormatSettings): Boolean;

  TLowerCaseA = function(const S: AnsiString): AnsiString;
  TUpperCaseA = function(const S: AnsiString): AnsiString;

  TSameTextA = function(const S1, S2: AnsiString): Boolean;
  TStringReplaceA = function(const Source, OldPattern, NewPattern: AnsiString; Flags: TReplaceFlags): AnsiString;

  TTrimA = function(const S: AnsiString): AnsiString;

  TQuotedStrA = function(const S: AnsiString): AnsiString;

const
  StrLenA: TStrLenA = System.AnsiStrings.StrLen;
  StrLCopyA: TStrLCopyA = System.AnsiStrings.StrLCopy;
  StrICompA: TStrICompA = System.AnsiStrings.StrIComp;

  TextToFloatA: TTextToFloatA = System.AnsiStrings.TextToFloat;

  LowerCaseA: TLowerCaseA = System.AnsiStrings.LowerCase;
  UpperCaseA: TUpperCaseA = System.AnsiStrings.UpperCase;

  SameTextA: TSameTextA = System.AnsiStrings.SameText;
  StringReplaceA: TStringReplaceA = System.AnsiStrings.StringReplace;

  TrimA: TTrimA = System.AnsiStrings.Trim;

  QuotedStrA: TQuotedStrA = System.AnsiStrings.QuotedStr;

function FormatA(const Format: AnsiString; const Args: array of const): AnsiString; overload;
function FormatA(const Format: AnsiString; const Args: array of const;
  const AFormatSettings: TFormatSettings): AnsiString; overload;

//----------------- Alcinoe ---------------------------------------------------
type
  TFormatSettingsA = Alcinoe.StringUtils.TALFormatSettingsA;

  TStringListA = class(Alcinoe.StringList.TALStringListA)
  public
    function Find(const S: AnsiString; var Index: Integer): Boolean; override;
  end;

  TStringsEnumeratorA = Alcinoe.StringList.TALStringsEnumeratorA;

  TStringStreamA = Alcinoe.StringUtils.TALStringStreamA;

  TFloatToStrA = function(Value: Extended; const AFormatSettings: TALFormatSettingsA): AnsiString;
  TFloatToStrFA = function(Value: Extended; Format: TFloatFormat; Precision, Digits: Integer;
    const AFormatSettings: TALFormatSettingsA): AnsiString;

  TStrToFloatA = function(const S: AnsiString; const AFormatSettings: TALFormatSettingsA): Extended;

  TIntToHexA = function(Value: Integer; Digits: Integer): AnsiString;
  TIntToStrA = function(Value: Integer): AnsiString;

  TStrToIntA = function(const S: AnsiString): Integer;
  TStrToIntDefA = function(const S: AnsiString; Default: Integer): Integer;
  TTryStrToIntA = function(const S: AnsiString; out Value: Integer): Boolean;
  TTryStrToInt64A = function(const S: AnsiString; out Value: Int64): Boolean;

  TStrToDateA = function(const S: AnsiString; const AFormatSettings: TALFormatSettingsA): TDateTime;
  TStrToTimeA = function(const S: AnsiString; const AFormatSettings: TALFormatSettingsA): TDateTime;
  TDateTimeToStrA = function(const DateTime: TDateTime; const AFormatSettings: TALFormatSettingsA): AnsiString;
  TFormatDateTimeA = function(const Format: AnsiString; DateTime: TDateTime; const AFormatSettings: TALFormatSettingsA): AnsiString;

const
  FloatToStrA: TFloatToStrA = Alcinoe.StringUtils.ALFloatToStrA;
  FloatToStrFA: TFloatToStrFA = Alcinoe.StringUtils.ALFloatToStrFA;

  StrToFloatA: TStrToFloatA = Alcinoe.StringUtils.ALStrToFloat;

  IntToHexA: TIntToHexA = Alcinoe.StringUtils.ALIntToHexA;
  IntToStrA: TIntToStrA = Alcinoe.StringUtils.ALIntToStrA;

  StrToIntA: TStrToIntA = Alcinoe.StringUtils.ALStrToInt;
  StrToIntDefA: TStrToIntDefA = Alcinoe.StringUtils.ALStrToIntDef;

  TryStrToIntA: TTryStrToIntA = Alcinoe.StringUtils.ALTryStrToInt;
  TryStrToInt64A: TTryStrToInt64A = Alcinoe.StringUtils.ALTryStrToInt64;

  StrToDateA: TStrToDateA = Alcinoe.StringUtils.ALStrToDate;
  StrToTimeA: TStrToTimeA = Alcinoe.StringUtils.ALStrToTime;
  DateTimeToStrA: TDateTimeToStrA = Alcinoe.StringUtils.ALDateTimeToStrA;
  FormatDateTimeA: TFormatDateTimeA = Alcinoe.StringUtils.ALFormatDateTimeA;

//----------------- System ----------------------------------------------------

function PosA(const SubStr, Str: AnsiString; const Offset: Integer = 1): Integer; inline;

//----------------- Helpers ---------------------------------------------------

function IsAscii(const P: PChar; const Len: Integer): Boolean; overload; inline;
function IsAscii(const s: string): Boolean; overload; inline;

function IsAscii(const P: PAnsiChar; const Len: Integer): Boolean; overload; inline;
function IsAscii(const s: AnsiString): Boolean; overload; inline;

function StringToAsciiSafe(const s: string): AnsiString; inline;

function IsAnsi(const s: string): Boolean; inline;
function StringToAnsiSafe(const s: string): AnsiString; inline;

implementation

function FormatA(const Format: AnsiString; const Args: array of const): AnsiString;
begin
  System.AnsiStrings.FmtStr(Result, Format, Args, FormatSettings);
end;

function FormatA(const Format: AnsiString; const Args: array of const;
  const AFormatSettings: TFormatSettings): AnsiString;
begin
  System.AnsiStrings.FmtStr(Result, Format, Args, AFormatSettings);
end;

function PosA(const SubStr, Str: AnsiString; const Offset: Integer = 1): Integer;
begin
  Result := System.Pos(SubStr, Str, Offset);
end;

function IsAscii(const s: string): Boolean; overload;
var
  VLen: Integer;
begin
  VLen := Length(s);
  if VLen > 0 then begin
    Result := IsAscii(PChar(s), VLen);
  end else begin
    Result := True;
  end;
end;

function IsAscii(const P: PChar; const Len: Integer): Boolean;
var
  I: Integer;
begin
  Result := False;
  if P <> nil then begin
    for I := 0 to Len - 1 do begin
      if Ord(P[I]) > 127 then begin
        Exit;
      end;
    end;
  end;
  Result := True;
end;

function IsAscii(const P: PAnsiChar; const Len: Integer): Boolean;
var
  I: Integer;
begin
  Result := False;
  if P <> nil then begin
    for I := 0 to Len - 1 do begin
      if Ord(P[I]) > 127 then begin
        Exit;
      end;
    end;
  end;
  Result := True;
end;

function IsAscii(const s: AnsiString): Boolean;
var
  VLen: Integer;
begin
  VLen := Length(s);
  if VLen > 0 then begin
    Result := IsAscii(PAnsiChar(s), VLen);
  end else begin
    Result := True;
  end;
end;

function StringToAsciiSafe(const s: string): AnsiString;
begin
  if IsAscii(s) then begin
    Result := AnsiString(s);
  end else begin
    raise Exception.CreateFmt('String "%s" contain non-ascii characters!', [s]);
  end;
end;

function IsAnsi(const s: string): Boolean;
var
  VAnsi: AnsiString;
  VStr: string;
begin
  VAnsi := AnsiString(s);
  VStr := string(VAnsi);
  Result := VStr = s;
end;

function StringToAnsiSafe(const s: string): AnsiString;
begin
  Result := AnsiString(s);
  if string(Result) <> s then begin
    raise Exception.CreateFmt('String "%s" contain non-ansi characters!', [s]);
  end;
end;

{ TStringListA }

function TStringListA.Find(const S: AnsiString; var Index: Integer): Boolean;
begin
  Result := inherited Find(S, Index);
end;

end.
