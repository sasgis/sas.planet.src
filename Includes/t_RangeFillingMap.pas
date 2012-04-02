{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit t_RangeFillingMap;

interface

uses
  Windows;

type
  // Warning! Used for DLL-based tile storages! Add new fields only at the end of the record!
  TRangeFillingMapInfo = packed record
    SourceZoom: Byte;
    Zoom: Byte;
    ItemSize: SmallInt;
    TileMapSize: LongWord;
    TileMapAddr: Pointer;
  end;
  PRangeFillingMapInfo = ^TRangeFillingMapInfo;

  // Warning!
  // Used for DLL-based tile storages to minimize required buffer size.
  // Do not modify without changing those DLLs.

  // flags only
  TRangeFillingItem1 = packed record {1 byte}
    // 0 - unknown
    // 1 - tile exists
    // 2 - tne exists
    // 3 - neither tile nor tne
    Flags: Byte;
    // routines
    function GetNeitherTileNorTNE: Boolean;
    function IsTileExists: Boolean;
    function IsTNEExists: Boolean;
    procedure SetWithoutDateTime(const AFlag: Byte);
  end;
  PRangeFillingItem1 = ^TRangeFillingItem1;
  
  // approx. datetime with flags
  TRangeFillingItem4 = packed record {4 bytes}
    // 0 - unknown
    // 1 - tile exists
    // 2 - tne exists
    // 3 - neither tile nor tne
    // value shr 2 = ((((year*16+month)*32+day)*32+hour)*64+min)*2+halfmin
    // so:
    // year from 0 to 512 - i.e. number of year since January 1, 1601 up to 2112
    // any time (to within 30 seconds)
    // allow obtain datetime for both tile and tne
    ValueWithFlags: LongWord;
    // routines
    function GetAsDateTime: TDateTime;
    function GetNeitherTileNorTNE: Boolean;
    function IsTileExists: Boolean;
    function IsTNEExists: Boolean;
    procedure SetAsDateTime(const AValue: TDateTime; const AFlag: Byte);
    procedure SetAsYMD(const y,m,d: Word; const AFlag: Byte);
    procedure SetWithoutDateTime(const AFlag: Byte);
  end;
  PRangeFillingItem4 = ^TRangeFillingItem4;

  // full datetime with flags
  TRangeFillingItem8 = packed record {8 bytes}
    // routines
    function GetAsDateTime: TDateTime;
    function GetNeitherTileNorTNE: Boolean;
    function IsTileExists: Boolean;
    function IsTNEExists: Boolean;
    procedure SetAsDateTime(const AValue: TDateTime; const AFlag: Byte);
    procedure SetAsYMD(const y,m,d: Word; const AFlag: Byte);
    procedure SetWithoutDateTime(const AFlag: Byte);
    // sizeof(TDateTime) = sizeof(UInt64) = 8 Bytes
    // the highest byte of TDateTime - see bellow
    // so:
    // full TDateTime range
    // allow obtain datetime for both tile and tne
  case Boolean of
  FALSE: (
    FullValue: UInt64;
  );
  TRUE: (
    LowPart: LongWord;
    HighWord: Word;
    HighByteL: Byte;
    HighByteH: Byte;
  );
  end;
  PRangeFillingItem8 = ^TRangeFillingItem8;

const
  CbitN8 = 4;
  CbitM8 = 8;
  CbitT8 = 16;

  CbitN4 = 3;
  CbitM4 = 2;
  CbitT4 = 1;

  CbitN1 = 3;
  CbitM1 = 2;
  CbitT1 = 1;

(*
0x000000003D5B262E = DateTime: 30.12.1899
0x3EE842AD3D5B262E = DateTime: 30.12.1899
0x3EE842AE3D5B262E = DateTime: 30.12.1899 00:00:01
0x3EF000003D5B262E = DateTime: 30.12.1899 00:00:01
0x3EFE00003D5B262E = DateTime: 30.12.1899 00:00:02
0x3F0000003D5B262E = DateTime: 30.12.1899 00:00:02
0x3FFF00003D5B262E = DateTime: 31.12.1899 22:30:00
0x3FFFFFFF3D5B262E = DateTime: 31.12.1899 23:59:59
0x400000003D5B262E = DateTime: 01.01.1900
0x408000003D5B262E = DateTime: 26.05.1901 00:00:10
0x410000003D5B262E = DateTime: 10.11.2258 00:43:08
0x41DF00003D5B262E = DateTime: 13.05.61677 10:10:51
0x41E000003D5B262E = DateTime: 00.00.0000 20:21:42

0x0000000000000000 = DateTime: 30.12.1899
0x3EE842AD00000000 = DateTime: 30.12.1899
0x3EE842AE00000000 = DateTime: 30.12.1899 00:00:01
0x3EF0000000000000 = DateTime: 30.12.1899 00:00:01
0x3EFE000000000000 = DateTime: 30.12.1899 00:00:02
0x3F00000000000000 = DateTime: 30.12.1899 00:00:02
0x3FFF000000000000 = DateTime: 31.12.1899 22:30:00
0x3FFFFFFF00000000 = DateTime: 31.12.1899 23:59:59
0x4000000000000000 = DateTime: 01.01.1900
0x4080000000000000 = DateTime: 26.05.1901
0x4100000000000000 = DateTime: 10.11.2258
0x41DF000000000000 = DateTime: 10.09.61676
0x41E0000000000000 = DateTime: 00.00.0000


0xBEE842AD00000000 = DateTime: 30.12.1899
0xBEE842AE00000000 = DateTime: 30.12.1899 00:00:01
0xBEE842AF00000000 = DateTime: 30.12.1899 00:00:01
0xBF00000000000000 = DateTime: 30.12.1899 00:00:02
0xBFF0000000000000 = DateTime: 29.12.1899
0xBFFF000000000000 = DateTime: 29.12.1899 22:30:00
0xC000000000000000 = DateTime: 28.12.1899
0xC008000000000000 = DateTime: 27.12.1899
0xC100000000000000 = DateTime: 18.02.1541
0xC1252AB200000000 = DateTime: 01.01.0001
0xC1252AB300000000 = DateTime: 01.01.0001 12:00:00
0xC1252AB400000000 = DateTime: 00.00.0000

0x00000000423A35C7 = DateTime: 30.12.1899
0xBEE842AD423A35C7 = DateTime: 30.12.1899
0xBEE842AE423A35C7 = DateTime: 30.12.1899 00:00:01
0xBEE842AF423A35C7 = DateTime: 30.12.1899 00:00:01
0xBF000000423A35C7 = DateTime: 30.12.1899 00:00:02
0xBFF00000423A35C7 = DateTime: 29.12.1899
0xBFFF0000423A35C7 = DateTime: 29.12.1899 22:30:00
0xC0000000423A35C7 = DateTime: 28.12.1899
0xC1000000423A35C7 = DateTime: 18.02.1541 00:46:33
0xC1252AB2423A35C7 = DateTime: 01.01.0001 03:06:15
0xC1252AB3423A35C7 = DateTime: 01.01.0001 15:06:15
0xC1252AB4423A35C7 = DateTime: 00.00.0000 03:06:15

Highest byte is:
00 = 00 0000 00
3E = 00 1111 10
3F = 00 1111 11
40 = 01 0000 00
41 = 01 0000 01
BE = 10 1111 10
BF = 10 1111 11
C0 = 11 0000 00
C1 = 11 0000 01
        RTMN
        R - reference bit
        T - tile exists bit (TRUE if inverted)
        M - TNE marker exists bit (TRUE if inverted)
        N - neither tile nor tne bit (TRUE if inverted)
*)

implementation

uses
  SysUtils {for TryEncodeDate},
  DateUtils {for TryEncodeDateTime and DecodeDateTime};

const
  CbitR8 = 32;
  CmaskR8 = (CbitT8+CbitM8+CbitN8);
  CexceptR8 = ($FF - CmaskR8);

{ TRangeFillingItem8 }

function TRangeFillingItem8.GetAsDateTime: TDateTime;
begin
  Move(FullValue, Result, SizeOf(Result));
  // modify result
  with PRangeFillingItem8(Pointer(@Result))^ do begin
    // copy reference bit to other flags
    if ((HighByteH and CbitR8) <> 0) then begin
      // on
      HighByteH := (HighByteH or CmaskR8);
    end else begin
      // off
      HighByteH := (HighByteH and CexceptR8);
    end;
  end;
end;

function TRangeFillingItem8.GetNeitherTileNorTNE: Boolean;
begin
  // check reference bit
  if ((HighByteH and CbitR8) <> 0) then begin
    // reference bit on - true if N bit off
    Result := (0 = (HighByteH and CbitN8));
  end else begin
    // reference bit off - true if N bit on
    Result := (0 <> (HighByteH and CbitN8));
  end;
end;

function TRangeFillingItem8.IsTileExists: Boolean;
begin
  // check reference bit
  if ((HighByteH and CbitR8) <> 0) then begin
    // reference bit on - true if M bit off
    Result := (0 = (HighByteH and CbitM8));
  end else begin
    // reference bit off - true if M bit on
    Result := (0 <> (HighByteH and CbitM8));
  end;
end;

function TRangeFillingItem8.IsTNEExists: Boolean;
begin
  // check reference bit
  if ((HighByteH and CbitR8) <> 0) then begin
    // reference bit on - true if T bit off
    Result := (0 = (HighByteH and CbitT8));
  end else begin
    // reference bit off - true if T bit on
    Result := (0 <> (HighByteH and CbitT8));
  end;
end;

procedure TRangeFillingItem8.SetAsDateTime(const AValue: TDateTime; const AFlag: Byte);
begin
  // set datetime
  Move(AValue, FullValue, SizeOf(AValue));
  // invert T or M bit
  HighByteH := (HighByteH xor AFlag);
end;

procedure TRangeFillingItem8.SetAsYMD(const y, m, d: Word; const AFlag: Byte);
var dt: TDateTime;
begin
  if TryEncodeDate(y,m,d,dt) then begin
    // set datetime
    Move(dt, FullValue, SizeOf(dt));
  end;
  // invert T or M bit
  HighByteH := (HighByteH xor AFlag);
end;

procedure TRangeFillingItem8.SetWithoutDateTime(const AFlag: Byte);
begin
  // set only if all zero - so just set bit
  HighByteH := (HighByteH or AFlag);
end;

{ TRangeFillingItem4 }

function TRangeFillingItem4.GetAsDateTime: TDateTime;
var
  st: TSystemTime;
  VValue: LongWord;
begin
  // value shr 2 = ((((year*16+month)*32+day)*32+hour)*64+min)*2+halfmin
  VValue := ValueWithFlags shr 2;
  if (0=VValue) then begin
    // without date
    Result := 0;
  end else begin
    // some value
    st.wDayOfWeek := 0;
    st.wMilliseconds := 0;
    if (0 <> (LoByte(LoWord(VValue)) and 1)) then
      st.wSecond := 30
    else
      st.wSecond := 0;
    // shift and div
    VValue := VValue shr 1; // div 2
    // min
    st.wMinute := LoWord(VValue) and 63;
    VValue := VValue shr 6; // div 64
    // hour
    st.wHour := LoWord(VValue) and 31;
    VValue := VValue shr 5; // div 32
    // day
    st.wDay := LoWord(VValue) and 31;
    VValue := VValue shr 5; // div 32
    // month
    st.wMonth := LoWord(VValue) and 15;
    VValue := VValue shr 4; // div 16
    // year
    st.wYear := LoWord(VValue);
    // convert
    // SystemTimeToDateTime()
    if not TryEncodeDateTime(st.wYear, st.wMonth, st.wDay, st.wHour, st.wMinute, st.wSecond, 0, Result) then
      Result := 0;
  end;
end;

function TRangeFillingItem4.GetNeitherTileNorTNE: Boolean;
begin
  Result := (CbitN4 = (LoByte(LoWord(ValueWithFlags)) and CbitN4));
end;

function TRangeFillingItem4.IsTileExists: Boolean;
begin
  Result := (CbitT4 = (LoByte(LoWord(ValueWithFlags)) and CbitN4));
end;

function TRangeFillingItem4.IsTNEExists: Boolean;
begin
  Result := (CbitM4 = (LoByte(LoWord(ValueWithFlags)) and CbitN4));
end;

procedure TRangeFillingItem4.SetAsDateTime(const AValue: TDateTime; const AFlag: Byte);
var st: TSystemTime;
begin
  // value shr 2 = ((((year*16+month)*32+day)*32+hour)*64+min)*2+halfmin
  if (0=AValue) then begin
    // no value
    ValueWithFlags := 0;
  end else
  try
    // some value
    DecodeDateTime(AValue, st.wYear, st.wMonth, st.wDay, st.wHour, st.wMinute, st.wSecond, st.wMilliseconds);
    // add datetime
    Inc(ValueWithFlags, st.wYear);
    ValueWithFlags := ValueWithFlags shl 4; // *16
    Inc(ValueWithFlags, st.wMonth);
    ValueWithFlags := ValueWithFlags shl 5; // *32
    Inc(ValueWithFlags, st.wDay);
    ValueWithFlags := ValueWithFlags shl 5; // *32
    Inc(ValueWithFlags, st.wHour);
    ValueWithFlags := ValueWithFlags shl 6; // *64
    Inc(ValueWithFlags, st.wMinute);
    ValueWithFlags := ValueWithFlags shl 1; // *2
    // add halfmin
    if (st.wSecond>=45) then
      Inc(ValueWithFlags, 2)
    else if (st.wSecond>=15) then
      Inc(ValueWithFlags, 1);
    // shift
    ValueWithFlags := ValueWithFlags shl 2;
  except
  end;
  SetWithoutDateTime(AFlag);
end;

procedure TRangeFillingItem4.SetAsYMD(const y, m, d: Word; const AFlag: Byte);
begin
  if ((0=y) and (0=m) and (0=d)) then begin
    // no value
    ValueWithFlags := 0;
  end else begin
    // some YMD
    Inc(ValueWithFlags, y);
    ValueWithFlags := ValueWithFlags shl 4; // *16
    Inc(ValueWithFlags, m);
    ValueWithFlags := ValueWithFlags shl 5; // *32
    Inc(ValueWithFlags, d);
    ValueWithFlags := ValueWithFlags shl (5+6+1); // *32 *64 *2
    // shift
    ValueWithFlags := ValueWithFlags shl 2;
  end;
  SetWithoutDateTime(AFlag);
end;

procedure TRangeFillingItem4.SetWithoutDateTime(const AFlag: Byte);
begin
  ValueWithFlags := (ValueWithFlags or AFlag);
end;

{ TRangeFillingItem1 }

function TRangeFillingItem1.GetNeitherTileNorTNE: Boolean;
begin
  Result := (CbitN1 = (Flags and CbitN1));
end;

function TRangeFillingItem1.IsTileExists: Boolean;
begin
  Result := (CbitT1 = (Flags and CbitN1));
end;

function TRangeFillingItem1.IsTNEExists: Boolean;
begin
  Result := (CbitM1 = (Flags and CbitN1));
end;

procedure TRangeFillingItem1.SetWithoutDateTime(const AFlag: Byte);
begin
  Flags := (Flags or AFlag);
end;

end.
