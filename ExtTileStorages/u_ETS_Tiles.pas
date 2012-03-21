{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
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

unit u_ETS_Tiles;

interface

uses
  SysUtils,
  t_ETS_Tiles;

type
  ETileIdFormatError = class(Exception);

function Convert_XYZ_to_0123(const pXYZ: PTILE_ID_XYZ): AnsiString;
function Convert_0123_to_AHP1(const str_0123: AnsiString): AnsiString;
function Convert_AHP1_to_0123(const str_AHP1: AnsiString): AnsiString;

function ETS_TileId_Conversion_Routine(const ASrcFormat: LongWord;
                                       const ASrcData: Pointer;
                                       const ADstFormat: LongWord;
                                       const ADstData: Pointer): Boolean; stdcall;
                                            
implementation

// can work with string and with buffer of AnsiChars (no unicode here!)
procedure Convert_XYZ_to_0123_Ex(const pXYZ: PTILE_ID_XYZ;
                                 const pSTR: PAnsiString;
                                 const p0123: PTILE_ID_0123);
var
  i, cur: Byte;
  osX,osY,prX,prY: Integer;

  procedure _AddChar(const AChr: AnsiChar);
  begin
    if (nil<>pSTR) then
      pSTR^:=pSTR^+AChr;
    if (nil<>p0123) then begin
      p0123^.id0123[cur]:=AChr;
      Inc(cur);
    end;
  end;
  
begin
  cur:=0;
  if (nil<>pSTR) then
    pSTR^:='';
  if (nil<>p0123) then
    p0123^.id0123[cur]:=#0;
  // check zoom
  if (1<pXYZ^.z) and (pXYZ^.z<=24) then begin
    // correct zoom
    //osX:=round(intpower(2,pXYZ^.z-1)) div 2;
    osX:=(1 shl (pXYZ^.z-2));
    osY:=osX;
    prX:=osX;
    prY:=osY;
    // loop
    for i:=2 to pXYZ^.z do
    begin
      prX:=prX div 2;
      prY:=prY div 2;
      if (pXYZ^.x<osX) then begin
        osX:=osX-prX;
        if (pXYZ^.y<osY) then begin
          osY:=osY-prY;
          _AddChar('0');
        end else begin
          osY:=osY+prY;
          _AddChar('2');
        end;
      end else begin
        osX:=osX+prX;
        if (pXYZ^.y<osY) then begin
          osY:=osY-prY;
          _AddChar('1');
        end else begin
          osY:=osY+prY;
          _AddChar('3');
        end;
      end;
    end;
    // add last #0 to buffer
    if (nil<>p0123) then
      p0123^.id0123[cur]:=#0;
  end;
end;

function Convert_XYZ_to_0123(const pXYZ: PTILE_ID_XYZ): AnsiString;
begin
  Convert_XYZ_to_0123_Ex(pXYZ, @Result, nil);
end;

procedure Convert_0123_to_AHP1_Ex(const pSTR_0123_SRC: PAnsiString;
                                  const p0123_SRC: PTILE_ID_0123;
                                  const pSTR_AHP1_DST: PAnsiString;
                                  const pAHP1_DST: PTILE_ID_AHP1);

  procedure _CheckByte(const bt: Byte);
  var s: String;
  begin
    if (not (bt in [1,2,3])) then begin
      // make exception text
      if (nil<>p0123_SRC) then begin
        // from buffer
        s:=PAnsiChar(@(p0123_SRC^.id0123[0]));
      end else begin
        // from string
        s:=pSTR_0123_SRC^;
      end;
      raise ETileIdFormatError.Create(s);
    end;
  end;

var
  cur,i,v1,v2: Byte;
  c1,c2: AnsiChar;

  procedure _AddChar(const AChr: AnsiChar);
  begin
    if (nil<>pSTR_AHP1_DST) then
      pSTR_AHP1_DST^:=pSTR_AHP1_DST^+AChr;
    if (nil<>pAHP1_DST) then begin
      pAHP1_DST^.idAHP1[cur]:=AChr;
      Inc(cur);
      pAHP1_DST^.idAHP1[cur]:=#0;
    end;
  end;
  
begin
  cur:=0;

  // reset results
  if (nil<>pSTR_AHP1_DST) then
    pSTR_AHP1_DST^:='';
  if (nil<>pAHP1_DST) then
    pAHP1_DST^.idAHP1[cur]:=#0;

  // no source id
  if (nil<>pSTR_0123_SRC) then
    if (0=Length(pSTR_0123_SRC^)) then
      Exit;
  if (nil<>p0123_SRC) then
    if (#0=p0123_SRC^.id0123[0]) then
      Exit;

  // loop (index by array - for string use +1)
  i:=0;
  repeat
    // if no first char - exit
    // if no second char - add first char to result and exit
    if (nil<>p0123_SRC) then begin
      // from buffer
      if (#0=p0123_SRC^.id0123[i]) then
        Exit;
      c1:=p0123_SRC^.id0123[i];
      Inc(i);
      if (#0=p0123_SRC^.id0123[i]) then begin
        _AddChar(c1);
        Exit;
      end;
      c2:=p0123_SRC^.id0123[i];
      Inc(i);
    end else begin
      // from string
      if ((i+1)>Length(pSTR_0123_SRC^)) then
        Exit;
      c1:=pSTR_0123_SRC^[i+1];
      Inc(i);
      if ((i+1)>Length(pSTR_0123_SRC^)) then begin
        _AddChar(c1);
        Exit;
      end;
      c2:=pSTR_0123_SRC^[i+1];
      Inc(i);
    end;

    // found 2 chars - c1 and c2 in ['0'..'3']
    if ('0'=c1) then
      v1:=0
    else begin
      v1:=(Ord(c1)-Ord('1')+1);
      _CheckByte(v1);
    end;

    if ('0'=c2) then
      v2:=0
    else begin
      v2:=(Ord(c2)-Ord('1')+1);
      _CheckByte(v2);
    end;

    v2:=v2+4*v1; // v2 in 0..15

    // map to 'ABCD EFGH IJKL MNOP'
    _AddChar(Chr(Ord('A')+v2));
  until FALSE;
end;

function Convert_0123_to_AHP1(const str_0123: AnsiString): AnsiString;
begin
  Convert_0123_to_AHP1_Ex(@str_0123, nil, @Result, nil);
end;

procedure Convert_AHP1_to_0123_Ex(const pSTR_AHP1_SRC: PAnsiString;
                                  const pAHP1_SRC: PTILE_ID_AHP1;
                                  const pSTR_0123_DST: PAnsiString;
                                  const p0123_DST: PTILE_ID_0123);

  function _get_src_id: String;
  begin
    if (nil<>pAHP1_SRC) then begin
      // from buffer
      Result:=PAnsiChar(@(pAHP1_SRC^.idAHP1[0]));
    end else begin
      // from string
      Result:=pSTR_AHP1_SRC^;
    end;
  end;
  
var
  cur,i,v1,v2: Byte;
  c1: AnsiChar;

  procedure _AddChar(const AChr: AnsiChar);
  begin
    if (nil<>pSTR_0123_DST) then
      pSTR_0123_DST^:=pSTR_0123_DST^+AChr;
    if (nil<>p0123_DST) then begin
      p0123_DST^.id0123[cur]:=AChr;
      Inc(cur);
      p0123_DST^.id0123[cur]:=#0;
    end;
  end;
  
begin
  cur:=0;

  // reset results
  if (nil<>pSTR_0123_DST) then
    pSTR_0123_DST^:='';
  if (nil<>p0123_DST) then
    p0123_DST^.id0123[cur]:=#0;

  // no source id
  if (nil<>pSTR_AHP1_SRC) then
    if (0=Length(pSTR_AHP1_SRC^)) then
      Exit;
  if (nil<>pAHP1_SRC) then
    if (#0=pAHP1_SRC^.idAHP1[0]) then
      Exit;

  // loop (index by array - for string use +1)
  i:=0;
  repeat
    // get char (if no char - exit)
    if (nil<>pAHP1_SRC) then begin
      // from buffer
      if (#0=pAHP1_SRC^.idAHP1[i]) then
        Exit;
      c1:=pAHP1_SRC^.idAHP1[i];
      Inc(i);
    end else begin
      // from string
      if ((i+1)>Length(pSTR_AHP1_SRC^)) then
        Exit;
      c1:=pSTR_AHP1_SRC^[i+1];
      Inc(i);
    end;

    // check char
    if (c1 in ['0','1','2','3']) then begin
      _AddChar(c1);
      // only last char goes here
      Exit;
    end else if (Ord(c1)<Ord('A')) then begin
      // invalid format
      raise ETileIdFormatError.Create(_get_src_id);
    end else begin
      v1:=(Ord(c1)-Ord('A'));
      // check v1 in 0..15
      if (v1>=16) then
        raise ETileIdFormatError.Create(_get_src_id);
      // divide into (4*v1+v2)
      v2:=(v1 mod 4);
      v1:=(v1 div 4);
      // add first char
      if (0=v1) then
        c1:='0'
      else
        c1:=Chr(Ord('1')+v1-1);
      _AddChar(c1);
      // add second char
      if (0=v2) then
        c1:='0'
      else
        c1:=Chr(Ord('1')+v2-1);
      _AddChar(c1);
    end;
  until FALSE;
end;

function Convert_AHP1_to_0123(const str_AHP1: AnsiString): AnsiString;
begin
  Convert_AHP1_to_0123_Ex(@str_AHP1, nil, @Result, nil);
end;

function ETS_TileId_Conversion_Routine(const ASrcFormat: LongWord;
                                       const ASrcData: Pointer;
                                       const ADstFormat: LongWord;
                                       const ADstData: Pointer): Boolean; stdcall;
var
  V0123: TTILE_ID_0123;
begin
  Result := FALSE;
  try
    if (TILE_ID_FORMAT_XYZ=ASrcFormat) and (TILE_ID_FORMAT_0123=ADstFormat) then begin
      // xyz -> 0123
      Convert_XYZ_to_0123_Ex(ASrcData, nil, ADstData);
      Inc(Result);
    end else if (TILE_ID_FORMAT_XYZ=ASrcFormat) and (TILE_ID_FORMAT_AHP1=ADstFormat) then begin
      // xyz -> 0123 -> AHP1
      Convert_XYZ_to_0123_Ex(ASrcData, nil, @V0123);
      Convert_0123_to_AHP1_Ex(nil, @V0123, nil, ADstData);
      Inc(Result);
    end else if (TILE_ID_FORMAT_0123=ASrcFormat) and (TILE_ID_FORMAT_AHP1=ADstFormat) then begin
      // 0123 -> AHP1
      Convert_0123_to_AHP1_Ex(nil, ASrcData, nil, ADstData);
      Inc(Result);
    end else if (TILE_ID_FORMAT_AHP1=ASrcFormat) and (TILE_ID_FORMAT_0123=ADstFormat) then begin
      // AHP1 -> 0123
      Convert_AHP1_to_0123_Ex(nil, ASrcData, nil, ADstData);
      Inc(Result);
    end else begin
      // not implemented yet or not supported
    end;
  except
    // conversion exception
  end;
end;

end.