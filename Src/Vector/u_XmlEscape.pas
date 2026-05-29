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

unit u_XmlEscape;

interface

function XmlEscapeText(const AText: string; const AUseNumericReference: Boolean = True): string;
function XmlEscapeAttr(const AValue: string; const AUseNumericReference: Boolean = True): string;

implementation

const
  XML_ESC_HIGH = 63; // Ord('>') = 62 - highest special character

type
  TXmlEscTable = array[0..XML_ESC_HIGH] of Byte; // 64 bytes

const
  XET_NONE    = 0;  // ordinary character, no escaping needed
  XET_TAB     = 1;  // #09
  XET_LF      = 2;  // #10
  XET_CR      = 3;  // #13
  XET_LT      = 4;  // <
  XET_GT      = 5;  // >
  XET_AMP     = 6;  // &
  XET_QUOT    = 7;  // "
  XET_APOS    = 8;  // '
  XET_ILLEGAL = 9;  // illegal control character - drop silently (replacement = '')

type
  TReplacement = record
    Ptr: PChar;
    Len: NativeInt;
  end;
  PReplacement = ^TReplacement;

  TReplacementArray = array[1..9] of TReplacement;

const
  XML_NAMED: TReplacementArray = (
    (Ptr: '&#x09;'; Len: 6),
    (Ptr: '&#x0a;'; Len: 6),
    (Ptr: '&#x0d;'; Len: 6),

    (Ptr: '&lt;';   Len: 4),
    (Ptr: '&gt;';   Len: 4),
    (Ptr: '&amp;';  Len: 5),

    (Ptr: '&quot;'; Len: 6),
    (Ptr: '&apos;'; Len: 6),

    (Ptr: '';       Len: 0)
  );

  XML_NUMERIC: TReplacementArray = (
    (Ptr: '&#09;';  Len: 5),
    (Ptr: '&#10;';  Len: 5),
    (Ptr: '&#13;';  Len: 5),

    (Ptr: '&#60;';  Len: 5),
    (Ptr: '&#62;';  Len: 5),
    (Ptr: '&#38;';  Len: 5),

    (Ptr: '&#34;';  Len: 5),
    (Ptr: '&#39;';  Len: 5),

    (Ptr: '';       Len: 0)
  );

var
  XML_ESC_TEXT : TXmlEscTable;
  XML_ESC_ATTR : TXmlEscTable;

procedure FillTable(var T: TXmlEscTable);
var
  I: Integer;
begin
  FillChar(T, SizeOf(T), XET_NONE);

  T[0] := XET_TAB; // go out of loop to abort

  // Illegal control chars
  for I := 1 to 31 do begin
    T[I] := XET_ILLEGAL;
  end;

  // Legal whitespace
  T[9]  := XET_TAB;
  T[10] := XET_LF;
  T[13] := XET_CR;

  // XML special chars
  T[Ord('<')] := XET_LT;
  T[Ord('>')] := XET_GT;
  T[Ord('&')] := XET_AMP;
end;

procedure InitTables;
begin
  FillTable(XML_ESC_TEXT);
  FillTable(XML_ESC_ATTR);

  XML_ESC_ATTR[Ord('"')]  := XET_QUOT;
  XML_ESC_ATTR[Ord('''')] := XET_APOS;
end;

function XmlEscape(
  const AText: string;
  const AEscTable: TXmlEscTable;
  const AReplacements: TReplacementArray
): string;
var
  VSrc, VDst, P: PChar;
  VEsc: Byte;
  VLen, VNewLen: NativeInt;
  VRepl: PReplacement;
  VIsModified: Boolean;
begin
  if AText = '' then begin
    Result := '';
    Exit;
  end;

  // Pass 1: calculate output length

  VSrc := Pointer(AText);

  VNewLen := 0;
  VIsModified := False;

  repeat
    P := VSrc;

    while (Ord(VSrc^) > XML_ESC_HIGH) or (AEscTable[Ord(VSrc^)] = XET_NONE) do begin
      Inc(VSrc);
    end;

    Inc(VNewLen, VSrc - P);

    if VSrc^ = #0 then begin
      Break;
    end;

    VEsc := AEscTable[Ord(VSrc^)];
    Inc(VNewLen, AReplacements[VEsc].Len);

    VIsModified := True;

    Inc(VSrc);
  until VSrc^ = #0;

  if not VIsModified then begin
    Result := AText;
    Exit;
  end;

  SetLength(Result, VNewLen);

  // Pass 2: write output

  VSrc := Pointer(AText);
  VDst := Pointer(Result);

  repeat
    P := VSrc;

    while (Ord(VSrc^) > XML_ESC_HIGH) or (AEscTable[Ord(VSrc^)] = XET_NONE) do begin
      Inc(VSrc);
    end;

    VLen := VSrc - P;

    if VLen > 0 then begin
      Move(P^, VDst^, VLen * SizeOf(Char));
      Inc(VDst, VLen);
    end;

    if VSrc^ = #0 then begin
      Break;
    end;

    VEsc := AEscTable[Ord(VSrc^)];
    VRepl := @AReplacements[VEsc];

    if VRepl.Len <> 0 then begin
      Move(VRepl.Ptr^, VDst^, VRepl.Len * SizeOf(Char));
      Inc(VDst, VRepl.Len);
    end;

    Inc(VSrc);
  until VSrc^ = #0;
end;

function XmlEscapeText(const AText: string; const AUseNumericReference: Boolean): string;
begin
  if AUseNumericReference then begin
    Result := XmlEscape(AText, XML_ESC_TEXT, XML_NUMERIC);
  end else begin
    Result := XmlEscape(AText, XML_ESC_TEXT, XML_NAMED);
  end;
end;

function XmlEscapeAttr(const AValue: string; const AUseNumericReference: Boolean): string;
begin
  if AUseNumericReference then begin
    Result := XmlEscape(AValue, XML_ESC_ATTR, XML_NUMERIC);
  end else begin
    Result := XmlEscape(AValue, XML_ESC_ATTR, XML_NAMED);
  end;
end;

initialization
  InitTables;

end.
