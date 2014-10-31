{******************************************************************************}
{* SAS.Planet (SAS.œÎ‡ÌÂÚ‡)                                                   *}
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

unit u_HtmlToHintTextConverterStuped;

interface

uses
  i_HtmlToHintTextConverter,
  u_BaseInterfacedObject;

type
  THtmlToHintTextConverterStuped = class(TBaseInterfacedObject, IHtmlToHintTextConverter)
  private
    function HTML2Txt(OrigHTML: String): String;
  private
    function Convert(const AName, ADescription: string): string;
  end;

function StupedHtmlToTextConverter(const ASource: String): String;

implementation

uses
  SysUtils,
  StrUtils;

function StupedHtmlToTextConverter(const ASource: String): String;
begin
  Result := ASource;
  Result := StringReplace(Result, '&#36;', '$', [rfReplaceAll]);
  Result := StringReplace(Result, '&#37;', '%', [rfReplaceAll]);
  Result := StringReplace(Result, '&#39;', '''', [rfReplaceAll]);

  Result := StringReplace(Result, '&amp;', '&', [rfReplaceAll]);
  Result := StringReplace(Result, '&#38;', '&', [rfReplaceAll]);

  Result := StringReplace(Result, '&quot;', '"', [rfReplaceAll]);
  Result := StringReplace(Result, '&#34;', '"', [rfReplaceAll]);

  Result := StringReplace(Result, '&lt;', '<', [rfReplaceAll]);
  Result := StringReplace(Result, '&#60;', '<', [rfReplaceAll]);

  Result := StringReplace(Result, '&gt;', '>', [rfReplaceAll]);
  Result := StringReplace(Result, '&#62;', '>', [rfReplaceAll]);

  Result := StringReplace(Result, '&nbsp;', ' ', [rfReplaceAll]);
  Result := StringReplace(Result, '&#160;', ' ', [rfReplaceAll]);

  Result := StringReplace(Result, '&raquo;', 'ª', [rfReplaceAll]);
  Result := StringReplace(Result, '&#187;', 'ª', [rfReplaceAll]);

  Result := StringReplace(Result, '&laquo;', '´', [rfReplaceAll]);
  Result := StringReplace(Result, '&#171', '´', [rfReplaceAll]);

  Result := StringReplace(Result, '&copy;', '©', [rfReplaceAll]);
  Result := StringReplace(Result, '&#169;', '©', [rfReplaceAll]);

  Result := StringReplace(Result, '&reg;', 'Æ', [rfReplaceAll]);
  Result := StringReplace(Result, '&#174;', 'Æ', [rfReplaceAll]);
end;

{ THtmlToHintTextConverterStuped }

function THtmlToHintTextConverterStuped.Convert(
  const AName, ADescription: string
): string;
var
  i, j: Integer;
  VNameInDesc: Boolean;
begin
  Result := '';

  if (length(AName) > 0) then begin
    // if name found in description - dont show name
    VNameInDesc := (System.Pos(AName, ADescription) > 0);
    if (not VNameInDesc) then begin
      if System.Pos('<', AName) > 0 then begin
        Result := HTML2Txt(AName);
      end else begin
        Result := AName;
      end;
    end;
  end;

  if (length(ADescription) > 0) then begin
    if length(Result) > 0 then begin
      Result := Result + #13#10;
    end;
    if System.Pos('<', ADescription) > 0 then begin
      Result := Result + HTML2Txt(ADescription);
    end else begin
      Result := Result + ADescription;
    end;
  end;

  Result := Trim(Result);

  i := 1;
  j := 0;
  while (i < length(Result)) and (i <> 0) do begin
    inc(j);
    if (Result[i] = #13) or (Result[i] = #10) then begin
      j := 0;
    end;
    if (j > 40) and (Result[i] = ' ') and (length(Result) - i > 5) then begin
      if i > 500 then begin
        Insert('...', Result, i);
        Delete(Result, i + 3, length(Result) - i + 3);
        i := 0;
        continue;
      end;
      Delete(Result, i, 1);
      Insert(#13#10, Result, i);
      j := 0;
    end;
    inc(i);
  end;
end;

function THtmlToHintTextConverterStuped.HTML2Txt(OrigHTML: String): String;
var
  NoHTML: String;

  function MidStr(
  const pString, pAbre, pFecha: String;
    pInclui: boolean
  ): string;
  var
    lIni, lFim: integer;
  begin
    if not pInclui then begin
      lIni := System.Pos(UpperCase(pAbre), UpperCase(pString)) + Length(pAbre);
      lFim := PosEx(UpperCase(pFecha), UpperCase(pString), lIni) + 1;
    end else begin
      lIni := System.Pos(UpperCase(pAbre), UpperCase(pString));
      lFim := PosEx(UpperCase(pFecha), UpperCase(pString), lIni + Length(pAbre)) + 1;
    end;
    result := Copy(pString, lIni, lFim - lIni);
  end;

  function Mid(
  const str: string;
    pos: integer
  ): string;
  begin
    result := copy(str, pos, length(str) - pos + 1);
  end;

begin
  if System.Pos('<body', LowerCase(OrigHTML)) > 0 Then begin
    OrigHTML := Mid(OrigHTML, System.Pos('<body', LowerCase(OrigHTML)));
    OrigHTML := Mid(OrigHTML, System.Pos('>', OrigHTML) + 1);
    if System.Pos('</body>', LowerCase(OrigHTML)) > 0 Then begin
      OrigHTML := Copy(OrigHTML, 1, System.Pos('</body>', LowerCase(OrigHTML)) - 1);
    end;
  end;
  OrigHTML := StringReplace(OrigHTML, Chr(13), '', [rfReplaceAll]);
  OrigHTML := StringReplace(OrigHTML, Chr(10), '', [rfReplaceAll]);
  while System.Pos('  ', OrigHTML) > 0 do begin
    OrigHTML := StringReplace(OrigHTML, '  ', ' ', [rfReplaceAll]);
  end;
  OrigHTML := StringReplace(OrigHTML, '<br>', #13#10, [rfReplaceAll, rfIgnoreCase]);
  OrigHTML := StringReplace(OrigHTML, '<br />', #13#10, [rfReplaceAll, rfIgnoreCase]);
  OrigHTML := StringReplace(OrigHTML, '</div>', #13#10#13#10, [rfReplaceAll, rfIgnoreCase]);
  while System.Pos('<p', OrigHTML) > 0 do begin
    NoHTML := MidStr(OrigHTML, '<p', '>', True);
    if NoHTML = '' then Break;
    OrigHTML := StringReplace(OrigHTML, NoHTML, (#13#10#13#10), [rfReplaceAll, rfIgnoreCase]);
  end;
  if System.Pos('<style', OrigHTML) > 0 then begin
    NoHTML := MidStr(OrigHTML, '<style', '</style>', False);
    OrigHTML := StringReplace(OrigHTML, NoHTML, '', [rfReplaceAll, rfIgnoreCase]);
  end;
  while System.Pos('<', OrigHTML) > 0 do begin
    NoHTML := MidStr(OrigHTML, '<', '>', True);
    if NoHTML = '' then Break;
    OrigHTML := StringReplace(OrigHTML, NoHTML, '', [rfReplaceAll, rfIgnoreCase]);
  end;
  OrigHTML := StupedHtmlToTextConverter(OrigHTML);
  OrigHTML := StringReplace(OrigHTML, '&aacute;', '?', [rfReplaceAll]);
  OrigHTML := StringReplace(OrigHTML, '&atilde;', '?', [rfReplaceAll]);
  OrigHTML := StringReplace(OrigHTML, '&ccedil;', '?', [rfReplaceAll]);
  OrigHTML := StringReplace(OrigHTML, '&eacute;', '?', [rfReplaceAll]);
  OrigHTML := StringReplace(OrigHTML, '&ecirc;', '?', [rfReplaceAll]);
  OrigHTML := StringReplace(OrigHTML, '&iacute;', '?', [rfReplaceAll]);
  OrigHTML := StringReplace(OrigHTML, '&oacute;', '?', [rfReplaceAll]);
  OrigHTML := StringReplace(OrigHTML, '&ocirc;', '?', [rfReplaceAll]);
  OrigHTML := StringReplace(OrigHTML, '&otilde;', '?', [rfReplaceAll]);
  OrigHTML := StringReplace(OrigHTML, '&Aacute;', '?', [rfReplaceAll]);
  OrigHTML := StringReplace(OrigHTML, '&Atilde;', '?', [rfReplaceAll]);
  OrigHTML := StringReplace(OrigHTML, '&Ccedil;', '?', [rfReplaceAll]);
  OrigHTML := StringReplace(OrigHTML, '&Eacute;', '?', [rfReplaceAll]);
  OrigHTML := StringReplace(OrigHTML, '&Ecirc;', '?', [rfReplaceAll]);
  OrigHTML := StringReplace(OrigHTML, '&Iacute;', '?', [rfReplaceAll]);
  OrigHTML := StringReplace(OrigHTML, '&Oacute;', '?', [rfReplaceAll]);
  OrigHTML := StringReplace(OrigHTML, '&Ocirc;', '?', [rfReplaceAll]);
  OrigHTML := StringReplace(OrigHTML, '&Otilde;', '?', [rfReplaceAll]);
  OrigHTML := StringReplace(OrigHTML, '&Uacute;', '?', [rfReplaceAll]);
  OrigHTML := StringReplace(OrigHTML, '&uacute;', '?', [rfReplaceAll]);
  OrigHTML := StringReplace(OrigHTML, '&uuml;', '?', [rfReplaceAll]);
  result := OrigHTML;
end;

end.
