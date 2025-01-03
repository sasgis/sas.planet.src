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

unit u_HtmlDoc;

interface

uses
  i_VectorItemSubset;

type
  THtmlDoc = record
    class function FormattedTextToHtml(
      const AText: string
    ): string; static;

    class function FromVectorItemsDescription(
      const AVectorItems: IVectorItemSubset;
      out ATitle: string;
      out AHtmlDoc: string
    ): Boolean; static;
  end;

implementation

uses
  SysUtils,
  gnugettext,
  c_InternalBrowser,
  i_VectorDataItemSimple;

{ THtmlDoc }

class function THtmlDoc.FormattedTextToHtml(const AText: string): string;
begin
  Result := AText;
  Result := StringReplace(Result, '<!-- sas.cut -->', '', [rfReplaceAll, rfIgnoreCase]);
  if Pos('<', Result) > 0 then begin
    Result := AText;
  end else begin
    Result := StringReplace(Result, #13#10, '<br>', [rfReplaceAll]);
  end;
end;

class function THtmlDoc.FromVectorItemsDescription(
  const AVectorItems: IVectorItemSubset;
  out ATitle: string;
  out AHtmlDoc: string
): Boolean;
const
  cTitleSep: array [Boolean] of string = ('', '; ');
var
  I: Integer;
  VMark: IVectorDataItem;
  VItemTitle: string;
begin
  ATitle := '';
  AHtmlDoc := '';
  if AVectorItems.Count > 1 then begin
    for I := 0 to AVectorItems.Count - 1 do begin
      VMark := AVectorItems.Items[I];
      VItemTitle := VMark.GetInfoCaption;
      if VItemTitle = '' then begin
        VItemTitle := VMark.GetInfoUrl;
      end else begin
        ATitle := ATitle + cTitleSep[ATitle <> ''] + VMark.GetInfoCaption;
      end;
      if VMark.GetInfoUrl <> '' then begin
        AHtmlDoc :=
          AHtmlDoc +
          '<hr><a href="' + VMark.GetInfoUrl + CVectorItemDescriptionSuffix + '">' +
          VItemTitle + '</a><br>'#13#10;
      end else begin
        AHtmlDoc := AHtmlDoc + '<hr>'#13#10;
      end;
      AHtmlDoc := AHtmlDoc + FormattedTextToHtml(VMark.Desc) + #13#10;
    end;
    AHtmlDoc := Format(_('Found %d items'), [AVectorItems.Count]) + '<br>' + AHtmlDoc;
    Result := True;
  end else begin
    VMark := AVectorItems.Items[0];
    Result := VMark.GetInfoUrl = '';
    if Result then begin
      ATitle := VMark.GetInfoCaption;
      AHtmlDoc := FormattedTextToHtml(VMark.Desc);
    end;
  end;
end;

end.
