{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2020, SAS.Planet development team.                      *}
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

unit u_HtmlDoc;

interface

uses
  i_VectorItemSubset;

type
  THtmlDoc = record
    class function FromVectorItemsDescription(
      const AVectorItems: IVectorItemSubset;
      out ATitle: string;
      out AHtmlDoc: string
    ): Boolean; static;
  end;

implementation

uses
  SysUtils,
  c_InternalBrowser,
  i_VectorDataItemSimple;

{ THtmlDoc }

class function THtmlDoc.FromVectorItemsDescription(
  const AVectorItems: IVectorItemSubset;
  out ATitle: string;
  out AHtmlDoc: string
): Boolean;
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
        ATitle := ATitle + VMark.GetInfoCaption + '; ';
      end;
      if VMark.GetInfoUrl <> '' then begin
        AHtmlDoc :=
          AHtmlDoc +
          '<hr><a href="' + VMark.GetInfoUrl + CVectorItemDescriptionSuffix + '">' +
          VItemTitle + '</a><br>'#13#10;
      end else begin
        AHtmlDoc := AHtmlDoc + '<hr>'#13#10;
      end;
      AHtmlDoc := AHtmlDoc + VMark.Desc + #13#10;
    end;
    AHtmlDoc := 'Found: ' + IntToStr(AVectorItems.Count) + '<br>' + AHtmlDoc;
    Result := True;
  end else begin
    VMark := AVectorItems.Items[0];
    Result := VMark.GetInfoUrl = '';
    if Result then begin
      ATitle := VMark.GetInfoCaption;
      AHtmlDoc := VMark.Desc;
    end;
  end;
end;

end.
