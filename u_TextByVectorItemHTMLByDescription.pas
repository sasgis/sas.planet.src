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

unit u_TextByVectorItemHTMLByDescription;

interface

uses
  i_VectorDataItemSimple,
  i_TextByVectorItem,
  u_BaseInterfacedObject;

type
  TTextByVectorItemHTMLByDescription = class(TBaseInterfacedObject, ITextByVectorItem)
  private
    function GetText(const AItem: IVectorDataItemSimple): string;
  end;

implementation

{ TTextByVectorItemHTMLByDescription }

function TTextByVectorItemHTMLByDescription.GetText(
  const AItem: IVectorDataItemSimple): string;
begin
  Result :=
    '<html>'#13#10 +
      '<head>'#13#10 +
        '<title>' + AItem.GetInfoCaption + '</title>'#13#10 +
      '</head>'#13#10 +
      '<body>'#13#10 +
        AItem.Desc + #13#10 +
      '</body>'#13#10 +
      '</html>';
end;

end.
