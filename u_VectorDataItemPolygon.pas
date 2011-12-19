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

unit u_VectorDataItemPolygon;

interface

uses
  t_GeoTypes,
  i_VectorDataItemSimple,
  i_HtmlToHintTextConverter,
  u_VectorDataItemBase;

type
  TVectorDataItemPolygon = class(TVectorDataItemBase)
  private
    FPoints: TArrayOfDoublePoint;
    FLLRect: TDoubleRect;
  protected
    function GetLLRect: TDoubleRect;  override;
    function GetPoints: TArrayOfDoublePoint;
  public
    constructor Create(
      AHintConverter: IHtmlToHintTextConverter;
      AName: string;
      ADesc: string;
      APoints: TArrayOfDoublePoint;
      ALLRect: TDoubleRect
    );
  end;

  TVectorDataItemPath = class(TVectorDataItemPolygon, IVectorDataItemLine)
  end;

  TVectorDataItemPoly = class(TVectorDataItemPolygon, IVectorDataItemPoly)
  end;


implementation

{ TVectorDataItemPolygon }

constructor TVectorDataItemPolygon.Create(
  AHintConverter: IHtmlToHintTextConverter;
  AName, ADesc: string;
  APoints: TArrayOfDoublePoint;
  ALLRect: TDoubleRect
);
begin
  inherited Create(AHintConverter, AName, ADesc);
  FLLRect := ALLRect;
  FPoints := APoints;
end;

function TVectorDataItemPolygon.GetLLRect: TDoubleRect;
begin
  Result := FLLRect;
end;

function TVectorDataItemPolygon.GetPoints: TArrayOfDoublePoint;
begin
  Result := FPoints;
end;

end.
