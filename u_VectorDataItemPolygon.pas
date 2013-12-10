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

unit u_VectorDataItemPolygon;

interface

uses
  t_Hash,
  t_GeoTypes,
  i_Appearance,
  i_LonLatRect,
  i_VectorDataItemSimple,
  i_GeometryLonLat,
  i_HtmlToHintTextConverter,
  u_VectorDataItemBase;

type
  TVectorDataItemPath = class(TVectorDataItemBase, IVectorDataItemLine)
  private
    FLine: IGeometryLonLatMultiLine;
  protected
    function GetLine: IGeometryLonLatMultiLine;
    function GetGeometry: IGeometryLonLat; override;
  public
    constructor Create(
      const AHash: THashValue;
      const AAppearance: IAppearance;
      const AHintConverter: IHtmlToHintTextConverter;
      const AName: string;
      const ADesc: string;
      const ALine: IGeometryLonLatMultiLine
    );
  end;

  TVectorDataItemPoly = class(TVectorDataItemBase, IVectorDataItemPoly)
  private
    FLine: IGeometryLonLatMultiPolygon;
  protected
    function GetLine: IGeometryLonLatMultiPolygon;
    function GetGeometry: IGeometryLonLat; override;
  public
    constructor Create(
      const AHash: THashValue;
      const AAppearance: IAppearance;
      const AHintConverter: IHtmlToHintTextConverter;
      const AName: string;
      const ADesc: string;
      const ALine: IGeometryLonLatMultiPolygon
    );
  end;


implementation

uses
  u_GeoFun;

{ TVectorDataItemPath }

constructor TVectorDataItemPath.Create(
  const AHash: THashValue;
  const AAppearance: IAppearance;
  const AHintConverter: IHtmlToHintTextConverter;
  const AName, ADesc: string;
  const ALine: IGeometryLonLatMultiLine
);
begin
  Assert(Assigned(ALine));
  inherited Create(AHash, AAppearance, AHintConverter, AName, ADesc);
  FLine := ALine;
end;

function TVectorDataItemPath.GetGeometry: IGeometryLonLat;
begin
  Result := FLine;
end;

function TVectorDataItemPath.GetLine: IGeometryLonLatMultiLine;
begin
  Result := FLine;
end;

{ TVectorDataItemPoly }

constructor TVectorDataItemPoly.Create(
  const AHash: THashValue;
  const AAppearance: IAppearance;
  const AHintConverter: IHtmlToHintTextConverter;
  const AName, ADesc: string;
  const ALine: IGeometryLonLatMultiPolygon
);
begin
  Assert(Assigned(ALine));
  inherited Create(AHash, AAppearance, AHintConverter, AName, ADesc);
  FLine := ALine;
end;

function TVectorDataItemPoly.GetGeometry: IGeometryLonLat;
begin
  Result := FLine;
end;

function TVectorDataItemPoly.GetLine: IGeometryLonLatMultiPolygon;
begin
  Result := FLine;
end;

end.
