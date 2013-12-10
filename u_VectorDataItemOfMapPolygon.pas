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

unit u_VectorDataItemOfMapPolygon;

interface

uses
  t_Hash,
  t_GeoTypes,
  i_StringProvider,
  i_LonLatRect,
  i_VectorDataItemSimple,
  i_GeometryLonLat,
  i_HtmlToHintTextConverter,
  u_VectorDataItemOfMapBase;

type
  TVectorDataItemOfMapPath = class(TVectorDataItemOfMapBase, IVectorDataItemLine)
  private
    FLine: IGeometryLonLatMultiLine;
  protected
    function GetGeometry: IGeometryLonLat; override;
    function GetLine: IGeometryLonLatMultiLine;
  public
    constructor Create(
      const AHash: THashValue;
      const AHintConverter: IHtmlToHintTextConverter;
      const AUrlPrefix: IStringProvider;
      const AIndex: Integer;
      const AName: string;
      const ADesc: string;
      const ALine: IGeometryLonLatMultiLine
    );
  end;

  TVectorDataItemOfMapPoly = class(TVectorDataItemOfMapBase, IVectorDataItemPoly)
  private
    FLine: IGeometryLonLatMultiPolygon;
  protected
    function GetGeometry: IGeometryLonLat; override;
    function GetLine: IGeometryLonLatMultiPolygon;
  public
    constructor Create(
      const AHash: THashValue;
      const AHintConverter: IHtmlToHintTextConverter;
      const AUrlPrefix: IStringProvider;
      const AIndex: Integer;
      const AName: string;
      const ADesc: string;
      const ALine: IGeometryLonLatMultiPolygon
    );
  end;


implementation

uses
  u_GeoFun;

{ TVectorDataItemPath }

constructor TVectorDataItemOfMapPath.Create(
  const AHash: THashValue;
  const AHintConverter: IHtmlToHintTextConverter;
  const AUrlPrefix: IStringProvider;
  const AIndex: Integer;
  const AName, ADesc: string;
  const ALine: IGeometryLonLatMultiLine
);
begin
  Assert(Assigned(ALine));
  inherited Create(
    AHash,
    AHintConverter,
    AUrlPrefix,
    AIndex,
    AName,
    ADesc
  );
  FLine := ALine;
end;

function TVectorDataItemOfMapPath.GetGeometry: IGeometryLonLat;
begin
  Result := FLine;
end;

function TVectorDataItemOfMapPath.GetLine: IGeometryLonLatMultiLine;
begin
  Result := FLine;
end;

{ TVectorDataItemPoly }

constructor TVectorDataItemOfMapPoly.Create(
  const AHash: THashValue;
  const AHintConverter: IHtmlToHintTextConverter;
  const AUrlPrefix: IStringProvider;
  const AIndex: Integer;
  const AName, ADesc: string;
  const ALine: IGeometryLonLatMultiPolygon
);
begin
  Assert(Assigned(ALine));
  inherited Create(
    AHash,
    AHintConverter,
    AUrlPrefix,
    AIndex,
    AName,
    ADesc
  );
  FLine := ALine;
end;

function TVectorDataItemOfMapPoly.GetGeometry: IGeometryLonLat;
begin
  Result := FLine;
end;

function TVectorDataItemOfMapPoly.GetLine: IGeometryLonLatMultiPolygon;
begin
  Result := FLine;
end;

end.
