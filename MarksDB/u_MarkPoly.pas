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

unit u_MarkPoly;

interface

uses
  t_Hash,
  t_GeoTypes,
  i_Appearance,
  i_LonLatRect,
  i_GeometryLonLat,
  i_VectorDataItemSimple,
  i_Category,
  i_HtmlToHintTextConverter,
  u_MarkFullBase;

type
  TMarkPoly = class(TMarkFullBase, IVectorDataItemPoly)
  private
    FLine: IGeometryLonLatMultiPolygon;
  protected
    function GetMarkType: TGUID; override;
  protected
    function GetGeometry: IGeometryLonLat; override;
    function IsEqual(const AMark: IVectorDataItemSimple): Boolean; override;
  private
    function GetLine: IGeometryLonLatMultiPolygon;
  public
    constructor Create(
      const AHash: THashValue;
      const AHintConverter: IHtmlToHintTextConverter;
      const AName: string;
      const AAppearance: IAppearance;
      const ACategory: ICategory;
      const ADesc: string;
      const ALine: IGeometryLonLatMultiPolygon
    );
  end;

implementation

uses
  SysUtils;

{ TMarkFull }

constructor TMarkPoly.Create(
  const AHash: THashValue;
  const AHintConverter: IHtmlToHintTextConverter;
  const AName: string;
  const AAppearance: IAppearance;
  const ACategory: ICategory;
  const ADesc: string;
  const ALine: IGeometryLonLatMultiPolygon
);
begin
  Assert(Assigned(ALine));
  inherited Create(AHash, AAppearance, AHintConverter, AName, ACategory, ADesc);
  FLine := ALine;
end;

function TMarkPoly.GetGeometry: IGeometryLonLat;
begin
  Result := FLine;
end;

function TMarkPoly.GetMarkType: TGUID;
begin
  Result := IVectorDataItemPoly;
end;

function TMarkPoly.IsEqual(const AMark: IVectorDataItemSimple): Boolean;
var
  VMarkPoly: IVectorDataItemPoly;
begin
  if AMark = IVectorDataItemSimple(Self) then begin
    Result := True;
    Exit;
  end;
  if not inherited IsEqual(AMark) then begin
    Result := False;
    Exit;
  end;
  if not Supports(AMark, IVectorDataItemPoly, VMarkPoly) then begin
    Result := False;
    Exit;
  end;
  if not FLine.IsSame(VMarkPoly.Line) then begin
    Result := False;
    Exit;
  end;
  Result := True;
end;

function TMarkPoly.GetLine: IGeometryLonLatMultiPolygon;
begin
  Result := FLine;
end;

end.
