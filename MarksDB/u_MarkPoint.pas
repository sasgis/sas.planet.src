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

unit u_MarkPoint;

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
  TMarkPoint = class(TMarkFullBase, IVectorDataItemPoint)
  private
    FPoint: IGeometryLonLatPoint;
  protected
    function GetMarkType: TGUID; override;
  protected
    function GetGeometry: IGeometryLonLat; override;
    function IsEqual(const AMark: IVectorDataItemSimple): Boolean; override;
  private
    function GetPoint: IGeometryLonLatPoint;
  public
    constructor Create(
      const AHash: THashValue;
      const AHintConverter: IHtmlToHintTextConverter;
      const AName: string;
      const AAppearance: IAppearance;
      const ACategory: ICategory;
      const ADesc: string;
      const APoint: IGeometryLonLatPoint
    );
  end;

implementation

uses
  SysUtils,
  u_GeoFun,
  u_LonLatRectByPoint;

{ TMarkPoint }

constructor TMarkPoint.Create(
  const AHash: THashValue;
  const AHintConverter: IHtmlToHintTextConverter;
  const AName: string;
  const AAppearance: IAppearance;
  const ACategory: ICategory;
  const ADesc: string;
  const APoint: IGeometryLonLatPoint
);
begin
  Assert(Assigned(APoint));
  inherited Create(AHash, AAppearance, AHintConverter, AName, ACategory, ADesc);
  FPoint := APoint;
end;

function TMarkPoint.IsEqual(const AMark: IVectorDataItemSimple): Boolean;
var
  VMarkPoint: IVectorDataItemPoint;
begin
  if AMark = IVectorDataItemSimple(Self) then begin
    Result := True;
    Exit;
  end;
  if not inherited IsEqual(AMark) then begin
    Result := False;
    Exit;
  end;
  if not FPoint.IsSameGeometry(AMark.Geometry) then begin
    Result := False;
    Exit;
  end;
  if not Supports(AMark, IVectorDataItemPoint, VMarkPoint) then begin
    Result := False;
    Exit;
  end;
  Result := True;
end;

function TMarkPoint.GetGeometry: IGeometryLonLat;
begin
  Result := FPoint;
end;

function TMarkPoint.GetPoint: IGeometryLonLatPoint;
begin
  Result := FPoint;
end;

function TMarkPoint.GetMarkType: TGUID;
begin
  Result := IVectorDataItemPoint;
end;

end.
