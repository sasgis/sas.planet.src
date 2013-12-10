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

unit u_MarkPointSml;

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
  u_MarkFullBaseSml;

type
  TMarkPointSml = class(TMarkFullBaseSml, IVectorDataItemPoint)
  private
    FPoint: IGeometryLonLatPoint;
  protected
    function GetMarkType: TGUID; override;
  protected
    function GetLLRect: ILonLatRect; override;
    function GetGoToLonLat: TDoublePoint; override;
    function IsEqual(const AMark: IVectorDataItemSimple): Boolean; override;
  private
    function GetPoint: IGeometryLonLatPoint;
  public
    constructor Create(
      const AHash: THashValue;
      const AHintConverter: IHtmlToHintTextConverter;
      const AName: string;
      AId: Integer;
      ADbId: Integer;
      AVisible: Boolean;
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

{ TMarkPointSml }

constructor TMarkPointSml.Create(
  const AHash: THashValue;
  const AHintConverter: IHtmlToHintTextConverter;
  const AName: string;
  AId: Integer;
  ADbId: Integer;
  AVisible: Boolean;
  const AAppearance: IAppearance;
  const ACategory: ICategory;
  const ADesc: string;
  const APoint: IGeometryLonLatPoint
);
begin
  Assert(Assigned(APoint));
  inherited Create(AHash, AAppearance, AHintConverter, AName, AId, ADbId, ACategory, ADesc, AVisible);
  FPoint := APoint;
end;

function TMarkPointSml.IsEqual(const AMark: IVectorDataItemSimple): Boolean;
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
  if not FPoint.Bounds.IsEqual(AMark.LLRect) then begin
    Result := False;
    Exit;
  end;
  if not Supports(AMark, IVectorDataItemPoint, VMarkPoint) then begin
    Result := False;
    Exit;
  end;
  Result := True;
end;

function TMarkPointSml.GetGoToLonLat: TDoublePoint;
begin
  Result := FPoint.GetGoToLonLat;
end;

function TMarkPointSml.GetLLRect: ILonLatRect;
begin
  Result := FPoint.Bounds;
end;

function TMarkPointSml.GetPoint: IGeometryLonLatPoint;
begin
  Result := FPoint;
end;

function TMarkPointSml.GetMarkType: TGUID;
begin
  Result := IVectorDataItemPoint;
end;

end.
