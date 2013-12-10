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

unit u_VectorDataItemPoint;

interface

uses
  t_Hash,
  t_GeoTypes,
  i_Appearance,
  i_LonLatRect,
  i_GeometryLonLat,
  i_VectorDataItemSimple,
  i_HtmlToHintTextConverter,
  u_VectorDataItemBase;

type
  TVectorDataItemPoint = class(TVectorDataItemBase, IVectorDataItemPoint)
  private
    FPoint: IGeometryLonLatPoint;
  protected
    function GetLLRect: ILonLatRect; override;
    function GetGoToLonLat: TDoublePoint; override;
    function GetPoint: IGeometryLonLatPoint;
  public
    constructor Create(
      const AHash: THashValue;
      const AAppearance: IAppearance;
      const AHintConverter: IHtmlToHintTextConverter;
      const AName: string;
      const ADesc: string;
      const APoint: IGeometryLonLatPoint
    );
  end;

implementation

uses
  u_GeoFun;

{ TVectorDataItemPoint }

constructor TVectorDataItemPoint.Create(
  const AHash: THashValue;
  const AAppearance: IAppearance;
  const AHintConverter: IHtmlToHintTextConverter;
  const AName, ADesc: string;
  const APoint: IGeometryLonLatPoint
);
begin
  Assert(Assigned(APoint));
  inherited Create(
    AHash,
    AAppearance,
    AHintConverter,
    AName,
    ADesc
  );
  FPoint := APoint;
end;

function TVectorDataItemPoint.GetGoToLonLat: TDoublePoint;
begin
  Result := FPoint.Point;
end;

function TVectorDataItemPoint.GetLLRect: ILonLatRect;
begin
  Result := FPoint.Bounds;
end;

function TVectorDataItemPoint.GetPoint: IGeometryLonLatPoint;
begin
  Result := FPoint;
end;

end.
