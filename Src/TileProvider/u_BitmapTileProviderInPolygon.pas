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
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

unit u_BitmapTileProviderInPolygon;

interface

uses
  Types,
  i_NotifierOperation,
  i_Bitmap32Static,
  i_ProjectionInfo,
  i_GeometryProjected,
  i_BitmapTileProvider,
  u_BaseInterfacedObject;

type
  TBitmapTileProviderInPolygon = class(TBaseInterfacedObject, IBitmapTileProvider)
  private
    FSourceProvider: IBitmapTileProvider;
    FLine: IGeometryProjectedSinglePolygon;
  private
    function GetProjectionInfo: IProjectionInfo;
    function GetTile(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ATile: TPoint
    ): IBitmap32Static;
  public
    constructor Create(
      const APolyProjected: IGeometryProjectedPolygon;
      const ASourceProvider: IBitmapTileProvider
    );
  end;

implementation

uses
  SysUtils,
  t_GeoTypes;

{ TBitmapTileProviderInPolygon }

constructor TBitmapTileProviderInPolygon.Create(
  const APolyProjected: IGeometryProjectedPolygon;
  const ASourceProvider: IBitmapTileProvider
);
var
  VMultiPolygon: IGeometryProjectedMultiPolygon;
begin
  Assert(ASourceProvider <> nil);
  Assert(APolyProjected <> nil);
  Assert(not APolyProjected.IsEmpty);
  inherited Create;
  FSourceProvider := ASourceProvider;
  if not Supports(APolyProjected, IGeometryProjectedSinglePolygon, FLine) then begin
    if Supports(APolyProjected, IGeometryProjectedMultiPolygon, VMultiPolygon) then begin
      if VMultiPolygon.Count > 0 then begin
        FLine := VMultiPolygon.Item[0];
      end;
    end else begin
      Assert(False);
      FLine := nil;
    end;
  end;
end;

function TBitmapTileProviderInPolygon.GetProjectionInfo: IProjectionInfo;
begin
  Result := FSourceProvider.ProjectionInfo;
end;

function TBitmapTileProviderInPolygon.GetTile(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ATile: TPoint
): IBitmap32Static;
var
  VMapRect: TDoubleRect;
begin
  VMapRect := FSourceProvider.ProjectionInfo.GeoConverter.TilePos2PixelRectFloat(ATile, FSourceProvider.ProjectionInfo.Zoom);
  if FLine.IsRectIntersectPolygon(VMapRect) then begin
    Result :=
      FSourceProvider.GetTile(
        AOperationID,
        ACancelNotifier,
        ATile
      );
  end else begin
    Result := nil;
  end;
end;

end.
