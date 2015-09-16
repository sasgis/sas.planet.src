{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2015, SAS.Planet development team.                      *}
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

unit u_BitmapLayerProviderFillingMap;

interface

uses
  Types,
  SysUtils,
  t_GeoTypes,
  i_NotifierOperation,
  i_Bitmap32Static,
  i_Bitmap32BufferFactory,
  i_ProjectionInfo,
  i_GeometryProjected,
  i_GeometryProjectedFactory,
  i_TileStorage,
  i_MapVersionRequest,
  i_BitmapLayerProvider,
  i_FillingMapColorer,
  i_GeometryLonLat,
  u_BaseInterfacedObject;

type
  TBitmapLayerProviderFillingMap = class(TBaseInterfacedObject, IBitmapTileUniProvider)
  private
    FBitmap32StaticFactory: IBitmap32StaticFactory;
    FGeometryProjectedFactory: IGeometryProjectedFactory;
    FStorage: ITileStorage;
    FVersion: IMapVersionRequest;
    FUseRelativeZoom: Boolean;
    FZoom: Byte;
    FProjection: IProjection;
    FPolygon: IGeometryLonLatPolygon;
    FProjectedPolygon: IGeometryProjectedPolygon;
    FColorer: IFillingMapColorer;

    function GetActualProjection(
      const AProjection: IProjection
    ): IProjection;
    function GetIntersectedRect(
      out AIntersectedLonLatRect: TDoubleRect;
      const ALonLatRect: TDoubleRect;
      const AProjection: IProjection;
      const AProjectedPolygon: IGeometryProjectedPolygon
    ): Boolean;
    function GetFillingMapBitmap(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ATargetProjection: IProjection;
      const AMapRect: TRect;
      const ASourceProjection: IProjection;
      const AProjectedPolygon: IGeometryProjectedPolygon;
      const AVersion: IMapVersionRequest;
      const AColorer: IFillingMapColorer
    ): IBitmap32Static;
  private
    function GetTile(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const AProjection: IProjection;
      const ATile: TPoint
    ): IBitmap32Static;
  public
    constructor Create(
      const ABitmap32StaticFactory: IBitmap32StaticFactory;
      const AGeometryProjectedFactory: IGeometryProjectedFactory;
      const AStorage: ITileStorage;
      const AVersion: IMapVersionRequest;
      const AUseRelativeZoom: Boolean;
      const AZoom: Byte;
      const APolygon: IGeometryLonLatPolygon;
      const AColorer: IFillingMapColorer
    );
  end;

implementation

uses
  Math,
  GR32,
  i_ProjectionType,
  i_ProjectionSet,
  i_TileRect,
  i_TileIterator,
  i_TileInfoBasic,
  u_TileRect,
  u_GeoFunc,
  u_TileIteratorByRect,
  u_Bitmap32ByStaticBitmap;

{ TBitmapLayerProviderFillingMap }

constructor TBitmapLayerProviderFillingMap.Create(
  const ABitmap32StaticFactory: IBitmap32StaticFactory;
  const AGeometryProjectedFactory: IGeometryProjectedFactory;
  const AStorage: ITileStorage;
  const AVersion: IMapVersionRequest;
  const AUseRelativeZoom: Boolean;
  const AZoom: Byte;
  const APolygon: IGeometryLonLatPolygon;
  const AColorer: IFillingMapColorer
);
begin
  Assert(Assigned(ABitmap32StaticFactory));
  Assert(Assigned(AStorage));
  Assert(Assigned(AVersion));
  Assert(Assigned(AColorer));
  inherited Create;
  FBitmap32StaticFactory := ABitmap32StaticFactory;
  FGeometryProjectedFactory := AGeometryProjectedFactory;
  FStorage := AStorage;
  FVersion := AVersion;
  FUseRelativeZoom := AUseRelativeZoom;
  FZoom := AZoom;
  FPolygon := APolygon;
  FColorer := AColorer;
  FProjectedPolygon := nil;
  FProjection := nil;
end;

function TBitmapLayerProviderFillingMap.GetActualProjection(
  const AProjection: IProjection
): IProjection;
var
  VProjectionSet: IProjectionSet;
  VZoom: Integer;
  VResultZoom: Byte;
begin
  VProjectionSet := FStorage.ProjectionSet;
  VZoom := FZoom;
  if FUseRelativeZoom then begin
    VZoom := VZoom + AProjection.Zoom;
  end;
  if VZoom < 0 then begin
    Result := VProjectionSet.Zooms[0];
  end else begin
    VResultZoom := VZoom;
    VProjectionSet.ValidateZoom(VResultZoom);
    Result := VProjectionSet.Zooms[VResultZoom];
  end;
end;

function TBitmapLayerProviderFillingMap.GetTile(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const AProjection: IProjection;
  const ATile: TPoint
): IBitmap32Static;
var
  VSourceProjection: IProjection;
  VReprojectPolygon: Boolean;
begin
  VSourceProjection := GetActualProjection(AProjection);
  if AProjection.Zoom > VSourceProjection.Zoom then begin
    Result := nil;
  end else begin

    // prepare projected polygon
    if Assigned(FPolygon) then begin
      VReprojectPolygon := False;
      if Assigned(FProjection) then begin
        if not FProjection.GetIsSameProjectionInfo(VSourceProjection) then begin
          FProjection := VSourceProjection;
          VReprojectPolygon := True;
        end;
      end else begin
        FProjection := VSourceProjection;
        VReprojectPolygon := True;
      end;
      if VReprojectPolygon or not Assigned(FProjectedPolygon) then begin
        FProjectedPolygon :=
          FGeometryProjectedFactory.CreateProjectedPolygonByLonLatPolygon(
            FProjection,
            FPolygon
          );
      end;
    end else begin
      FProjectedPolygon := nil;
    end;

    Result :=
      GetFillingMapBitmap(
        AOperationID,
        ACancelNotifier,
        AProjection,
        AProjection.TilePos2PixelRect(ATile),
        VSourceProjection,
        FProjectedPolygon,
        FVersion,
        FColorer
      );
  end;
end;

function TBitmapLayerProviderFillingMap.GetIntersectedRect(
  out AIntersectedLonLatRect: TDoubleRect;
  const ALonLatRect: TDoubleRect;
  const AProjection: IProjection;
  const AProjectedPolygon: IGeometryProjectedPolygon
): Boolean;
var
  I, J: Integer;
  VTmpRect: TDoubleRect;
  VMultiPolygonGeo: IGeometryLonLatMultiPolygon;
begin
  Result := False;

  if Supports(FPolygon, IGeometryLonLatMultiPolygon, VMultiPolygonGeo) then begin
    J := 0;
    for I := 0 to VMultiPolygonGeo.Count - 1 do begin
      if VMultiPolygonGeo.Item[I].Bounds.IntersecWithRect(VTmpRect, ALonLatRect) then begin
        Inc(J);
        AIntersectedLonLatRect := VTmpRect;
        Result := True;
      end;
    end;
    if J > 1 then begin
      Result := FPolygon.Bounds.IntersecWithRect(AIntersectedLonLatRect, ALonLatRect);
    end;
  end else begin
    Result := FPolygon.Bounds.IntersecWithRect(AIntersectedLonLatRect, ALonLatRect);
  end;

  if Result then begin
    Assert(AIntersectedLonLatRect.Left >= ALonLatRect.Left);
    Assert(AIntersectedLonLatRect.Right <= ALonLatRect.Right);
    Assert(AIntersectedLonLatRect.Top <= ALonLatRect.Top);
    Assert(AIntersectedLonLatRect.Bottom >= ALonLatRect.Bottom);

    VTmpRect := AProjection.LonLatRect2PixelRectFloat(AIntersectedLonLatRect);
    Result := AProjectedPolygon.IsRectIntersectPolygon(VTmpRect);
  end;
end;

function TBitmapLayerProviderFillingMap.GetFillingMapBitmap(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ATargetProjection: IProjection;
  const AMapRect: TRect;
  const ASourceProjection: IProjection;
  const AProjectedPolygon: IGeometryProjectedPolygon;
  const AVersion: IMapVersionRequest;
  const AColorer: IFillingMapColorer
): IBitmap32Static;
var
  VBitmap: TBitmap32ByStaticBitmap;
  VSize: TPoint;
  VSourceTileRect: TRect;
  VSourceLonLatRect: TDoubleRect;
  VSourceRelativeRect: TDoubleRect;
  VSourceProjectionType: IProjectionType;
  VTargetProjectionType: IProjectionType;
  VSameSourceAndTarget: Boolean;
  VLonLatRect: TDoubleRect;
  VIterator: ITileIterator;
  VRelativeRectOfTile: TDoubleRect;
  VLonLatRectOfTile: TDoubleRect;
  VSolidDrow: Boolean;
  VTileRectInfo: ITileRectInfo;
  VEnumTileInfo: IEnumTileInfo;
  VTileInfo: TTileInfo;
  VMapPixelRectOfTile: TDoubleRect;
  VLocalPixelRectOfTile: TRect;
  VTileColor: TColor32;
  VTileRect: ITileRect;
begin
  Result := nil;
  VBitmap := TBitmap32ByStaticBitmap.Create(FBitmap32StaticFactory);
  try
    VSize := Types.Point(AMapRect.Right - AMapRect.Left, AMapRect.Bottom - AMapRect.Top);
    VBitmap.SetSize(VSize.X, VSize.Y);
    VBitmap.Clear(0);

    VSourceProjectionType := ASourceProjection.ProjectionType;
    VTargetProjectionType := ATargetProjection.ProjectionType;

    VSameSourceAndTarget := VSourceProjectionType.IsSame(VTargetProjectionType);
    if VSameSourceAndTarget then begin
      VSourceRelativeRect := ATargetProjection.PixelRect2RelativeRect(AMapRect);
    end else begin
      VLonLatRect := ATargetProjection.PixelRect2LonLatRect(AMapRect);
      VSourceProjectionType.ValidateLonLatRect(VLonLatRect);
      VSourceRelativeRect := VSourceProjectionType.LonLatRect2RelativeRect(VLonLatRect);
    end;
    VSourceTileRect :=
      RectFromDoubleRect(
        ASourceProjection.RelativeRect2TileRectFloat(VSourceRelativeRect),
        t_GeoTypes.rrOutside
      );
    VSolidDrow :=
      (VSize.X <= (VSourceTileRect.Right - VSourceTileRect.Left) * 2) or
      (VSize.Y <= (VSourceTileRect.Bottom - VSourceTileRect.Top) * 2);

    if Assigned(FPolygon) then begin
      VSourceLonLatRect := ASourceProjection.TileRect2LonLatRect(VSourceTileRect);
      if GetIntersectedRect(VLonLatRect, VSourceLonLatRect, ASourceProjection, AProjectedPolygon) then begin
        VSourceTileRect :=
          RectFromDoubleRect(
            ASourceProjection.LonLatRect2TileRectFloat(VLonLatRect),
            t_GeoTypes.rrOutside
          );
      end else begin
        Exit;
      end;
    end;

    VTileRectInfo := FStorage.GetTileRectInfo(AOperationID, ACancelNotifier, VSourceTileRect, ASourceProjection.Zoom, AVersion);

    if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
      Exit;
    end;

    if VTileRectInfo <> nil then begin
      VTileRect := TTileRect.Create(ASourceProjection, VSourceTileRect);
      VIterator := TTileIteratorByRect.Create(VTileRect);
      VEnumTileInfo := VTileRectInfo.GetEnum(VIterator);
      while VEnumTileInfo.Next(VTileInfo) do begin
        VTileColor := AColorer.GetColor(VTileInfo);
        if VTileColor <> 0 then begin
          if VSameSourceAndTarget then begin
            VRelativeRectOfTile := ASourceProjection.TilePos2RelativeRect(VTileInfo.FTile);
          end else begin
            VLonLatRectOfTile := ASourceProjection.TilePos2LonLatRect(VTileInfo.FTile);
            VTargetProjectionType.ValidateLonLatRect(VLonLatRectOfTile);
            VRelativeRectOfTile := VTargetProjectionType.LonLatRect2RelativeRect(VLonLatRectOfTile);
          end;
          VMapPixelRectOfTile := ATargetProjection.RelativeRect2PixelRectFloat(VRelativeRectOfTile);
          VLocalPixelRectOfTile.Left := Trunc(VMapPixelRectOfTile.Left - AMapRect.Left);
          VLocalPixelRectOfTile.Top := Trunc(VMapPixelRectOfTile.Top - AMapRect.Top);
          VLocalPixelRectOfTile.Right := Trunc(VMapPixelRectOfTile.Right - AMapRect.Left);
          VLocalPixelRectOfTile.Bottom := Trunc(VMapPixelRectOfTile.Bottom - AMapRect.Top);
          if not VSolidDrow then begin
            Dec(VLocalPixelRectOfTile.Right);
            Dec(VLocalPixelRectOfTile.Bottom);
          end;
          VBitmap.FillRectS(VLocalPixelRectOfTile, VTileColor);
        end;
      end;
    end;
    Result := VBitmap.MakeAndClear;
  finally
    VBitmap.Free;
  end;
end;

end.
