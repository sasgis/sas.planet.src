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

unit u_BitmapLayerProviderFillingMap;

interface

uses
  i_NotifierOperation,
  i_Bitmap32Static,
  i_Bitmap32BufferFactory,
  i_ProjectionInfo,
  i_LocalCoordConverter,
  i_TileStorage,
  i_MapVersionRequest,
  i_BitmapLayerProvider,
  i_FillingMapColorer,
  u_BaseInterfacedObject;

type
  TBitmapLayerProviderFillingMap = class(TBaseInterfacedObject, IBitmapLayerProvider)
  private
    FBitmap32StaticFactory: IBitmap32StaticFactory;
    FStorage: ITileStorage;
    FVersion: IMapVersionRequest;
    FUseRelativeZoom: Boolean;
    FZoom: Integer;
    FColorer: IFillingMapColorer;

    function GetActualZoom(
      const AProjection: IProjectionInfo
    ): Byte;
    function GetFillingMapBitmap(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ALocalConverter: ILocalCoordConverter;
      ASourceZoom: byte;
      const AVersion: IMapVersionRequest;
      const AColorer: IFillingMapColorer
    ): IBitmap32Static;
  private
    function GetBitmapRect(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ALocalConverter: ILocalCoordConverter
    ): IBitmap32Static;
  public
    constructor Create(
      const ABitmap32StaticFactory: IBitmap32StaticFactory;
      const AStorage: ITileStorage;
      const AVersion: IMapVersionRequest;
      AUseRelativeZoom: Boolean;
      AZoom: Integer;
      const AColorer: IFillingMapColorer
    );
  end;

implementation

uses
  GR32,
  t_GeoTypes,
  i_CoordConverter,
  i_TileIterator,
  i_TileInfoBasic,
  u_GeoFunc,
  u_TileIteratorByRect,
  u_Bitmap32ByStaticBitmap;

{ TBitmapLayerProviderFillingMap }

constructor TBitmapLayerProviderFillingMap.Create(
  const ABitmap32StaticFactory: IBitmap32StaticFactory;
  const AStorage: ITileStorage;
  const AVersion: IMapVersionRequest;
  AUseRelativeZoom: Boolean;
  AZoom: Integer;
  const AColorer: IFillingMapColorer
);
begin
  Assert(Assigned(ABitmap32StaticFactory));
  Assert(Assigned(AStorage));
  Assert(Assigned(AVersion));
  Assert(Assigned(AColorer));
  inherited Create;
  FBitmap32StaticFactory := ABitmap32StaticFactory;
  FStorage := AStorage;
  FVersion := AVersion;
  FUseRelativeZoom := AUseRelativeZoom;
  FZoom := AZoom;
  FColorer := AColorer;
end;

function TBitmapLayerProviderFillingMap.GetActualZoom(
  const AProjection: IProjectionInfo
): Byte;
var
  VZoom: Integer;
begin
  VZoom := FZoom;
  if FUseRelativeZoom then begin
    VZoom := VZoom + AProjection.Zoom;
  end;
  if VZoom < 0 then begin
    Result := 0;
  end else begin
    Result := VZoom;
    AProjection.GeoConverter.ValidateZoom(Result);
  end;
end;

function TBitmapLayerProviderFillingMap.GetBitmapRect(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ALocalConverter: ILocalCoordConverter
): IBitmap32Static;
var
  VSourceZoom: Byte;
begin
  VSourceZoom := GetActualZoom(ALocalConverter.ProjectionInfo);
  if ALocalConverter.Zoom > VSourceZoom then begin
    Result := nil;
  end else begin
    Result :=
      GetFillingMapBitmap(
        AOperationID,
        ACancelNotifier,
        ALocalConverter,
        VSourceZoom,
        FVersion,
        FColorer
      );
  end;
end;

function TBitmapLayerProviderFillingMap.GetFillingMapBitmap(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ALocalConverter: ILocalCoordConverter;
  ASourceZoom: byte;
  const AVersion: IMapVersionRequest;
  const AColorer: IFillingMapColorer
): IBitmap32Static;
var
  VBitmap: TBitmap32ByStaticBitmap;
  VSize: TPoint;
  VTargetMapPixelRect: TDoubleRect;
  VSourceTileRect: TRect;
  VSourceRelativeRect: TDoubleRect;
  VSourceConverter: ICoordConverter;
  VTargetConverter: ICoordConverter;
  VSameSourceAndTarget: Boolean;
  VTargetZoom: Byte;
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
begin
  VBitmap := TBitmap32ByStaticBitmap.Create(FBitmap32StaticFactory);
  try
    VSize := ALocalConverter.GetLocalRectSize;
    VBitmap.SetSize(VSize.X, VSize.Y);
    VBitmap.Clear(0);

    VSourceConverter := FStorage.CoordConverter;
    VTargetConverter := ALocalConverter.GeoConverter;
    VTargetZoom := ALocalConverter.Zoom;

    VTargetMapPixelRect := ALocalConverter.GetRectInMapPixelFloat;
    VTargetConverter.ValidatePixelRectFloat(VTargetMapPixelRect, VTargetZoom);

    VSameSourceAndTarget := VSourceConverter.IsSameConverter(VTargetConverter);
    if VSameSourceAndTarget then begin
      VSourceRelativeRect := VSourceConverter.PixelRectFloat2RelativeRect(VTargetMapPixelRect, VTargetZoom);
    end else begin
      VLonLatRect := VTargetConverter.PixelRectFloat2LonLatRect(VTargetMapPixelRect, VTargetZoom);
      VSourceConverter.ValidateLonLatRect(VLonLatRect);
      VSourceRelativeRect := VSourceConverter.LonLatRect2RelativeRect(VLonLatRect);
    end;
    VSourceTileRect :=
      RectFromDoubleRect(
        VSourceConverter.RelativeRect2TileRectFloat(VSourceRelativeRect, ASourceZoom),
        rrOutside
      );
    VSolidDrow :=
      (VSize.X <= (VSourceTileRect.Right - VSourceTileRect.Left) * 2) or
      (VSize.Y <= (VSourceTileRect.Bottom - VSourceTileRect.Top) * 2);
    VTileRectInfo := FStorage.GetTileRectInfo(AOperationID, ACancelNotifier, VSourceTileRect, ASourceZoom, AVersion);
    if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
      Result := nil;
      Exit;
    end;

    if VTileRectInfo <> nil then begin
      VIterator := TTileIteratorByRect.Create(VSourceTileRect);
      VEnumTileInfo := VTileRectInfo.GetEnum(VIterator);
      while VEnumTileInfo.Next(VTileInfo) do begin
        VTileColor := AColorer.GetColor(VTileInfo);
        if VTileColor <> 0 then begin
          if VSameSourceAndTarget then begin
            VRelativeRectOfTile := VSourceConverter.TilePos2RelativeRect(VTileInfo.FTile, ASourceZoom);
          end else begin
            VLonLatRectOfTile := VSourceConverter.TilePos2LonLatRect(VTileInfo.FTile, ASourceZoom);
            VTargetConverter.ValidateLonLatRect(VLonLatRectOfTile);
            VRelativeRectOfTile := VTargetConverter.LonLatRect2RelativeRect(VLonLatRectOfTile);
          end;
          VMapPixelRectOfTile := VTargetConverter.RelativeRect2PixelRectFloat(VRelativeRectOfTile, VTargetZoom);
          VLocalPixelRectOfTile := RectFromDoubleRect(ALocalConverter.MapRectFloat2LocalRectFloat(VMapPixelRectOfTile), rrToTopLeft);
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
