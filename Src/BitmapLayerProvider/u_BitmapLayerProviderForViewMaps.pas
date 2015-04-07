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

unit u_BitmapLayerProviderForViewMaps;

interface

uses
  Types,
  i_NotifierOperation,
  i_CoordConverter,
  i_Bitmap32Static,
  i_Bitmap32BufferFactory,
  i_ProjectionInfo,
  i_MapType,
  i_MapTypeListStatic,
  i_BitmapLayerProvider,
  i_BitmapPostProcessing,
  i_TileError,
  u_BaseInterfacedObject;

type
  TBitmapLayerProviderForViewMaps = class(TBaseInterfacedObject, IBitmapTileUniProvider)
  private
    FMainMap: IMapType;
    FLayersList: IMapTypeListStatic;
    FBitmap32StaticFactory: IBitmap32StaticFactory;
    FUsePrevZoomAtMap: Boolean;
    FUsePrevZoomAtLayer: Boolean;
    FUseCache: Boolean;
    FPostProcessingConfig: IBitmapPostProcessing;
    FErrorLogger: ITileErrorLogger;

    function GetBitmapByMapType(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ATile: TPoint;
      AZoom: byte;
      const ACoordConverterTarget: ICoordConverter;
      const ASource: IBitmap32Static;
      AUsePrevZoom: Boolean;
      const AMapType: IMapType
    ): IBitmap32Static;
  private
    function GetTile(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const AProjectionInfo: IProjectionInfo;
      const ATile: TPoint
    ): IBitmap32Static;
  public
    constructor Create(
      const ABitmap32StaticFactory: IBitmap32StaticFactory;
      const AMainMap: IMapType;
      const ALayersList: IMapTypeListStatic;
      AUsePrevZoomAtMap: Boolean;
      AUsePrevZoomAtLayer: Boolean;
      AUseCache: Boolean;
      const APostProcessingConfig: IBitmapPostProcessing;
      const AErrorLogger: ITileErrorLogger
    );
  end;

implementation

uses
  SysUtils,
  GR32,
  i_TileObjCache,
  u_Bitmap32ByStaticBitmap,
  u_BitmapFunc,
  u_TileErrorInfo;

{ TBitmapLayerProviderForViewMaps }

constructor TBitmapLayerProviderForViewMaps.Create(
  const ABitmap32StaticFactory: IBitmap32StaticFactory;
  const AMainMap: IMapType;
  const ALayersList: IMapTypeListStatic;
  AUsePrevZoomAtMap, AUsePrevZoomAtLayer, AUseCache: Boolean;
  const APostProcessingConfig: IBitmapPostProcessing;
  const AErrorLogger: ITileErrorLogger
);
begin
  Assert(Assigned(ABitmap32StaticFactory));
  Assert(Assigned(AMainMap));
  inherited Create;
  FBitmap32StaticFactory := ABitmap32StaticFactory;
  FMainMap := AMainMap;
  FLayersList := ALayersList;
  FUsePrevZoomAtMap := AUsePrevZoomAtMap;
  FUsePrevZoomAtLayer := AUsePrevZoomAtLayer;
  FUseCache := AUseCache;
  FPostProcessingConfig := APostProcessingConfig;
  FErrorLogger := AErrorLogger;
end;

function TBitmapLayerProviderForViewMaps.GetBitmapByMapType(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ATile: TPoint;
  AZoom: byte;
  const ACoordConverterTarget: ICoordConverter;
  const ASource: IBitmap32Static;
  AUsePrevZoom: Boolean;
  const AMapType: IMapType
): IBitmap32Static;
var
  VCache: ITileObjCacheBitmap;
  VLayer: IBitmap32Static;
  VBitmap: TBitmap32ByStaticBitmap;
  VError: ITileErrorInfo;
begin
  Result := ASource;
  VLayer := nil;
  try
    VCache := nil;
    if FUseCache then begin
      VCache := AMapType.CacheBitmap;
    end;
    VLayer :=
      AMapType.LoadTileUni(
        ATile,
        AZoom,
        AMapType.VersionRequestConfig.GetStatic,
        ACoordConverterTarget,
        AUsePrevZoom,
        True,
        False,
        VCache
      );
  except
    on E: Exception do begin
      if FErrorLogger <> nil then begin
        VError :=
          TTileErrorInfo.Create(
            AMapType.Zmp.GUID,
            AZoom,
            ATile,
            E.Message
          );
        FErrorLogger.LogError(VError);
      end else begin
        raise;
      end;
    end;
    else if FErrorLogger <> nil then begin
        FErrorLogger.LogError(
          TTileErrorInfo.Create(
          AMapType.Zmp.GUID,
          AZoom,
          ATile,
          'Unexpected read tile error'
          )
        );
      end else begin
        raise;
      end;
  end;

  if VLayer <> nil then begin
    if Result = nil then begin
      Result := VLayer;
    end else begin
      VBitmap := TBitmap32ByStaticBitmap.Create(FBitmap32StaticFactory);
      try
        AssignStaticToBitmap32(VBitmap, Result);
        BlockTransferFull(
          VBitmap,
          0, 0,
          VLayer,
          dmBlend
        );
        Result := VBitmap.MakeAndClear;
      finally
        VBitmap.Free;
      end;
    end;
  end;
end;

function TBitmapLayerProviderForViewMaps.GetTile(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const AProjectionInfo: IProjectionInfo;
  const ATile: TPoint
): IBitmap32Static;
var
  VTile: TPoint;
  Vzoom: byte;
  VCoordConverterTarget: ICoordConverter;
  VPixelRect: TRect;
  i: Integer;
begin
  Vzoom := AProjectionInfo.Zoom;
  VCoordConverterTarget := AProjectionInfo.GeoConverter;
  VPixelRect := VCoordConverterTarget.TilePos2PixelRect(ATile, Vzoom);
  VTile := VCoordConverterTarget.PixelRect2TileRect(VPixelRect, Vzoom).TopLeft;
  Assert(Types.EqualRect(VPixelRect, VCoordConverterTarget.TilePos2PixelRect(VTile, Vzoom)));

  Result :=
    GetBitmapByMapType(
      AOperationID,
      ACancelNotifier,
      VTile,
      Vzoom,
      VCoordConverterTarget,
      nil,
      FUsePrevZoomAtMap,
      FMainMap
    );
  if FLayersList <> nil then begin
    for i := 0 to FLayersList.Count - 1 do begin
      Result :=
        GetBitmapByMapType(
          AOperationID,
          ACancelNotifier,
          VTile,
          Vzoom,
          VCoordConverterTarget,
          Result,
          FUsePrevZoomAtLayer,
          FLayersList.Items[i]
        );
    end;
  end;
  if FPostProcessingConfig <> nil then begin
    Result := FPostProcessingConfig.Process(Result);
  end;
end;

end.
