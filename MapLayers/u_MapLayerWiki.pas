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

unit u_MapLayerWiki;

interface

uses
  Windows,
  SysUtils,
  Classes,
  GR32,
  GR32_Image,
  i_Notifier,
  i_Listener,
  u_MapType,
  u_MapLayerBasic,
  t_GeoTypes,
  i_MapTypes,
  i_NotifierOperation,
  i_ActiveMapsConfig,
  i_ProjectionInfo,
  i_VectorItemProjected,
  i_VectorItmesFactory,
  i_IdCacheSimple,
  i_DoublePointsAggregator,
  i_KmlLayerConfig,
  i_ImageResamplerConfig,
  i_LocalCoordConverter,
  i_LocalCoordConverterFactorySimpe,
  i_LayerBitmapClearStrategy,
  i_InternalPerformanceCounter,
  i_TileError,
  i_ViewPortState,
  i_SimpleFlag,
  i_VectorDataItemSimple,
  i_MapElementsGuidedList,
  u_MapElementsGuidedList,
  u_MapLayerWithThreadDraw;

type
  TWikiLayer = class(TMapLayerTiledWithThreadDraw)
  private
    FConfig: IKmlLayerConfig;
    FVectorItmesFactory: IVectorItmesFactory;
    FLayersSet: IActiveMapsSet;
    FErrorLogger: ITileErrorLogger;

    FProjectedCache: IIdCacheSimple;
    FPointsAgregatorThread: IDoublePointsAggregator;
    FPointsAgregatorGUI: IDoublePointsAggregator;
    FTileChangeListener: IListener;
    FAllElements: IMapElementsGuidedList;

    FVectorMapsSet: IMapTypeSet;
    FVectorMapsSetCS: IReadWriteSync;
    FLinesClipRect: TDoubleRect;

    FTileUpdateFlag: ISimpleFlag;
    procedure OnTileChange;
    procedure OnTimer;

    function GetProjectedPath(
      const AData: IVectorDataItemLine;
      const AProjectionInfo: IProjectionInfo;
      const ATemp: IDoublePointsAggregator = nil
    ): IProjectedPath;
    function GetProjectedPolygon(
      const AData: IVectorDataItemPoly;
      const AProjectionInfo: IProjectionInfo;
      const ATemp: IDoublePointsAggregator = nil
    ): IProjectedPolygon;

    procedure ElementsClear;

    // define working list by map
    function GetElementsListByMap(const AMapType: TMapType): IInterfaceList;

    procedure AddWikiElement(
      const AElments: IInterfaceList;
      const AData: IVectorDataItemSimple;
      const ALocalConverter: ILocalCoordConverter
    );
    procedure AddElementsFromMap(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const AElments: IInterfaceList;
      Alayer: TMapType;
      const ALocalConverter: ILocalCoordConverter
    );
    procedure PrepareWikiElements(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ALocalConverter: ILocalCoordConverter
    );
    procedure OnConfigChange;
    procedure OnLayerSetChange;
    procedure ProcessDraw(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const AElments: IInterfaceList;
      const ALocalConverter: ILocalCoordConverter
    );
    function MouseOnElements(
      const AVisualConverter: ILocalCoordConverter;
      const ACopiedElements: IInterfaceList;
      const xy: TPoint;
      var AItemS: Double
    ): IVectorDataItemSimple;

  protected
    procedure DrawBitmap(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation
    ); override;
    procedure SetLayerCoordConverter(const AValue: ILocalCoordConverter); override;
    procedure DoHide; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      AParentMap: TImage32;
      const AViewPortState: IViewPortState;
      const AVectorItmesFactory: IVectorItmesFactory;
      const AResamplerConfig: IImageResamplerConfig;
      const AConverterFactory: ILocalCoordConverterFactorySimpe;
      const AClearStrategyFactory: ILayerBitmapClearStrategyFactory;
      const AErrorLogger: ITileErrorLogger;
      const ATimerNoifier: INotifier;
      const AConfig: IKmlLayerConfig;
      const ALayersSet: IActiveMapsSet
    );
    procedure StartThreads; override;
    procedure SendTerminateToThreads; override;

    // helper
    function MouseOnReg(
      const AVisualConverter: ILocalCoordConverter;
      const xy: TPoint;
      out AItemS: Double
    ): IVectorDataItemSimple; overload;
    function MouseOnReg(
      const AVisualConverter: ILocalCoordConverter;
      const xy: TPoint
    ): IVectorDataItemSimple; overload;

    function MouseOnRegWithGUID(
      const AVisualConverter: ILocalCoordConverter;
      const xy: TPoint;
      out AItemS: Double;
      out ALayerGUID: TGUID
    ): IVectorDataItemSimple;
  end;

implementation

uses
  ActiveX,
  GR32_Resamplers,
  u_Synchronizer,
  i_CoordConverter,
  i_LonLatRect,
  i_TileIterator,
  i_BitmapLayerProvider,
  i_Bitmap32Static,
  i_NotifierTileRectUpdate,
  u_ListenerByEvent,
  u_TileIteratorByRect,
  u_TileErrorInfo,
  u_ResStrings,
  u_SimpleFlagWithInterlock,
  u_DoublePointsAggregator,
  u_BitmapLayerProviderByVectorSubset,
  u_IdCacheSimpleThreadSafe,
  u_TileIteratorSpiralByRect;

{ TWikiLayer }

constructor TWikiLayer.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const AViewPortState: IViewPortState;
  const AVectorItmesFactory: IVectorItmesFactory;
  const AResamplerConfig: IImageResamplerConfig;
  const AConverterFactory: ILocalCoordConverterFactorySimpe;
  const AClearStrategyFactory: ILayerBitmapClearStrategyFactory;
  const AErrorLogger: ITileErrorLogger;
  const ATimerNoifier: INotifier;
  const AConfig: IKmlLayerConfig;
  const ALayersSet: IActiveMapsSet
);
begin
  inherited Create(
    APerfList,
    AAppStartedNotifier,
    AAppClosingNotifier,
    AParentMap,
    AViewPortState,
    AResamplerConfig,
    AConverterFactory,
    AClearStrategyFactory,
    ATimerNoifier,
    AConfig.ThreadConfig
  );
  FConfig := AConfig;
  FLayersSet := ALayersSet;
  FVectorItmesFactory := AVectorItmesFactory;
  FErrorLogger := AErrorLogger;
  FVectorMapsSetCS := MakeSyncRW_Var(Self);

  FProjectedCache := TIdCacheSimpleThreadSafe.Create;
  FPointsAgregatorThread := TDoublePointsAggregator.Create;
  FPointsAgregatorGUI := TDoublePointsAggregator.Create;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FConfig.GetChangeNotifier
  );

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnLayerSetChange),
    FLayersSet.GetChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnTimer),
    ATimerNoifier
  );
  FTileChangeListener := TNotifyNoMmgEventListener.Create(Self.OnTileChange);
  FTileUpdateFlag := TSimpleFlagWithInterlock.Create;

  FAllElements := TMapElementsGuidedList.Create;
end;

procedure TWikiLayer.DoHide;
begin
  inherited;
  ElementsClear;
end;

procedure TWikiLayer.AddWikiElement(
  const AElments: IInterfaceList;
  const AData: IVectorDataItemSimple;
  const ALocalConverter: ILocalCoordConverter
);
var
  VConverter: ICoordConverter;
  VSize: TPoint;
  VRect: ILonLatRect;
  VLLRect: TDoubleRect;
  VBounds: TDoubleRect;
begin
  if AData <> nil then begin
    VSize := ALocalConverter.GetLocalRectSize;
    VConverter := ALocalConverter.GetGeoConverter;
    VRect := AData.LLRect;
    if VRect <> nil then begin
      VLLRect := VRect.Rect;
      VConverter.CheckLonLatRect(VLLRect);
      VBounds := ALocalConverter.LonLatRect2LocalRectFloat(VLLRect);
      if ((VBounds.Top < VSize.Y) and (VBounds.Bottom > 0) and (VBounds.Left < VSize.X) and (VBounds.Right > 0)) then begin
        if Supports(AData, IVectorDataItemPoint) or (((VBounds.Right - VBounds.Left) > 1) and ((VBounds.Bottom - VBounds.Top) > 1)) then begin
          AElments.Add(AData);
        end;
      end;
    end;
  end;
end;

procedure TWikiLayer.AddElementsFromMap(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const AElments: IInterfaceList;
  Alayer: TMapType;
  const ALocalConverter: ILocalCoordConverter
);
var
  ii: integer;
  kml: IVectorDataItemList;
  VTileIterator: ITileIterator;
  VZoom: Byte;
  VSourceGeoConvert: ICoordConverter;
  VGeoConvert: ICoordConverter;
  VBitmapOnMapPixelRect: TDoubleRect;
  VSourceLonLatRect: TDoubleRect;
  VTileSourceRect: TRect;
  VTile: TPoint;
  VErrorString: string;
  VError: ITileErrorInfo;
begin
  VZoom := ALocalConverter.GetZoom;
  VSourceGeoConvert := Alayer.GeoConvert;
  VGeoConvert := ALocalConverter.GetGeoConverter;

  VBitmapOnMapPixelRect := ALocalConverter.GetRectInMapPixelFloat;
  VGeoConvert.CheckPixelRectFloat(VBitmapOnMapPixelRect, VZoom);

  VSourceLonLatRect := VGeoConvert.PixelRectFloat2LonLatRect(VBitmapOnMapPixelRect, VZoom);
  VTileSourceRect := VSourceGeoConvert.LonLatRect2TileRect(VSourceLonLatRect, VZoom);
  VTileIterator := TTileIteratorByRect.Create(VTileSourceRect);

  while VTileIterator.Next(VTile) do begin
    VErrorString := '';
    try
      kml := Alayer.LoadTileVector(VTile, Vzoom, False, Alayer.CacheVector);
      if kml <> nil then begin
        if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
          Break;
        end else begin
          for ii := 0 to KML.Count - 1 do begin
            AddWikiElement(AElments, KML.GetItem(ii), ALocalConverter);
          end;
        end;
      end;
    except
      on E: Exception do begin
        VErrorString := E.Message;
      end;
      else
        VErrorString := SAS_ERR_TileDownloadUnexpectedError;
    end;
    if VErrorString <> '' then begin
      VError :=
        TTileErrorInfo.Create(
          Alayer,
          VZoom,
          VTile,
          VErrorString
        );
      FErrorLogger.LogError(VError);
    end;
    kml := nil;
  end;
end;

procedure TWikiLayer.PrepareWikiElements(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ALocalConverter: ILocalCoordConverter
);
var
  VVectorMapsSet: IMapTypeSet;
  VEnum: IEnumGUID;
  VGUID: TGUID;
  Vcnt: Cardinal;
  VItem: IMapType;
  VMapType: TMapType;
  VElements: IInterfaceList;
begin
  FVectorMapsSetCS.BeginRead;
  try
    VVectorMapsSet := FVectorMapsSet;
  finally
    FVectorMapsSetCS.EndRead;
  end;

  if VVectorMapsSet <> nil then begin
    VEnum := VVectorMapsSet.GetIterator;
    while VEnum.Next(1, VGUID, Vcnt) = S_OK do begin
      VItem := VVectorMapsSet.GetMapTypeByGUID(VGUID);
      VMapType := VItem.GetMapType;
      if VMapType.IsKmlTiles then begin
        VElements := GetElementsListByMap(VMapType);
        AddElementsFromMap(AOperationID, ACancelNotifier, VElements, VMapType, ALocalConverter);
        if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
          Break;
        end;
      end;
    end;
  end;
end;

procedure TWikiLayer.ProcessDraw(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const AElments: IInterfaceList;
  const ALocalConverter: ILocalCoordConverter
);
var
  VColorMain: TColor32;
  VColorBG: TColor32;
  VPointColor: TColor32;
  VTileIterator: ITileIterator;
  VGeoConvert: ICoordConverter;

  VZoom: Byte;
  { Прямоугольник пикселей растра в координатах основного конвертера }
  VBitmapOnMapPixelRect: TRect;
  { Прямоугольник тайлов текущего зума, покрывающий растр, в кооординатах
    основного конвертера }
  VTileSourceRect: TRect;
  { Текущий тайл в кооординатах основного конвертера }
  VTile: TPoint;
  { Прямоугольник пикслов текущего тайла в кооординатах основного конвертера }
  VCurrTilePixelRect: TRect;
  { Прямоугольник пикселов в которые будет скопирован текущий тайл }
  VCurrTileOnBitmapRect: TRect;
  VTileConverter: ILocalCoordConverter;
  VProvider: IBitmapLayerProvider;
  VBitmapTile: IBitmap32Static;
begin
  FConfig.LockRead;
  try
    VColorMain := FConfig.MainColor;
    VColorBG := FConfig.ShadowColor;
    VPointColor := FConfig.PointColor;
  finally
    FConfig.UnlockRead;
  end;
  if AElments.Count > 0 then begin
    if not ACancelNotifier.IsOperationCanceled(AOperationID) then begin
      VProvider :=
        TBitmapLayerProviderByVectorSubset.Create(
          VColorMain,
          VColorBG,
          VPointColor,
          FVectorItmesFactory,
          ALocalConverter.ProjectionInfo,
          FProjectedCache,
          FLinesClipRect,
          AElments
        );
      VGeoConvert := ALocalConverter.GetGeoConverter;
      VZoom := ALocalConverter.GetZoom;

      VBitmapOnMapPixelRect := ALocalConverter.GetRectInMapPixel;
      VGeoConvert.CheckPixelRect(VBitmapOnMapPixelRect, VZoom);

      VTileSourceRect := VGeoConvert.PixelRect2TileRect(VBitmapOnMapPixelRect, VZoom);
      VTileIterator := TTileIteratorSpiralByRect.Create(VTileSourceRect);
      while VTileIterator.Next(VTile) do begin
        if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
          break;
        end;
        VTileConverter := ConverterFactory.CreateForTile(VTile, VZoom, VGeoConvert);
        VCurrTilePixelRect := VTileConverter.GetRectInMapPixel;
        VCurrTileOnBitmapRect := ALocalConverter.MapRect2LocalRect(VCurrTilePixelRect);

        VBitmapTile :=
          VProvider.GetBitmapRect(
            AOperationID,
            ACancelNotifier,
            VTileConverter
          );

        Layer.Bitmap.Lock;
        try
          if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
            break;
          end;
          if VBitmapTile <> nil then begin
            BlockTransfer(
              Layer.Bitmap,
              VCurrTileOnBitmapRect.Left,
              VCurrTileOnBitmapRect.Top,
              Layer.Bitmap.ClipRect,
              VBitmapTile.Bitmap,
              VBitmapTile.Bitmap.BoundsRect,
              dmOpaque
            );
          end else begin
            Layer.Bitmap.FillRectS(
              VCurrTileOnBitmapRect.Left,
              VCurrTileOnBitmapRect.Top,
              VCurrTileOnBitmapRect.Right,
              VCurrTileOnBitmapRect.Bottom,
              0
            );
          end;
          SetBitmapChanged;
        finally
          Layer.Bitmap.UnLock;
        end;
      end;
    end;
  end;
end;

procedure TWikiLayer.OnConfigChange;
begin
  ViewUpdateLock;
  try
    SetNeedRedraw;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TWikiLayer.OnLayerSetChange;
var
  VOldLayersSet: IMapTypeSet;
  VNewLayersSet: IMapTypeSet;
  VEnum: IEnumGUID;
  VGUID: TGUID;
  cnt: Cardinal;
  VNotifier: INotifierTileRectUpdate;
  VMap: IMapType;
  VLocalConverter: ILocalCoordConverter;
  VZoom: Byte;
  VMapPixelRect: TDoubleRect;
  VLonLatRect: TDoubleRect;
  VTileRect: TRect;
  VMapConverter: ICoordConverter;
begin
  ViewUpdateLock;
  try
    VNewLayersSet := FLayersSet.GetSelectedMapsSet;

    FVectorMapsSetCS.BeginWrite;
    try
      VOldLayersSet := FVectorMapsSet;
      FVectorMapsSet := VNewLayersSet;
    finally
      FVectorMapsSetCS.EndWrite;
    end;

    VLocalConverter := LayerCoordConverter;
    if VLocalConverter <> nil then begin
      VZoom := VLocalConverter.GetZoom;
      if VOldLayersSet <> nil then begin
        VEnum := VOldLayersSet.GetIterator;
        while VEnum.Next(1, VGUID, cnt) = S_OK do begin
          if (VNewLayersSet = nil) or (VNewLayersSet.GetMapTypeByGUID(VGUID) = nil) then begin
            VMap := VOldLayersSet.GetMapTypeByGUID(VGUID);
            if VMap <> nil then begin
              VNotifier := VMap.MapType.NotifierByZoom[VZoom];
              if VNotifier <> nil then begin
                VNotifier.Remove(FTileChangeListener);
              end;
            end;
          end;
        end;
      end;
      if VNewLayersSet <> nil then begin
        VMapPixelRect := VLocalConverter.GetRectInMapPixelFloat;
        VLocalConverter.GetGeoConverter.CheckPixelRectFloat(VMapPixelRect, VZoom);
        VLonLatRect := VLocalConverter.GetGeoConverter.PixelRectFloat2LonLatRect(VMapPixelRect, VZoom);
        VEnum := VNewLayersSet.GetIterator;
        while VEnum.Next(1, VGUID, cnt) = S_OK do begin
          if (VOldLayersSet = nil) or (VOldLayersSet.GetMapTypeByGUID(VGUID) = nil) then begin
            VMap := VNewLayersSet.GetMapTypeByGUID(VGUID);
            if VMap <> nil then begin
              VNotifier := VMap.MapType.NotifierByZoom[VZoom];
              if VNotifier <> nil then begin
                VMapConverter := VMap.MapType.GeoConvert;
                VMapConverter.CheckLonLatRect(VLonLatRect);
                VTileRect := VMapConverter.LonLatRect2TileRect(VLonLatRect, VZoom);
                VNotifier.Add(FTileChangeListener, VTileRect);
              end;
            end;
          end;
        end;
      end;
    end;
    SetNeedRedraw;
    SetVisible(VNewLayersSet.GetIterator.Next(1, VGUID, cnt) = S_OK);
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TWikiLayer.OnTileChange;
begin
  FTileUpdateFlag.SetFlag;
end;

procedure TWikiLayer.OnTimer;
begin
  if FTileUpdateFlag.CheckFlagAndReset then begin
    ViewUpdateLock;
    try
      SetNeedRedraw;
    finally
      ViewUpdateUnlock;
    end;
  end;
end;

procedure TWikiLayer.SendTerminateToThreads;
var
  VZoom: Byte;
  VMap: IMapType;
  VNotifier: INotifierTileRectUpdate;
  VLayersSet: IMapTypeSet;
  VEnum: IEnumGUID;
  VGUID: TGUID;
  cnt: Cardinal;
begin
  inherited;
  if LayerCoordConverter <> nil then begin
    VZoom := LayerCoordConverter.GetZoom;

    FVectorMapsSetCS.BeginRead;
    try
      VLayersSet := FVectorMapsSet;
    finally
      FVectorMapsSetCS.EndRead;
    end;

    if VLayersSet <> nil then begin
      VEnum := VLayersSet.GetIterator;
      while VEnum.Next(1, VGUID, cnt) = S_OK do begin
        VMap := VLayersSet.GetMapTypeByGUID(VGUID);
        if VMap <> nil then begin
          VNotifier := VMap.MapType.NotifierByZoom[VZoom];
          if VNotifier <> nil then begin
            VNotifier.Remove(FTileChangeListener);
          end;
        end;
      end;
    end;
  end;
end;

procedure TWikiLayer.SetLayerCoordConverter(const AValue: ILocalCoordConverter);
var
  VOldZoom: Byte;
  VZoom: Byte;
  VMap: IMapType;
  VNotifier: INotifierTileRectUpdate;
  VLayersSet: IMapTypeSet;
  VEnum: IEnumGUID;
  VGUID: TGUID;
  cnt: Cardinal;
  VMapPixelRect: TDoubleRect;
  VLonLatRect: TDoubleRect;
  VTileRect: TRect;
begin
  VOldZoom := 255;
  if LayerCoordConverter <> nil then begin
    VOldZoom := LayerCoordConverter.GetZoom;
  end;
  inherited;
  VZoom := AValue.GetZoom;
  if VZoom <> VOldZoom then begin
    if VOldZoom <> 255 then begin
      FVectorMapsSetCS.BeginRead;
      try
        VLayersSet := FVectorMapsSet;
      finally
        FVectorMapsSetCS.EndRead;
      end;

      if VLayersSet <> nil then begin
        VEnum := VLayersSet.GetIterator;
        while VEnum.Next(1, VGUID, cnt) = S_OK do begin
          VMap := VLayersSet.GetMapTypeByGUID(VGUID);
          if VMap <> nil then begin
            VNotifier := VMap.MapType.NotifierByZoom[VOldZoom];
            if VNotifier <> nil then begin
              VNotifier.Remove(FTileChangeListener);
            end;
          end;
        end;
      end;
    end;
  end;
  VMapPixelRect := AValue.GetRectInMapPixelFloat;
  AValue.GetGeoConverter.CheckPixelRectFloat(VMapPixelRect, VZoom);
  VLonLatRect := AValue.GetGeoConverter.PixelRectFloat2LonLatRect(VMapPixelRect, VZoom);

  FVectorMapsSetCS.BeginRead;
  try
    VLayersSet := FVectorMapsSet;
  finally
    FVectorMapsSetCS.EndRead;
  end;

  if VLayersSet <> nil then begin
    VEnum := VLayersSet.GetIterator;
    while VEnum.Next(1, VGUID, cnt) = S_OK do begin
      VMap := VLayersSet.GetMapTypeByGUID(VGUID);
      if VMap <> nil then begin
        VNotifier := VMap.MapType.NotifierByZoom[VZoom];
        if VNotifier <> nil then begin
          VMap.MapType.GeoConvert.CheckLonLatRect(VLonLatRect);
          VTileRect := VMap.MapType.GeoConvert.LonLatRect2TileRect(VLonLatRect, VZoom);
          VNotifier.Add(FTileChangeListener, VTileRect);
        end;
      end;
    end;
  end;
end;

procedure TWikiLayer.StartThreads;
begin
  inherited;
  OnLayerSetChange;
end;

procedure TWikiLayer.DrawBitmap(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation
);
var
  VBitmapConverter: ILocalCoordConverter;
  VList: IInterfaceList;
  VMapPixelRect: TDoubleRect;
begin
  inherited;
  VBitmapConverter := LayerCoordConverter;
  if VBitmapConverter <> nil then begin
    VMapPixelRect := VBitmapConverter.GetRectInMapPixelFloat;
    FLinesClipRect.Left := VMapPixelRect.Left - 10;
    FLinesClipRect.Top := VMapPixelRect.Top - 10;
    FLinesClipRect.Right := VMapPixelRect.Right + 10;
    FLinesClipRect.Bottom := VMapPixelRect.Bottom + 10;
    ElementsClear;
    FProjectedCache.Clear;
    PrepareWikiElements(AOperationID, ACancelNotifier, VBitmapConverter);
    VList := TInterfaceList.Create;
    // copy all elements into
    FAllElements.CopyMapElementsToList(TRUE, TRUE, VList);
    // work
    if VList.Count > 0 then begin
      ProcessDraw(AOperationID, ACancelNotifier, VList, VBitmapConverter);
    end else begin
      Layer.Bitmap.Lock;
      try
        if not ACancelNotifier.IsOperationCanceled(AOperationID) then begin
          Layer.Bitmap.Clear(0);
          SetBitmapChanged;
        end;
      finally
        Layer.Bitmap.UnLock;
      end;
    end;
  end;
end;

function TWikiLayer.GetElementsListByMap(const AMapType: TMapType): IInterfaceList;
begin
  if Assigned(AMapType.Zmp.MapAttachmentsInfo) then begin
    Result := FAllElements.GetMapElementsWithGUID(AMapType.Zmp.GUID);
  end else begin
    Result := FAllElements.GetMapElementsWithoutGUID;
  end;
end;

function TWikiLayer.GetProjectedPath(
  const AData: IVectorDataItemLine;
  const AProjectionInfo: IProjectionInfo;
  const ATemp: IDoublePointsAggregator = nil
): IProjectedPath;
var
  VID: Integer;
begin
  VID := Integer(AData);
  if not Supports(FProjectedCache.GetByID(VID), IProjectedPath, Result) then begin
    Result :=
      FVectorItmesFactory.CreateProjectedPathWithClipByLonLatPath(
        AProjectionInfo,
        AData.Line,
        FLinesClipRect,
        ATemp
      );
    FProjectedCache.Add(VID, Result);
  end;
end;

function TWikiLayer.GetProjectedPolygon(
  const AData: IVectorDataItemPoly;
  const AProjectionInfo: IProjectionInfo;
  const ATemp: IDoublePointsAggregator = nil
): IProjectedPolygon;
var
  VID: Integer;
begin
  VID := Integer(AData);
  if not Supports(FProjectedCache.GetByID(VID), IProjectedPath, Result) then begin
    Result :=
      FVectorItmesFactory.CreateProjectedPolygonWithClipByLonLatPolygon(
        AProjectionInfo,
        AData.Line,
        FLinesClipRect,
        ATemp
      );
    FProjectedCache.Add(VID, Result);
  end;
end;

procedure TWikiLayer.ElementsClear;
begin
  FAllElements.ClearMapElements;
end;

function TWikiLayer.MouseOnReg(
  const AVisualConverter: ILocalCoordConverter;
  const xy: TPoint;
  out AItemS: Double
): IVectorDataItemSimple;
var
  VElements: IInterfaceList;
begin
  Result := nil;
  AItemS := 0;
  // single call on ALL elements
  VElements := TInterfaceList.Create;
  try
    FAllElements.CopyMapElementsToList(TRUE, TRUE, VElements);
    Result := MouseOnElements(AVisualConverter, VElements, xy, AItemS);
  finally
    VElements := nil;
  end;
end;

function TWikiLayer.MouseOnElements(
  const AVisualConverter: ILocalCoordConverter;
  const ACopiedElements: IInterfaceList;
  const xy: TPoint;
  var AItemS: Double
): IVectorDataItemSimple;
var
  VRect: TRect;
  VLocalConverter: ILocalCoordConverter;
  VConverter: ICoordConverter;
  VZoom: Byte;
  VMapRect: TDoubleRect;
  VLonLatRect: TDoubleRect;
  VPixelPos: TDoublePoint;
  i: integer;
  VItem: IVectorDataItemSimple;
  VProjectdPath: IProjectedPath;
  VItemLine: IVectorDataItemLine;
  VItemPoly: IVectorDataItemPoly;
  VProjectdPolygon: IProjectedPolygon;
  VSquare: Double;
begin
  // return TRUE if breaks loop
  Result := nil;

  // if has elements
  if ACopiedElements.Count > 0 then begin
    VLocalConverter := LayerCoordConverter;
    if not AVisualConverter.ProjectionInfo.GetIsSameProjectionInfo(VLocalConverter.ProjectionInfo) then begin
      Exit;
    end;
    // TODO: execute this only once (for multiple calls)
    VRect.Left := xy.X - 3;
    VRect.Top := xy.Y - 3;
    VRect.Right := xy.X + 3;
    VRect.Bottom := xy.Y + 3;

    VConverter := VLocalConverter.GetGeoConverter;
    VZoom := VLocalConverter.GetZoom;
    VMapRect := AVisualConverter.LocalRect2MapRectFloat(VRect);
    VConverter.CheckPixelRectFloat(VMapRect, VZoom);
    VLonLatRect := VConverter.PixelRectFloat2LonLatRect(VMapRect, VZoom);
    VPixelPos := AVisualConverter.LocalPixel2MapPixelFloat(xy);

    // check element
    for i := 0 to ACopiedElements.Count - 1 do begin
      VItem := IVectorDataItemSimple(Pointer(ACopiedElements[i]));
      if VItem.LLRect.IsIntersecWithRect(VLonLatRect) then begin
        if Supports(VItem, IVectorDataItemPoint) then begin
          Result := VItem;
          AItemS := 0;
          Exit;
        end else if Supports(VItem, IVectorDataItemLine, VItemLine) then begin
          VProjectdPath := GetProjectedPath(VItemLine, VLocalConverter.ProjectionInfo, FPointsAgregatorGUI);
          if VProjectdPath.IsPointOnPath(VPixelPos, 2) then begin
            Result := VItem;
            AItemS := 0;
            Exit;
          end;
        end else if Supports(VItem, IVectorDataItemPoly, VItemPoly) then begin
          VProjectdPolygon := GetProjectedPolygon(VItemPoly, VLocalConverter.ProjectionInfo, FPointsAgregatorGUI);
          if VProjectdPolygon.IsPointInPolygon(VPixelPos) then begin
            VSquare := VProjectdPolygon.CalcArea;
            if (Result = nil) or (VSquare < AItemS) then begin
              Result := VItem;
              AItemS := VSquare;
            end;
          end;
        end;
      end;
    end;
  end;
end;

function TWikiLayer.MouseOnReg(
  const AVisualConverter: ILocalCoordConverter;
  const xy: TPoint
): IVectorDataItemSimple;
var
  VItemS: Double;
begin
  Result := MouseOnReg(AVisualConverter, xy, VItemS);
end;

function TWikiLayer.MouseOnRegWithGUID(
  const AVisualConverter: ILocalCoordConverter;
  const xy: TPoint;
  out AItemS: Double;
  out ALayerGUID: TGUID
): IVectorDataItemSimple;
var
  VElements: IInterfaceList;
  VEnumGUID: IEnumGUID;
  celtFetched: Cardinal;
begin
  Result := nil;
  ALayerGUID := GUID_NULL;
  AItemS := 0;

  VElements := TInterfaceList.Create;
  try
    // loop through guided
    VEnumGUID := FAllElements.GetGUIDEnum;
    try
      if Assigned(VEnumGUID) then begin
        while (S_OK = VEnumGUID.Next(1, ALayerGUID, celtFetched)) do begin
          VElements.Clear;
          CopyMapElements(FAllElements.GetMapElementsWithGUID(ALayerGUID), VElements);
          Result := MouseOnElements(AVisualConverter, VElements, xy, AItemS);
          if Result <> nil then begin
            Exit;
          end;
        end;
      end;
    finally
      VEnumGUID := nil;
    end;

    // no guid at all
    ALayerGUID := GUID_NULL;

    // without guid
    VElements.Clear;
    CopyMapElements(FAllElements.GetMapElementsWithoutGUID, VElements);
    Result := MouseOnElements(AVisualConverter, VElements, xy, AItemS);
  finally
    VElements := nil;
  end;
end;

end.
