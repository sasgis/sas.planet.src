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
  Types,
  Classes,
  GR32,
  GR32_Polygons,
  GR32_Image,
  i_JclNotify,
  u_MapType,
  u_MapLayerBasic,
  t_GeoTypes,
  i_MapTypes,
  i_OperationNotifier,
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
    FPolygon: TPolygon32;
    FPointsAgregatorThread: IDoublePointsAggregator;
    FPointsAgregatorGUI: IDoublePointsAggregator;
    FTileChangeListener: IJclListener;
    FAllElements: IMapElementsGuidedList;

    FVectorMapsSet: IMapTypeSet;
    FVectorMapsSetCS: IReadWriteSync;
    FLinesClipRect: TDoubleRect;

    FFixedPointArray: TArrayOfFixedPoint;
    FTileUpdateCounter: Integer;
    procedure OnTileChange;
    procedure OnTimer;

    function GetProjectedPath(
      AData: IVectorDataItemLine;
      AProjectionInfo: IProjectionInfo;
      ATemp: IDoublePointsAggregator = nil
    ): IProjectedPath;
    function GetProjectedPolygon(
      AData: IVectorDataItemPoly;
      AProjectionInfo: IProjectionInfo;
      ATemp: IDoublePointsAggregator = nil
    ): IProjectedPolygon;

    procedure ElementsClear;

    // define working list by map
    function GetElementsListByMap(const AMapType: TMapType): IInterfaceList;

    procedure DrawPoint(
      ATargetBmp: TCustomBitmap32;
      APointColor: TColor32;
      APointColorBG: TColor32;
      AData: IVectorDataItemPoint;
      ALocalConverter: ILocalCoordConverter
    );
    procedure DrawLine(
      ATargetBmp: TCustomBitmap32;
      AColorMain: TColor32;
      AColorBG: TColor32;
      AData: IVectorDataItemLine;
      ALocalConverter: ILocalCoordConverter
    );
    procedure DrawPoly(
      ATargetBmp: TCustomBitmap32;
      AColorMain: TColor32;
      AColorBG: TColor32;
      AData: IVectorDataItemPoly;
      ALocalConverter: ILocalCoordConverter
    );
    procedure DrawWikiElement(
      ATargetBmp: TCustomBitmap32;
      AColorMain: TColor32;
      AColorBG: TColor32;
      APointColor: TColor32;
      AData: IVectorDataItemSimple;
      ALocalConverter: ILocalCoordConverter
    );
    procedure AddWikiElement(
      AElments: IInterfaceList;
      AData: IVectorDataItemSimple;
      ALocalConverter: ILocalCoordConverter
    );
    procedure AddElementsFromMap(
      AOperationID: Integer;
      ACancelNotifier: IOperationNotifier;
      AElments: IInterfaceList;
      Alayer: TMapType;
      ALocalConverter: ILocalCoordConverter
    );
    procedure PrepareWikiElements(
      AOperationID: Integer;
      ACancelNotifier: IOperationNotifier;
      ALocalConverter: ILocalCoordConverter
    );
    procedure OnConfigChange;
    procedure OnLayerSetChange;
    procedure GetBitmapRect(
      AOperationID: Integer;
      ACancelNotifier: IOperationNotifier;
      AElments: IInterfaceList;
      ATargetBmp: TCustomBitmap32;
      ALocalConverter: ILocalCoordConverter;
      AColorMain: TColor32;
      AColorBG: TColor32;
      APointColor: TColor32
    );
    procedure ProcessDraw(
      AOperationID: Integer;
      ACancelNotifier: IOperationNotifier;
      AElments: IInterfaceList;
      ALocalConverter: ILocalCoordConverter
    );
    function MouseOnElements(const ACopiedElements: IInterfaceList;
                             const xy: TPoint;
                             var AItem: IVectorDataItemSimple;
                             var AItemS: Double): Boolean;
    
  protected
    procedure DrawBitmap(
      AOperationID: Integer;
      ACancelNotifier: IOperationNotifier
    ); override;
    procedure SetLayerCoordConverter(AValue: ILocalCoordConverter); override;
    procedure DoHide; override;
  public
    constructor Create(
      APerfList: IInternalPerformanceCounterList;
      AAppClosingNotifier: IJclNotifier;
      AParentMap: TImage32;
      AViewPortState: IViewPortState;
      AVectorItmesFactory: IVectorItmesFactory;
      AResamplerConfig: IImageResamplerConfig;
      AConverterFactory: ILocalCoordConverterFactorySimpe;
      AClearStrategyFactory: ILayerBitmapClearStrategyFactory;
      AErrorLogger: ITileErrorLogger;
      ATimerNoifier: IJclNotifier;
      AConfig: IKmlLayerConfig;
      ALayersSet: IActiveMapsSet
    );
    destructor Destroy; override;
    procedure StartThreads; override;
    procedure SendTerminateToThreads; override;

    // helper
    procedure MouseOnReg(xy: TPoint; out AItem: IVectorDataItemSimple; out AItemS: Double); overload;
    procedure MouseOnReg(xy: TPoint; out AItem: IVectorDataItemSimple); overload;

    procedure MouseOnRegWithGUID(const xy: TPoint;
                                 out AItem: IVectorDataItemSimple;
                                 out AItemS: Double;
                                 out ALayerGUID: TGUID);
  end;

implementation

uses
  ActiveX,
  GR32_Resamplers,
  u_Synchronizer,
  i_CoordConverter,
  i_TileIterator,
  i_EnumDoublePoint,
  i_TileRectUpdateNotifier,
  u_NotifyEventListener,
  u_TileIteratorByRect,
  u_TileErrorInfo,
  u_ResStrings,
  u_DoublePointsAggregator,
  u_IdCacheSimpleThreadSafe,
  u_TileIteratorSpiralByRect;

{ TWikiLayer }

constructor TWikiLayer.Create(
  APerfList: IInternalPerformanceCounterList;
  AAppClosingNotifier: IJclNotifier;
  AParentMap: TImage32;
  AViewPortState: IViewPortState;
  AVectorItmesFactory: IVectorItmesFactory;
  AResamplerConfig: IImageResamplerConfig;
  AConverterFactory: ILocalCoordConverterFactorySimpe;
  AClearStrategyFactory: ILayerBitmapClearStrategyFactory;
  AErrorLogger: ITileErrorLogger;
  ATimerNoifier: IJclNotifier;
  AConfig: IKmlLayerConfig;
  ALayersSet: IActiveMapsSet
);
begin
  inherited Create(
    APerfList,
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
  FTileUpdateCounter := 0;

  FAllElements := TMapElementsGuidedList.Create;

  FPolygon := TPolygon32.Create;
  FPolygon.Antialiased := true;
  FPolygon.AntialiasMode := am4times;
end;

destructor TWikiLayer.Destroy;
begin
  FAllElements := nil;
  FreeAndNil(FPolygon);
  FVectorMapsSetCS := nil;
  inherited;
end;

procedure TWikiLayer.DoHide;
begin
  inherited;
  ElementsClear;
end;

procedure TWikiLayer.AddWikiElement(AElments: IInterfaceList; AData: IVectorDataItemSimple; ALocalConverter: ILocalCoordConverter);
var
  VConverter: ICoordConverter;
  VSize: TPoint;
  VLLRect: TDoubleRect;
  VBounds: TDoubleRect;
begin
  if AData <> nil then begin
    VSize := ALocalConverter.GetLocalRectSize;
    VConverter := ALocalConverter.GetGeoConverter;
    VLLRect := AData.LLRect;
    VConverter.CheckLonLatRect(VLLRect);
    VBounds := ALocalConverter.LonLatRect2LocalRectFloat(VLLRect);
    if ((VBounds.Top < VSize.Y) and (VBounds.Bottom > 0) and (VBounds.Left < VSize.X) and (VBounds.Right > 0)) then begin
      if Supports(AData, IVectorDataItemPoint) or (((VBounds.Right - VBounds.Left) > 1) and ((VBounds.Bottom - VBounds.Top) > 1)) then begin
        AElments.Add(AData);
      end;
    end;
  end;
end;

procedure TWikiLayer.AddElementsFromMap(
  AOperationID: Integer;
  ACancelNotifier: IOperationNotifier;
  AElments: IInterfaceList;
  Alayer: TMapType;
  ALocalConverter: ILocalCoordConverter
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
      if Alayer.LoadTile(kml, VTile, Vzoom, False, Alayer.CacheVector) then begin
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
      FErrorLogger.LogError(
        TTileErrorInfo.Create(
          Alayer,
          VZoom,
          VTile,
          VErrorString
        )
      );
    end;
    kml := nil;
  end;
end;

procedure TWikiLayer.PrepareWikiElements(
  AOperationID: Integer;
  ACancelNotifier: IOperationNotifier;
  ALocalConverter: ILocalCoordConverter
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
  ACancelNotifier: IOperationNotifier;
  AElments: IInterfaceList;
  ALocalConverter: ILocalCoordConverter
);
var
  VColorMain: TColor32;
  VColorBG: TColor32;
  VPointColor: TColor32;
  VTileToDrawBmp: TCustomBitmap32;
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
  { Прямоугольник тайла подлежащий отображению на текущий растр }
  VTilePixelsToDraw: TRect;
  { Прямоугольник пикселов в которые будет скопирован текущий тайл }
  VCurrTileOnBitmapRect: TRect;
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
      VTileToDrawBmp := TCustomBitmap32.Create;
      try
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
          VCurrTilePixelRect := VGeoConvert.TilePos2PixelRect(VTile, VZoom);

          VTilePixelsToDraw.TopLeft := Point(0, 0);
          VTilePixelsToDraw.Right := VCurrTilePixelRect.Right - VCurrTilePixelRect.Left;
          VTilePixelsToDraw.Bottom := VCurrTilePixelRect.Bottom - VCurrTilePixelRect.Top;

          VCurrTileOnBitmapRect:= ALocalConverter.MapRect2LocalRect(VCurrTilePixelRect);

          VTileToDrawBmp.SetSize(VTilePixelsToDraw.Right, VTilePixelsToDraw.Bottom);
          VTileToDrawBmp.Clear(0);
          GetBitmapRect(
            AOperationID,
            ACancelNotifier,
            AElments,
            VTileToDrawBmp,
            ConverterFactory.CreateForTile(VTile, VZoom, VGeoConvert),
            VColorMain,
            VColorBG,
            VPointColor
          );

          Layer.Bitmap.Lock;
          try
            if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
              break;
            end;
            BlockTransfer(
              Layer.Bitmap,
              VCurrTileOnBitmapRect.Left,
              VCurrTileOnBitmapRect.Top,
              Layer.Bitmap.ClipRect,
              VTileToDrawBmp,
              VTilePixelsToDraw,
              dmOpaque
            );
            SetBitmapChanged;
          finally
            Layer.Bitmap.UnLock;
          end;
        end;
      finally
        VTileToDrawBmp.Free;
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
  VNotifier: ITileRectUpdateNotifier;
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
  InterlockedIncrement(FTileUpdateCounter);
end;

procedure TWikiLayer.OnTimer;
begin
  if InterlockedExchange(FTileUpdateCounter, 0) > 0 then begin
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
  VNotifier: ITileRectUpdateNotifier;
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

procedure TWikiLayer.SetLayerCoordConverter(AValue: ILocalCoordConverter);
var
  VOldZoom: Byte;
  VZoom: Byte;
  VMap: IMapType;
  VNotifier: ITileRectUpdateNotifier;
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
  ACancelNotifier: IOperationNotifier
);
var
  VLocalConverter: ILocalCoordConverter;
  VList: IInterfaceList;
  VMapPixelRect: TDoubleRect;
begin
  inherited;
  VLocalConverter := LayerCoordConverter;
  if VLocalConverter <> nil then begin
    VMapPixelRect := VLocalConverter.GetRectInMapPixelFloat;
    FLinesClipRect.Left := VMapPixelRect.Left - 10;
    FLinesClipRect.Top := VMapPixelRect.Top - 10;
    FLinesClipRect.Right := VMapPixelRect.Right + 10;
    FLinesClipRect.Bottom := VMapPixelRect.Bottom + 10;
    ElementsClear;
    FProjectedCache.Clear;
    PrepareWikiElements(AOperationID, ACancelNotifier, VLocalConverter);
    VList := TInterfaceList.Create;
    // copy all elements into
    FAllElements.CopyMapElementsToList(TRUE, TRUE, VList);
    // work
    if VList.Count > 0 then begin
      ProcessDraw(AOperationID, ACancelNotifier, VList, VLocalConverter);
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

procedure TWikiLayer.GetBitmapRect(
  AOperationID: Integer;
  ACancelNotifier: IOperationNotifier;
  AElments: IInterfaceList;
  ATargetBmp: TCustomBitmap32;
  ALocalConverter: ILocalCoordConverter; AColorMain, AColorBG,
  APointColor: TColor32);
var
  i: Integer;
  VItem: IVectorDataItemSimple;
  VZoom: Byte;
  VGeoConvert: ICoordConverter;
  VMapPixelRect: TDoubleRect;
  VLLRect: TDoubleRect;
  VMarkLonLatRect: TDoubleRect;
begin
  VGeoConvert := ALocalConverter.GetGeoConverter;
  VZoom := ALocalConverter.GetZoom;
  VMapPixelRect := ALocalConverter.GetRectInMapPixelFloat;
  VGeoConvert.CheckPixelRectFloat(VMapPixelRect, VZoom);
  VLLRect := VGeoConvert.PixelRectFloat2LonLatRect(VMapPixelRect, VZoom);

  for i := 0 to AElments.Count - 1 do begin
    VItem := IVectorDataItemSimple(AElments[i]);
    VMarkLonLatRect := VItem.LLRect;
    if(
      (VLLRect.Right >= VMarkLonLatRect.Left)and
      (VLLRect.Left <= VMarkLonLatRect.Right)and
      (VLLRect.Bottom <= VMarkLonLatRect.Top)and
      (VLLRect.Top >= VMarkLonLatRect.Bottom))
    then begin
      DrawWikiElement(ATargetBmp, AColorMain, AColorBG, APointColor, VItem, ALocalConverter);
      if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
        Break;
      end;
    end;
  end;
end;

function TWikiLayer.GetElementsListByMap(const AMapType: TMapType): IInterfaceList;
begin
  if Assigned(AMapType.Zmp.MapAttachmentsInfo) then
    Result := FAllElements.GetMapElementsWithGUID(AMapType.Zmp.GUID)
  else
    Result := FAllElements.GetMapElementsWithoutGUID;
end;

function TWikiLayer.GetProjectedPath(
  AData: IVectorDataItemLine;
  AProjectionInfo: IProjectionInfo;
  ATemp: IDoublePointsAggregator = nil
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
  AData: IVectorDataItemPoly;
  AProjectionInfo: IProjectionInfo;
  ATemp: IDoublePointsAggregator = nil
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

procedure TWikiLayer.DrawPoint(
  ATargetBmp: TCustomBitmap32;
  APointColor: TColor32;
  APointColorBG: TColor32;
  AData: IVectorDataItemPoint;
  ALocalConverter: ILocalCoordConverter
);
var
  VConverter: ICoordConverter;
  VPointLL: TDoublePoint;
  VRect: TRect;
begin
  VConverter := ALocalConverter.GetGeoConverter;
  VPointLL := AData.Point;
  VConverter.CheckLonLatPos(VPointLL);
  VRect.TopLeft := ALocalConverter.LonLat2LocalPixel(VPointLL);
  VRect.BottomRight := VRect.TopLeft;
  Dec(VRect.Left, 3);
  Dec(VRect.Top, 3);
  Inc(VRect.Right, 3);
  Inc(VRect.Bottom, 3);
  ATargetBmp.FillRectS(VRect, APointColorBG);
  Inc(VRect.Left);
  Inc(VRect.Top);
  Dec(VRect.Right);
  Dec(VRect.Bottom);
  ATargetBmp.FillRectS(VRect, APointColor);
end;

procedure TWikiLayer.DrawPoly(ATargetBmp: TCustomBitmap32; AColorMain,
  AColorBG: TColor32; AData: IVectorDataItemPoly;
  ALocalConverter: ILocalCoordConverter);
var
  VLen: integer;
  i: integer;
  VPoint: TDoublePoint;
  VConverter: ICoordConverter;
  VPointOnBitmap: TDoublePoint;
  VProjectdPolygon: IProjectedPolygon;
  VLine: IProjectedPolygonLine;
  VEnum: IEnumProjectedPoint;
begin
  if AData.Line.Count > 0 then begin
    VProjectdPolygon := GetProjectedPolygon(AData, ALocalConverter.ProjectionInfo, FPointsAgregatorThread);
    if VProjectdPolygon.Count > 0 then begin
      VLine := VProjectdPolygon.Item[0];
      if VLine.Count > 1 then begin
        VConverter := ALocalConverter.GetGeoConverter;

        VLen := VLine.Count + 1;
        if Length(FFixedPointArray) < VLen then begin
          SetLength(FFixedPointArray, VLen);
        end;
        VEnum := VLine.GetEnum;

        i := 0;
        while VEnum.Next(VPoint) do begin
          VPointOnBitmap := ALocalConverter.MapPixelFloat2LocalPixelFloat(VPoint);
          FFixedPointArray[i] := FixedPoint(VPointOnBitmap.X, VPointOnBitmap.Y);
          Inc(i);
        end;

        FPolygon.Clear;
        FPolygon.AddPoints(FFixedPointArray[0], VLen);
        FPolygon.DrawEdge(ATargetBmp, AColorBG);
        FPolygon.Offset(Fixed(0.9), Fixed(0.9));
        FPolygon.DrawEdge(ATargetBmp, AColorMain);
      end;
    end;
  end;
end;

procedure TWikiLayer.DrawLine(
  ATargetBmp: TCustomBitmap32;
  AColorMain, AColorBG: TColor32;
  AData: IVectorDataItemLine;
  ALocalConverter: ILocalCoordConverter
);
var
  VLen: integer;
  i: integer;
  VPoint: TDoublePoint;
  VConverter: ICoordConverter;
  VPointOnBitmap: TDoublePoint;
  VLineIndex: Integer;
  VProjectdPath: IProjectedPath;
  VLine: IProjectedPathLine;
  VEnum: IEnumProjectedPoint;
begin
  if AData.Line.Count > 0 then begin
    VProjectdPath := GetProjectedPath(AData, ALocalConverter.ProjectionInfo, FPointsAgregatorThread);
    if VProjectdPath.Count > 0 then begin
      FPolygon.Clear;
      for VLineIndex := 0 to AData.Line.Count - 1 do begin
        VLine := VProjectdPath.Item[VLineIndex];
        if VLine.Count > 1 then begin
          VConverter := ALocalConverter.GetGeoConverter;

          VLen := VLine.Count + 1;
          if Length(FFixedPointArray) < VLen then begin
            SetLength(FFixedPointArray, VLen);
          end;
          VEnum := VLine.GetEnum;

          i := 0;
          while VEnum.Next(VPoint) do begin
            VPointOnBitmap := ALocalConverter.MapPixelFloat2LocalPixelFloat(VPoint);
            FFixedPointArray[i] := FixedPoint(VPointOnBitmap.X, VPointOnBitmap.Y);
            Inc(i);
          end;

          FPolygon.AddPoints(FFixedPointArray[0], VLen);
          FPolygon.NewLine;
        end;
      end;
      FPolygon.DrawEdge(ATargetBmp, AColorBG);
      FPolygon.Offset(Fixed(0.9), Fixed(0.9));
      FPolygon.DrawEdge(ATargetBmp, AColorMain);
    end;
  end;
end;

procedure TWikiLayer.DrawWikiElement(
  ATargetBmp: TCustomBitmap32;
  AColorMain: TColor32;
  AColorBG: TColor32;
  APointColor: TColor32;
  AData: IVectorDataItemSimple;
  ALocalConverter: ILocalCoordConverter
);
var
  VItemPoint: IVectorDataItemPoint;
  VItemLine: IVectorDataItemLine;
  VItemPoly: IVectorDataItemPoly;
begin
  if Supports(AData, IVectorDataItemPoint, VItemPoint) then begin
    DrawPoint(ATargetBmp, APointColor, AColorBG, VItemPoint, ALocalConverter);
  end else if Supports(AData, IVectorDataItemLine, VItemLine) then begin
    DrawLine(ATargetBmp, AColorMain, AColorBG, VItemLine, ALocalConverter);
  end else if Supports(AData, IVectorDataItemPoly, VItemPoly) then begin
    DrawPoly(ATargetBmp, AColorMain, AColorBG, VItemPoly, ALocalConverter);
  end;
end;

procedure TWikiLayer.ElementsClear;
begin
  FAllElements.ClearMapElements;
end;

procedure TWikiLayer.MouseOnReg(xy: TPoint; out AItem: IVectorDataItemSimple; out AItemS: Double);
var
  VElements: IInterfaceList;
begin
  AItem := nil;
  AItemS := 0;
  // single call on ALL elements
  VElements:=TInterfaceList.Create;
  try
    FAllElements.CopyMapElementsToList(TRUE, TRUE, VElements);
    MouseOnElements(VElements, xy, AItem, AItemS);
  finally
    VElements:=nil;
  end;
end;

function TWikiLayer.MouseOnElements(const ACopiedElements: IInterfaceList;
                                    const xy: TPoint;
                                    var AItem: IVectorDataItemSimple;
                                    var AItemS: Double): Boolean;
var
  VRect: TRect;
  VLocalConverter: ILocalCoordConverter;
  VConverter: ICoordConverter;
  VZoom: Byte;
  VVisualConverter: ILocalCoordConverter;
  VMapRect: TDoubleRect;
  VLonLatRect: TDoubleRect;
  VPixelPos: TDoublePoint;
  i: integer;
  VItem: IVectorDataItemSimple;
  VMarkLonLatRect: TDoubleRect; // TODO: can remove it?
  VProjectdPath: IProjectedPath;
  VItemLine: IVectorDataItemLine;
  VItemPoly: IVectorDataItemPoly;
  VProjectdPolygon: IProjectedPolygon;
  VSquare:Double;
begin
  // return TRUE if breaks loop
  Result := FALSE;

  // if has elements
  if ACopiedElements.Count > 0 then begin
    // TODO: execute this only once (for multiple calls)
    VRect.Left := xy.X - 3;
    VRect.Top := xy.Y - 3;
    VRect.Right := xy.X + 3;
    VRect.Bottom := xy.Y + 3;
    VLocalConverter := LayerCoordConverter;
    VConverter := VLocalConverter.GetGeoConverter;
    VZoom := VLocalConverter.GetZoom;
    VVisualConverter := ViewCoordConverter;
    VMapRect := VVisualConverter.LocalRect2MapRectFloat(VRect);
    VConverter.CheckPixelRectFloat(VMapRect, VZoom);
    VLonLatRect := VConverter.PixelRectFloat2LonLatRect(VMapRect, VZoom);
    VPixelPos := VVisualConverter.LocalPixel2MapPixelFloat(xy);

    // check element
    for i := 0 to ACopiedElements.Count - 1 do begin
      VItem := IVectorDataItemSimple(Pointer(ACopiedElements[i]));
      VMarkLonLatRect := VItem.LLRect;
      if((VLonLatRect.Right>VMarkLonLatRect.Left)and(VLonLatRect.Left<VMarkLonLatRect.Right)and
      (VLonLatRect.Bottom<VMarkLonLatRect.Top)and(VLonLatRect.Top>VMarkLonLatRect.Bottom))then begin
        if Supports(VItem, IVectorDataItemPoint) then begin
          AItem := VItem;
          AItemS := 0;
          Result:=TRUE;
          Exit;
        end else if Supports(VItem, IVectorDataItemLine, VItemLine) then begin
          VProjectdPath := GetProjectedPath(VItemLine, VLocalConverter.ProjectionInfo, FPointsAgregatorGUI);
          if VProjectdPath.IsPointOnPath(VPixelPos, 2) then begin
            AItem := VItem;
            AItemS := 0;
            Result:=TRUE;
            Exit;
          end;
        end else if Supports(VItem, IVectorDataItemPoly, VItemPoly) then begin
          VProjectdPolygon := GetProjectedPolygon(VItemPoly, VLocalConverter.ProjectionInfo, FPointsAgregatorGUI);
          if VProjectdPolygon.IsPointInPolygon(VPixelPos) then begin
            VSquare := VProjectdPolygon.CalcArea;
            if (AItem = nil) or (VSquare<AItemS) then begin
              AItem := VItem;
              AItemS := VSquare;
              Result:=TRUE;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TWikiLayer.MouseOnReg(xy: TPoint; out AItem: IVectorDataItemSimple);
var
  VItemS: Double;
begin
  MouseOnReg(xy, AItem, VItemS);
end;

procedure TWikiLayer.MouseOnRegWithGUID(const xy: TPoint;
                                        out AItem: IVectorDataItemSimple;
                                        out AItemS: Double;
                                        out ALayerGUID: TGUID);
var
  VElements: IInterfaceList;
  VEnumGUID: IEnumGUID;
  VResult: Boolean;
  celtFetched: Cardinal;
begin
  AItem := nil;
  ALayerGUID := GUID_NULL;
  AItemS := 0;

  VElements:=TInterfaceList.Create;
  try
    // loop through guided
    VEnumGUID := FAllElements.GetGUIDEnum;
    try
      if Assigned(VEnumGUID) then
      while (S_OK=VEnumGUID.Next(1, ALayerGUID, celtFetched)) do begin
        VElements.Clear;
        CopyMapElements(FAllElements.GetMapElementsWithGUID(ALayerGUID), VElements);
        VResult:=MouseOnElements(VElements, xy, AItem, AItemS);
        if VResult then
          Exit;
      end;
    finally
      VEnumGUID:=nil;
    end;

    // no guid at all
    ALayerGUID:=GUID_NULL;

    // without guid
    VElements.Clear;
    CopyMapElements(FAllElements.GetMapElementsWithoutGUID, VElements);
    MouseOnElements(VElements, xy, AItem, AItemS);
  finally
    VElements:=nil;
  end;
end;

end.
