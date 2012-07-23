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

unit u_MapMainLayer;

interface

uses
  Windows,
  SysUtils,
  GR32,
  GR32_Image,
  i_Notifier,
  i_Listener,
  i_CoordConverter,
  i_LocalCoordConverterFactorySimpe,
  i_InternalPerformanceCounter,
  i_NotifierOperation,
  i_LayerBitmapClearStrategy,
  i_LocalCoordConverter,
  i_MapTypes,
  i_ActiveMapsConfig,
  i_ViewPortState,
  i_SimpleFlag,
  i_ImageResamplerConfig,
  i_MainMapLayerConfig,
  i_BitmapPostProcessingConfig,
  i_BitmapLayerProvider,
  i_TileError,
  u_MapType,
  u_MapLayerWithThreadDraw;

type
  TMapMainLayer = class(TMapLayerTiledWithThreadDraw)
  private
    FErrorLogger: ITileErrorLogger;
    FMainMapChangeable: IMapTypeChangeable;
    FLayersSetChangeable: IMapTypeSetChangeable;
    FPostProcessingConfig: IBitmapPostProcessingConfig;
    FConfig: IMainMapLayerConfig;
    FTileChangeListener: IListener;

    FBitmapProvider: IBitmapLayerProvider;
    FBitmapProviderCS: IReadWriteSync;

    FMainMap: IMapType;
    FMainMapCS: IReadWriteSync;
    FLayersSet: IMapTypeSet;
    FLayersSetCS: IReadWriteSync;

    FTileUpdateFlag: ISimpleFlag;

    procedure CreateBitmapProvider;
    procedure OnTileChange;
    procedure OnTimer;

    procedure OnMainMapChange;
    procedure OnLayerSetChange;
    procedure OnConfigChange;
    function GetLayersSet: IMapTypeSet;
    function GetMainMap: IMapType;
    procedure SetLayersSet(const Value: IMapTypeSet);
    procedure SetMainMap(const Value: IMapType);

    property MainMap: IMapType read GetMainMap write SetMainMap;
    property LayersSet: IMapTypeSet read GetLayersSet write SetLayersSet;
  protected
    procedure DrawBitmap(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation
    ); override;
    procedure SetLayerCoordConverter(const AValue: ILocalCoordConverter); override;
    procedure DoRedraw; override;
    procedure StartThreads; override;
    procedure SendTerminateToThreads; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      AParentMap: TImage32;
      const AViewPortState: IViewPortState;
      const AResamplerConfig: IImageResamplerConfig;
      const AConverterFactory: ILocalCoordConverterFactorySimpe;
      const AClearStrategyFactory: ILayerBitmapClearStrategyFactory;
      const AMainMap: IMapTypeChangeable;
      const ALayersSet: IMapTypeSetChangeable;
      const APostProcessingConfig: IBitmapPostProcessingConfig;
      const AConfig: IMainMapLayerConfig;
      const AErrorLogger: ITileErrorLogger;
      const ATimerNoifier: INotifier
    );
    destructor Destroy; override;
  end;

implementation

uses
  ActiveX,
  GR32_Resamplers,
  u_Synchronizer,
  t_GeoTypes,
  i_Bitmap32Static,
  i_TileIterator,
  i_UseTilePrevZoomConfig,
  i_NotifierTileRectUpdate,
  u_ListenerByEvent,
  u_GeoFun,
  u_MapTypeListStatic,
  u_SimpleFlagWithInterlock,
  u_BitmapLayerProviderForViewMaps,
  u_TileIteratorSpiralByRect;

{ TMapMainLayer }

constructor TMapMainLayer.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const AViewPortState: IViewPortState;
  const AResamplerConfig: IImageResamplerConfig;
  const AConverterFactory: ILocalCoordConverterFactorySimpe;
  const AClearStrategyFactory: ILayerBitmapClearStrategyFactory;
  const AMainMap: IMapTypeChangeable;
  const ALayersSet: IMapTypeSetChangeable;
  const APostProcessingConfig: IBitmapPostProcessingConfig;
  const AConfig: IMainMapLayerConfig;
  const AErrorLogger: ITileErrorLogger;
  const ATimerNoifier: INotifier
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
  FMainMapChangeable := AMainMap;
  FLayersSetChangeable := ALayersSet;
  FErrorLogger := AErrorLogger;
  FPostProcessingConfig := APostProcessingConfig;
  FConfig := AConfig;

  FMainMapCS := MakeSyncRW_Var(Self);
  FLayersSetCS := MakeSyncRW_Var(Self);
  FBitmapProviderCS := MakeSyncRW_Var(Self);

  FTileUpdateFlag := TSimpleFlagWithInterlock.Create;
  FTileChangeListener := TNotifyNoMmgEventListener.Create(Self.OnTileChange);

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnMainMapChange),
    FMainMapChangeable.ChangeNotifier
  );

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnLayerSetChange),
    FLayersSetChangeable.ChangeNotifier
  );

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FConfig.GetChangeNotifier
  );

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FPostProcessingConfig.GetChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnTimer),
    ATimerNoifier
  );
end;

procedure TMapMainLayer.CreateBitmapProvider;
var
  VMainMap: IMapType;
  VLayersSet: IMapTypeSet;
  VPostProcessingConfig: IBitmapPostProcessingConfigStatic;

  VLayers: array of IMapType;
  VLayersList: IMapTypeListStatic;
  VProvider: IBitmapLayerProvider;
  VItem: IMapType;
  VEnum: IEnumGUID;
  VGUID: TGUID;
  VCnt: Cardinal;
  i: Integer;
  VLayersCount: Integer;
  VZOrder: Integer;
  VIndex: Integer;
  VUsePrevConfig: IUseTilePrevZoomTileConfigStatic;
begin
  VMainMap := MainMap;
  VLayersSet := LayersSet;
  VUsePrevConfig := FConfig.UseTilePrevZoomConfig.GetStatic;
  VPostProcessingConfig := FPostProcessingConfig.GetStatic;

  VLayersCount := 0;
  try
    if VLayersSet <> nil then begin
      VEnum := VLayersSet.GetIterator;
      while VEnum.Next(1, VGUID, VCnt) = S_OK do begin
        VItem := VLayersSet.GetMapTypeByGUID(VGUID);
        if VItem.MapType.IsBitmapTiles then begin
          VZOrder := VItem.MapType.LayerDrawConfig.LayerZOrder;
          Inc(VLayersCount);
          SetLength(VLayers, VLayersCount);
          VIndex := 0;
          if VLayersCount > 1 then begin
            for i := VLayersCount - 2 downto 0 do begin
              if VLayers[i].MapType.LayerDrawConfig.LayerZOrder > VZOrder then begin
                VLayers[i + 1] := VLayers[i];
              end else begin
                VIndex := i + 1;
                Break;
              end;
            end;
          end;
          VLayers[VIndex] := VItem;
        end;
      end;
    end;
    VLayersList := TMapTypeListStatic.Create(VLayers);
  finally
    for i := 0 to Length(VLayers) - 1 do begin
      VLayers[i] := nil;
    end;
    VLayers := nil;
  end;
  VProvider :=
    TBitmapLayerProviderForViewMaps.Create(
      VMainMap,
      VLayersList,
      VUsePrevConfig.UsePrevZoomAtMap,
      VUsePrevConfig.UsePrevZoomAtLayer,
      True,
      VPostProcessingConfig,
      FErrorLogger
    );
  FBitmapProviderCS.BeginWrite;
  try
    FBitmapProvider := VProvider;
  finally
    FBitmapProviderCS.EndWrite;
  end;
end;

destructor TMapMainLayer.Destroy;
begin
  FMainMapCS := nil;
  FLayersSetCS := nil;
  inherited;
end;

procedure TMapMainLayer.DoRedraw;
begin
  CreateBitmapProvider;
  inherited;
end;

procedure TMapMainLayer.DrawBitmap(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation
);
var
  VBitmapTile: IBitmap32Static;

  VGeoConvert: ICoordConverter;
  VBitmapConverter: ILocalCoordConverter;
  VTileIterator: ITileIterator;
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
  VProvider: IBitmapLayerProvider;
  VTileConverter: ILocalCoordConverter;
begin
  VBitmapConverter := LayerCoordConverter;
  FBitmapProviderCS.BeginRead;
  try
    VProvider := FBitmapProvider;
  finally
    FBitmapProviderCS.EndRead;
  end;

  if (VBitmapConverter = nil) or (VProvider = nil) then begin
    Exit;
  end;

  VGeoConvert := VBitmapConverter.GetGeoConverter;
  VZoom := VBitmapConverter.GetZoom;

  VBitmapOnMapPixelRect := VBitmapConverter.GetRectInMapPixel;
  VGeoConvert.CheckPixelRect(VBitmapOnMapPixelRect, VZoom);

  VTileSourceRect := VGeoConvert.PixelRect2TileRect(VBitmapOnMapPixelRect, VZoom);
  VTileIterator := TTileIteratorSpiralByRect.Create(VTileSourceRect);

  if not ACancelNotifier.IsOperationCanceled(AOperationID) then begin
    while VTileIterator.Next(VTile) do begin
      if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
        break;
      end;
      VTileConverter := ConverterFactory.CreateForTile(VTile, VZoom, VGeoConvert);

      VCurrTilePixelRect := VTileConverter.GetRectInMapPixel;
      VCurrTileOnBitmapRect := VBitmapConverter.MapRect2LocalRect(VCurrTilePixelRect);

      VBitmapTile :=
        VProvider.GetBitmapRect(
          AOperationID,
          ACancelNotifier,
          VTileConverter
        );
      if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
        break;
      end;
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

function TMapMainLayer.GetLayersSet: IMapTypeSet;
begin
  FLayersSetCS.BeginRead;
  try
    Result := FLayersSet;
  finally
    FLayersSetCS.EndRead;
  end;
end;

function TMapMainLayer.GetMainMap: IMapType;
begin
  FMainMapCS.BeginRead;
  try
    Result := FMainMap;
  finally
    FMainMapCS.EndRead;
  end;
end;

procedure TMapMainLayer.OnConfigChange;
begin
  ViewUpdateLock;
  try
    SetNeedRedraw;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMapMainLayer.OnLayerSetChange;
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
    VNewLayersSet := FLayersSetChangeable.GetStatic;

    FLayersSetCS.BeginWrite;
    try
      VOldLayersSet := FLayersSet;
      FLayersSet := VNewLayersSet;
    finally
      FLayersSetCS.EndWrite;
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
                VTileRect :=
                  RectFromDoubleRect(
                    VMapConverter.LonLatRect2TileRectFloat(VLonLatRect, VZoom),
                    rrToTopLeft
                  );
                VNotifier.Add(FTileChangeListener, VTileRect);
              end;
            end;
          end;
        end;
      end;
    end;
    SetNeedRedraw;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMapMainLayer.OnMainMapChange;
var
  VOldMainMap: IMapType;
  VNewMainMap: IMapType;
  VZoom: Byte;
  VLocalConverter: ILocalCoordConverter;
  VNotifier: INotifierTileRectUpdate;
  VMapPixelRect: TDoubleRect;
  VLonLatRect: TDoubleRect;
  VTileRect: TRect;
begin
  ViewUpdateLock;
  try
    VNewMainMap := FMainMapChangeable.GetStatic;

    FMainMapCS.BeginWrite;
    try
      VOldMainMap := FMainMap;
      FMainMap := VNewMainMap;
    finally
      FMainMapCS.EndWrite;
    end;

    if VOldMainMap <> VNewMainMap then begin
      VLocalConverter := LayerCoordConverter;
      VZoom := VLocalConverter.GetZoom;
      if VOldMainMap <> nil then begin
        VNotifier := VOldMainMap.MapType.NotifierByZoom[VZoom];
        if VNotifier <> nil then begin
          VNotifier.Remove(FTileChangeListener);
        end;
      end;
      if VNewMainMap <> nil then begin
        VNotifier := VNewMainMap.MapType.NotifierByZoom[VZoom];
        if VNotifier <> nil then begin
          VMapPixelRect := VLocalConverter.GetRectInMapPixelFloat;
          VLocalConverter.GetGeoConverter.CheckPixelRectFloat(VMapPixelRect, VZoom);
          VLonLatRect := VLocalConverter.GetGeoConverter.PixelRectFloat2LonLatRect(VMapPixelRect, VZoom);
          VNewMainMap.MapType.GeoConvert.CheckLonLatRect(VLonLatRect);
          VTileRect :=
            RectFromDoubleRect(
              VNewMainMap.MapType.GeoConvert.LonLatRect2TileRectFloat(VLonLatRect, VZoom),
              rrToTopLeft
            );
          VNotifier.Add(FTileChangeListener, VTileRect);
        end;
      end;
    end;
    SetNeedRedraw;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMapMainLayer.OnTileChange;
begin
  FTileUpdateFlag.SetFlag;
end;

procedure TMapMainLayer.OnTimer;
begin
  if FTileUpdateFlag.CheckFlagAndReset then begin
    DelicateRedraw;
  end;
end;

procedure TMapMainLayer.SendTerminateToThreads;
var
  VZoom: Byte;
  VMap: IMapType;
  VNotifier: INotifierTileRectUpdate;
  VLayersSet: IMapTypeSet;
  VEnum: IEnumGUID;
  VGUID: TGUID;
  cnt: Cardinal;
  VLocalConverter: ILocalCoordConverter;
begin
  inherited;
  VLocalConverter := LayerCoordConverter;
  if VLocalConverter <> nil then begin
    VZoom := VLocalConverter.GetZoom;

    VMap := MainMap;

    if VMap <> nil then begin
      VNotifier := VMap.MapType.NotifierByZoom[VZoom];
      if VNotifier <> nil then begin
        VNotifier.Remove(FTileChangeListener);
      end;
    end;

    VLayersSet := LayersSet;

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

procedure TMapMainLayer.SetLayerCoordConverter(
  const AValue: ILocalCoordConverter
);
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
  VLocalConverter: ILocalCoordConverter;
begin
  VOldZoom := 255;
  VLocalConverter := LayerCoordConverter;
  if VLocalConverter <> nil then begin
    VOldZoom := VLocalConverter.GetZoom;
  end;
  inherited;
  VZoom := AValue.GetZoom;
  if VZoom <> VOldZoom then begin
    if VOldZoom <> 255 then begin
      VMap := MainMap;

      if VMap <> nil then begin
        VNotifier := VMap.MapType.NotifierByZoom[VOldZoom];
        if VNotifier <> nil then begin
          VNotifier.Remove(FTileChangeListener);
        end;
      end;

      VLayersSet := LayersSet;

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

  VMap := MainMap;

  if VMap <> nil then begin
    VNotifier := VMap.MapType.NotifierByZoom[VZoom];
    if VNotifier <> nil then begin
      VMap.MapType.GeoConvert.CheckLonLatRect(VLonLatRect);
      VTileRect :=
        RectFromDoubleRect(
          VMap.MapType.GeoConvert.LonLatRect2TileRectFloat(VLonLatRect, VZoom),
          rrToTopLeft
        );
      VNotifier.Add(FTileChangeListener, VTileRect);
    end;
  end;

  VLayersSet := LayersSet;

  if VLayersSet <> nil then begin
    VEnum := VLayersSet.GetIterator;
    while VEnum.Next(1, VGUID, cnt) = S_OK do begin
      VMap := VLayersSet.GetMapTypeByGUID(VGUID);
      if VMap <> nil then begin
        VNotifier := VMap.MapType.NotifierByZoom[VZoom];
        if VNotifier <> nil then begin
          VMap.MapType.GeoConvert.CheckLonLatRect(VLonLatRect);
          VTileRect :=
            RectFromDoubleRect(
              VMap.MapType.GeoConvert.LonLatRect2TileRectFloat(VLonLatRect, VZoom),
              rrToTopLeft
            );
          VNotifier.Add(FTileChangeListener, VTileRect);
        end;
      end;
    end;
  end;
end;

procedure TMapMainLayer.SetLayersSet(const Value: IMapTypeSet);
begin
  FLayersSetCS.BeginWrite;
  try
    FLayersSet := Value;
  finally
    FLayersSetCS.EndWrite;
  end;
end;

procedure TMapMainLayer.SetMainMap(const Value: IMapType);
begin
  FMainMapCS.BeginWrite;
  try
    FMainMap := Value;
  finally
    FMainMapCS.EndWrite;
  end;
end;

procedure TMapMainLayer.StartThreads;
begin
  inherited;
  OnConfigChange;
  OnMainMapChange;
  OnLayerSetChange;
  Show;
end;

end.
