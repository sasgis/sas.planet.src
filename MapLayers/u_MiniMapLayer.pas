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

unit u_MiniMapLayer;

interface

uses
  Windows,
  SysUtils,
  GR32,
  GR32_Image,
  GR32_Layers,
  i_Notifier,
  t_GeoTypes,
  i_NotifierOperation,
  i_MapTypes,
  i_ActiveMapsConfig,
  i_BackgroundTask,
  i_InternalPerformanceCounter,
  i_LayerBitmapClearStrategy,
  i_CoordConverter,
  i_LocalCoordConverter,
  i_LocalCoordConverterFactorySimpe,
  i_GlobalViewMainConfig,
  i_ViewPortState,
  i_SimpleFlag,
  i_TileError,
  i_BitmapLayerProvider,
  i_MapTypeListChangeable,
  i_MiniMapLayerConfig,
  i_BitmapPostProcessingConfig,
  i_UseTilePrevZoomConfig,
  u_MapType,
  u_WindowLayerWithPos;

type
  TMiniMapLayer = class(TWindowLayerFixedSizeWithBitmap)
  private
    FConfig: IMiniMapLayerConfig;
    FViewConfig: IGlobalViewMainConfig;
    FErrorLogger: ITileErrorLogger;
    FMainMap: IMapTypeChangeable;
    FLayerList: IMapTypeListChangeable;
    FParentMap: TImage32;
    FConverterFactory: ILocalCoordConverterFactorySimpe;
    FPostProcessingConfig: IBitmapPostProcessingConfig;
    FViewPortState: IViewPortState;

    FBitmapProvider: IBitmapLayerProvider;
    FBitmapProviderCS: IReadWriteSync;

    FDrawTask: IBackgroundTask;
    FUpdateFlag: ISimpleFlag;
    FBgDrawCounter: IInternalPerformanceCounter;
    FClearStrategy: ILayerBitmapClearStrategy;
    FClearStrategyFactory: ILayerBitmapClearStrategyFactory;
    procedure CreateBitmapProvider;

    function GetActualZoom(const AVisualCoordConverter: ILocalCoordConverter): Byte;

    procedure OnConfigChange;

    procedure OnMainMapChange;
    procedure OnLayerSetChange;
    procedure OnDrawBitmap(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation
    );
    procedure OnTimer;

    procedure SetBitmapChanged;
    procedure DrawBitmap(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation
    );
    procedure ClearLayerBitmap;
    property ConverterFactory: ILocalCoordConverterFactorySimpe read FConverterFactory;
  protected
    function GetMapLayerLocationRect(const ANewVisualCoordConverter: ILocalCoordConverter): TFloatRect; override;
    procedure DoRedraw; override;
    function GetLayerSizeForView(
      const ANewVisualCoordConverter: ILocalCoordConverter
    ): TPoint; override;
    function GetLayerCoordConverterByViewConverter(
      const ANewVisualCoordConverter: ILocalCoordConverter
    ): ILocalCoordConverter; override;
    procedure SetLayerCoordConverter(const AValue: ILocalCoordConverter); override;
    procedure SetNeedRedraw; override;
    procedure SetNeedUpdateLayerSize; override;
    procedure StartThreads; override;
    procedure SendTerminateToThreads; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      AParentMap: TImage32;
      const AViewPortState: IViewPortState;
      const ACoordConverterFactory: ILocalCoordConverterFactorySimpe;
      const AClearStrategyFactory: ILayerBitmapClearStrategyFactory;
      const AConfig: IMiniMapLayerConfig;
      const AMainMap: IMapTypeChangeable;
      const ALayerList: IMapTypeListChangeable;
      const AViewConfig: IGlobalViewMainConfig;
      const APostProcessingConfig: IBitmapPostProcessingConfig;
      const AErrorLogger: ITileErrorLogger;
      const ATimerNoifier: INotifier
    );
    destructor Destroy; override;
  end;

implementation

uses
  ActiveX,
  Types,
  GR32_Resamplers,
  u_Synchronizer,
  u_GeoFun,
  u_SimpleFlagWithInterlock,
  i_TileIterator,
  i_Bitmap32Static,
  u_ListenerByEvent,
  u_MapTypeListStatic,
  u_BitmapLayerProviderForViewMaps,
  u_BackgroundTask,
  u_TileIteratorSpiralByRect;

{ TMapMainLayer }

constructor TMiniMapLayer.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const AViewPortState: IViewPortState;
  const ACoordConverterFactory: ILocalCoordConverterFactorySimpe;
  const AClearStrategyFactory: ILayerBitmapClearStrategyFactory;
  const AConfig: IMiniMapLayerConfig;
  const AMainMap: IMapTypeChangeable;
  const ALayerList: IMapTypeListChangeable;
  const AViewConfig: IGlobalViewMainConfig;
  const APostProcessingConfig: IBitmapPostProcessingConfig;
  const AErrorLogger: ITileErrorLogger;
  const ATimerNoifier: INotifier
);
begin
  inherited Create(
    APerfList,
    AAppStartedNotifier,
    AAppClosingNotifier,
    AParentMap,
    AViewPortState
  );
  FBgDrawCounter := PerfList.CreateAndAddNewCounter('BgDraw');
  FViewPortState := AViewPortState;
  FConfig := AConfig;
  FMainMap := AMainMap;
  FLayerList := ALayerList;
  FErrorLogger := AErrorLogger;
  FClearStrategyFactory := AClearStrategyFactory;
  FViewConfig := AViewConfig;
  FConverterFactory := ACoordConverterFactory;
  FPostProcessingConfig := APostProcessingConfig;
  FParentMap := AParentMap;

  FBitmapProviderCS := MakeSyncRW_Var(Self);

  Layer.Bitmap.BeginUpdate;

  FDrawTask :=
    TBackgroundTask.Create(
      AAppClosingNotifier,
      OnDrawBitmap,
      FConfig.ThreadConfig,
      Self.ClassName
    );
  FUpdateFlag := TSimpleFlagWithInterlock.Create;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnMainMapChange),
    FMainMap.ChangeNotifier
  );

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnLayerSetChange),
    FLayerList.ChangeNotifier
  );

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FConfig.GetChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FViewConfig.GetChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    APostProcessingConfig.GetChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnTimer),
    ATimerNoifier
  );
end;

procedure TMiniMapLayer.CreateBitmapProvider;
var
  VMainMap: IMapType;
  VPostProcessingConfig: IBitmapPostProcessing;

  VLayersList: IMapTypeListStatic;
  VProvider: IBitmapLayerProvider;
  VUsePrevConfig: IUseTilePrevZoomTileConfigStatic;
begin
  VMainMap := FMainMap.GetStatic;
  VLayersList := FLayerList.List;
  VUsePrevConfig := FConfig.UseTilePrevZoomConfig.GetStatic;
  VPostProcessingConfig := FPostProcessingConfig.GetStatic;

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

destructor TMiniMapLayer.Destroy;
begin
  FConverterFactory := nil;
  FDrawTask := nil;
  inherited;
end;

procedure TMiniMapLayer.ClearLayerBitmap;
begin
  if Visible then begin
    Layer.Bitmap.Lock;
    try
      if FClearStrategy <> nil then begin
        FClearStrategy.Clear(Layer.Bitmap);
        FClearStrategy := nil;
      end;
    finally
      Layer.Bitmap.UnLock;
    end;
  end;
end;

function TMiniMapLayer.GetLayerCoordConverterByViewConverter(
  const ANewVisualCoordConverter: ILocalCoordConverter
): ILocalCoordConverter;
var
  VVisualMapCenter: TDoublePoint;
  VZoom: Byte;
  VSourceZoom: Byte;
  VConverter: ICoordConverter;
  VVisualMapCenterInRelative: TDoublePoint;
  VVisualMapCenterInLayerMap: TDoublePoint;
  VLocalTopLeftAtMap: TPoint;
  VLocalTopLeftAtMapFloat: TDoublePoint;
  VLayerSize: TPoint;
begin
  VVisualMapCenter := ANewVisualCoordConverter.GetCenterMapPixelFloat;
  VSourceZoom := ANewVisualCoordConverter.GetZoom;
  VConverter := ANewVisualCoordConverter.GetGeoConverter;
  VConverter.CheckPixelPosFloatStrict(VVisualMapCenter, VSourceZoom, True);
  VVisualMapCenterInRelative := VConverter.PixelPosFloat2Relative(VVisualMapCenter, VSourceZoom);
  VZoom := GetActualZoom(ANewVisualCoordConverter);
  VVisualMapCenterInLayerMap := VConverter.Relative2PixelPosFloat(VVisualMapCenterInRelative, VZoom);
  VLayerSize := Point(Layer.Bitmap.Width, Layer.Bitmap.Height);
  VLocalTopLeftAtMapFloat :=
    DoublePoint(
      VVisualMapCenterInLayerMap.X - VLayerSize.X / 2,
      VVisualMapCenterInLayerMap.Y - VLayerSize.Y / 2
    );
  VLocalTopLeftAtMap := PointFromDoublePoint(VLocalTopLeftAtMapFloat, prToTopLeft);

  Result := ConverterFactory.CreateConverterNoScale(
    Rect(0, 0, VLayerSize.X, VLayerSize.Y),
    VZoom,
    VConverter,
    VLocalTopLeftAtMap
  );
end;

function TMiniMapLayer.GetLayerSizeForView(
  const ANewVisualCoordConverter: ILocalCoordConverter
): TPoint;
var
  VWidth: Integer;
begin
  VWidth := FConfig.Width;
  Result := Point(VWidth, VWidth);
end;

procedure TMiniMapLayer.DoRedraw;
begin
  FDrawTask.StopExecute;
  CreateBitmapProvider;
  inherited;
  ClearLayerBitmap;
  if Visible then begin
    FDrawTask.StartExecute;
  end;
end;

procedure TMiniMapLayer.DrawBitmap(
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

function TMiniMapLayer.GetActualZoom(
  const AVisualCoordConverter: ILocalCoordConverter
): Byte;
var
  VZoom: Byte;
  VGeoConvert: ICoordConverter;
  VZoomDelta: Integer;
begin
  VZoom := AVisualCoordConverter.GetZoom;
  VGeoConvert := AVisualCoordConverter.GetGeoConverter;
  VZoomDelta := FConfig.ZoomDelta;
  if VZoomDelta = 0 then begin
    Result := VZoom;
  end else if VZoomDelta > 0 then begin
    if VZoom > VZoomDelta then begin
      Result := VZoom - VZoomDelta;
    end else begin
      Result := 0;
    end;
  end else begin
    Result := VZoom - VZoomDelta;
    VGeoConvert.CheckZoom(Result);
  end;
end;

function TMiniMapLayer.GetMapLayerLocationRect(const ANewVisualCoordConverter: ILocalCoordConverter): TFloatRect;
var
  VSize: TPoint;
  VViewSize: TPoint;
begin
  if ANewVisualCoordConverter <> nil then begin
    VSize := Point(Layer.Bitmap.Width, Layer.Bitmap.Height);
    VViewSize := ANewVisualCoordConverter.GetLocalRectSize;
    Result.Right := VViewSize.X;
    Result.Bottom := VViewSize.Y - FConfig.BottomMargin;
    Result.Left := Result.Right - VSize.X;
    Result.Top := Result.Bottom - VSize.Y;
  end else begin
    Result.Left := 0;
    Result.Bottom := 0;
    Result.Right := 0;
    Result.Top := 0;
  end;
end;

procedure TMiniMapLayer.OnConfigChange;
begin
  ViewUpdateLock;
  try
    FConfig.LockRead;
    try
      Layer.Bitmap.Lock;
      try
        Layer.Bitmap.MasterAlpha := FConfig.MasterAlpha;
      finally
        Layer.Bitmap.Unlock;
      end;
      SetVisible(FConfig.Visible);
      PosChange(ViewCoordConverter);
      SetNeedRedraw;
      SetNeedUpdateLayerSize;
    finally
      FConfig.UnlockRead;
    end;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMiniMapLayer.OnDrawBitmap(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation
);
var
  VCounterContext: TInternalPerformanceCounterContext;
begin
  VCounterContext := FBgDrawCounter.StartOperation;
  try
    DrawBitmap(AOperationID, ACancelNotifier);
  finally
    FBgDrawCounter.FinishOperation(VCounterContext);
  end;
end;

procedure TMiniMapLayer.OnLayerSetChange;
begin
  ViewUpdateLock;
  try
    SetNeedRedraw;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMiniMapLayer.OnMainMapChange;
begin
  ViewUpdateLock;
  try
    SetNeedRedraw;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMiniMapLayer.OnTimer;
begin
  if FUpdateFlag.CheckFlagAndReset then begin
    Layer.Changed;
  end;
end;

procedure TMiniMapLayer.SendTerminateToThreads;
begin
  inherited;
  FDrawTask.Terminate;
end;

procedure TMiniMapLayer.SetBitmapChanged;
begin
  FUpdateFlag.SetFlag;
end;

procedure TMiniMapLayer.SetLayerCoordConverter(
  const AValue: ILocalCoordConverter
);
var
  VNewSize: TPoint;
  VLocalConverter: ILocalCoordConverter;
begin
  VLocalConverter := LayerCoordConverter;
  VNewSize := GetLayerSizeForView(AValue);
  Layer.Bitmap.Lock;
  try
    if Visible then begin
      FClearStrategy := FClearStrategyFactory.GetStrategy(VLocalConverter, AValue, Layer.Bitmap, FClearStrategy);
    end else begin
      FClearStrategy := nil;
    end;
    if (Layer.Bitmap.Width <> VNewSize.X) or (Layer.Bitmap.Height <> VNewSize.Y) then begin
      SetNeedUpdateLayerSize;
    end;
  finally
    Layer.Bitmap.Unlock;
  end;
  if (VLocalConverter = nil) or (not VLocalConverter.GetIsSameConverter(AValue)) then begin
    SetNeedRedraw;
  end;
  SetNeedUpdateLocation;
  inherited;
end;

procedure TMiniMapLayer.SetNeedRedraw;
begin
  FDrawTask.StopExecute;
  inherited;
end;

procedure TMiniMapLayer.SetNeedUpdateLayerSize;
begin
  inherited;
  FDrawTask.StopExecute;
end;

procedure TMiniMapLayer.StartThreads;
begin
  inherited;
  OnConfigChange;
  OnMainMapChange;
  OnLayerSetChange;
  FDrawTask.Start;
  if Visible then begin
    FDrawTask.StartExecute;
  end;
end;

end.
