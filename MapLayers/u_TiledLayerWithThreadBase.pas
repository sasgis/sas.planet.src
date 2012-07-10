unit u_TiledLayerWithThreadBase;

interface

uses
  Windows,
  GR32,
  GR32_Image,
  GR32_Layers,
  SysUtils,
  i_Notifier,
  i_Listener,
  i_ThreadConfig,
  i_NotifierOperation,
  i_LocalCoordConverter,
  i_LocalCoordConverterFactorySimpe,
  i_BitmapLayerProvider,
  i_ImageResamplerConfig,
  i_ViewPortState,
  i_SimpleFlag,
  i_TileMatrix,
  i_BackgroundTask,
  i_InternalPerformanceCounter,
  u_WindowLayerWithPos;

type
  TTiledLayerWithThreadBase = class(TWindowLayerWithPosBase)
  private
    FTileMatrixFactory: ITileMatrixFactory;
    FImageResamplerConfig: IImageResamplerConfig;
    FUpdateLayerProviderOnPosChange: Boolean;

    FTileMatrix: ITileMatrix;
    FTileMatrixCS: IReadWriteSync;

    FLayerProvider: IBitmapLayerProvider;
    FLayerProviderCS: IReadWriteSync;

    FLayer: TCustomLayer;

    FDrawTask: IBackgroundTask;
    FRectUpdateListener: IListener;

    FBgDrawCounter: IInternalPerformanceCounter;
    FPrepareLayerProviderCounter: IInternalPerformanceCounter;
    FOneTilePrepareCounter: IInternalPerformanceCounter;
    FOnPaintCounter: IInternalPerformanceCounter;
    FOneTilePaintCounter: IInternalPerformanceCounter;

    FDelicateRedrawFlag: ISimpleFlag;
    FLayerChangedFlag: ISimpleFlag;
    FUpdateLayerProviderFlag: ISimpleFlag;
    FTileMatrixChangeFlag: ISimpleFlag;

    procedure UpdateLayerIfNeed;
    procedure UpdateTileMatrixIfNeed;

    function GetTileMatrix: ITileMatrix;
    procedure SetTileMatrix(const Value: ITileMatrix);

    procedure SetMatrixNotReady(const ATileMatrix: ITileMatrix);
    procedure OnPrepareTileMatrix(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation
    );

    procedure PrepareTileMatrix(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ATileMatrix: ITileMatrix;
      const ALayerProvider: IBitmapLayerProvider
    );

    procedure OnPaintLayer(
      Sender: TObject;
      Buffer: TBitmap32
    );
    procedure PaintLayer(
      ABuffer: TBitmap32;
      const ALocalConverter: ILocalCoordConverter;
      const ATileMatrix: ITileMatrix
    );
    procedure OnTimer;
    procedure OnRectUpdate(const AMsg: IInterface);
    function GetLayerProvider: IBitmapLayerProvider;
    procedure SetLayerProvider(const Value: IBitmapLayerProvider);
    property LayerProvider: IBitmapLayerProvider read GetLayerProvider write SetLayerProvider;
  protected
    function CreateLayerProvider(
      const ALayerConverter: ILocalCoordConverter
    ): IBitmapLayerProvider; virtual; abstract;
    function CreteTileMatrix(
      const ASource: ITileMatrix;
      const ANewConverter: ILocalCoordConverter
    ): ITileMatrix; virtual;
    procedure DelicateRedraw;
    procedure DelicateRedrawWithFullUpdate;

    procedure SetNeedUpdateTileMatrix;
    procedure DoUpdateTileMatrix; virtual;

    procedure SetNeedUpdateLayerProvider;

    procedure SetNeedUpdateLayer;
    procedure DoUpdateLayer; virtual;
  protected
    procedure StartThreads; override;
    procedure SendTerminateToThreads; override;
    procedure SetLayerCoordConverter(const AValue: ILocalCoordConverter); override;
    procedure SetViewCoordConverter(const AValue: ILocalCoordConverter); override;
    procedure DoViewUpdate; override;
    property TileMatrix: ITileMatrix read GetTileMatrix;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      AParentMap: TImage32;
      const AViewPortState: IViewPortState;
      const AResamplerConfig: IImageResamplerConfig;
      const AConverterFactory: ILocalCoordConverterFactorySimpe;
      const ATimerNoifier: INotifier;
      AUpdateLayerProviderOnPosChange: Boolean;
      const AThreadConfig: IThreadConfig
    );
  end;

implementation

uses
  GR32_Resamplers,
  t_GeoTypes,
  i_TileIterator,
  i_CoordConverter,
  i_LonLatRect,
  u_SimpleFlagWithInterlock,
  i_Bitmap32Static,
  u_Synchronizer,
  u_ListenerByEvent,
  u_TileIteratorSpiralByRect,
  i_BitmapLayerProviderWithListener,
  u_TileIteratorByRect,
  u_TileMatrixFactory,
  u_BackgroundTask;


{ TTiledLayerWithThreadBase }

constructor TTiledLayerWithThreadBase.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const AViewPortState: IViewPortState;
  const AResamplerConfig: IImageResamplerConfig;
  const AConverterFactory: ILocalCoordConverterFactorySimpe;
  const ATimerNoifier: INotifier;
  AUpdateLayerProviderOnPosChange: Boolean;
  const AThreadConfig: IThreadConfig
);
begin
  inherited Create(
    APerfList,
    AAppStartedNotifier,
    AAppClosingNotifier,
    AViewPortState,
    True
  );
  FUpdateLayerProviderOnPosChange := AUpdateLayerProviderOnPosChange;
  FLayer := TCustomLayer.Create(AParentMap.Layers);
  FImageResamplerConfig := AResamplerConfig;

  FLayerProviderCS := MakeSyncRW_Var(Self);
  FTileMatrixCS := MakeSyncRW_Var(Self);

  FBgDrawCounter := PerfList.CreateAndAddNewCounter('BgDraw');
  FOneTilePrepareCounter := PerfList.CreateAndAddNewCounter('OneTilePrepare');
  FOnPaintCounter := PerfList.CreateAndAddNewCounter('OnPaint');
  FOneTilePaintCounter := PerfList.CreateAndAddNewCounter('OneTilePaint');
  FPrepareLayerProviderCounter := PerfList.CreateAndAddNewCounter('PrepareLayerProvider');

  FDrawTask := TBackgroundTask.Create(AAppClosingNotifier, OnPrepareTileMatrix, AThreadConfig);
  FTileMatrixFactory :=
    TTileMatrixFactory.Create(
      AResamplerConfig,
      AConverterFactory
    );

  FDelicateRedrawFlag := TSimpleFlagWithInterlock.Create;
  FLayerChangedFlag := TSimpleFlagWithInterlock.Create;
  FUpdateLayerProviderFlag := TSimpleFlagWithInterlock.Create;
  FTileMatrixChangeFlag := TSimpleFlagWithInterlock.Create;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnTimer),
    ATimerNoifier
  );
  FRectUpdateListener := TNotifyEventListener.Create(Self.OnRectUpdate);
end;

function TTiledLayerWithThreadBase.CreteTileMatrix(
  const ASource: ITileMatrix;
  const ANewConverter: ILocalCoordConverter
): ITileMatrix;
begin
  Result := FTileMatrixFactory.BuildNewMatrix(ASource, ANewConverter);
end;

procedure TTiledLayerWithThreadBase.DelicateRedraw;
begin
  FDelicateRedrawFlag.SetFlag;
  FDrawTask.StartExecute;
end;

procedure TTiledLayerWithThreadBase.DelicateRedrawWithFullUpdate;
begin
  SetMatrixNotReady(TileMatrix);
  FDelicateRedrawFlag.SetFlag;
  FDrawTask.StartExecute;
end;

procedure TTiledLayerWithThreadBase.DoUpdateLayer;
begin
  FLayer.Changed;
end;

procedure TTiledLayerWithThreadBase.DoUpdateTileMatrix;
var
  VOldTileMatrix: ITileMatrix;
  VNewTileMatrix: ITileMatrix;
  VProviderWithListener: IBitmapLayerProviderWithListener;
begin
  VOldTileMatrix := TileMatrix;
  VNewTileMatrix := CreteTileMatrix(VOldTileMatrix, LayerCoordConverter);
  if VOldTileMatrix <> VNewTileMatrix then begin
    FDrawTask.StopExecute;
    SetTileMatrix(VNewTileMatrix);
    if Supports(LayerProvider, IBitmapLayerProviderWithListener, VProviderWithListener) then begin
      VProviderWithListener.SetListener(FRectUpdateListener, VNewTileMatrix.LocalConverter);
    end;
    if FUpdateLayerProviderOnPosChange then begin
      SetNeedUpdateLayerProvider;
    end;
    SetNeedUpdateLayer;
  end;
end;

procedure TTiledLayerWithThreadBase.DoViewUpdate;
begin
  inherited;
  UpdateTileMatrixIfNeed;
  FDrawTask.StartExecute;
  UpdateLayerIfNeed;
end;

function TTiledLayerWithThreadBase.GetLayerProvider: IBitmapLayerProvider;
begin
  FLayerProviderCS.BeginRead;
  try
    Result := FLayerProvider;
  finally
    FLayerProviderCS.EndRead;
  end;
end;

function TTiledLayerWithThreadBase.GetTileMatrix: ITileMatrix;
begin
  FTileMatrixCS.BeginRead;
  try
    Result := FTileMatrix;
  finally
    FTileMatrixCS.EndRead;
  end;
end;

procedure TTiledLayerWithThreadBase.OnPaintLayer(
  Sender: TObject;
  Buffer: TBitmap32
);
var
  VTileMatrix: ITileMatrix;
  VLocalConverter: ILocalCoordConverter;
  VCounterContext: TInternalPerformanceCounterContext;
begin
  VLocalConverter := ViewCoordConverter;
  VTileMatrix := TileMatrix;
  if (VLocalConverter <> nil) and (VTileMatrix <> nil) then begin
    VCounterContext := FOnPaintCounter.StartOperation;
    try
      PaintLayer(Buffer, VLocalConverter, VTileMatrix);
    finally
      FOnPaintCounter.FinishOperation(VCounterContext);
    end;
  end;
end;

procedure TTiledLayerWithThreadBase.OnPrepareTileMatrix(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation
);
var
  VTileMatrix: ITileMatrix;
  VProvider: IBitmapLayerProvider;
  VProviderWithListener: IBitmapLayerProviderWithListener;
  VLayerConverter: ILocalCoordConverter;
  VNeedRedraw: Boolean;
  VCounterContext: TInternalPerformanceCounterContext;
begin
  FDelicateRedrawFlag.CheckFlagAndReset;
  VNeedRedraw := True;
  while VNeedRedraw do begin
    VTileMatrix := TileMatrix;
    if VTileMatrix = nil then begin
      Exit;
    end;
    if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
      Exit;
    end;
    VLayerConverter := VTileMatrix.LocalConverter;

    VProvider := LayerProvider;
    if FUpdateLayerProviderFlag.CheckFlagAndReset then begin
      if Supports(VProvider, IBitmapLayerProviderWithListener, VProviderWithListener) then begin
        VProviderWithListener.RemoveListener;
        VProviderWithListener := nil;
      end;
      VProvider := nil;
      LayerProvider := nil;
    end;
    if VProvider = nil then begin
      VCounterContext := FPrepareLayerProviderCounter.StartOperation;
      try
        VProvider := CreateLayerProvider(VLayerConverter);
      finally
        FPrepareLayerProviderCounter.FinishOperation(VCounterContext);
      end;
      if VProvider = nil then begin
        Exit;
      end;
      if Supports(VProvider, IBitmapLayerProviderWithListener, VProviderWithListener) then begin
        VProviderWithListener.SetListener(FRectUpdateListener, VTileMatrix.LocalConverter);
      end;
      LayerProvider := VProvider;
    end;
    if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
      Exit;
    end;
    VCounterContext := FBgDrawCounter.StartOperation;
    try
      PrepareTileMatrix(AOperationID, ACancelNotifier, VTileMatrix, VProvider);
    finally
      FBgDrawCounter.FinishOperation(VCounterContext);
    end;
    VNeedRedraw := FDelicateRedrawFlag.CheckFlagAndReset;
  end;
end;

procedure TTiledLayerWithThreadBase.OnRectUpdate(const AMsg: IInterface);
var
  VLonLatRect: ILonLatRect;
  VTileMatrix: ITileMatrix;
  VTileRect: TRect;
  VMapLonLatRect: TDoubleRect;
  VConverter: ICoordConverter;
  VZoom: Byte;
  VTileRectToUpdate: TRect;
  i, j: Integer;
  VTile: TPoint;
  VElement: ITileMatrixElement;
begin
  if Supports(AMsg, ILonLatRect, VLonLatRect) then begin
    VTileMatrix := TileMatrix;
    if VTileMatrix <> nil then begin
      VMapLonLatRect := VLonLatRect.Rect;
      VConverter := VTileMatrix.LocalConverter.GeoConverter;
      VZoom := VTileMatrix.LocalConverter.Zoom;
      VConverter.CheckLonLatRect(VMapLonLatRect);
      VTileRect := VConverter.LonLatRect2TileRect(VMapLonLatRect, VZoom);
      if IntersectRect(VTileRectToUpdate, VTileRect, VTileMatrix.TileRect) then begin
        for i := VTileRectToUpdate.Top to VTileRectToUpdate.Bottom - 1 do begin
          VTile.Y := i;
          for j := VTileRectToUpdate.Left to VTileRectToUpdate.Right - 1 do begin
            VTile.X := j;
            VElement := VTileMatrix.GetElementByTile(VTile);
            if VElement <> nil then begin
              VElement.IncExpectedID;
            end;
          end;
        end;
        DelicateRedraw;
      end;
    end;
  end else begin
    DelicateRedrawWithFullUpdate;
  end;
end;

procedure TTiledLayerWithThreadBase.PrepareTileMatrix(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ATileMatrix: ITileMatrix;
  const ALayerProvider: IBitmapLayerProvider
);
var
  VTileIterator: ITileIterator;
  VTile: TPoint;
  VElement: ITileMatrixElement;
  VBitmap: IBitmap32Static;
  VCounterContext: TInternalPerformanceCounterContext;
  VId: Integer;
begin
  if ALayerProvider <> nil then begin
    VTileIterator := TTileIteratorSpiralByRect.Create(ATileMatrix.TileRect);
    while VTileIterator.Next(VTile) do begin
      VElement := ATileMatrix.GetElementByTile(VTile);
      if VElement <> nil then begin
        VId := VElement.ExpectedID;
        if VElement.ReadyID <> VId then begin
          VCounterContext := FOneTilePrepareCounter.StartOperation;
          try
            VBitmap := ALayerProvider.GetBitmapRect(AOperationID, ACancelNotifier, VElement.LocalConverter);
            VElement.UpdateBitmap(VId, VBitmap);
            SetNeedUpdateLayer;
          finally
            FOneTilePrepareCounter.FinishOperation(VCounterContext);
          end;
        end;
      end;
      if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
        Exit;
      end;
    end;
  end else begin
    VTileIterator := TTileIteratorSpiralByRect.Create(ATileMatrix.TileRect);
    while VTileIterator.Next(VTile) do begin
      VElement := ATileMatrix.GetElementByTile(VTile);
      if VElement <> nil then begin
        VId := VElement.ExpectedID;
        VElement.UpdateBitmap(VId, nil);
      end;
    end;
  end;
end;

procedure TTiledLayerWithThreadBase.OnTimer;
begin
  UpdateLayerIfNeed;
end;

procedure TTiledLayerWithThreadBase.PaintLayer(
  ABuffer: TBitmap32;
  const ALocalConverter: ILocalCoordConverter;
  const ATileMatrix: ITileMatrix
);
var
  VTileRectInClipRect: TRect;
  VConverter: ICoordConverter;
  VZoom: Byte;
  VTileIterator: ITileIterator;
  VTile: TPoint;
  VElement: ITileMatrixElement;
  VBitmap: IBitmap32Static;
  VResampler: TCustomResampler;
  VDstRect: TRect;
  VCounterContext: TInternalPerformanceCounterContext;
  VMapPixelRect: TRect;
begin
  inherited;
  VConverter := ALocalConverter.GeoConverter;
  if not VConverter.IsSameConverter(ATileMatrix.LocalConverter.GeoConverter) then begin
    Exit;
  end;
  VZoom := ALocalConverter.Zoom;
  if VZoom <> ATileMatrix.LocalConverter.Zoom then begin
    Exit;
  end;
  VMapPixelRect := ALocalConverter.LocalRect2MapRect(ABuffer.ClipRect);
  VConverter.CheckPixelRect(VMapPixelRect, VZoom);
  VTileRectInClipRect := VConverter.PixelRect2TileRect(VMapPixelRect, VZoom);
  VResampler := nil;
  try
    VTileIterator := TTileIteratorByRect.Create(VTileRectInClipRect);
    while VTileIterator.Next(VTile) do begin
      VElement := ATileMatrix.GetElementByTile(VTile);
      if VElement <> nil then begin
        VBitmap := VElement.Bitmap;
        if VBitmap <> nil then begin
          VDstRect := ALocalConverter.MapRect2LocalRect(VElement.LocalConverter.GetRectInMapPixel);
          if not ABuffer.MeasuringMode then begin
            if VResampler = nil then begin
              VResampler := FImageResamplerConfig.GetActiveFactory.CreateResampler;
            end;
            Assert(VResampler <> nil);
            VCounterContext := FOneTilePaintCounter.StartOperation;
            try
              StretchTransfer(
                ABuffer,
                VDstRect,
                ABuffer.ClipRect,
                VBitmap.Bitmap,
                VBitmap.Bitmap.BoundsRect,
                VResampler,
                dmBlend,
                cmBlend
              );
            finally
              FOneTilePaintCounter.FinishOperation(VCounterContext);
            end;
          end else begin
            ABuffer.Changed(VDstRect);
          end;
        end;
      end;
    end;
  finally
    VResampler.Free;
  end;
end;

procedure TTiledLayerWithThreadBase.SetLayerCoordConverter(
  const AValue: ILocalCoordConverter
);
begin
  inherited;
  SetNeedUpdateTileMatrix;
end;

procedure TTiledLayerWithThreadBase.SetLayerProvider(
  const Value: IBitmapLayerProvider);
begin
  FLayerProviderCS.BeginWrite;
  try
    FLayerProvider := Value;
  finally
    FLayerProviderCS.EndWrite;
  end;
end;

procedure TTiledLayerWithThreadBase.SetMatrixNotReady(
  const ATileMatrix: ITileMatrix
);
var
  i, j: Integer;
  VTileRect: TRect;
  VElement: ITileMatrixElement;
begin
  if ATileMatrix <> nil then begin
    VTileRect := ATileMatrix.TileRect;
    for i := 0 to VTileRect.Right - VTileRect.Left - 1 do begin
      for j := 0 to VTileRect.Bottom - VTileRect.Top - 1 do begin
        VElement := ATileMatrix.Items[i, j];
        if VElement <> nil then begin
          VElement.IncExpectedID;
        end;
      end;
    end;
  end;
end;

procedure TTiledLayerWithThreadBase.SetNeedUpdateLayer;
begin
  FLayerChangedFlag.SetFlag;
end;

procedure TTiledLayerWithThreadBase.SetNeedUpdateLayerProvider;
begin
  FDrawTask.StopExecute;
  SetMatrixNotReady(TileMatrix);
  FUpdateLayerProviderFlag.SetFlag;
end;

procedure TTiledLayerWithThreadBase.SetNeedUpdateTileMatrix;
begin
  FTileMatrixChangeFlag.SetFlag;
end;

procedure TTiledLayerWithThreadBase.SetTileMatrix(const Value: ITileMatrix);
begin
  FTileMatrixCS.BeginWrite;
  try
    FTileMatrix := Value;
  finally
    FTileMatrixCS.EndWrite;
  end;
end;

procedure TTiledLayerWithThreadBase.SetViewCoordConverter(
  const AValue: ILocalCoordConverter
);
begin
  inherited;
  SetNeedUpdateLayer;
end;

procedure TTiledLayerWithThreadBase.SendTerminateToThreads;
begin
  inherited;
  FDrawTask.Terminate;
end;

procedure TTiledLayerWithThreadBase.StartThreads;
begin
  inherited;
  FLayer.OnPaint := OnPaintLayer;
  FDrawTask.Start;
  FDrawTask.StartExecute;
end;

procedure TTiledLayerWithThreadBase.UpdateLayerIfNeed;
begin
  if FLayerChangedFlag.CheckFlagAndReset then begin
    DoUpdateLayer;
  end;
end;

procedure TTiledLayerWithThreadBase.UpdateTileMatrixIfNeed;
begin
  if FTileMatrixChangeFlag.CheckFlagAndReset then begin
    DoUpdateTileMatrix;
  end;
end;

end.
