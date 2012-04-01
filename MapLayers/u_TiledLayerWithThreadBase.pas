unit u_TiledLayerWithThreadBase;

interface

uses
  Windows,
  Classes,
  GR32,
  GR32_Image,
  GR32_Layers,
  SysUtils,
  i_JclNotify,
  i_ThreadConfig,
  i_OperationNotifier,
  i_LocalCoordConverter,
  i_LocalCoordConverterFactorySimpe,
  i_BitmapLayerProvider,
  i_ImageResamplerConfig,
  i_ViewPortState,
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
    FLayerProviderCS:IReadWriteSync;

    FLayer: TCustomLayer;

    FDrawTask: IBackgroundTask;

    FBgDrawCounter: IInternalPerformanceCounter;
    FPrepareLayerProviderCounter: IInternalPerformanceCounter;
    FOneTilePrepareCounter: IInternalPerformanceCounter;
    FOnPaintCounter: IInternalPerformanceCounter;
    FOneTilePaintCounter: IInternalPerformanceCounter;

    FDelicateRedrawCounter: Integer;
    FLayerChangedCounter: Integer;
    FUpdateLayerProviderCounter: Integer;
    FTileMatrixChangeCounter: Integer;

    procedure UpdateLayerIfNeed;
    procedure UpdateTileMatrixIfNeed;
    procedure UpdateLayerProviderIfNeed;

    function GetTileMatrix: ITileMatrix;
    procedure SetTileMatrix(const Value: ITileMatrix);

    procedure SetMatrixNotReady(ATileMatrix: ITileMatrix);
    procedure OnPrepareTileMatrix(
      AOperationID: Integer;
      ACancelNotifier: IOperationNotifier
    );

    procedure PrepareTileMatrix(
      AOperationID: Integer;
      ACancelNotifier: IOperationNotifier;
      ATileMatrix: ITileMatrix;
      ALayerProvider: IBitmapLayerProvider
    );

    procedure OnPaintLayer(Sender: TObject; Buffer: TBitmap32);
    procedure PaintLayer(
      ABuffer: TBitmap32;
      ALocalConverter: ILocalCoordConverter;
      ATileMatrix: ITileMatrix
    );
    procedure OnTimer;
    function GetLayerProvider: IBitmapLayerProvider;
    procedure SetLayerProvider(const Value: IBitmapLayerProvider);
    property LayerProvider: IBitmapLayerProvider read GetLayerProvider write SetLayerProvider;
  protected
    function CreateLayerProvider(ALayerConverter: ILocalCoordConverter): IBitmapLayerProvider; virtual; abstract;
    function CreteTileMatrix(ASource: ITileMatrix; ANewConverter: ILocalCoordConverter): ITileMatrix; virtual;
    procedure DelicateRedraw;

    procedure SetNeedUpdateTileMatrix;
    procedure DoUpdateTileMatrix; virtual;

    procedure SetNeedUpdateLayerProvider;
    procedure DoRedrawWithUpdateProvider; virtual;

    procedure SetNeedUpdateLayer;
    procedure DoUpdateLayer; virtual;
  protected
    procedure SetLayerCoordConverter(AValue: ILocalCoordConverter); override;
    procedure SetViewCoordConverter(AValue: ILocalCoordConverter); override;
    procedure DoViewUpdate; override;
    property TileMatrix: ITileMatrix read GetTileMatrix;
  public
    procedure StartThreads; override;
    procedure SendTerminateToThreads; override;
  public
    constructor Create(
      APerfList: IInternalPerformanceCounterList;
      AAppClosingNotifier: IJclNotifier;
      AParentMap: TImage32;
      AViewPortState: IViewPortState;
      AResamplerConfig: IImageResamplerConfig;
      AConverterFactory: ILocalCoordConverterFactorySimpe;
      ATimerNoifier: IJclNotifier;
      AUpdateLayerProviderOnPosChange: Boolean;
      AThreadConfig: IThreadConfig
    );
  end;

implementation

uses
  GR32_Resamplers,
  i_TileIterator,
  i_CoordConverter,
  i_Bitmap32Static,
  u_Synchronizer,
  u_NotifyEventListener,
  u_TileIteratorSpiralByRect,
  u_TileIteratorByRect,
  u_TileMatrixFactory,
  u_BackgroundTaskLayerDrawBase;


{ TTiledLayerWithThreadBase }

constructor TTiledLayerWithThreadBase.Create(
  APerfList: IInternalPerformanceCounterList;
  AAppClosingNotifier: IJclNotifier;
  AParentMap: TImage32;
  AViewPortState: IViewPortState;
  AResamplerConfig: IImageResamplerConfig;
  AConverterFactory: ILocalCoordConverterFactorySimpe;
  ATimerNoifier: IJclNotifier;
  AUpdateLayerProviderOnPosChange: Boolean;
  AThreadConfig: IThreadConfig
);
begin
  inherited Create(APerfList, AViewPortState, True);
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

  FDrawTask := TBackgroundTaskLayerDrawBase.Create(AAppClosingNotifier, OnPrepareTileMatrix, AThreadConfig);
  FTileMatrixFactory :=
    TTileMatrixFactory.Create(
      AResamplerConfig,
      AConverterFactory
    );

  FDelicateRedrawCounter := 0;
  FLayerChangedCounter := 0;
  FUpdateLayerProviderCounter := 0;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnTimer),
    ATimerNoifier
  );
end;

function TTiledLayerWithThreadBase.CreteTileMatrix(ASource: ITileMatrix;
  ANewConverter: ILocalCoordConverter): ITileMatrix;
begin
  Result := FTileMatrixFactory.BuildNewMatrix(ASource, ANewConverter);
end;

procedure TTiledLayerWithThreadBase.DelicateRedraw;
begin
  InterlockedIncrement(FDelicateRedrawCounter);
  FDrawTask.StartExecute;
end;

procedure TTiledLayerWithThreadBase.DoRedrawWithUpdateProvider;
begin
  FDrawTask.StopExecute;
  SetMatrixNotReady(TileMatrix);
  LayerProvider := nil;
end;

procedure TTiledLayerWithThreadBase.DoUpdateLayer;
begin
  FLayer.Changed;
end;

procedure TTiledLayerWithThreadBase.DoUpdateTileMatrix;
var
  VOldTileMatrix: ITileMatrix;
  VNewTileMatrix: ITileMatrix;
begin
  VOldTileMatrix := TileMatrix;
  VNewTileMatrix := CreteTileMatrix(VOldTileMatrix, LayerCoordConverter);
  if VOldTileMatrix <> VNewTileMatrix then begin
    FDrawTask.StopExecute;
    SetTileMatrix(VNewTileMatrix);
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
  UpdateLayerProviderIfNeed;
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
  ACancelNotifier: IOperationNotifier
);
var
  VTileMatrix: ITileMatrix;
  VProvider: IBitmapLayerProvider;
  VLayerConverter: ILocalCoordConverter;
  VDelicateRedrawCounter: Integer;
  VCounterContext: TInternalPerformanceCounterContext;
begin
  InterlockedExchange(FDelicateRedrawCounter, 0);
  VDelicateRedrawCounter := 1;
  while VDelicateRedrawCounter > 0 do begin
    VTileMatrix := TileMatrix;
    if VTileMatrix = nil then begin
      Exit;
    end;
    if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
      Exit;
    end;
    VLayerConverter := VTileMatrix.LocalConverter;

    VProvider := LayerProvider;
    if VProvider = nil then begin
      VCounterContext := FPrepareLayerProviderCounter.StartOperation;
      try
        VProvider := CreateLayerProvider(VLayerConverter);
      finally
        FPrepareLayerProviderCounter.FinishOperation(VCounterContext);
      end;
      if VProvider = nil then begin
        Exit
      end;
      LayerProvider :=  VProvider;
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
    VDelicateRedrawCounter := InterlockedExchange(FDelicateRedrawCounter, 0);
  end;
end;

procedure TTiledLayerWithThreadBase.PrepareTileMatrix(AOperationID: Integer;
  ACancelNotifier: IOperationNotifier; ATileMatrix: ITileMatrix;
  ALayerProvider: IBitmapLayerProvider);
var
  VTileIterator: ITileIterator;
  VTile: TPoint;
  VElement: ITileMatrixElement;
  VBitmap: IBitmap32Static;
  VCounterContext: TInternalPerformanceCounterContext;
begin
  VTileIterator := TTileIteratorSpiralByRect.Create(ATileMatrix.TileRect);
  while VTileIterator.Next(VTile) do begin
    VElement := ATileMatrix.GetElementByTile(VTile);
    if VElement <> nil then begin
      if not VElement.IsReady then begin
        VCounterContext := FOneTilePrepareCounter.StartOperation;
        try
          VBitmap := ALayerProvider.GetBitmapRect(AOperationID, ACancelNotifier, VElement.LocalConverter);
          if (VBitmap <> nil) or ((VBitmap = nil) and (VElement.Bitmap <> nil)) then begin
            VElement.Bitmap := VBitmap;
            VElement.IsReady := True;
            SetNeedUpdateLayer;
          end else begin
            VElement.IsReady := True;
          end;
        finally
          FOneTilePrepareCounter.FinishOperation(VCounterContext);
        end;
      end;
    end;
    if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
      Exit;
    end;
  end;
end;

procedure TTiledLayerWithThreadBase.OnTimer;
begin
  ViewUpdateLock;
  ViewUpdateUnlock;
end;

procedure TTiledLayerWithThreadBase.PaintLayer(
  ABuffer: TBitmap32;
  ALocalConverter: ILocalCoordConverter;
  ATileMatrix: ITileMatrix
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
  VTileRectInClipRect :=
    VConverter.PixelRectFloat2TileRect(
      ALocalConverter.LocalRect2MapRectFloat(ABuffer.ClipRect),
      VZoom
    );
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
  AValue: ILocalCoordConverter
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
    FLayerProvider :=  Value;
  finally
    FLayerProviderCS.EndWrite;
  end;
end;

procedure TTiledLayerWithThreadBase.SetMatrixNotReady(ATileMatrix: ITileMatrix);
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
          VElement.IsReady := False;
        end;
      end;
    end;
  end;
end;

procedure TTiledLayerWithThreadBase.SetNeedUpdateLayer;
begin
  InterlockedIncrement(FLayerChangedCounter);
end;

procedure TTiledLayerWithThreadBase.SetNeedUpdateLayerProvider;
begin
  InterlockedIncrement(FUpdateLayerProviderCounter);
end;

procedure TTiledLayerWithThreadBase.SetNeedUpdateTileMatrix;
begin
  InterlockedIncrement(FTileMatrixChangeCounter);
end;

procedure TTiledLayerWithThreadBase.SetTileMatrix(const Value: ITileMatrix);
begin
  FTileMatrixCS.BeginWrite;
  try
    FTileMatrix :=  Value;
  finally
    FTileMatrixCS.EndWrite;
  end;
end;

procedure TTiledLayerWithThreadBase.SetViewCoordConverter(
  AValue: ILocalCoordConverter
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
  if InterlockedExchange(FLayerChangedCounter, 0) > 0 then begin
    DoUpdateLayer;
  end;
end;

procedure TTiledLayerWithThreadBase.UpdateLayerProviderIfNeed;
begin
  if InterlockedExchange(FUpdateLayerProviderCounter, 0) > 0 then begin
    DoRedrawWithUpdateProvider;
  end;
end;

procedure TTiledLayerWithThreadBase.UpdateTileMatrixIfNeed;
begin
  if InterlockedExchange(FTileMatrixChangeCounter, 0) > 0 then begin
    DoUpdateTileMatrix;
  end;
end;

end.
