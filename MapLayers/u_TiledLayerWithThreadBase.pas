unit u_TiledLayerWithThreadBase;

interface

uses
  Types,
  GR32,
  GR32_Image,
  GR32_Layers,
  SysUtils,
  i_Notifier,
  i_NotifierTime,
  i_Listener,
  i_ThreadConfig,
  i_NotifierOperation,
  i_LocalCoordConverter,
  i_LocalCoordConverterChangeable,
  i_BitmapLayerProvider,
  i_SimpleFlag,
  i_TileMatrix,
  i_BackgroundTask,
  i_InternalPerformanceCounter,
  u_WindowLayerWithPos;

type
  TTiledLayerWithThreadBase = class(TWindowLayerWithLocationBase)
  private
    FTileMatrixFactory: ITileMatrixFactory;
    FPosition: ILocalCoordConverterChangeable;
    FView: ILocalCoordConverterChangeable;
    FUpdateLayerProviderOnPosChange: Boolean;

    FTileMatrix: ITileMatrix;
    FTileMatrixCS: IReadWriteSync;

    FLayerProvider: IBitmapLayerProvider;
    FLayerProviderCS: IReadWriteSync;

    FDrawTask: IBackgroundTask;
    FRectUpdateListener: IListener;

    FBgDrawCounter: IInternalPerformanceCounter;
    FPrepareLayerProviderCounter: IInternalPerformanceCounter;
    FOneTilePrepareCounter: IInternalPerformanceCounter;
    FOneTilePaintSimpleCounter: IInternalPerformanceCounter;
    FOneTilePaintResizeCounter: IInternalPerformanceCounter;
    FTileMatrixUpdateCounter: IInternalPerformanceCounter;

    FDelicateRedrawFlag: ISimpleFlag;
    FLayerChangedFlag: ISimpleFlag;
    FUpdateLayerProviderFlag: ISimpleFlag;
    FTileMatrixChangeFlag: ISimpleFlag;

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

    procedure PaintLayerFromTileMatrix(
      ABuffer: TBitmap32;
      const ALocalConverter: ILocalCoordConverter;
      const ATileMatrix: ITileMatrix
    );
    procedure OnTimer;
    procedure OnRectUpdate(const AMsg: IInterface);
    function GetLayerProvider: IBitmapLayerProvider;
    procedure SetLayerProvider(const Value: IBitmapLayerProvider);

    property LayerProvider: IBitmapLayerProvider read GetLayerProvider write SetLayerProvider;

    procedure OnPosChange;
    procedure OnScaleChange;
  protected
    procedure DoUpdateLayerVisibility; override;
    procedure DoViewUpdate; override;

    function CreateLayerProvider(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ALayerConverter: ILocalCoordConverter
    ): IBitmapLayerProvider; virtual; abstract;
    procedure DelicateRedraw;
    procedure DelicateRedrawWithFullUpdate; virtual;

    procedure SetNeedUpdateTileMatrix;
    procedure DoUpdateTileMatrix; virtual;

    procedure SetNeedUpdateLayerProvider;
    property UpdateLayerProviderFlag: ISimpleFlag read FUpdateLayerProviderFlag;
  protected
    function GetNewLayerLocation: TFloatRect; override;
    procedure PaintLayer(ABuffer: TBitmap32); override;
    procedure StartThreads; override;
    procedure SendTerminateToThreads; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      AParentMap: TImage32;
      const APosition: ILocalCoordConverterChangeable;
      const AView: ILocalCoordConverterChangeable;
      const ATileMatrixFactory: ITileMatrixFactory;
      const ATimerNoifier: INotifierTime;
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
  u_ListenerTime,
  u_TileIteratorSpiralByRect,
  i_ObjectWithListener,
  u_TileIteratorByRect,
  u_GeoFun,
  u_BitmapFunc,
  u_BackgroundTask;


{ TTiledLayerWithThreadBase }

constructor TTiledLayerWithThreadBase.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const APosition: ILocalCoordConverterChangeable;
  const AView: ILocalCoordConverterChangeable;
  const ATileMatrixFactory: ITileMatrixFactory;
  const ATimerNoifier: INotifierTime;
  AUpdateLayerProviderOnPosChange: Boolean;
  const AThreadConfig: IThreadConfig
);
begin
  inherited Create(
    APerfList,
    AAppStartedNotifier,
    AAppClosingNotifier,
    TPositionedLayer.Create(AParentMap.Layers)
  );
  FUpdateLayerProviderOnPosChange := AUpdateLayerProviderOnPosChange;
  FTileMatrixFactory := ATileMatrixFactory;
  FView := AView;
  FPosition := APosition;

  FLayerProviderCS := MakeSyncRW_Var(Self);
  FTileMatrixCS := MakeSyncRW_Var(Self);

  FBgDrawCounter := APerfList.CreateAndAddNewCounter('BgDraw');
  FOneTilePrepareCounter := APerfList.CreateAndAddNewCounter('OneTilePrepare');
  FOneTilePaintSimpleCounter := APerfList.CreateAndAddNewCounter('OneTilePaintSimple');
  FOneTilePaintResizeCounter := APerfList.CreateAndAddNewCounter('OneTilePaintResize');
  FPrepareLayerProviderCounter := APerfList.CreateAndAddNewCounter('PrepareLayerProvider');
  FTileMatrixUpdateCounter := APerfList.CreateAndAddNewCounter('TileMatrixUpdate');

  FDelicateRedrawFlag := TSimpleFlagWithInterlock.Create;
  FLayerChangedFlag := TSimpleFlagWithInterlock.Create;
  FUpdateLayerProviderFlag := TSimpleFlagWithInterlock.Create;
  FTileMatrixChangeFlag := TSimpleFlagWithInterlock.Create;
  FRectUpdateListener := TNotifyEventListener.Create(Self.OnRectUpdate);

  FDrawTask := TBackgroundTask.Create(
    AAppClosingNotifier,
    OnPrepareTileMatrix,
    AThreadConfig,
    Self.ClassName
  );

  LinksList.Add(
    TListenerTimeCheck.Create(Self.OnTimer, 100),
    ATimerNoifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnPosChange),
    FPosition.ChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnScaleChange),
    FView.ChangeNotifier
  );
end;

procedure TTiledLayerWithThreadBase.DelicateRedraw;
begin
  FDelicateRedrawFlag.SetFlag;
  FDrawTask.StartExecute;
end;

procedure TTiledLayerWithThreadBase.DelicateRedrawWithFullUpdate;
begin
  SetMatrixNotReady(GetTileMatrix);
  FDelicateRedrawFlag.SetFlag;
  FDrawTask.StartExecute;
end;

procedure TTiledLayerWithThreadBase.DoUpdateLayerVisibility;
begin
  inherited;
  SetNeedUpdateTileMatrix;
  SetNeedUpdateLayerProvider;
end;

procedure TTiledLayerWithThreadBase.DoUpdateTileMatrix;
var
  VOldTileMatrix: ITileMatrix;
  VNewTileMatrix: ITileMatrix;
  VProviderWithListener: IObjectWithListener;
begin
  VOldTileMatrix := GetTileMatrix;
  if Visible then begin
    VNewTileMatrix := FTileMatrixFactory.BuildNewMatrix(VOldTileMatrix, FPosition.GetStatic);
  end else begin
    VNewTileMatrix := nil;
  end;
  if VOldTileMatrix <> VNewTileMatrix then begin
    FDrawTask.StopExecute;
    SetTileMatrix(VNewTileMatrix);
    if Supports(LayerProvider, IObjectWithListener, VProviderWithListener) then begin
      if VNewTileMatrix <> nil then begin
        VProviderWithListener.SetListener(FRectUpdateListener, VNewTileMatrix.LocalConverter);
      end else begin
        VProviderWithListener.RemoveListener;
      end;
    end;
    if FUpdateLayerProviderOnPosChange then begin
      SetNeedUpdateLayerProvider;
    end;
    FLayerChangedFlag.SetFlag;
  end;
end;

procedure TTiledLayerWithThreadBase.DoViewUpdate;
begin
  inherited;
  UpdateTileMatrixIfNeed;
  FDrawTask.StartExecute;
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

function TTiledLayerWithThreadBase.GetNewLayerLocation: TFloatRect;
var
  VLocalConverter: ILocalCoordConverter;
begin
  VLocalConverter := FView.GetStatic;
  if Visible and (VLocalConverter <> nil) then begin
    Result := FloatRect(VLocalConverter.GetLocalRect);
  end else begin
    Result := FloatRect(0, 0, 0, 0);
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

procedure TTiledLayerWithThreadBase.OnPosChange;
begin
  ViewUpdateLock;
  try
    SetNeedUpdateTileMatrix;
    SetNeedUpdateLayerLocation;
  finally
    ViewUpdateUnlock
  end;
end;

procedure TTiledLayerWithThreadBase.OnPrepareTileMatrix(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation
);
var
  VTileMatrix: ITileMatrix;
  VProvider: IBitmapLayerProvider;
  VProviderWithListener: IObjectWithListener;
  VLayerConverter: ILocalCoordConverter;
  VNeedRedraw: Boolean;
  VCounterContext: TInternalPerformanceCounterContext;
begin
  FDelicateRedrawFlag.CheckFlagAndReset;
  VNeedRedraw := True;
  while VNeedRedraw do begin
    VTileMatrix := GetTileMatrix;
    if VTileMatrix = nil then begin
      Exit;
    end;
    if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
      Exit;
    end;
    VLayerConverter := VTileMatrix.LocalConverter;

    VProvider := LayerProvider;
    if FUpdateLayerProviderFlag.CheckFlagAndReset then begin
      if Supports(VProvider, IObjectWithListener, VProviderWithListener) then begin
        VProviderWithListener.RemoveListener;
        VProviderWithListener := nil;
      end;
      VProvider := nil;
      LayerProvider := nil;
    end;
    if VProvider = nil then begin
      VCounterContext := FPrepareLayerProviderCounter.StartOperation;
      try
        VProvider := CreateLayerProvider(AOperationID, ACancelNotifier, VLayerConverter);
      finally
        FPrepareLayerProviderCounter.FinishOperation(VCounterContext);
      end;
      if Supports(VProvider, IObjectWithListener, VProviderWithListener) then begin
        VProviderWithListener.SetListener(FRectUpdateListener, VTileMatrix.LocalConverter);
      end;
      SetMatrixNotReady(VTileMatrix);
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
    VTileMatrix := GetTileMatrix;
    if VTileMatrix <> nil then begin
      VMapLonLatRect := VLonLatRect.Rect;
      VConverter := VTileMatrix.LocalConverter.GeoConverter;
      VZoom := VTileMatrix.LocalConverter.Zoom;
      VConverter.CheckLonLatRect(VMapLonLatRect);
      VTileRect := RectFromDoubleRect(VConverter.LonLatRect2TileRectFloat(VMapLonLatRect, VZoom), rrOutside);
      if Types.IntersectRect(VTileRectToUpdate, VTileRect, VTileMatrix.TileRect) then begin
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

procedure TTiledLayerWithThreadBase.OnScaleChange;
begin
  ViewUpdateLock;
  try
    SetNeedUpdateLayerLocation;
    SetNeedFullRepaintLayer;
  finally
    ViewUpdateUnlock;
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
            FLayerChangedFlag.SetFlag;
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
    FLayerChangedFlag.SetFlag;
  end;
end;

procedure TTiledLayerWithThreadBase.OnTimer;
var
  VLocalConverter: ILocalCoordConverter;
  VTileMatrix: ITileMatrix;
  VElement: ITileMatrixElement;
  VTileIterator: ITileIterator;
  VTile: TPoint;
  VDstRect: TRect;
begin
  if FLayerChangedFlag.CheckFlagAndReset then begin
    VTileMatrix := GetTileMatrix;
    VLocalConverter := FView.GetStatic;
    if (VLocalConverter <> nil) and (VTileMatrix <> nil) then begin
      if not VLocalConverter.ProjectionInfo.GetIsSameProjectionInfo(VTileMatrix.LocalConverter.ProjectionInfo) then begin
        Layer.Changed;
        Exit;
      end;
      VTileIterator := TTileIteratorByRect.Create(VTileMatrix.TileRect);
      while VTileIterator.Next(VTile) do begin
        VElement := VTileMatrix.GetElementByTile(VTile);
        if VElement <> nil then begin
          if not VElement.IsRedyWasShown then begin
            VDstRect := VLocalConverter.MapRect2LocalRect(VElement.LocalConverter.GetRectInMapPixel, rrOutside);
            Layer.Changed(VDstRect);
          end;
        end;
      end;
    end;
  end;
end;

procedure TTiledLayerWithThreadBase.PaintLayer(ABuffer: TBitmap32);
var
  VTileMatrix: ITileMatrix;
  VLocalConverter: ILocalCoordConverter;
begin
  VLocalConverter := FView.GetStatic;
  VTileMatrix := GetTileMatrix;
  if (VLocalConverter <> nil) and (VTileMatrix <> nil) then begin
    PaintLayerFromTileMatrix(ABuffer, VLocalConverter, VTileMatrix);
  end;
end;

procedure TTiledLayerWithThreadBase.PaintLayerFromTileMatrix(
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
  VDstSize: TPoint;
  VCounterContext: TInternalPerformanceCounterContext;
  VMapPixelRect: TDoubleRect;
  VClipedDstRect: TRect;
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
  VMapPixelRect := ALocalConverter.LocalRect2MapRectFloat(ABuffer.ClipRect);
  VConverter.CheckPixelRectFloat(VMapPixelRect, VZoom);
  VTileRectInClipRect := RectFromDoubleRect(VConverter.PixelRectFloat2TileRectFloat(VMapPixelRect, VZoom), rrOutside);
  VResampler := nil;
  try
    VTileIterator := TTileIteratorByRect.Create(VTileRectInClipRect);
    while VTileIterator.Next(VTile) do begin
      VElement := ATileMatrix.GetElementByTile(VTile);
      if VElement <> nil then begin
        VDstRect := ALocalConverter.MapRect2LocalRect(VElement.LocalConverter.GetRectInMapPixel, rrToTopLeft);
        Types.IntersectRect(VClipedDstRect, VDstRect, ABuffer.ClipRect);

        if ABuffer.MeasuringMode or not Types.EqualRect(VDstRect, VClipedDstRect) then begin
          VBitmap := VElement.GetBitmap;
        end else begin
          VBitmap := VElement.GetBitmapForShow;
        end;
        if VBitmap <> nil then begin
          if not ABuffer.MeasuringMode then begin
            VDstSize := Types.Point(VDstRect.Right - VDstRect.Left, VDstRect.Bottom - VDstRect.Top);
            if (VDstSize.X = VBitmap.Size.X) and (VDstSize.Y = VBitmap.Size.Y) then begin
              VCounterContext := FOneTilePaintSimpleCounter.StartOperation;
              try
                BlockTransferFull(
                  ABuffer,
                  VDstRect.Left,
                  VDstRect.Top,
                  VBitmap,
                  dmBlend,
                  cmBlend
                );
              finally
                FOneTilePaintSimpleCounter.FinishOperation(VCounterContext);
              end;
            end else begin
              if VResampler = nil then begin
                VResampler := TNearestResampler.Create;
              end;
              Assert(VResampler <> nil);
              VCounterContext := FOneTilePaintResizeCounter.StartOperation;
              try
                StretchTransferFull(
                  ABuffer,
                  VDstRect,
                  VBitmap,
                  VResampler,
                  dmBlend,
                  cmBlend
                );
              finally
                FOneTilePaintResizeCounter.FinishOperation(VCounterContext);
              end;
            end;
          end else begin
            ABuffer.Changed(VDstRect);
          end;
        end else begin
          ABuffer.Changed(VDstRect);
        end;
      end;
    end;
  finally
    VResampler.Free;
  end;
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

procedure TTiledLayerWithThreadBase.SetNeedUpdateLayerProvider;
begin
  FDrawTask.StopExecute;
  SetMatrixNotReady(GetTileMatrix);
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

procedure TTiledLayerWithThreadBase.SendTerminateToThreads;
begin
  inherited;
  FDrawTask.Terminate;
end;

procedure TTiledLayerWithThreadBase.StartThreads;
begin
  inherited;
  FDrawTask.Start;
  FDrawTask.StartExecute;
end;

procedure TTiledLayerWithThreadBase.UpdateTileMatrixIfNeed;
var
  VCounterContext: TInternalPerformanceCounterContext;
begin
  if FTileMatrixChangeFlag.CheckFlagAndReset then begin
    VCounterContext := FTileMatrixUpdateCounter.StartOperation;
    try
      DoUpdateTileMatrix;
    finally
      FTileMatrixUpdateCounter.FinishOperation(VCounterContext);
    end;
  end;
end;

end.
