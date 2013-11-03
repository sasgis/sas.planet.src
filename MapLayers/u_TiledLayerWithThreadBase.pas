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
  i_BitmapLayerProviderChangeable,
  i_SimpleFlag,
  i_TileMatrix,
  i_ObjectWithListener,
  i_BackgroundTask,
  i_TileMatrixChangeable,
  i_InternalPerformanceCounter,
  u_WindowLayerWithPos;

type
  TTiledLayerWithThreadBase = class(TWindowLayerBasicBase)
  private
    FTileMatrix: ITileMatrixChangeable;
    FView: ILocalCoordConverterChangeable;

    FOneTilePaintSimpleCounter: IInternalPerformanceCounter;
    FOneTilePaintResizeCounter: IInternalPerformanceCounter;

    FTileMatrixChangeFlag: ISimpleFlag;

    FLastPaintConverter: ILocalCoordConverter;
    procedure PaintLayerFromTileMatrix(
      ABuffer: TBitmap32;
      const ALocalConverter: ILocalCoordConverter;
      const ATileMatrix: ITileMatrix
    );
    procedure OnTimer;

    procedure OnScaleChange;
    procedure OnTileMatrixChange;
  protected
    procedure PaintLayer(ABuffer: TBitmap32); override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      AParentMap: TImage32;
      const APosition: ILocalCoordConverterChangeable;
      const AView: ILocalCoordConverterChangeable;
      const ATileMatrixFactory: ITileMatrixFactory;
      const ALayerProvider: IBitmapLayerProviderChangeable;
      const ASourcUpdateNotyfier: IObjectWithListener;
      const ATimerNoifier: INotifierTime;
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
  u_TileIteratorByRect,
  u_GeoFun,
  u_BitmapFunc,
  u_BackgroundTask,
  u_TileMatrixChangeableWithThread;


{ TTiledLayerWithThreadBase }

constructor TTiledLayerWithThreadBase.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const APosition: ILocalCoordConverterChangeable;
  const AView: ILocalCoordConverterChangeable;
  const ATileMatrixFactory: ITileMatrixFactory;
  const ALayerProvider: IBitmapLayerProviderChangeable;
  const ASourcUpdateNotyfier: IObjectWithListener;
  const ATimerNoifier: INotifierTime;
  const AThreadConfig: IThreadConfig
);
begin
  inherited Create(
    APerfList,
    AAppStartedNotifier,
    AAppClosingNotifier,
    TPositionedLayer.Create(AParentMap.Layers)
  );
  FView := AView;

  FOneTilePaintSimpleCounter := APerfList.CreateAndAddNewCounter('OneTilePaintSimple');
  FOneTilePaintResizeCounter := APerfList.CreateAndAddNewCounter('OneTilePaintResize');

  FTileMatrixChangeFlag := TSimpleFlagWithInterlock.Create;
  FTileMatrix :=
    TTileMatrixChangeableWithThread.Create(
      APerfList,
      AAppStartedNotifier,
      AAppClosingNotifier,
      APosition,
      ATileMatrixFactory,
      ALayerProvider,
      ASourcUpdateNotyfier,
      AThreadConfig
    );

  LinksList.Add(
    TListenerTimeCheck.Create(Self.OnTimer, 10),
    ATimerNoifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnScaleChange),
    FView.ChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnTileMatrixChange),
    FTileMatrix.ChangeNotifier
  );
end;

procedure TTiledLayerWithThreadBase.OnScaleChange;
begin
  FTileMatrixChangeFlag.SetFlag;
end;

procedure TTiledLayerWithThreadBase.OnTileMatrixChange;
begin
  FTileMatrixChangeFlag.SetFlag;
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
  if FTileMatrixChangeFlag.CheckFlagAndReset then begin
    VTileMatrix := FTileMatrix.GetStatic;
    if Visible <> Assigned(VTileMatrix) then begin
      Visible := Assigned(VTileMatrix);
    end;
    VLocalConverter := FView.GetStatic;
    if (VLocalConverter <> nil) and (VTileMatrix <> nil) then begin
      if not VLocalConverter.GetIsSameConverter(FLastPaintConverter) then begin
        if Assigned(FLastPaintConverter) then begin
          Layer.Changed(FLastPaintConverter.GetLocalRect);
        end;
        Layer.Changed(VLocalConverter.GetLocalRect);
      end else begin
        if VLocalConverter.ProjectionInfo.GetIsSameProjectionInfo(VTileMatrix.LocalConverter.ProjectionInfo) then begin
          VTileIterator := TTileIteratorByRect.Create(VTileMatrix.TileRect);
          while VTileIterator.Next(VTile) do begin
            VElement := VTileMatrix.GetElementByTile(VTile);
            if VElement <> nil then begin
              if VElement.CheckForShow then begin
                VDstRect := VLocalConverter.MapRect2LocalRect(VElement.LocalConverter.GetRectInMapPixel, rrClosest);
                Layer.Changed(VDstRect);
              end;
            end;
          end;
        end;
      end;
      FLastPaintConverter := VLocalConverter;
    end;
  end;
end;

procedure TTiledLayerWithThreadBase.PaintLayer(ABuffer: TBitmap32);
var
  VTileMatrix: ITileMatrix;
  VLocalConverter: ILocalCoordConverter;
  VOldClipRect: TRect;
  VNewClipRect: TRect;
begin
  VLocalConverter := FView.GetStatic;
  VTileMatrix := FTileMatrix.GetStatic;
  if (VLocalConverter <> nil) and (VTileMatrix <> nil) then begin
    VOldClipRect := ABuffer.ClipRect;
    if Types.IntersectRect(VNewClipRect, VOldClipRect, VLocalConverter.GetLocalRect) then begin
      ABuffer.ClipRect := VNewClipRect;
      try
        PaintLayerFromTileMatrix(ABuffer, VLocalConverter, VTileMatrix);
      finally
        ABuffer.ClipRect := VOldClipRect;
      end;
    end;
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
  VZoomDst: Byte;
  VZoomSrc: Byte;
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
  VRelativeRect: TDoubleRect;
begin
  inherited;
  VConverter := ALocalConverter.GeoConverter;
  if not VConverter.IsSameConverter(ATileMatrix.LocalConverter.GeoConverter) then begin
    Exit;
  end;
  VZoomDst := ALocalConverter.Zoom;
  VZoomSrc := ATileMatrix.LocalConverter.Zoom;
  if VZoomDst <> VZoomSrc then begin
    VMapPixelRect := ALocalConverter.LocalRect2MapRectFloat(ABuffer.ClipRect);
    VConverter.CheckPixelRectFloat(VMapPixelRect, VZoomDst);
    VRelativeRect := VConverter.PixelRectFloat2RelativeRect(VMapPixelRect, VZoomDst);
    VTileRectInClipRect := RectFromDoubleRect(VConverter.RelativeRect2TileRectFloat(VRelativeRect, VZoomSrc), rrOutside);
  end else begin
    VMapPixelRect := ALocalConverter.LocalRect2MapRectFloat(ABuffer.ClipRect);
    VConverter.CheckPixelRectFloat(VMapPixelRect, VZoomDst);
    VTileRectInClipRect := RectFromDoubleRect(VConverter.PixelRectFloat2TileRectFloat(VMapPixelRect, VZoomSrc), rrOutside);
  end;
  if IntersectRect(VTileRectInClipRect, VTileRectInClipRect, ATileMatrix.TileRect) then begin
    VResampler := nil;
    try
      VTileIterator := TTileIteratorByRect.Create(VTileRectInClipRect);
      while VTileIterator.Next(VTile) do begin
        VElement := ATileMatrix.GetElementByTile(VTile);
        if VElement <> nil then begin
          if VZoomDst <> VZoomSrc then begin
            VRelativeRect :=
              VConverter.PixelRectFloat2RelativeRect(
                VElement.LocalConverter.GetRectInMapPixelFloat,
                VZoomSrc
              );
            VDstRect :=
              RectFromDoubleRect(
                ALocalConverter.MapRectFloat2LocalRectFloat(VConverter.RelativeRect2PixelRectFloat(VRelativeRect, VZoomDst)),
                rrClosest
              );
          end else begin
            VDstRect :=
              ALocalConverter.MapRect2LocalRect(
                VElement.LocalConverter.GetRectInMapPixel,
                rrClosest
              );
          end;

          Types.IntersectRect(VClipedDstRect, VDstRect, ABuffer.ClipRect);

          VBitmap := VElement.GetBitmap;
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
end;

end.
