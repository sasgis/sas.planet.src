{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
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

unit u_TiledMapLayer;

interface

uses
  Types,
  GR32,
  GR32_Image,
  GR32_Layers,
  i_HashFunction,
  i_Notifier,
  i_NotifierTime,
  i_NotifierOperation,
  i_LocalCoordConverter,
  i_LocalCoordConverterChangeable,
  i_SimpleFlag,
  i_BitmapTileMatrix,
  i_BitmapTileMatrixChangeable,
  i_InternalPerformanceCounter,
  i_HashTileMatrixBuilder,
  u_WindowLayerAbstract;

type
  TTiledMapLayer = class(TWindowLayerAbstract)
  private
    FLayer: TPositionedLayer;
    FTileMatrix: IBitmapTileMatrixChangeable;
    FView: ILocalCoordConverterChangeable;
    FDebugName: string;

    FShownIdMatrix: IHashTileMatrixBuilder;
    FOnPaintCounter: IInternalPerformanceCounter;
    FOneTilePaintSimpleCounter: IInternalPerformanceCounter;
    FOneTilePaintResizeCounter: IInternalPerformanceCounter;

    FTileMatrixChangeFlag: ISimpleFlag;

    FLastPaintConverter: ILocalCoordConverter;
    procedure OnPaintLayer(
      Sender: TObject;
      Buffer: TBitmap32
    );
    procedure PaintLayerFromTileMatrix(
      ABuffer: TBitmap32;
      const ALocalConverter: ILocalCoordConverter;
      const ATileMatrix: IBitmapTileMatrix
    );
    procedure OnTimer;

    procedure OnScaleChange;
    procedure OnTileMatrixChange;
  protected
    procedure StartThreads; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      AParentMap: TImage32;
      const AHashFunction: IHashFunction;
      const AView: ILocalCoordConverterChangeable;
      const ATileMatrix: IBitmapTileMatrixChangeable;
      const ATimerNoifier: INotifierTime;
      const ADebugName: string
    );
  end;

implementation

uses
  GR32_Resamplers,
  t_GeoTypes,
  t_Hash,
  i_TileIterator,
  i_CoordConverter,
  i_Bitmap32Static,
  u_SimpleFlagWithInterlock,
  u_ListenerByEvent,
  u_ListenerTime,
  u_TileIteratorByRect,
  u_HashTileMatrixBuilder,
  u_GeoFunc,
  u_BitmapFunc;


{ TTiledMapLayer }

constructor TTiledMapLayer.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const AHashFunction: IHashFunction;
  const AView: ILocalCoordConverterChangeable;
  const ATileMatrix: IBitmapTileMatrixChangeable;
  const ATimerNoifier: INotifierTime;
  const ADebugName: string
);
begin
  Assert(Assigned(AHashFunction));
  inherited Create(
    AAppStartedNotifier,
    AAppClosingNotifier
  );
  FLayer := TPositionedLayer.Create(AParentMap.Layers);
  FLayer.Visible := False;
  FLayer.MouseEvents := False;
  FView := AView;
  FTileMatrix := ATileMatrix;
  FDebugName := ADebugName;

  FOnPaintCounter := APerfList.CreateAndAddNewCounter('OnPaint');
  FOneTilePaintSimpleCounter := APerfList.CreateAndAddNewCounter('OneTilePaintSimple');
  FOneTilePaintResizeCounter := APerfList.CreateAndAddNewCounter('OneTilePaintResize');

  FShownIdMatrix := THashTileMatrixBuilder.Create(AHashFunction);
  FTileMatrixChangeFlag := TSimpleFlagWithInterlock.Create;

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

procedure TTiledMapLayer.OnPaintLayer(
  Sender: TObject;
  Buffer: TBitmap32
);
var
  VTileMatrix: IBitmapTileMatrix;
  VLocalConverter: ILocalCoordConverter;
  VCounterContext: TInternalPerformanceCounterContext;
  VOldClipRect: TRect;
  VNewClipRect: TRect;
begin
  VLocalConverter := FView.GetStatic;
  if Assigned(VLocalConverter) then begin
    VTileMatrix := FTileMatrix.GetStatic;
    if Assigned(VTileMatrix) then begin
      VCounterContext := FOnPaintCounter.StartOperation;
      try
        VOldClipRect := Buffer.ClipRect;
        if Types.IntersectRect(VNewClipRect, VOldClipRect, VLocalConverter.GetLocalRect) then begin
          Buffer.ClipRect := VNewClipRect;
          try
            PaintLayerFromTileMatrix(Buffer, VLocalConverter, VTileMatrix);
          finally
            Buffer.ClipRect := VOldClipRect;
          end;
        end;
      finally
        FOnPaintCounter.FinishOperation(VCounterContext);
      end;
    end else begin
      Buffer.Changed;
    end;
  end;
end;

procedure TTiledMapLayer.OnScaleChange;
begin
  FTileMatrixChangeFlag.SetFlag;
end;

procedure TTiledMapLayer.OnTileMatrixChange;
begin
  FTileMatrixChangeFlag.SetFlag;
end;

procedure TTiledMapLayer.OnTimer;
var
  VLocalConverter: ILocalCoordConverter;
  VTileMatrix: IBitmapTileMatrix;
  VTileIterator: ITileIterator;
  VTile: TPoint;
  VDstRect: TRect;
  VShownId: THashValue;
  VMapRect: TRect;
  VBitmap: IBitmap32Static;
  VReadyId: THashValue;
begin
  if FTileMatrixChangeFlag.CheckFlagAndReset then begin
    VTileMatrix := FTileMatrix.GetStatic;
    if Assigned(VTileMatrix) then begin
      if not FLayer.Visible then begin
        FLayer.Visible := True;
      end;
      VLocalConverter := FView.GetStatic;
      if Assigned(VLocalConverter) then begin
        if not VLocalConverter.GetIsSameConverter(FLastPaintConverter) then begin
          FLayer.Location := FloatRect(VLocalConverter.GetLocalRect);
          FLayer.Changed(VLocalConverter.GetLocalRect);
          FShownIdMatrix.SetRectWithReset(VTileMatrix.TileRect, 0);
        end else begin
          if VLocalConverter.ProjectionInfo.GetIsSameProjectionInfo(VTileMatrix.TileRect.ProjectionInfo) then begin
            VTileIterator := TTileIteratorByRect.Create(VTileMatrix.TileRect.Rect);
            while VTileIterator.Next(VTile) do begin
              VShownId := FShownIdMatrix.Tiles[VTile];
              VBitmap := VTileMatrix.GetElementByTile(VTile);
              if Assigned(VBitmap) then begin
                VReadyId := VBitmap.Hash;
              end else begin
                VReadyId := 0;
              end;
              if VReadyId <> VShownId then begin
                VMapRect :=
                  VLocalConverter.GeoConverter.TilePos2PixelRect(
                    VTile,
                    VLocalConverter.Zoom
                  );
                VDstRect := VLocalConverter.MapRect2LocalRect(VMapRect, rrClosest);
                FShownIdMatrix.Tiles[VTile] := VReadyId;
                FLayer.Changed(VDstRect);
              end;
            end;
          end;
        end;
        FLastPaintConverter := VLocalConverter;
      end;
    end else begin
      if FLayer.Visible then begin
        FLayer.Visible := False;
      end;
    end;
  end;
end;

procedure TTiledMapLayer.PaintLayerFromTileMatrix(
  ABuffer: TBitmap32;
  const ALocalConverter: ILocalCoordConverter;
  const ATileMatrix: IBitmapTileMatrix
);
var
  VTileRectInClipRect: TRect;
  VConverter: ICoordConverter;
  VZoomDst: Byte;
  VZoomSrc: Byte;
  VTileIterator: TTileIteratorByRectRecord;
  VTile: TPoint;
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
  if not VConverter.IsSameConverter(ATileMatrix.TileRect.ProjectionInfo.GeoConverter) then begin
    Exit;
  end;
  VZoomDst := ALocalConverter.Zoom;
  VZoomSrc := ATileMatrix.TileRect.ProjectionInfo.Zoom;
  if VZoomDst <> VZoomSrc then begin
    VMapPixelRect := ALocalConverter.LocalRect2MapRectFloat(ABuffer.ClipRect);
    VConverter.ValidatePixelRectFloat(VMapPixelRect, VZoomDst);
    VRelativeRect := VConverter.PixelRectFloat2RelativeRect(VMapPixelRect, VZoomDst);
    VTileRectInClipRect := RectFromDoubleRect(VConverter.RelativeRect2TileRectFloat(VRelativeRect, VZoomSrc), rrOutside);
  end else begin
    VMapPixelRect := ALocalConverter.LocalRect2MapRectFloat(ABuffer.ClipRect);
    VConverter.ValidatePixelRectFloat(VMapPixelRect, VZoomDst);
    VTileRectInClipRect := RectFromDoubleRect(VConverter.PixelRectFloat2TileRectFloat(VMapPixelRect, VZoomSrc), rrOutside);
  end;
  if Types.IntersectRect(VTileRectInClipRect, VTileRectInClipRect, ATileMatrix.TileRect.Rect) then begin
    VResampler := nil;
    try
      VTileIterator.Init(VTileRectInClipRect);
      while VTileIterator.Next(VTile) do begin
        VBitmap := ATileMatrix.GetElementByTile(VTile);
        if Assigned(VBitmap) then begin
          if VZoomDst <> VZoomSrc then begin
            VRelativeRect := VConverter.TilePos2RelativeRect(VTile, VZoomSrc);
            VDstRect :=
              RectFromDoubleRect(
                ALocalConverter.MapRectFloat2LocalRectFloat(VConverter.RelativeRect2PixelRectFloat(VRelativeRect, VZoomDst)),
                rrClosest
              );
          end else begin
            VDstRect :=
              ALocalConverter.MapRect2LocalRect(
                VConverter.TilePos2PixelRect(VTile, VZoomSrc),
                rrClosest
              );
          end;

          Types.IntersectRect(VClipedDstRect, VDstRect, ABuffer.ClipRect);

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
        end;
      end;
    finally
      VResampler.Free;
    end;
  end;
end;

procedure TTiledMapLayer.StartThreads;
begin
  inherited;
  FLayer.OnPaint := OnPaintLayer;
end;

end.
