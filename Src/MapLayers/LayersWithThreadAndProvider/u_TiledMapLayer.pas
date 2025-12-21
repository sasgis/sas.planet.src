{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_TiledMapLayer;

{$IFDEF DEBUG}
  {.$DEFINE ENABLE_TILED_MAP_LAYER_LOG}
{$ENDIF}

interface

uses
  Windows,
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
  i_MainFormState,
  u_WindowLayerAbstract;

type
  TTiledMapLayer = class(TWindowLayerAbstract)
  private
    FLayer: TPositionedLayer;
    FTileMatrix: IBitmapTileMatrixChangeable;
    FView: ILocalCoordConverterChangeable;
    FMainFormState: IMainFormState;
    FDebugName: string;

    FShownIdMatrix: IHashTileMatrixBuilder;
    FOnPaintCounter: IInternalPerformanceCounter;
    FOneTilePaintSimpleCounter: IInternalPerformanceCounter;
    FOneTilePaintResizeCounter: IInternalPerformanceCounter;

    FZoomingFlag: ISimpleFlag;
    FViewChangeFlag: ISimpleFlag;
    FTileMatrixChangeFlag: ISimpleFlag;

    FZoomingTileMatrixKeepAlive: Cardinal;
    FZoomingTileMatrix: IBitmapTileMatrix;

    FLastPaintConverter: ILocalCoordConverter;

    procedure OnPaintLayer(
      Sender: TObject;
      Buffer: TBitmap32
    );
    procedure PaintLayerFromTileMatrix(
      const ABuffer: TBitmap32;
      const ALocalConverter: ILocalCoordConverter;
      const ATileMatrix: IBitmapTileMatrix
    );

    procedure OnTimer;
    procedure OnMainFormStateChange;
    procedure OnViewChange;
    procedure OnTileMatrixChange;
  protected
    procedure StartThreads; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      const AParentMap: TImage32;
      const AHashFunction: IHashFunction;
      const AView: ILocalCoordConverterChangeable;
      const ATileMatrix: IBitmapTileMatrixChangeable;
      const AMainFormState: IMainFormState;
      const AGuiSyncronizedTimerNotifier: INotifierTime;
      const ADebugName: string
    );
  end;

implementation

uses
  Math,
  GR32_Resamplers,
  t_GeoTypes,
  t_Hash,
  i_TileIterator,
  i_Projection,
  i_Bitmap32Static,
  {$IFDEF ENABLE_TILED_MAP_LAYER_LOG}
  u_DebugLogger,
  {$ENDIF}
  u_SimpleFlagWithInterlock,
  u_ListenerByEvent,
  u_ListenerTime,
  u_TileIteratorByRect,
  u_HashTileMatrixBuilder,
  u_GeoFunc,
  u_BitmapFunc;

const
  CFakeHashValue: THashValue = MaxInt;
  CZoomingMatrixKeepAliveTimeOut: Cardinal = 350; // milliseconds

{ TTiledMapLayer }

constructor TTiledMapLayer.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  const AParentMap: TImage32;
  const AHashFunction: IHashFunction;
  const AView: ILocalCoordConverterChangeable;
  const ATileMatrix: IBitmapTileMatrixChangeable;
  const AMainFormState: IMainFormState;
  const AGuiSyncronizedTimerNotifier: INotifierTime;
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
  FMainFormState := AMainFormState;
  FDebugName := ADebugName;

  FOnPaintCounter := APerfList.CreateAndAddNewCounter('OnPaint');
  FOneTilePaintSimpleCounter := APerfList.CreateAndAddNewCounter('OneTilePaintSimple');
  FOneTilePaintResizeCounter := APerfList.CreateAndAddNewCounter('OneTilePaintResize');

  FShownIdMatrix := THashTileMatrixBuilder.Create(AHashFunction);

  FZoomingFlag := TSimpleFlagWithInterlock.Create;
  FViewChangeFlag := TSimpleFlagWithInterlock.Create;
  FTileMatrixChangeFlag := TSimpleFlagWithInterlock.Create;

  LinksList.Add(
    TListenerTimeCheck.Create(Self.OnTimer, 25),
    AGuiSyncronizedTimerNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnViewChange),
    FView.ChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnTileMatrixChange),
    FTileMatrix.ChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnMainFormStateChange),
    FMainFormState.ChangeNotifier
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
  {$IFDEF ENABLE_TILED_MAP_LAYER_LOG}
  GLog.Write(Self, '%s: OnPaintLayer (IsMesuring = %s)', [FDebugName, GLog.ToStr(Buffer.MeasuringMode)]);
  {$ENDIF}
  VLocalConverter := FView.GetStatic;
  if Assigned(VLocalConverter) then begin
    FTileMatrixChangeFlag.CheckFlagAndReset;
    VTileMatrix := FTileMatrix.GetStatic;
    if Assigned(VTileMatrix) or Assigned(FZoomingTileMatrix) then begin
      VCounterContext := FOnPaintCounter.StartOperation;
      Buffer.BeginUpdate;
      try
        VOldClipRect := Buffer.ClipRect;
        if Types.IntersectRect(VNewClipRect, VOldClipRect, VLocalConverter.GetLocalRect) then begin
          Buffer.ClipRect := VNewClipRect;
          try
            if Assigned(FZoomingTileMatrix) then begin
              if not Assigned(VTileMatrix) or (VTileMatrix.Hash <> FZoomingTileMatrix.Hash) then begin
                {$IFDEF ENABLE_TILED_MAP_LAYER_LOG}
                GLog.Write(Self, '%s: Painting Zooming TileMatrix', [FDebugName]);
                {$ENDIF}
                PaintLayerFromTileMatrix(Buffer, VLocalConverter, FZoomingTileMatrix);
              end;
            end;
            if Assigned(VTileMatrix) then begin
              {$IFDEF ENABLE_TILED_MAP_LAYER_LOG}
              GLog.Write(Self, '%s: Painting Regular TileMatrix', [FDebugName]);
              {$ENDIF}
              PaintLayerFromTileMatrix(Buffer, VLocalConverter, VTileMatrix);
            end;
          finally
            Buffer.ClipRect := VOldClipRect;
          end;
        end;
      finally
        Buffer.EndUpdate;
        FOnPaintCounter.FinishOperation(VCounterContext);
      end;
    end else begin
      {$IFDEF ENABLE_TILED_MAP_LAYER_LOG}
      GLog.Write(Self, '%s: <<<  Matrix is empty  >>>', [FDebugName]);
      {$ENDIF}
      Buffer.Changed;
    end;
  end;
end;

procedure TTiledMapLayer.OnMainFormStateChange;
begin
  if FLastPaintConverter = nil then begin
    Exit;
  end;
  if FMainFormState.IsMapZooming then begin
    FZoomingFlag.SetFlag;
    FZoomingTileMatrix := FTileMatrix.GetStatic;
    {$IFDEF ENABLE_TILED_MAP_LAYER_LOG}
    GLog.Write(Self, '%s: ZoomingTileMatrix assigned', [FDebugName]);
    {$ENDIF}
  end else
  if FZoomingFlag.CheckFlagAndReset then begin
    FZoomingTileMatrixKeepAlive := GetTickCount + CZoomingMatrixKeepAliveTimeOut;
    FTileMatrixChangeFlag.SetFlag;
    {$IFDEF ENABLE_TILED_MAP_LAYER_LOG}
    GLog.Write(Self, '%s: Zooming keep-alive ON at %d until %d', [FDebugName, FZoomingTileMatrixKeepAlive - CZoomingMatrixKeepAliveTimeOut, FZoomingTileMatrixKeepAlive]);
    {$ENDIF}
  end;
end;

procedure TTiledMapLayer.OnViewChange;
begin
  if not FZoomingFlag.CheckFlag then begin
    FViewChangeFlag.SetFlag;
  end;
end;

procedure TTiledMapLayer.OnTileMatrixChange;
begin
  FTileMatrixChangeFlag.SetFlag;
end;

procedure TTiledMapLayer.OnTimer;
var
  VIsAlive: Boolean;
  VLocalConverter: ILocalCoordConverter;
  VTileMatrix: IBitmapTileMatrix;
  VTileIterator: ITileIterator;
  VTile: TPoint;
  VShownId: THashValue;
  VBitmap: IBitmap32Static;
  VReadyId: THashValue;
  VIsChanged: Boolean;
begin
  if FZoomingFlag.CheckFlag then begin
    Exit;
  end;

  VIsChanged := False;

  if FZoomingTileMatrixKeepAlive <> 0 then begin
    // Although zooming is complete, FTileMatrix may still be processing tiles.
    // We'll keep FZoomsTileMatrix alive until the user changes the View, or FTileMatrix completes its work.
    // Since there are no "finished" notifications to rely on, we'll assume the job is done
    // if we don't receive any update notifications within the CZoomingMatrixKeepAliveTimeOut period.
    VIsAlive := FZoomingTileMatrixKeepAlive > GetTickCount;
    if not VIsAlive and FTileMatrixChangeFlag.CheckFlag then begin
      VIsAlive := True;
      FZoomingTileMatrixKeepAlive := GetTickCount + CZoomingMatrixKeepAliveTimeOut;
      {$IFDEF ENABLE_TILED_MAP_LAYER_LOG}
      GLog.Write(Self, '%s: Zooming keep-alive ON at %d until %d', [FDebugName, FZoomingTileMatrixKeepAlive - CZoomingMatrixKeepAliveTimeOut, FZoomingTileMatrixKeepAlive]);
      {$ENDIF}
    end;
    if not VIsAlive or FViewChangeFlag.CheckFlag then begin
      FZoomingTileMatrixKeepAlive := 0;
      FZoomingTileMatrix := nil;
      VIsChanged := True; // force update layer
      {$IFDEF ENABLE_TILED_MAP_LAYER_LOG}
      GLog.Write(Self, '%s: Zooming keep-alive OFF at %d', [FDebugName, GetTickCount]);
      {$ENDIF}
    end;
  end;

  if FTileMatrixChangeFlag.CheckFlagAndReset or FViewChangeFlag.CheckFlagAndReset then begin
    VTileMatrix := FTileMatrix.GetStatic;
    if Assigned(VTileMatrix) then begin
      if not FLayer.Visible then begin
        FLayer.Visible := True;
      end;
      VLocalConverter := FView.GetStatic;
      if Assigned(VLocalConverter) then begin
        if not VLocalConverter.GetIsSameConverter(FLastPaintConverter) then begin
          FShownIdMatrix.SetRectWithReset(VTileMatrix.TileRect, 0);
          FLayer.Location := FloatRect(VLocalConverter.GetLocalRect);
          VIsChanged := True;
        end else begin
          if VLocalConverter.Projection.IsSame(VTileMatrix.TileRect.Projection) then begin
            VTileIterator := TTileIteratorByRect.Create(VTileMatrix.TileRect);
            while VTileIterator.Next(VTile) do begin
              VShownId := FShownIdMatrix.Tiles[VTile];
              VBitmap := VTileMatrix.GetElementByTile(VTile);
              if Assigned(VBitmap) then begin
                VReadyId := VBitmap.Hash;
              end else begin
                VReadyId := CFakeHashValue;
              end;
              if VReadyId <> VShownId then begin
                FShownIdMatrix.Tiles[VTile] := VReadyId;
                VIsChanged := True;
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
  if VIsChanged then begin
    {$IFDEF ENABLE_TILED_MAP_LAYER_LOG}
    GLog.Write(Self, '%s: Update OnTimer', [FDebugName]);
    {$ENDIF}
    FLayer.Update;
  end;
end;

procedure TTiledMapLayer.PaintLayerFromTileMatrix(
  const ABuffer: TBitmap32;
  const ALocalConverter: ILocalCoordConverter;
  const ATileMatrix: IBitmapTileMatrix
);
var
  {$IFDEF ENABLE_TILED_MAP_LAYER_LOG}
  VTotalCount, VBlendCount, VStretchCount: Integer;
  {$ENDIF}
  VTileRectInClipRect: TRect;
  VProjectionDst: IProjection;
  VProjectionSrc: IProjection;
  VSameProjection: Boolean;
  VTileIterator: TTileIteratorByRectRecord;
  VTile: TPoint;
  VBitmap: IBitmap32Static;
  VResampler: TCustomResampler;
  VDstRect: TRect;
  VDstSize: TPoint;
  VCounterContext: TInternalPerformanceCounterContext;
  VMapPixelRect: TDoubleRect;
  VRelativeRect: TDoubleRect;
begin
  {$IFDEF ENABLE_TILED_MAP_LAYER_LOG}
  VTotalCount := 0;
  VBlendCount := 0;
  VStretchCount := 0;
  {$ENDIF}

  VProjectionDst := ALocalConverter.Projection;
  VProjectionSrc := ATileMatrix.TileRect.Projection;
  VSameProjection := VProjectionDst.IsSame(VProjectionSrc);

  if not VSameProjection then begin
    VMapPixelRect := ALocalConverter.LocalRect2MapRectFloat(ABuffer.ClipRect);
    VProjectionDst.ValidatePixelRectFloat(VMapPixelRect);
    VRelativeRect := VProjectionDst.PixelRectFloat2RelativeRect(VMapPixelRect);
    VTileRectInClipRect := RectFromDoubleRect(VProjectionSrc.RelativeRect2TileRectFloat(VRelativeRect), rrOutside);
  end else begin
    if not VProjectionDst.ProjectionType.IsSame(VProjectionSrc.ProjectionType) then begin
      Exit;
    end;
    VMapPixelRect := ALocalConverter.LocalRect2MapRectFloat(ABuffer.ClipRect);
    VProjectionDst.ValidatePixelRectFloat(VMapPixelRect);
    VTileRectInClipRect := RectFromDoubleRect(VProjectionSrc.PixelRectFloat2TileRectFloat(VMapPixelRect), rrOutside);
  end;

  if Types.IntersectRect(VTileRectInClipRect, VTileRectInClipRect, ATileMatrix.TileRect.Rect) then begin
    VResampler := nil;
    try
      VTileIterator.Init(VTileRectInClipRect);
      while VTileIterator.Next(VTile) do begin
        VBitmap := ATileMatrix.GetElementByTile(VTile);
        if Assigned(VBitmap) then begin
          {$IFDEF ENABLE_TILED_MAP_LAYER_LOG}
          Inc(VTotalCount);
          {$ENDIF}
          if not VSameProjection then begin
            VRelativeRect := VProjectionSrc.TilePos2RelativeRect(VTile);
            VDstRect :=
              RectFromDoubleRect(
                ALocalConverter.MapRectFloat2LocalRectFloat(VProjectionDst.RelativeRect2PixelRectFloat(VRelativeRect)),
                rrClosest
              );
          end else begin
            VDstRect :=
              ALocalConverter.MapRect2LocalRect(
                VProjectionSrc.TilePos2PixelRect(VTile),
                rrClosest
              );
          end;
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
              {$IFDEF ENABLE_TILED_MAP_LAYER_LOG}
              Inc(VBlendCount);
              {$ENDIF}
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
              {$IFDEF ENABLE_TILED_MAP_LAYER_LOG}
              Inc(VStretchCount);
              {$ENDIF}
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
  {$IFDEF ENABLE_TILED_MAP_LAYER_LOG}
  GLog.Write(Self, '%s: Paint %d tiles (Blend: %d; Stretch: %d)', [FDebugName, VTotalCount, VBlendCount, VStretchCount]);
  {$ENDIF}
end;

procedure TTiledMapLayer.StartThreads;
begin
  inherited;
  FLayer.OnPaint := OnPaintLayer;
end;

end.
