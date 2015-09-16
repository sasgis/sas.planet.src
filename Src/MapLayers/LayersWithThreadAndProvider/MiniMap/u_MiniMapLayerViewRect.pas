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

unit u_MiniMapLayerViewRect;

interface

uses
  Types,
  Classes,
  Controls,
  GR32,
  GR32_Image,
  GR32_Layers,
  t_GeoTypes,
  i_NotifierTime,
  i_NotifierOperation,
  i_InternalPerformanceCounter,
  i_SimpleFlag,
  i_LocalCoordConverter,
  i_LocalCoordConverterChangeable,
  i_MiniMapLayerConfig,
  i_ViewPortState,
  i_PopUp,
  u_WindowLayerAbstract;

type
  TMiniMapLayerViewRect = class(TWindowLayerAbstract)
  private
    FParentMap: TImage32;
    FLayer: TPositionedLayer;
    FLocationConfig: IMiniMapLayerLocationConfig;
    FPosition: ILocalCoordConverterChangeable;
    FViewPortState: IViewPortState;
    FPopupMenu: IPopUp;

    FPosMoved: Boolean;
    FViewRectMoveDelta: TDoublePoint;
    FLayerChangeFlag: ISimpleFlag;
    FNeedUpdateLayerVisibilityFlag: ISimpleFlag;
    FNeedUpdateLayerLocationFlag: ISimpleFlag;
    FOnPaintCounter: IInternalPerformanceCounter;

    procedure OnPaintLayer(
      Sender: TObject;
      Buffer: TBitmap32
    );
    procedure OnTimer;
    procedure OnConfigChange;
    procedure OnViewChange;
    procedure OnPosChange;
    procedure DrawMainViewRect(
      ABuffer: TBitmap32;
      const AMiniMapConverter: ILocalCoordConverter;
      const AViewConverter: ILocalCoordConverter
    );

    procedure LayerMouseDown(
      Sender: TObject;
      Button: TMouseButton;
      Shift: TShiftState;
      X, Y: Integer
    );
    procedure LayerMouseUP(
      Sender: TObject;
      Button: TMouseButton;
      Shift: TShiftState;
      X, Y: Integer
    );
    procedure LayerMouseMove(
      Sender: TObject;
      Shift: TShiftState;
      X, Y: Integer
    );
  protected
    procedure StartThreads; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      AParentMap: TImage32;
      const AViewPortState: IViewPortState;
      const APosition: ILocalCoordConverterChangeable;
      const ATimerNoifier: INotifierTime;
      const APopupMenu: IPopUp;
      const ALocationConfig: IMiniMapLayerLocationConfig
    );
  end;

implementation

uses
  Math,
  i_ProjectionInfo,
  u_ListenerByEvent,
  u_SimpleFlagWithInterlock,
  u_ListenerTime,
  u_GeoFunc;

{ TMiniMapLayerTopBorder }

constructor TMiniMapLayerViewRect.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier, AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const AViewPortState: IViewPortState;
  const APosition: ILocalCoordConverterChangeable;
  const ATimerNoifier: INotifierTime;
  const APopupMenu: IPopUp;
  const ALocationConfig: IMiniMapLayerLocationConfig
);
begin
  inherited Create(
    AAppStartedNotifier,
    AAppClosingNotifier
  );
  FLocationConfig := ALocationConfig;
  FParentMap := AParentMap;
  FPosition := APosition;
  FViewPortState := AViewPortState;
  FPopupMenu := APopupMenu;

  FLayer := TPositionedLayer.Create(AParentMap.Layers);
  FLayer.Visible := False;
  FLayer.MouseEvents := False;
  FLayer.OnMouseDown := LayerMouseDown;
  FLayer.OnMouseUp := LayerMouseUP;
  FLayer.OnMouseMove := LayerMouseMove;

  FLayerChangeFlag := TSimpleFlagWithInterlock.Create;
  FNeedUpdateLayerVisibilityFlag := TSimpleFlagWithInterlock.Create;
  FNeedUpdateLayerLocationFlag := TSimpleFlagWithInterlock.Create;
  FOnPaintCounter := APerfList.CreateAndAddNewCounter('OnPaint');

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FLocationConfig.ChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnPosChange),
    FPosition.ChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnViewChange),
    FViewPortState.View.ChangeNotifier
  );
  LinksList.Add(
    TListenerTimeCheck.Create(Self.OnTimer, 100),
    ATimerNoifier
  );
end;

procedure TMiniMapLayerViewRect.DrawMainViewRect(
  ABuffer: TBitmap32;
  const AMiniMapConverter: ILocalCoordConverter;
  const AViewConverter: ILocalCoordConverter
);
var
  VViewMapSourceRect: TDoubleRect;
  VProjectionSource: IProjection;
  VMiniMapRect: TDoubleRect;
  VBitmapRect: TDoubleRect;
  VLonLatRect: TDoubleRect;
  VProjectionMiniMap: IProjection;
  VZoomDelta: Integer;
  VFillColor: TColor32;
  VBorderColor: TColor32;
  VDrawRect: TRect;
begin
  VViewMapSourceRect := AViewConverter.GetRectInMapPixelFloat;
  VProjectionSource := AViewConverter.ProjectionInfo;
  VProjectionSource.ValidatePixelRectFloat(VViewMapSourceRect);
  VLonLatRect := VProjectionSource.PixelRectFloat2LonLatRect(VViewMapSourceRect);
  VProjectionMiniMap := AMiniMapConverter.ProjectionInfo;
  VProjectionMiniMap.ProjectionType.ValidateLonLatRect(VLonLatRect);
  VBitmapRect := AMiniMapConverter.LonLatRect2LocalRectFloat(VLonLatRect);

  VBitmapRect.Left := VBitmapRect.Left + FViewRectMoveDelta.X;
  VBitmapRect.Top := VBitmapRect.Top + FViewRectMoveDelta.Y;
  VBitmapRect.Right := VBitmapRect.Right + FViewRectMoveDelta.X;
  VBitmapRect.Bottom := VBitmapRect.Bottom + FViewRectMoveDelta.Y;

  VMiniMapRect := DoubleRect(AMiniMapConverter.GetLocalRect);
  if IsIntersecProjectedRect(VBitmapRect, VMiniMapRect) then begin
    if
      (VMiniMapRect.Left < VBitmapRect.Left) or
      (VMiniMapRect.Top < VBitmapRect.Top) or
      (VMiniMapRect.Right > VBitmapRect.Right) or
      (VMiniMapRect.Bottom > VBitmapRect.Bottom)
    then begin
      VZoomDelta := VProjectionSource.Zoom - AMiniMapConverter.ProjectionInfo.Zoom;
      VFillColor := SetAlpha(clWhite32, (VZoomDelta) * 35);
      VBorderColor := SetAlpha(clNavy32, (VZoomDelta) * 43);
      VDrawRect := RectFromDoubleRect(VBitmapRect, rrClosest);
      ABuffer.FillRectTS(VDrawRect, VFillColor);
      ABuffer.FrameRectTS(VDrawRect, VBorderColor);
      ABuffer.FrameRectTS(
        VDrawRect.Left - 1,
        VDrawRect.Top + 1,
        VDrawRect.Right + 1,
        VDrawRect.Bottom - 1,
        VBorderColor
      );
      ABuffer.FrameRectTS(
        VDrawRect.Left + 1,
        VDrawRect.Top - 1,
        VDrawRect.Right - 1,
        VDrawRect.Bottom + 1,
        VBorderColor
      );
    end;
  end;
end;

procedure TMiniMapLayerViewRect.LayerMouseDown(
  Sender: TObject;
  Button: TMouseButton;
  Shift: TShiftState;
  X, Y: Integer
);
var
  VVisibleCenter: TDoublePoint;
  VLocalConverter: ILocalCoordConverter;
begin
  case Button of
    mbRight: begin
      FPopupMenu.PopUp;
    end;
    mbLeft: begin
      VLocalConverter := FPosition.GetStatic;
      VVisibleCenter := RectCenter(VLocalConverter.GetLocalRect);
      FPosMoved := True;
      FViewRectMoveDelta := DoublePoint(X - VVisibleCenter.X, Y - VVisibleCenter.Y);
      FLayer.Changed;
    end;
  end;
end;

procedure TMiniMapLayerViewRect.LayerMouseMove(
  Sender: TObject;
  Shift: TShiftState;
  X, Y: Integer
);
var
  VVisibleCenter: TDoublePoint;
  VLocalRect: TRect;
  VLocalConverter: ILocalCoordConverter;
begin
  if FPosMoved then begin
    VLocalConverter := FPosition.GetStatic;
    VLocalRect := VLocalConverter.GetLocalRect;
    VVisibleCenter := RectCenter(VLocalRect);

    if X < VLocalRect.Left then begin
      FViewRectMoveDelta.X := VLocalRect.Left - VVisibleCenter.X;
    end else if X > VLocalRect.Right then begin
      FViewRectMoveDelta.X := VLocalRect.Right - VVisibleCenter.X;
    end else begin
      FViewRectMoveDelta.X := X - VVisibleCenter.X;
    end;
    if Y < VLocalRect.Top then begin
      FViewRectMoveDelta.Y := VLocalRect.Top - VVisibleCenter.Y;
    end else if Y > VLocalRect.Bottom then begin
      FViewRectMoveDelta.Y := VLocalRect.Bottom - VVisibleCenter.Y;
    end else begin
      FViewRectMoveDelta.Y := Y - VVisibleCenter.Y;
    end;

    FLayer.Changed;
  end;
end;

procedure TMiniMapLayerViewRect.LayerMouseUP(
  Sender: TObject;
  Button: TMouseButton;
  Shift: TShiftState;
  X, Y: Integer
);
var
  VLocalConverter: ILocalCoordConverter;
  VProjection: IProjection;
  VMapPoint: TDoublePoint;
  VLonLat: TDoublePoint;
begin
  if FPosMoved then begin
    if FLayer.HitTest(X, Y) then begin
      VLocalConverter := FPosition.GetStatic;
      VProjection := VLocalConverter.ProjectionInfo;

      VMapPoint := VLocalConverter.LocalPixel2MapPixelFloat(Point(X, Y));
      VProjection.ValidatePixelPosFloatStrict(VMapPoint, False);
      VLonLat := VProjection.PixelPosFloat2LonLat(VMapPoint);
      FViewRectMoveDelta := DoublePoint(0, 0);

      FViewPortState.ChangeLonLat(VLonLat);
    end else begin
      FViewRectMoveDelta := DoublePoint(0, 0);
      FLayer.Changed;
    end;
  end;
  FPosMoved := False;
end;

procedure TMiniMapLayerViewRect.OnConfigChange;
begin
  FNeedUpdateLayerVisibilityFlag.SetFlag;
end;

procedure TMiniMapLayerViewRect.OnPaintLayer(
  Sender: TObject;
  Buffer: TBitmap32
);
var
  VCounterContext: TInternalPerformanceCounterContext;
  VOldClipRect: TRect;
  VNewClipRect: TRect;
  VMiniMapConverter: ILocalCoordConverter;
  VViewConverter: ILocalCoordConverter;
begin
  VMiniMapConverter := FPosition.GetStatic;
  VViewConverter := FViewPortState.View.GetStatic;
  if (VMiniMapConverter <> nil) and (VViewConverter <> nil) and (VMiniMapConverter.ProjectionInfo.Zoom < VViewConverter.ProjectionInfo.Zoom) then begin
    VCounterContext := FOnPaintCounter.StartOperation;
    try
      VOldClipRect := Buffer.ClipRect;
      if Types.IntersectRect(VNewClipRect, VOldClipRect, VMiniMapConverter.GetLocalRect) then begin
        Buffer.ClipRect := VNewClipRect;
        try
          DrawMainViewRect(Buffer, VMiniMapConverter, VViewConverter);
        finally
          Buffer.ClipRect := VOldClipRect;
        end;
      end;
    finally
      FOnPaintCounter.FinishOperation(VCounterContext);
    end;
  end;
end;

procedure TMiniMapLayerViewRect.OnPosChange;
begin
  FNeedUpdateLayerLocationFlag.SetFlag;
end;

procedure TMiniMapLayerViewRect.OnTimer;
var
  VConfig: IMiniMapLayerLocationConfigStatic;
  VMiniMapConverter: ILocalCoordConverter;
begin
  if FNeedUpdateLayerVisibilityFlag.CheckFlagAndReset then begin
    VConfig := FLocationConfig.GetStatic;
    if FLayer.Visible <> VConfig.Visible then begin
      FLayer.Visible := VConfig.Visible;
      FLayer.MouseEvents := VConfig.Visible;
      FNeedUpdateLayerLocationFlag.SetFlag;
    end;
  end;
  if FNeedUpdateLayerLocationFlag.CheckFlagAndReset then begin
    VMiniMapConverter := FPosition.GetStatic;
    if Assigned(VMiniMapConverter) then begin
      FLayer.Location := FloatRect(VMiniMapConverter.GetLocalRect);
    end;
  end;
  if FLayerChangeFlag.CheckFlagAndReset then begin
    FLayer.Changed;
  end;
end;

procedure TMiniMapLayerViewRect.OnViewChange;
begin
  FLayerChangeFlag.SetFlag;
end;

procedure TMiniMapLayerViewRect.StartThreads;
begin
  inherited;
  FLayer.OnPaint := OnPaintLayer;
  OnConfigChange;
end;

end.
