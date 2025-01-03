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

unit u_MapLayerNavToMark;

interface

uses
  GR32,
  GR32_Image,
  i_Notifier,
  t_GeoTypes,
  i_NotifierOperation,
  i_LocalCoordConverter,
  i_LocalCoordConverterChangeable,
  i_InternalPerformanceCounter,
  i_NavigationToPoint,
  i_MapLayerNavToPointMarkerConfig,
  i_MarkerDrawable,
  i_MainFormState,
  u_MapLayerBasicNoBitmap;

type
  TMapLayerNavToMark = class(TMapLayerBasicNoBitmap)
  private
    FMainFormState: IMainFormState;
    FConfig: IMapLayerNavToPointMarkerConfig;
    FNavToPoint: INavigationToPoint;
    FArrowMarkerChangeable: IMarkerDrawableWithDirectionChangeable;
    FReachedMarkerChangeable: IMarkerDrawableChangeable;

    FRect: TRect;
    FArrowMarker: IMarkerDrawableWithDirection;
    FReachedMarker: IMarkerDrawable;
    FMarkerPos: TDoublePoint;
    FMarkerAngle: Double;
    FIsReached: Boolean;
    FIsValid: Boolean;

    FMarkPoint: TDoublePoint;

    procedure OnNavToPointChange;
    procedure OnConfigChange;
  protected
    procedure InvalidateLayer(const ALocalConverter: ILocalCoordConverter); override;
    procedure PaintLayer(ABuffer: TBitmap32); override;
    procedure StartThreads; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      AParentMap: TImage32;
      const AView: ILocalCoordConverterChangeable;
      const AMainFormState: IMainFormState;
      const ANavToPoint: INavigationToPoint;
      const AArrowMarkerChangeable: IMarkerDrawableWithDirectionChangeable;
      const AReachedMarkerChangeable: IMarkerDrawableChangeable;
      const AConfig: IMapLayerNavToPointMarkerConfig
    );
  end;

implementation

uses
  Math,
  i_Projection,
  u_ListenerByEvent;

{ TMapLayerNavToMark }

constructor TMapLayerNavToMark.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const AView: ILocalCoordConverterChangeable;
  const AMainFormState: IMainFormState;
  const ANavToPoint: INavigationToPoint;
  const AArrowMarkerChangeable: IMarkerDrawableWithDirectionChangeable;
  const AReachedMarkerChangeable: IMarkerDrawableChangeable;
  const AConfig: IMapLayerNavToPointMarkerConfig
);
begin
  inherited Create(
    APerfList,
    AAppStartedNotifier,
    AAppClosingNotifier,
    AParentMap,
    AView
  );

  FMainFormState := AMainFormState;
  FNavToPoint := ANavToPoint;
  FArrowMarkerChangeable := AArrowMarkerChangeable;
  FReachedMarkerChangeable := AReachedMarkerChangeable;
  FConfig := AConfig;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FConfig.ChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FArrowMarkerChangeable.ChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FReachedMarkerChangeable.ChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnNavToPointChange),
    FNavToPoint.ChangeNotifier
  );
end;

procedure TMapLayerNavToMark.OnConfigChange;
begin
  FArrowMarker := FArrowMarkerChangeable.GetStatic;
  FReachedMarker := FReachedMarkerChangeable.GetStatic;
  if Visible then begin
    ViewUpdateLock;
    try
      SetNeedRedraw;
    finally
      ViewUpdateUnlock;
    end;
  end;
end;

procedure TMapLayerNavToMark.OnNavToPointChange;
begin
  ViewUpdateLock;
  try
    Visible := FNavToPoint.IsActive;
    FMarkPoint := FNavToPoint.LonLat;
    SetNeedRedraw;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMapLayerNavToMark.InvalidateLayer(const ALocalConverter: ILocalCoordConverter);
var
  VLonLat: TDoublePoint;
  VMarkMapPos: TDoublePoint;
  VScreenCenterMapPos: TDoublePoint;
  VDelta: TDoublePoint;
  VDeltaNormed: TDoublePoint;
  VProjection: IProjection;
  VCrossDist: Double;
  VDistInPixel: Double;
begin
  if FIsValid then begin
    FIsValid := False;
    DoInvalidateRect(FRect); // erase
  end;

  if not Visible then begin
    Exit;
  end;

  VProjection := ALocalConverter.Projection;
  VScreenCenterMapPos := ALocalConverter.GetCenterMapPixelFloat;
  VLonLat := FMarkPoint;
  VProjection.ProjectionType.ValidateLonLatPos(VLonLat);
  VMarkMapPos := VProjection.LonLat2PixelPosFloat(VLonLat);
  VDelta.X := VMarkMapPos.X - VScreenCenterMapPos.X;
  VDelta.Y := VMarkMapPos.Y - VScreenCenterMapPos.Y;
  VDistInPixel := Sqrt(Sqr(VDelta.X) + Sqr(VDelta.Y));
  VCrossDist := FConfig.CrossDistInPixels;

  FIsReached := VDistInPixel < VCrossDist;

  if FIsReached then begin
    FMarkerPos := ALocalConverter.LonLat2LocalPixelFloat(VLonLat);
    FRect := FReachedMarker.GetBoundsForPosition(FMarkerPos);
  end else begin
    VDeltaNormed.X := VDelta.X / VDistInPixel * VCrossDist;
    VDeltaNormed.Y := VDelta.Y / VDistInPixel * VCrossDist;
    VMarkMapPos.X := VScreenCenterMapPos.X + VDeltaNormed.X;
    VMarkMapPos.Y := VScreenCenterMapPos.Y + VDeltaNormed.Y;
    FMarkerPos := ALocalConverter.MapPixelFloat2LocalPixelFloat(VMarkMapPos);
    FMarkerAngle := ArcSin(VDelta.X / VDistInPixel) / Pi * 180;
    if VDelta.Y > 0 then begin
      FMarkerAngle := 180 - FMarkerAngle;
    end;
    FRect := FArrowMarker.GetBoundsForPosition(FMarkerPos, FMarkerAngle);
  end;

  FIsValid := True;

  // draw
  if FMainFormState.IsMapMoving then begin
    DoInvalidateFull;
  end else begin
    DoInvalidateRect(FRect);
  end;
end;

procedure TMapLayerNavToMark.PaintLayer(ABuffer: TBitmap32);
begin
  if FIsValid then begin
    if ABuffer.MeasuringMode then begin
      ABuffer.Changed(FRect);
    end else begin
      if FIsReached then begin
        FReachedMarker.DrawToBitmap(ABuffer, FMarkerPos);
      end else begin
        FArrowMarker.DrawToBitmapWithDirection(ABuffer, FMarkerPos, FMarkerAngle);
      end;
    end;
  end;
end;

procedure TMapLayerNavToMark.StartThreads;
begin
  inherited;
  ViewUpdateLock;
  try
    OnConfigChange;
    OnNavToPointChange;
  finally
    ViewUpdateUnlock;
  end;
end;

end.
