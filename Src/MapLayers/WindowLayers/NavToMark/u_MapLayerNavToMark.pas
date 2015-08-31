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
  u_MapLayerBasicNoBitmap;

type
  TMapLayerNavToMark = class(TMapLayerBasicNoBitmap)
  private
    FConfig: IMapLayerNavToPointMarkerConfig;
    FNavToPoint: INavigationToPoint;
    FArrowMarkerChangeable: IMarkerDrawableWithDirectionChangeable;
    FReachedMarkerChangeable: IMarkerDrawableChangeable;

    FMarkPoint: TDoublePoint;
    procedure OnNavToPointChange;
    procedure OnConfigChange;
  protected
    procedure PaintLayer(
      ABuffer: TBitmap32;
      const ALocalConverter: ILocalCoordConverter
    ); override;
    procedure StartThreads; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      AParentMap: TImage32;
      const AView: ILocalCoordConverterChangeable;
      const ANavToPoint: INavigationToPoint;
      const AArrowMarkerChangeable: IMarkerDrawableWithDirectionChangeable;
      const AReachedMarkerChangeable: IMarkerDrawableChangeable;
      const AConfig: IMapLayerNavToPointMarkerConfig
    );
  end;

implementation

uses
  Math,
  i_ProjectionInfo,
  u_ListenerByEvent;

{ TMapLayerNavToMark }

constructor TMapLayerNavToMark.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const AView: ILocalCoordConverterChangeable;
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
  FNavToPoint := ANavToPoint;
  FArrowMarkerChangeable := AArrowMarkerChangeable;
  FReachedMarkerChangeable := AReachedMarkerChangeable;
  FConfig := AConfig;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FConfig.GetChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FArrowMarkerChangeable.GetChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FReachedMarkerChangeable.GetChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnNavToPointChange),
    FNavToPoint.GetChangeNotifier
  );
end;

procedure TMapLayerNavToMark.OnConfigChange;
begin
  ViewUpdateLock;
  try
    SetNeedRedraw;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMapLayerNavToMark.OnNavToPointChange;
begin
  ViewUpdateLock;
  try
    SetNeedRedraw;
    FMarkPoint := FNavToPoint.LonLat;
    Visible := FNavToPoint.IsActive;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMapLayerNavToMark.PaintLayer(
  ABuffer: TBitmap32;
  const ALocalConverter: ILocalCoordConverter
);
var
  VLonLat: TDoublePoint;
  VMarkMapPos: TDoublePoint;
  VScreenCenterMapPos: TDoublePoint;
  VDelta: TDoublePoint;
  VDeltaNormed: TDoublePoint;
  VProjection: IProjectionInfo;
  VCrossDist: Double;
  VDistInPixel: Double;
  VAngle: Double;
  VFixedOnView: TDoublePoint;
begin
  VProjection := ALocalConverter.ProjectionInfo;
  VScreenCenterMapPos := ALocalConverter.GetCenterMapPixelFloat;
  VLonLat := FMarkPoint;
  VProjection.ProjectionType.ValidateLonLatPos(VLonLat);
  VMarkMapPos := VProjection.LonLat2PixelPosFloat(VLonLat);
  VDelta.X := VMarkMapPos.X - VScreenCenterMapPos.X;
  VDelta.Y := VMarkMapPos.Y - VScreenCenterMapPos.Y;
  VDistInPixel := Sqrt(Sqr(VDelta.X) + Sqr(VDelta.Y));
  VCrossDist := FConfig.CrossDistInPixels;
  if VDistInPixel < VCrossDist then begin
    VFixedOnView := ALocalConverter.LonLat2LocalPixelFloat(VLonLat);
    FReachedMarkerChangeable.GetStatic.DrawToBitmap(ABuffer, VFixedOnView);
  end else begin
    VDeltaNormed.X := VDelta.X / VDistInPixel * VCrossDist;
    VDeltaNormed.Y := VDelta.Y / VDistInPixel * VCrossDist;
    VMarkMapPos.X := VScreenCenterMapPos.X + VDeltaNormed.X;
    VMarkMapPos.Y := VScreenCenterMapPos.Y + VDeltaNormed.Y;
    VFixedOnView := ALocalConverter.MapPixelFloat2LocalPixelFloat(VMarkMapPos);
    VAngle := ArcSin(VDelta.X / VDistInPixel) / Pi * 180;
    if VDelta.Y > 0 then begin
      VAngle := 180 - VAngle;
    end;
    FArrowMarkerChangeable.GetStatic.DrawToBitmapWithDirection(ABuffer, VFixedOnView, VAngle);
  end;
end;

procedure TMapLayerNavToMark.StartThreads;
begin
  inherited;
  OnNavToPointChange;
end;

end.
