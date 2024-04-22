{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
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

unit u_MapLayerGpsTrackGoToMarker;

interface

uses
  Math,
  GR32,
  GR32_Image,
  GR32_Layers,
  t_GeoTypes,
  i_Notifier,
  i_NotifierOperation,
  i_LocalCoordConverterChangeable,
  i_InternalPerformanceCounter,
  i_MarkerDrawable,
  i_MapViewGoto,
  i_LocalCoordConverter,
  u_WindowLayerBasicBase;

type
  TMapLayerGpsTrackGoToMarker = class(TWindowLayerBasicBase)
  private
    FMapViewGoTo: IMapViewGoTo;
    FMarkerChangeable: IMarkerDrawableChangeable;
    FLocalConverter: ILocalCoordConverterChangeable;

    procedure OnGoToChange;
    procedure OnPosChange;
  protected
    procedure PaintLayer(ABuffer: TBitmap32); override;
    procedure StartThreads; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      const AParentMap: TImage32;
      const ALocalConverter: ILocalCoordConverterChangeable;
      const AMarkerChangeable: IMarkerDrawableChangeable;
      const AMapViewGoTo: IMapViewGoTo
    );
  end;

implementation

uses
  i_ProjectionType,
  u_ListenerByEvent,
  u_GeoFunc;

{ TMapLayerGpsTrackGoToMarker }

constructor TMapLayerGpsTrackGoToMarker.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  const AParentMap: TImage32;
  const ALocalConverter: ILocalCoordConverterChangeable;
  const AMarkerChangeable: IMarkerDrawableChangeable;
  const AMapViewGoTo: IMapViewGoTo
);
begin
  inherited Create(
    APerfList,
    AAppStartedNotifier,
    AAppClosingNotifier,
    AParentMap,
    TCustomLayer.Create(AParentMap.Layers)
  );

  FLocalConverter := ALocalConverter;
  FMarkerChangeable := AMarkerChangeable;
  FMapViewGoTo := AMapViewGoTo;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnGoToChange),
    FMapViewGoTo.GetChangeNotifier
  );

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnPosChange),
    FLocalConverter.ChangeNotifier
  );
end;

procedure TMapLayerGpsTrackGoToMarker.OnGoToChange;
begin
  ViewUpdateLock;
  try
    Visible :=
      (FMapViewGoTo.LastGotoPos <> nil) and
      not PointIsEmpty(FMapViewGoTo.LastGotoPos.LonLat);

    SetNeedFullRepaintLayer;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMapLayerGpsTrackGoToMarker.OnPosChange;
begin
  ViewUpdateLock;
  try
    SetNeedFullRepaintLayer;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMapLayerGpsTrackGoToMarker.PaintLayer(
  ABuffer: TBitmap32
);
var
  VLocalConverter: ILocalCoordConverter;
  VGotoPos: IGotoPosStatic;
  VMarker: IMarkerDrawable;
  VGotoLonLat: TDoublePoint;
  VFixedOnView: TDoublePoint;
  VProjectionType: IProjectionType;
begin
  inherited;

  VGotoPos := FMapViewGoTo.LastGotoPos;

  if VGotoPos = nil then begin
    Exit;
  end;

  VGotoLonLat := VGotoPos.LonLat;
  if Visible and not PointIsEmpty(VGotoLonLat) then begin
    VLocalConverter := FLocalConverter.GetStatic;
    VProjectionType := VLocalConverter.Projection.ProjectionType;
    VProjectionType.ValidateLonLatPos(VGotoLonLat);
    VFixedOnView := VLocalConverter.LonLat2LocalPixelFloat(VGotoLonLat);

    VMarker := FMarkerChangeable.GetStatic;
    VMarker.DrawToBitmap(ABuffer, VFixedOnView);
  end;
end;

procedure TMapLayerGpsTrackGoToMarker.StartThreads;
begin
  inherited;
  OnGoToChange;
end;

end.
