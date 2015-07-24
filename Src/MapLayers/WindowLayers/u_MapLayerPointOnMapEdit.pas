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

unit u_MapLayerPointOnMapEdit;

interface

uses
  GR32,
  GR32_Image,
  t_GeoTypes,
  i_NotifierOperation,
  i_InternalPerformanceCounter,
  i_MarkerDrawable,
  i_LocalCoordConverter,
  i_LocalCoordConverterChangeable,
  i_PointOnMapEdit,
  u_WindowLayerBasicBase;

type
  TMapLayerPointOnMapEdit = class(TWindowLayerBasicBase)
  private
    FLocalConverter: ILocalCoordConverterChangeable;
    FPointOnMap: IPointOnMapEdit;
    FMarker: IMarkerDrawableChangeable;

    procedure OnPointChange;
    procedure OnPosChange;
  protected
    procedure PaintLayer(ABuffer: TBitmap32); override;
    procedure StartThreads; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      AParentMap: TImage32;
      const ALocalConverter: ILocalCoordConverterChangeable;
      const AMarker: IMarkerDrawableChangeable;
      const APointOnMap: IPointOnMapEdit
    );
  end;

implementation

uses
  Math,
  GR32_Layers,
  i_Listener,
  i_CoordConverter,
  u_ListenerByEvent,
  u_GeoFunc;

{ TPointOnMapEditLayer }

constructor TMapLayerPointOnMapEdit.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const ALocalConverter: ILocalCoordConverterChangeable;
  const AMarker: IMarkerDrawableChangeable;
  const APointOnMap: IPointOnMapEdit
);
var
  VListener: IListener;
begin
  inherited Create(
    APerfList,
    AAppStartedNotifier,
    AAppClosingNotifier,
    TCustomLayer.Create(AParentMap.Layers)
  );
  FPointOnMap := APointOnMap;
  FLocalConverter := ALocalConverter;
  FMarker := AMarker;

  VListener := TNotifyNoMmgEventListener.Create(Self.OnPointChange);
  LinksList.Add(
    VListener,
    FPointOnMap.GetChangeNotifier
  );

  LinksList.Add(
    VListener,
    FMarker.ChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnPosChange),
    FLocalConverter.ChangeNotifier
  );
end;

procedure TMapLayerPointOnMapEdit.OnPointChange;
var
  VPoint: TDoublePoint;
begin
  VPoint := FPointOnMap.Point;
  ViewUpdateLock;
  try
    Visible := not PointIsEmpty(VPoint);
    SetNeedFullRepaintLayer;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMapLayerPointOnMapEdit.OnPosChange;
begin
  ViewUpdateLock;
  try
    SetNeedFullRepaintLayer;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMapLayerPointOnMapEdit.PaintLayer(ABuffer: TBitmap32);
var
  VLocalConverter: ILocalCoordConverter;
  VConverter: ICoordConverter;
  VMarker: IMarkerDrawable;
  VLonLat: TDoublePoint;
  VFixedOnView: TDoublePoint;
begin
  inherited;
  VLocalConverter := FLocalConverter.GetStatic;
  VLonLat := FPointOnMap.Point;
  if not PointIsEmpty(VLonLat) then begin
    VConverter := VLocalConverter.GetGeoConverter;
    VConverter.ValidateLonLatPos(VLonLat);
    VMarker := FMarker.GetStatic;
    VFixedOnView := VLocalConverter.LonLat2LocalPixelFloat(VLonLat);
    VMarker.DrawToBitmap(ABuffer, VFixedOnView);
  end;
end;

procedure TMapLayerPointOnMapEdit.StartThreads;
begin
  inherited;
  OnPointChange;
end;

end.
