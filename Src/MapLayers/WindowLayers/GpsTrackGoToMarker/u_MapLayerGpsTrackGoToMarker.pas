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
  i_NotifierOperation,
  i_LocalCoordConverterChangeable,
  i_InternalPerformanceCounter,
  i_MainFormState,
  i_MarkerDrawable,
  i_MapViewGoto,
  u_WindowLayerBasicBase;

type
  TMapLayerGpsTrackGoToMarker = class(TWindowLayerBasicBase)
  private
    FMainFormState: IMainFormState;
    FMapViewGoTo: IMapViewGoTo;
    FMarker: IMarkerDrawable;
    FMarkerChangeable: IMarkerDrawableChangeable;
    FLocalConverter: ILocalCoordConverterChangeable;

    FPos: TDoublePoint;
    FRect: TRect;
    FIsValid: Boolean;

    function GetGotoPos(out APos: TDoublePoint): Boolean;

    procedure OnConfigChange;
    procedure OnPosChange;
  protected
    procedure InvalidateLayer; override;
    procedure PaintLayer(ABuffer: TBitmap32); override;
    procedure StartThreads; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      const AParentMap: TImage32;
      const ALocalConverter: ILocalCoordConverterChangeable;
      const AMainFormState: IMainFormState;
      const AMarkerChangeable: IMarkerDrawableChangeable;
      const AMapViewGoTo: IMapViewGoTo
    );
  end;

implementation

uses
  i_ProjectionType,
  i_Listener,
  i_LocalCoordConverter,
  u_ListenerByEvent,
  u_GeoFunc;

{ TMapLayerGpsTrackGoToMarker }

constructor TMapLayerGpsTrackGoToMarker.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  const AParentMap: TImage32;
  const ALocalConverter: ILocalCoordConverterChangeable;
  const AMainFormState: IMainFormState;
  const AMarkerChangeable: IMarkerDrawableChangeable;
  const AMapViewGoTo: IMapViewGoTo
);
var
  VListener: IListener;
begin
  inherited Create(
    APerfList,
    AAppStartedNotifier,
    AAppClosingNotifier,
    AParentMap,
    TCustomLayer.Create(AParentMap.Layers)
  );

  FLocalConverter := ALocalConverter;
  FMainFormState := AMainFormState;
  FMarkerChangeable := AMarkerChangeable;
  FMapViewGoTo := AMapViewGoTo;

  FMarker := FMarkerChangeable.GetStatic;

  VListener := TNotifyNoMmgEventListener.Create(Self.OnConfigChange);
  LinksList.Add(
    VListener,
    FMapViewGoTo.ChangeNotifier
  );
  LinksList.Add(
    VListener,
    FMarkerChangeable.ChangeNotifier
  );

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnPosChange),
    FLocalConverter.ChangeNotifier
  );
end;

procedure TMapLayerGpsTrackGoToMarker.OnConfigChange;
var
  VPos: IGotoPosStatic;
begin
  ViewUpdateLock;
  try
    FMarker := FMarkerChangeable.GetStatic;

    VPos := FMapViewGoTo.LastGotoPos;
    Visible := (VPos <> nil) and not PointIsEmpty(VPos.LonLat);

    SetNeedFullRepaintLayer;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMapLayerGpsTrackGoToMarker.OnPosChange;
begin
  if Visible then begin
    ViewUpdateLock;
    try
      SetNeedFullRepaintLayer;
    finally
      ViewUpdateUnlock;
    end;
  end;
end;

function TMapLayerGpsTrackGoToMarker.GetGotoPos(out APos: TDoublePoint): Boolean;
var
  VGotoPos: IGotoPosStatic;
  VGotoLonLat: TDoublePoint;
  VLocalConverter: ILocalCoordConverter;
  VProjectionType: IProjectionType;
begin
  Result := False;
  VGotoPos := FMapViewGoTo.LastGotoPos;
  if VGotoPos <> nil then begin
    VGotoLonLat := VGotoPos.LonLat;
    if not PointIsEmpty(VGotoLonLat) then begin
      VLocalConverter := FLocalConverter.GetStatic;
      VProjectionType := VLocalConverter.Projection.ProjectionType;
      VProjectionType.ValidateLonLatPos(VGotoLonLat);
      APos := VLocalConverter.LonLat2LocalPixelFloat(VGotoLonLat);
      Result := True;
    end;
  end;
end;

procedure TMapLayerGpsTrackGoToMarker.InvalidateLayer;
begin
  if FIsValid then begin
    FIsValid := False;
    DoInvalidateRect(FRect); // erase
  end;

  FIsValid := Visible and GetGotoPos(FPos);

  if FIsValid then begin
    FRect := FMarker.GetBoundsForPosition(FPos);

    // draw
    if FMainFormState.IsMapMoving then begin
      DoInvalidateFull;
    end else begin
      DoInvalidateRect(FRect);
    end;
  end;
end;

procedure TMapLayerGpsTrackGoToMarker.PaintLayer(ABuffer: TBitmap32);
begin
  if FIsValid then begin
    if ABuffer.MeasuringMode then begin
      ABuffer.Changed(FRect);
    end else begin
      FMarker.DrawToBitmap(ABuffer, FPos);
    end;
  end;
end;

procedure TMapLayerGpsTrackGoToMarker.StartThreads;
begin
  inherited;
  OnConfigChange;
end;

end.
