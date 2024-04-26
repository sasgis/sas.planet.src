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

unit u_MapLayerPointOnMapEdit;

interface

uses
  GR32,
  GR32_Image,
  t_GeoTypes,
  i_NotifierOperation,
  i_InternalPerformanceCounter,
  i_MainFormState,
  i_MarkerDrawable,
  i_LocalCoordConverter,
  i_LocalCoordConverterChangeable,
  i_PointOnMapEdit,
  u_WindowLayerBasicBase;

type
  TMapLayerPointOnMapEdit = class(TWindowLayerBasicBase)
  private
    FMainFormState: IMainFormState;
    FLocalConverter: ILocalCoordConverterChangeable;
    FPointOnMap: IPointOnMapEdit;
    FMarker: IMarkerDrawableChangeable;

    FPos: TDoublePoint;
    FIsPosValid: Boolean;
    function GetGotoPos(out APos: TDoublePoint): Boolean;

    procedure OnPointChange;
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
      const AMarker: IMarkerDrawableChangeable;
      const APointOnMap: IPointOnMapEdit
    );
  end;

implementation

uses
  Math,
  GR32_Layers,
  i_Listener,
  i_ProjectionType,
  u_ListenerByEvent,
  u_GeoFunc;

{ TPointOnMapEditLayer }

constructor TMapLayerPointOnMapEdit.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  const AParentMap: TImage32;
  const ALocalConverter: ILocalCoordConverterChangeable;
  const AMainFormState: IMainFormState;
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
    AParentMap,
    TCustomLayer.Create(AParentMap.Layers)
  );

  FPointOnMap := APointOnMap;
  FLocalConverter := ALocalConverter;
  FMainFormState := AMainFormState;
  FMarker := AMarker;

  VListener := TNotifyNoMmgEventListener.Create(Self.OnPointChange);
  LinksList.Add(
    VListener,
    FPointOnMap.ChangeNotifier
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

function TMapLayerPointOnMapEdit.GetGotoPos(out APos: TDoublePoint): Boolean;
var
  VLocalConverter: ILocalCoordConverter;
  VProjectionType: IProjectionType;
  VLonLat: TDoublePoint;
begin
  Result := False;
  VLocalConverter := FLocalConverter.GetStatic;
  VLonLat := FPointOnMap.Point;
  if not PointIsEmpty(VLonLat) then begin
    VProjectionType := VLocalConverter.Projection.ProjectionType;
    VProjectionType.ValidateLonLatPos(VLonLat);
    APos := VLocalConverter.LonLat2LocalPixelFloat(VLonLat);
    Result := True;
  end;
end;

procedure TMapLayerPointOnMapEdit.InvalidateLayer;
var
  VMarker: IMarkerDrawable;
begin
  if Visible then begin
    if FMainFormState.IsMapMoving then begin
      FIsPosValid := GetGotoPos(FPos);
      DoInvalidateFull;
    end else begin
      VMarker := FMarker.GetStatic;
      if FIsPosValid then begin
        FIsPosValid := False;
        DoInvalidateRect(VMarker.GetBoundsForPosition(FPos)); // erase old
      end;

      FIsPosValid := GetGotoPos(FPos);
      if FIsPosValid then begin
        DoInvalidateRect(VMarker.GetBoundsForPosition(FPos)); // draw new
      end;
    end;
  end else begin
    FIsPosValid := False;
  end;
end;

procedure TMapLayerPointOnMapEdit.PaintLayer(ABuffer: TBitmap32);
var
  VMarker: IMarkerDrawable;
begin
  if FIsPosValid then begin
    VMarker := FMarker.GetStatic;
    VMarker.DrawToBitmap(ABuffer, FPos);
  end;
end;

procedure TMapLayerPointOnMapEdit.StartThreads;
begin
  inherited;
  OnPointChange;
end;

end.
