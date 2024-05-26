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

unit u_MapLayerGotoMarker;

interface

uses
  GR32,
  GR32_Image,
  t_GeoTypes,
  i_NotifierTime,
  i_NotifierOperation,
  i_LocalCoordConverterChangeable,
  i_InternalPerformanceCounter,
  i_MainFormState,
  i_MarkerDrawable,
  i_MapViewGoto,
  i_GotoLayerConfig,
  u_WindowLayerBasicBase;

type
  TMapLayerGotoMarker = class(TWindowLayerBasicBase)
  private
    FMainFormState: IMainFormState;
    FLocalConverter: ILocalCoordConverterChangeable;
    FConfig: IGotoLayerConfig;
    FMapGoto: IMapViewGoto;

    FMarker: IMarkerDrawable;
    FMarkerChangeable: IMarkerDrawableChangeable;

    FPos: TDoublePoint;
    FRect: TRect;
    FIsValid: Boolean;

    procedure OnTimer;
    procedure OnPosChange;
    procedure OnConfigChange;

    function GetIsVisible: Boolean;
    function GetGotoPos(out APos: TDoublePoint): Boolean;
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
      const ATimerNoifier: INotifierTime;
      const ALocalConverter: ILocalCoordConverterChangeable;
      const AMainFormState: IMainFormState;
      const AMarkerChangeable: IMarkerDrawableChangeable;
      const AMapGoto: IMapViewGoto;
      const AConfig: IGotoLayerConfig
    );
  end;

implementation

uses
  Math,
  SysUtils,
  GR32_Layers,
  i_Listener,
  i_ProjectionType,
  i_LocalCoordConverter,
  u_ListenerTime,
  u_ListenerByEvent,
  u_GeoFunc;

{ TMapLayerGotoMarker }

constructor TMapLayerGotoMarker.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  const AParentMap: TImage32;
  const ATimerNoifier: INotifierTime;
  const ALocalConverter: ILocalCoordConverterChangeable;
  const AMainFormState: IMainFormState;
  const AMarkerChangeable: IMarkerDrawableChangeable;
  const AMapGoto: IMapViewGoto;
  const AConfig: IGotoLayerConfig
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
  FConfig := AConfig;
  FMarkerChangeable := AMarkerChangeable;
  FMapGoto := AMapGoto;

  FMarker := FMarkerChangeable.GetStatic;

  VListener := TNotifyNoMmgEventListener.Create(Self.OnConfigChange);
  LinksList.Add(
    VListener,
    FConfig.ChangeNotifier
  );
  LinksList.Add(
    VListener,
    FMarkerChangeable.ChangeNotifier
  );
  LinksList.Add(
    VListener,
    FMapGoto.ChangeNotifier
  );

  LinksList.Add(
    TListenerTimeCheck.Create(Self.OnTimer, 1000),
    ATimerNoifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnPosChange),
    FLocalConverter.ChangeNotifier
  );
end;

function TMapLayerGotoMarker.GetIsVisible: Boolean;
var
  VCurrTime: TDateTime;
  VGotoTime: TDateTime;
  VTimeDelta: Double;
  VGotoPos: IGotoPosStatic;
  VGotoLonLat: TDoublePoint;
  VShowTimeDelta: TDateTime;
begin
  Result := False;
  VGotoPos := FMapGoto.LastGotoPos;
  if VGotoPos <> nil then begin
    VGotoTime := VGotoPos.GotoTime;
    if not IsNan(VGotoTime) then begin
      VGotoLonLat := VGotoPos.LonLat;
      if not PointIsEmpty(VGotoLonLat) then begin
        VCurrTime := Now;
        if VGotoTime <= VCurrTime then begin
          VShowTimeDelta := (FConfig.ShowTickCount / 1000) / 60 / 60 / 24;
          VTimeDelta := VCurrTime - VGotoTime;
          if VTimeDelta < VShowTimeDelta then begin
            Result := True;
          end;
        end;
      end;
    end;
  end;
end;

procedure TMapLayerGotoMarker.OnConfigChange;
begin
  ViewUpdateLock;
  try
    FMarker := FMarkerChangeable.GetStatic;
    Visible := GetIsVisible;
    SetNeedFullRepaintLayer;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMapLayerGotoMarker.OnPosChange;
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

procedure TMapLayerGotoMarker.OnTimer;
begin
  if Visible then begin
    Visible := GetIsVisible;
  end;
end;

function TMapLayerGotoMarker.GetGotoPos(out APos: TDoublePoint): Boolean;
var
  VGotoPos: IGotoPosStatic;
  VGotoLonLat: TDoublePoint;
  VLocalConverter: ILocalCoordConverter;
  VProjectionType: IProjectionType;
begin
  Result := False;
  VGotoPos := FMapGoto.LastGotoPos;
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

procedure TMapLayerGotoMarker.InvalidateLayer;
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

procedure TMapLayerGotoMarker.PaintLayer(ABuffer: TBitmap32);
begin
  if FIsValid then begin
    if ABuffer.MeasuringMode then begin
      ABuffer.Changed(FRect);
    end else begin
      FMarker.DrawToBitmap(ABuffer, FPos);
    end;
  end;
end;

procedure TMapLayerGotoMarker.StartThreads;
begin
  inherited;
  OnConfigChange;
end;

end.
