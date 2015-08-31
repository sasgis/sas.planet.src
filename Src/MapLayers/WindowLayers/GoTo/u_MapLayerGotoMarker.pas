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

unit u_MapLayerGotoMarker;

interface

uses
  GR32,
  GR32_Image,
  t_GeoTypes,
  i_Notifier,
  i_NotifierTime,
  i_NotifierOperation,
  i_LocalCoordConverterChangeable,
  i_InternalPerformanceCounter,
  i_MarkerDrawable,
  i_MapViewGoto,
  i_LocalCoordConverter,
  i_GotoLayerConfig,
  u_WindowLayerBasicBase;

type
  TMapLayerGotoMarker = class(TWindowLayerBasicBase)
  private
    FLocalConverter: ILocalCoordConverterChangeable;
    FConfig: IGotoLayerConfig;
    FMapGoto: IMapViewGoto;
    FMarkerChangeable: IMarkerDrawableChangeable;

    procedure OnTimer;
    procedure OnConfigChange;
    procedure OnPosChange;
    function GetIsVisible: Boolean;
  protected
    procedure PaintLayer(ABuffer: TBitmap32); override;
    procedure StartThreads; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      AParentMap: TImage32;
      const ATimerNoifier: INotifierTime;
      const ALocalConverter: ILocalCoordConverterChangeable;
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
  i_CoordConverter,
  i_ProjectionType,
  u_ListenerTime,
  u_ListenerByEvent,
  u_GeoFunc;

{ TMapLayerGotoMarker }

constructor TMapLayerGotoMarker.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const ATimerNoifier: INotifierTime;
  const ALocalConverter: ILocalCoordConverterChangeable;
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
    TCustomLayer.Create(AParentMap.Layers)
  );
  FLocalConverter := ALocalConverter;
  FConfig := AConfig;
  FMarkerChangeable := AMarkerChangeable;
  FMapGoto := AMapGoto;

  VListener := TNotifyNoMmgEventListener.Create(Self.OnConfigChange);
  LinksList.Add(
    VListener,
    FConfig.GetChangeNotifier
  );

  LinksList.Add(
    VListener,
    FMarkerChangeable.ChangeNotifier
  );

  LinksList.Add(
    VListener,
    FMapGoto.GetChangeNotifier
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
  VGotoPos := FMapGoto.LastGotoPos;
  Result := False;
  if VGotoPos <> nil then begin
    VGotoTime := VGotoPos.GotoTime;
    if not IsNan(VGotoTime) then begin
      VGotoLonLat := VGotoPos.LonLat;
      if not PointIsEmpty(VGotoLonLat) then begin
        VCurrTime := Now;
        if (VGotoTime <= VCurrTime) then begin
          VShowTimeDelta := (FConfig.ShowTickCount / 1000) / 60 / 60 / 24;
          VTimeDelta := VCurrTime - VGotoTime;
          if (VTimeDelta < VShowTimeDelta) then begin
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
    Visible := GetIsVisible;
    SetNeedFullRepaintLayer;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMapLayerGotoMarker.OnPosChange;
begin
  ViewUpdateLock;
  try
    SetNeedFullRepaintLayer;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMapLayerGotoMarker.OnTimer;
begin
  if Visible then begin
    ViewUpdateLock;
    try
      Visible := GetIsVisible;
    finally
      ViewUpdateLock;
    end;
  end;
end;

procedure TMapLayerGotoMarker.PaintLayer(
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
  VLocalConverter := FLocalConverter.GetStatic;

  VGotoPos := FMapGoto.LastGotoPos;

  if VGotoPos = nil then begin
    Exit;
  end;

  VGotoLonLat := VGotoPos.LonLat;
  if not PointIsEmpty(VGotoLonLat) then begin
    VProjectionType := VLocalConverter.ProjectionInfo.ProjectionType;
    VProjectionType.ValidateLonLatPos(VGotoLonLat);
    VMarker := FMarkerChangeable.GetStatic;
    VFixedOnView := VLocalConverter.LonLat2LocalPixelFloat(VGotoLonLat);
    VMarker.DrawToBitmap(ABuffer, VFixedOnView);
  end;
end;

procedure TMapLayerGotoMarker.StartThreads;
begin
  inherited;
  OnConfigChange;
end;

end.
