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

unit u_MapLayerGPSMarker;

interface

uses
  GR32,
  GR32_Image,
  t_GeoTypes,
  i_NotifierTime,
  i_NotifierOperation,
  i_LocalCoordConverter,
  i_LocalCoordConverterChangeable,
  i_InternalPerformanceCounter,
  i_SimpleFlag,
  i_MainFormState,
  i_MarkerDrawable,
  i_MapLayerGPSMarkerConfig,
  i_GPSRecorder,
  u_MapLayerBasicNoBitmap;

type
  TMapLayerGPSMarker = class(TMapLayerBasicNoBitmap)
  private
    FMainFormState: IMainFormState;
    FConfig: IMapLayerGPSMarkerConfig;
    FGpsRecorder: IGPSRecorder;
    FArrowMarkerChangeable: IMarkerDrawableWithDirectionChangeable;
    FStopedMarkerChangeable: IMarkerDrawableChangeable;

    FGpsPosChangeFlag: ISimpleFlag;

    FArrowMarker: IMarkerDrawableWithDirection;
    FStopedMarker: IMarkerDrawable;

    FIsValid: Boolean;
    FRect: TRect;
    FLonLatPos: TDoublePoint;
    FMarkerPos: TDoublePoint;
    FIsStopped: Boolean;
    FDirection: Double;

    procedure OnGpsPosChange;
    procedure OnConfigChange;
    procedure OnTimer;
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
      const AGuiSyncronizedTimerNotifier: INotifierTime;
      const AConfig: IMapLayerGPSMarkerConfig;
      const AArrowMarkerChangeable: IMarkerDrawableWithDirectionChangeable;
      const AStopedMarkerChangeable: IMarkerDrawableChangeable;
      const AGPSRecorder: IGPSRecorder
    );
  end;

implementation

uses
  Math,
  i_GPS,
  u_GeoFunc,
  u_SimpleFlagWithInterlock,
  u_ListenerTime,
  u_ListenerByEvent;

{ TMapLayerGPSMarker }

constructor TMapLayerGPSMarker.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const AView: ILocalCoordConverterChangeable;
  const AMainFormState: IMainFormState;
  const AGuiSyncronizedTimerNotifier: INotifierTime;
  const AConfig: IMapLayerGPSMarkerConfig;
  const AArrowMarkerChangeable: IMarkerDrawableWithDirectionChangeable;
  const AStopedMarkerChangeable: IMarkerDrawableChangeable;
  const AGPSRecorder: IGPSRecorder
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
  FConfig := AConfig;
  FGpsRecorder := AGPSRecorder;
  FArrowMarkerChangeable := AArrowMarkerChangeable;
  FStopedMarkerChangeable := AStopedMarkerChangeable;

  FGpsPosChangeFlag := TSimpleFlagWithInterlock.Create;

  LinksList.Add(
    TListenerTimeCheck.Create(Self.OnTimer, 200),
    AGuiSyncronizedTimerNotifier
  );
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
    FStopedMarkerChangeable.ChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnGpsPosChange),
    FGpsRecorder.ChangeNotifier
  );
end;

procedure TMapLayerGPSMarker.OnGpsPosChange;
begin
  FGpsPosChangeFlag.SetFlag;
end;

procedure TMapLayerGPSMarker.OnConfigChange;
begin
  FStopedMarker := FStopedMarkerChangeable.GetStatic;
  FArrowMarker := FArrowMarkerChangeable.GetStatic;
  if Visible then begin
    ViewUpdateLock;
    try
      SetNeedRedraw;
    finally
      ViewUpdateUnlock;
    end;
  end;
end;

procedure TMapLayerGPSMarker.OnTimer;
var
  VGPSPosition: IGPSPosition;
begin
  if FGpsPosChangeFlag.CheckFlagAndReset then begin
    ViewUpdateLock;
    try
      VGPSPosition := FGpsRecorder.CurrentPosition;
      if not VGPSPosition.PositionOK then begin
        // no position
        Hide;
      end else begin
        // ok
        FLonLatPos := VGPSPosition.LonLat;
        FIsStopped := not VGPSPosition.SpeedOK;
        if not FIsStopped then begin
          FIsStopped := VGPSPosition.Speed_KMH <= FConfig.MinMoveSpeed;
        end;
        if not FIsStopped then begin
          FDirection := VGPSPosition.Heading;
        end else begin
          FDirection := 0;
        end;
        Show;
        SetNeedRedraw;
      end;
    finally
      ViewUpdateUnlock;
    end;
  end;
end;

procedure TMapLayerGPSMarker.InvalidateLayer(const ALocalConverter: ILocalCoordConverter);
begin
  if FIsValid then begin
    FIsValid := False;
    DoInvalidateRect(FRect); // erase
  end;

  FIsValid := Visible and not PointIsEmpty(FLonLatPos);

  if FIsValid then begin
    FMarkerPos := ALocalConverter.LonLat2LocalPixelFloat(FLonLatPos);
    if FIsStopped then begin
      FRect := FStopedMarker.GetBoundsForPosition(FMarkerPos);
    end else begin
      FRect := FArrowMarker.GetBoundsForPosition(FMarkerPos, FDirection);
    end;

    // draw
    if FMainFormState.IsMapMoving then begin
      DoInvalidateFull;
    end else begin
      DoInvalidateRect(FRect);
    end;
  end;
end;

procedure TMapLayerGPSMarker.PaintLayer(ABuffer: TBitmap32);
begin
  if FIsValid then begin
    if ABuffer.MeasuringMode then begin
      ABuffer.Changed(FRect);
    end else begin
      if FIsStopped then begin
        FStopedMarker.DrawToBitmap(ABuffer, FMarkerPos);
      end else begin
        FArrowMarker.DrawToBitmapWithDirection(ABuffer, FMarkerPos, FDirection);
      end;
    end;
  end;
end;

procedure TMapLayerGPSMarker.StartThreads;
begin
  inherited;
  OnConfigChange;
end;

end.
