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

unit u_MapLayerGPSMarker;

interface

uses
  SysUtils,
  GR32,
  GR32_Image,
  t_GeoTypes,
  i_Notifier,
  i_NotifierTime,
  i_NotifierOperation,
  i_LocalCoordConverter,
  i_LocalCoordConverterChangeable,
  i_InternalPerformanceCounter,
  i_SimpleFlag,
  i_MarkerDrawable,
  i_MapLayerGPSMarkerConfig,
  i_GPSRecorder,
  u_MapLayerBasicNoBitmap;

type
  TMapLayerGPSMarker = class(TMapLayerBasicNoBitmap)
  private
    FConfig: IMapLayerGPSMarkerConfig;
    FGPSRecorder: IGPSRecorder;
    FArrowMarkerChangeable: IMarkerDrawableWithDirectionChangeable;
    FStopedMarkerChangeable: IMarkerDrawableChangeable;

    FGpsPosChangeFlag: ISimpleFlag;

    FPositionCS: IReadWriteSync;
    FPositionLonLat: TDoublePoint;
    FStopped: Boolean;
    FDirectionAngle: Double;

    procedure GPSReceiverReceive;
    procedure OnConfigChange;
    procedure OnTimer;
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
      const ATimerNoifier: INotifierTime;
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
  u_Synchronizer,
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
  const ATimerNoifier: INotifierTime;
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
  FConfig := AConfig;
  FGPSRecorder := AGPSRecorder;
  FArrowMarkerChangeable := AArrowMarkerChangeable;
  FStopedMarkerChangeable := AStopedMarkerChangeable;

  FGpsPosChangeFlag := TSimpleFlagWithInterlock.Create;
  FPositionCS := GSync.SyncVariable.Make(Self.ClassName);

  LinksList.Add(
    TListenerTimeCheck.Create(Self.OnTimer, 200),
    ATimerNoifier
  );
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
    FStopedMarkerChangeable.GetChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.GPSReceiverReceive),
    FGPSRecorder.GetChangeNotifier
  );
end;

procedure TMapLayerGPSMarker.GPSReceiverReceive;
begin
  FGpsPosChangeFlag.SetFlag;
end;

procedure TMapLayerGPSMarker.OnConfigChange;
begin
  ViewUpdateLock;
  try
    SetNeedRedraw;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMapLayerGPSMarker.OnTimer;
var
  VGPSPosition: IGPSPosition;
begin
  if FGpsPosChangeFlag.CheckFlagAndReset then begin
    ViewUpdateLock;
    try
      VGPSPosition := FGPSRecorder.CurrentPosition;
      if (not VGPSPosition.PositionOK) then begin
        // no position
        Hide;
      end else begin
        // ok
        FPositionCS.BeginWrite;
        try
          FPositionLonLat := VGPSPosition.LonLat;
          FStopped := not VGPSPosition.SpeedOK;
          if not FStopped then begin
            FStopped := VGPSPosition.Speed_KMH <= FConfig.MinMoveSpeed;
          end;
          if not FStopped then begin
            FDirectionAngle := VGPSPosition.Heading;
          end else begin
            FDirectionAngle := 0;
          end;
        finally
          FPositionCS.EndWrite;
        end;
        Show;
        SetNeedRedraw;
      end;
    finally
      ViewUpdateUnlock;
    end;
  end;
end;

procedure TMapLayerGPSMarker.PaintLayer(
  ABuffer: TBitmap32;
  const ALocalConverter: ILocalCoordConverter
);
var
  VFixedOnView: TDoublePoint;
  VPositionLonLat: TDoublePoint;
  VStopped: Boolean;
  VDirection: Double;
begin
  FPositionCS.BeginRead;
  try
    VPositionLonLat := FPositionLonLat;
    VStopped := FStopped;
    VDirection := FDirectionAngle;
  finally
    FPositionCS.EndRead;
  end;
  if not PointIsEmpty(FPositionLonLat) then begin
    VFixedOnView := ALocalConverter.LonLat2LocalPixelFloat(FPositionLonLat);
    if VStopped then begin
      FStopedMarkerChangeable.GetStatic.DrawToBitmap(ABuffer, VFixedOnView);
    end else begin
      FArrowMarkerChangeable.GetStatic.DrawToBitmapWithDirection(ABuffer, VFixedOnView, VDirection);
    end;
  end;
end;

procedure TMapLayerGPSMarker.StartThreads;
begin
  inherited;
  OnConfigChange;
end;

end.
