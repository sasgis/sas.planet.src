{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2017, SAS.Planet development team.                      *}
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

unit u_WindowLayerSunCalcInfoBase;

interface

uses
  GR32_Image,
  GR32_Layers,
  t_GeoTypes,
  t_SunCalcConfig,
  i_NotifierTime,
  i_NotifierOperation,
  i_LocalCoordConverterChangeable,
  i_InternalPerformanceCounter,
  i_SunCalcShapesGenerator,
  i_SunCalcProvider,
  i_SunCalcDataProvider,
  i_SunCalcConfig,
  u_WindowLayerBasicBase;

type
  TWindowLayerSunCalcInfoBase = class(TWindowLayerBasicBase)
  protected
    FLocation: TDoublePoint;
    FDateTime: TDateTime;
    FTzOffset: Extended;

    FSunCalcConfig: ISunCalcConfig;
    FSunCalcProvider: ISunCalcProvider;
    FSunCalcDataProvider: ISunCalcDataProvider;

    FLocalCoordConverter: ILocalCoordConverterChangeable;

    FFont: TSunCalcFontInfo;
    FColor: TSunCalcDetailsPanelColors;

    FShapesColors: TSunCalcShapesColors;
    FShapesGenerator: ISunCalcShapesGenerator;

    FRepaintOnDayChange: Boolean;
    FRepaintOnTimeChange: Boolean;
    FRepaintOnLocationChange: Boolean;

    procedure OnSunCalcConfigChange;
    procedure OnSunCalcProviderChange;
    procedure OnSunCalcDataProviderChange;
    procedure OnPosChange;
    procedure OnTimer;
  protected
    procedure StartThreads; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      const AParentMap: TImage32;
      const ALocalCoordConverter: ILocalCoordConverterChangeable;
      const ASunCalcConfig: ISunCalcConfig;
      const ASunCalcProvider: ISunCalcProvider;
      const ATimerNoifier: INotifierTime = nil
    );
  end;

implementation

uses
  Math,
  SysUtils,
  DateUtils,
  u_GeoFunc,
  u_ListenerTime,
  u_ListenerByEvent,
  u_TimeZoneInfo,
  u_SunCalcShapesGenerator;

{ TWindowLayerSunCalcInfoBase }

constructor TWindowLayerSunCalcInfoBase.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  const AParentMap: TImage32;
  const ALocalCoordConverter: ILocalCoordConverterChangeable;
  const ASunCalcConfig: ISunCalcConfig;
  const ASunCalcProvider: ISunCalcProvider;
  const ATimerNoifier: INotifierTime
);
begin
  Assert(ASunCalcProvider <> nil);

  inherited Create(
    APerfList,
    AAppStartedNotifier,
    AAppClosingNotifier,
    TCustomLayer.Create(AParentMap.Layers)
  );

  FLocalCoordConverter := ALocalCoordConverter;
  FSunCalcConfig := ASunCalcConfig;
  FSunCalcProvider := ASunCalcProvider;

  FLocation := CEmptyDoublePoint;
  FDateTime := 0;
  FTzOffset := 0;
  FSunCalcDataProvider := FSunCalcProvider.GetDataProviderChangeable.GetStatic;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnSunCalcConfigChange),
    FSunCalcConfig.ChangeNotifier
  );

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnSunCalcConfigChange),
    FSunCalcConfig.ColorSchemaList.ChangeNotifier
  );

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnSunCalcProviderChange),
    FSunCalcProvider.ChangeNotifier
  );

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnSunCalcDataProviderChange),
    FSunCalcProvider.GetDataProviderChangeable.ChangeNotifier
  );

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnPosChange),
    FLocalCoordConverter.ChangeNotifier
  );

  if ATimerNoifier <> nil then begin
    LinksList.Add(
      TListenerTimeCheck.Create(Self.OnTimer, 5000),
      ATimerNoifier
    );
  end;

  FShapesGenerator :=
    TSunCalcShapesGenerator.Create(
      ALocalCoordConverter,
      FSunCalcConfig.CircleRadius
    );

  FRepaintOnDayChange := True;
  FRepaintOnTimeChange := True;
  FRepaintOnLocationChange := True;
end;

procedure TWindowLayerSunCalcInfoBase.OnSunCalcConfigChange;
var
  VColorSchema: ISunCalcColorSchema;
begin
  ViewUpdateLock;
  try
    Visible := FSunCalcConfig.Visible;

    VColorSchema := FSunCalcConfig.ColorSchemaList.GetActiveColorSchema;

    FFont := VColorSchema.DetailsPanelFont;
    FColor := VColorSchema.DetailsPanelColors;
    FShapesColors := VColorSchema.ShapesColors;

    SetNeedUpdateLayerVisibility;
    SetNeedFullRepaintLayer;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TWindowLayerSunCalcInfoBase.OnSunCalcProviderChange;
var
  VLocation: TDoublePoint;
  VDateTime: TDateTime;
  VTzOffset: Extended;
  VIsSameDate: Boolean;
  VIsSameDateTime: Boolean;
  VIsSameLocation: Boolean;
begin
  ViewUpdateLock;
  try
    FSunCalcProvider.LockRead;
    try
      VLocation := FSunCalcProvider.Location;
      VDateTime := FSunCalcProvider.UTCDateTime;
    finally
      FSunCalcProvider.UnlockRead;
    end;

    if (VDateTime = 0) or PointIsEmpty(VLocation) then begin
      Exit;
    end;

    VIsSameLocation := DoublePointsEqual(FLocation, VLocation);
    VIsSameDate := SameDate(FDateTime, VDateTime);
    VIsSameDateTime := VIsSameDate and SameDateTime(FDateTime, VDateTime);

    if not VIsSameLocation or not VIsSameDate then begin
      if not FSunCalcProvider.GetTzOffset(VDateTime, VTzOffset) then begin
        VTzOffset := TTimeZoneInfo.GetSystemTzOffset(VDateTime);
      end;
    end else begin
      VTzOffset := FTzOffset;
    end;

    if not VIsSameLocation then begin
      FShapesGenerator.SetLocation(VLocation);
    end;

    FShapesGenerator.SetDateTime(VDateTime, VTzOffset);

    if FShapesGenerator.IsIntersectScreenRect then begin
      if (FRepaintOnLocationChange and not VIsSameLocation) or
         (FRepaintOnDayChange and not VIsSameDate) or
         (FRepaintOnTimeChange and not VIsSameDateTime)
      then begin
        SetNeedFullRepaintLayer;
      end;
    end;

    FLocation := VLocation;
    FDateTime := VDateTime;
    FTzOffset := VTzOffset;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TWindowLayerSunCalcInfoBase.OnSunCalcDataProviderChange;
begin
  ViewUpdateLock;
  try
    FSunCalcDataProvider := FSunCalcProvider.GetDataProviderChangeable.GetStatic;
    FShapesGenerator.SetDataProvider(FSunCalcDataProvider);
    SetNeedFullRepaintLayer;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TWindowLayerSunCalcInfoBase.OnPosChange;
begin
  if Visible then begin
    ViewUpdateLock;
    try
      if FShapesGenerator.IsIntersectScreenRect then begin
        SetNeedFullRepaintLayer;
      end;
    finally
      ViewUpdateUnlock;
    end;
  end;
end;

procedure TWindowLayerSunCalcInfoBase.OnTimer;
begin
  if Visible and FSunCalcConfig.IsRealTime then begin
    FSunCalcProvider.LocalDateTime := Now;
  end;
end;

procedure TWindowLayerSunCalcInfoBase.StartThreads;
begin
  inherited;
  OnSunCalcConfigChange;
  OnSunCalcProviderChange;
  OnSunCalcDataProviderChange;
end;

end.
