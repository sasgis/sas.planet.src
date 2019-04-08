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
  i_NotifierOperation,  
  i_LocalCoordConverterChangeable,
  i_InternalPerformanceCounter,
  i_SunCalcShapesGenerator,
  i_SunCalcProvider,
  i_SunCalcConfig,
  u_WindowLayerBasicBase;

type
  TWindowLayerSunCalcInfoBase = class(TWindowLayerBasicBase)
  protected
    FLocation: TDoublePoint;
    FDateTime: TDateTime;

    FSunCalcConfig: ISunCalcConfig;
    FSunCalcProvider: ISunCalcProvider;
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
    procedure OnPosChange;
  protected
    procedure StartThreads; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      AParentMap: TImage32;
      const ALocalCoordConverter: ILocalCoordConverterChangeable;
      const ASunCalcConfig: ISunCalcConfig;
      const ASunCalcProvider: ISunCalcProvider
    );
    destructor Destroy; override;
  end;

implementation

uses
  Math,
  DateUtils,
  u_GeoFunc,
  u_ListenerByEvent,
  u_SunCalcShapesGenerator;

{ TWindowLayerSunCalcInfoBase }

constructor TWindowLayerSunCalcInfoBase.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const ALocalCoordConverter: ILocalCoordConverterChangeable;
  const ASunCalcConfig: ISunCalcConfig;
  const ASunCalcProvider: ISunCalcProvider
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
    TNotifyNoMmgEventListener.Create(Self.OnPosChange),
    FLocalCoordConverter.ChangeNotifier
  );

  FShapesGenerator :=
    TSunCalcShapesGenerator.Create(
      ALocalCoordConverter,
      FSunCalcConfig.CircleRadius
    );

  FRepaintOnDayChange := True;
  FRepaintOnTimeChange := True;
  FRepaintOnLocationChange := True;
end;

destructor TWindowLayerSunCalcInfoBase.Destroy;
begin
  inherited Destroy;
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

    FShapesGenerator.SetLocation(VLocation);
    FShapesGenerator.SetDateTime(VDateTime);

    if FShapesGenerator.IsIntersectScreenRect then begin
      if (FRepaintOnLocationChange and not DoublePointsEqual(FLocation, VLocation)) or
         (FRepaintOnDayChange and not SameDate(FDateTime, VDateTime)) or
         (FRepaintOnTimeChange and not SameDateTime(FDateTime, VDateTime))
      then begin
        SetNeedFullRepaintLayer;
      end;
    end;

    FLocation := VLocation;
    FDateTime := VDateTime;
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

procedure TWindowLayerSunCalcInfoBase.StartThreads;
begin
  inherited;
  OnSunCalcConfigChange;
  OnSunCalcProviderChange;
end;

end.
