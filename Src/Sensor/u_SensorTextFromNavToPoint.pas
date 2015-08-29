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

unit u_SensorTextFromNavToPoint;

interface

uses
  i_Notifier,
  i_NavigationToPoint,
  i_LocalCoordConverterChangeable,
  i_Sensor,
  u_SensorBase;

type
  TSensorTextFromNavToPoint = class(TSensorDoubeleValue, ISensorDistance)
  private
    FViewPortState: ILocalCoordConverterChangeable;
    FNavigationToPoint: INavigationToPoint;

    FSourceDataUpdateNotifier: INotifierInternal;

    procedure OnPosChanged;
    procedure OnNavToPointChanged;
  protected
    function GetSensorTypeIID: TGUID; override;
    function GetCurrentValue: Double; override;
  public
    constructor Create(
      const AViewPortState: ILocalCoordConverterChangeable;
      const ANavigationToPoint: INavigationToPoint
    );
  end;

implementation

uses
  Math,
  t_GeoTypes,
  i_CoordConverter,
  i_ProjectionType,
  i_LocalCoordConverter,
  u_ListenerByEvent,
  u_Notifier,
  u_Synchronizer;

{ TSensorTextFromNavToPoint }

constructor TSensorTextFromNavToPoint.Create(
  const AViewPortState: ILocalCoordConverterChangeable;
  const ANavigationToPoint: INavigationToPoint
);
begin
  FSourceDataUpdateNotifier :=
    TNotifierBase.Create(
      GSync.SyncVariable.Make(Self.ClassName + 'Notifier')
    );
  inherited Create(FSourceDataUpdateNotifier);
  FViewPortState := AViewPortState;
  FNavigationToPoint := ANavigationToPoint;


  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnPosChanged),
    FViewPortState.GetChangeNotifier
  );

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnNavToPointChanged),
    FNavigationToPoint.GetChangeNotifier
  );
end;

function TSensorTextFromNavToPoint.GetCurrentValue: Double;
var
  VVisualConverter: ILocalCoordConverter;
  VProjectionType: IProjectionType;
  VNavActive: Boolean;
  VNavLonLat: TDoublePoint;
  VCenterLonLat: TDoublePoint;
begin
  VVisualConverter := FViewPortState.GetStatic;
  Result := NaN;
  if VVisualConverter <> nil then begin
    FNavigationToPoint.LockRead;
    try
      VNavActive := FNavigationToPoint.IsActive;
      VNavLonLat := FNavigationToPoint.LonLat;
    finally
      FNavigationToPoint.UnlockRead;
    end;
    if VNavActive then begin
      VProjectionType := VVisualConverter.ProjectionInfo.ProjectionType;

      VCenterLonLat := VVisualConverter.GetCenterLonLat;
      VProjectionType.ValidateLonLatPos(VNavLonLat);
      VProjectionType.ValidateLonLatPos(VCenterLonLat);
      Result := VProjectionType.Datum.CalcDist(VNavLonLat, VCenterLonLat);
    end;
  end;
end;

function TSensorTextFromNavToPoint.GetSensorTypeIID: TGUID;
begin
  Result := ISensorDistance;
end;

procedure TSensorTextFromNavToPoint.OnNavToPointChanged;
begin
  FSourceDataUpdateNotifier.Notify(nil);
end;

procedure TSensorTextFromNavToPoint.OnPosChanged;
begin
  FSourceDataUpdateNotifier.Notify(nil);
end;

end.
