{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_SensorTextFromNavToPoint;

interface

uses
  i_NavigationToPoint,
  i_ViewPortState,
  i_ValueToStringConverter,
  i_LocalCoordConverter,
  i_LanguageManager,
  i_Sensor,
  u_SensorBase;

type
  TSensorTextFromNavToPoint = class(TSensorBase, ISensorText)
  private
    FViewPortState: IViewPortState;
    FNavigationToPoint: INavigationToPoint;
    FVisualConverter: ILocalCoordConverter;
    FValueConverterConfig: IValueToStringConverterConfig;
    FValueConverter: IValueToStringConverter;

    procedure OnConverterConfigChange;
    procedure OnPosChanged;
    procedure OnNavToPointChanged;
  protected
    function GetText: string;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AViewPortState: IViewPortState;
      const ANavigationToPoint: INavigationToPoint;
      const AValueConverterConfig: IValueToStringConverterConfig
    );
  end;

implementation

uses
  c_SensorsGUIDSimple,
  t_GeoTypes,
  u_NotifyEventListener,
  u_ResStrings;

{ TSensorTextFromNavToPoint }

constructor TSensorTextFromNavToPoint.Create(
  const ALanguageManager: ILanguageManager;
  const AViewPortState: IViewPortState;
  const ANavigationToPoint: INavigationToPoint;
  const AValueConverterConfig: IValueToStringConverterConfig
);
begin
  inherited Create(False);
  FViewPortState := AViewPortState;
  FNavigationToPoint := ANavigationToPoint;
  FValueConverterConfig := AValueConverterConfig;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnPosChanged),
    FViewPortState.GetChangeNotifier
  );

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnNavToPointChanged),
    FNavigationToPoint.GetChangeNotifier
  );

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConverterConfigChange),
    FValueConverterConfig.GetChangeNotifier
  );

  OnConverterConfigChange;
end;

function TSensorTextFromNavToPoint.GetText: string;
var
  VValue: Double;
  VNavActive: Boolean;
  VNavLonLat: TDoublePoint;
  VCenterLonLat: TDoublePoint;
begin
  LockRead;
  try
    Result := '-';
    if FVisualConverter <> nil then begin
      FNavigationToPoint.LockRead;
      try
        VNavActive := FNavigationToPoint.IsActive;
        VNavLonLat := FNavigationToPoint.LonLat;
      finally
        FNavigationToPoint.UnlockRead;
      end;
      if VNavActive then begin
        VCenterLonLat := FVisualConverter.GetCenterLonLat;
        FVisualConverter.GetGeoConverter.CheckLonLatPos(VNavLonLat);
        FVisualConverter.GetGeoConverter.CheckLonLatPos(VCenterLonLat);
        VValue := FVisualConverter.GetGeoConverter.Datum.CalcDist(VNavLonLat, VCenterLonLat);
        Result := FValueConverter.DistConvert(VValue);
      end;
    end;
  finally
    UnlockRead;
  end;
end;

procedure TSensorTextFromNavToPoint.OnConverterConfigChange;
begin
  LockWrite;
  try
    FValueConverter := FValueConverterConfig.GetStatic;
  finally
    UnlockWrite;
  end;
  NotifyDataUpdate;
end;

procedure TSensorTextFromNavToPoint.OnNavToPointChanged;
begin
  NotifyDataUpdate;
end;

procedure TSensorTextFromNavToPoint.OnPosChanged;
begin
  LockWrite;
  try
    FVisualConverter := FViewPortState.GetVisualCoordConverter;
    NotifyDataUpdate;
  finally
    UnlockWrite;
  end;
end;

end.
