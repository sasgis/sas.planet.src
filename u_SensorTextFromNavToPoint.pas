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
  i_JclNotify,
  i_NavigationToPoint,
  i_ViewPortState,
  i_LanguageManager,
  i_Sensor,
  u_SensorBase;

type
  TSensorTextFromNavToPoint = class(TSensorDoubeleValue, ISensorDistance)
  private
    FViewPortState: IViewPortState;
    FNavigationToPoint: INavigationToPoint;

    FSourceDataUpdateNotifier: IJclNotifier;

    procedure OnPosChanged;
    procedure OnNavToPointChanged;
  protected
    function GetCurrentValue: Double; override;
  public
    constructor Create(
      const AViewPortState: IViewPortState;
      const ANavigationToPoint: INavigationToPoint
    );
  end;

implementation

uses
  Math,
  t_GeoTypes,
  i_CoordConverter,
  i_LocalCoordConverter,
  u_NotifyEventListener,
  u_JclNotify,
  u_ResStrings;

{ TSensorTextFromNavToPoint }

constructor TSensorTextFromNavToPoint.Create(
  const AViewPortState: IViewPortState;
  const ANavigationToPoint: INavigationToPoint
);
begin
  FSourceDataUpdateNotifier := TJclBaseNotifier.Create;
  inherited Create(False, FSourceDataUpdateNotifier);
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
  VGeoConverter: ICoordConverter;
  VValue: Double;
  VNavActive: Boolean;
  VNavLonLat: TDoublePoint;
  VCenterLonLat: TDoublePoint;
begin
  VVisualConverter := FViewPortState.GetVisualCoordConverter;
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
      VGeoConverter := VVisualConverter.GeoConverter;

      VCenterLonLat := VVisualConverter.GetCenterLonLat;
      VGeoConverter.CheckLonLatPos(VNavLonLat);
      VGeoConverter.CheckLonLatPos(VCenterLonLat);
      Result := VGeoConverter.Datum.CalcDist(VNavLonLat, VCenterLonLat);
    end;
  end;
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
