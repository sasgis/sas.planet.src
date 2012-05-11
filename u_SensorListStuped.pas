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

unit u_SensorListStuped;

interface

uses
  i_NavigationToPoint,
  i_ViewPortState,
  i_GPSRecorder,
  i_ValueToStringConverter,
  i_LanguageManager,
  u_SensorListBase;

type
  TSensorListStuped = class(TSensorListBase)
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AViewPortState: IViewPortState;
      const ANavigationToPoint: INavigationToPoint;
      const AGPSRecorder: IGPSRecorder;
      const AValueConverterConfig: IValueToStringConverterConfig
    );
  end;

implementation

uses
  i_SensorList,
  u_SensorTextFromNavToPoint,
  u_SensorBatteryStatus,
  u_SensorsFromGPSRecorder;


{ TSensorListStuped }

constructor TSensorListStuped.Create(
  const ALanguageManager: ILanguageManager;
  const AViewPortState: IViewPortState;
  const ANavigationToPoint: INavigationToPoint;
  const AGPSRecorder: IGPSRecorder;
  const AValueConverterConfig: IValueToStringConverterConfig
);
var
  VEntity: ISensorListEntity;
begin
  inherited Create;
  VEntity := TSensorFromGPSRecorderLastSpeed.Create(ALanguageManager, AGPSRecorder, AValueConverterConfig);
  Self.Add(VEntity);

  VEntity := TSensorFromGPSRecorderAvgSpeed.Create(ALanguageManager, AGPSRecorder, AValueConverterConfig);
  Self.Add(VEntity);

  VEntity := TSensorFromGPSRecorderMaxSpeed.Create(ALanguageManager, AGPSRecorder, AValueConverterConfig);
  Self.Add(VEntity);

  VEntity := TSensorFromGPSRecorderDist.Create(ALanguageManager, AGPSRecorder, AValueConverterConfig);
  Self.Add(VEntity);

  VEntity := TSensorFromGPSRecorderOdometer1.Create(ALanguageManager, AGPSRecorder, AValueConverterConfig);
  Self.Add(VEntity);

  VEntity := TSensorFromGPSRecorderOdometer2.Create(ALanguageManager, AGPSRecorder, AValueConverterConfig);
  Self.Add(VEntity);

  VEntity := TSensorTextFromNavToPoint.Create(ALanguageManager, AViewPortState, ANavigationToPoint, AValueConverterConfig);
  Self.Add(VEntity);

  VEntity := TSensorBatteryStatus.Create(ALanguageManager);
  Self.Add(VEntity);

  VEntity := TSensorFromGPSRecorderAltitude.Create(ALanguageManager, AGPSRecorder, AValueConverterConfig);
  Self.Add(VEntity);

  VEntity := TSensorFromGPSRecorderHeading.Create(ALanguageManager, AGPSRecorder, AValueConverterConfig);
  Self.Add(VEntity);

  VEntity := TSensorFromGPSRecorderHDOP.Create(ALanguageManager, AGPSRecorder, AValueConverterConfig);
  Self.Add(VEntity);

  VEntity := TSensorFromGPSRecorderVDOP.Create(ALanguageManager, AGPSRecorder, AValueConverterConfig);
  Self.Add(VEntity);

  VEntity := TSensorFromGPSRecorderUTCTime.Create(ALanguageManager, AGPSRecorder, AValueConverterConfig);
  Self.Add(VEntity);

  VEntity := TSensorFromGPSRecorderLocalTime.Create(ALanguageManager, AGPSRecorder, AValueConverterConfig);
  Self.Add(VEntity);

  VEntity := TSensorFromGPSRecorderDGPS.Create(ALanguageManager, AGPSRecorder, AValueConverterConfig);
  Self.Add(VEntity);

  VEntity := TSensorFromGPSRecorderGPSUnitInfo.Create(ALanguageManager, AGPSRecorder, AValueConverterConfig);
  Self.Add(VEntity);
end;

end.
