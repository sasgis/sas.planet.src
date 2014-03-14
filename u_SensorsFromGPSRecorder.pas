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

unit u_SensorsFromGPSRecorder;

interface

uses
  i_GPS,
  i_GPSModule,
  i_GPSRecorder,
  i_SystemTimeProvider,
  i_Sensor,
  u_SensorFromGPSRecorderBase;

type
  TSensorFromGPSRecorderLastSpeed = class(TSensorDoubeleValueFromGPSRecorder, ISensorSpeed)
  protected
    function GetSensorTypeIID: TGUID; override;
    function GetCurrentValue: Double; override;
  end;

  TSensorFromGPSRecorderAvgSpeed = class(TSensorDoubeleValueFromGPSRecorder, ISensorSpeed, ISensorResetable)
  protected
    function GetSensorTypeIID: TGUID; override;
    function GetCurrentValue: Double; override;
    procedure Reset;
  end;

  TSensorFromGPSRecorderMaxSpeed = class(TSensorDoubeleValueFromGPSRecorder, ISensorSpeed, ISensorResetable)
  protected
    function GetSensorTypeIID: TGUID; override;
    function GetCurrentValue: Double; override;
    procedure Reset;
  end;

  TSensorFromGPSRecorderDist = class(TSensorDoubeleValueFromGPSRecorder, ISensorDistance, ISensorResetable)
  protected
    function GetSensorTypeIID: TGUID; override;
    function GetCurrentValue: Double; override;
    procedure Reset;
  end;

  TSensorFromGPSRecorderOdometer1 = class(TSensorDoubeleValueFromGPSRecorder, ISensorDistance, ISensorResetable)
  protected
    function GetSensorTypeIID: TGUID; override;
    function GetCurrentValue: Double; override;
    procedure Reset;
  end;

  TSensorFromGPSRecorderOdometer2 = class(TSensorDoubeleValueFromGPSRecorder, ISensorDistance, ISensorResetable)
  protected
    function GetSensorTypeIID: TGUID; override;
    function GetCurrentValue: Double; override;
    procedure Reset;
  end;

  TSensorFromGPSRecorderAltitude = class(TSensorDoubeleValueFromGPSRecorder, ISensorDistance)
  protected
    function GetSensorTypeIID: TGUID; override;
    function GetCurrentValue: Double; override;
  end;

  TSensorFromGPSRecorderHeading = class(TSensorDoubeleValueFromGPSRecorder, ISensorDegrees)
  protected
    function GetSensorTypeIID: TGUID; override;
    function GetCurrentValue: Double; override;
  end;

  TSensorFromGPSRecorderHDOP = class(TSensorDoubeleValueFromGPSRecorder, ISensorDouble)
  protected
    function GetSensorTypeIID: TGUID; override;
    function GetCurrentValue: Double; override;
  end;

  TSensorFromGPSRecorderVDOP = class(TSensorDoubeleValueFromGPSRecorder, ISensorDouble)
  protected
    function GetSensorTypeIID: TGUID; override;
    function GetCurrentValue: Double; override;
  end;

  TSensorFromGPSRecorderUTCTime = class(TSensorDateTimeValueFromGPSRecorder, ISensorTime)
  protected
    function GetSensorTypeIID: TGUID; override;
    function GetCurrentValue: TDateTime; override;
  end;

  TSensorFromGPSRecorderLocalTime = class(TSensorDateTimeValueFromGPSRecorder, ISensorTime, ISensorResetable)
  private
    FSystemTime: ISystemTimeProvider;
    FGPSModule: IGPSModule;
  protected
    function GetSensorTypeIID: TGUID; override;
    function GetCurrentValue: TDateTime; override;
    procedure Reset;
  public
    constructor Create(
      const ASystemTime: ISystemTimeProvider;
      const AGPSRecorder: IGPSRecorder;
      const AGPSModule: IGPSModule
    );
  end;

  TSensorFromGPSRecorderDGPS = class(TSensorTextValueFromGPSRecorder, ISensorText, ISensorResetable)
  private
    FGPSModule: IGPSModule;
  protected
    function GetSensorTypeIID: TGUID; override;
    function GetCurrentValue: string; override;
    procedure Reset;
  public
    constructor Create(
      const AGPSRecorder: IGPSRecorder;
      const AGPSModule: IGPSModule
    );
  end;

  TSensorFromGPSRecorderGPSUnitInfo = class(TSensorTextValueFromGPSRecorder, ISensorText, ISensorResetable)
  private
    FGPSModule: IGPSModule;
  protected
    function GetSensorTypeIID: TGUID; override;
    function GetCurrentValue: string; override;
    procedure Reset;
  public
    constructor Create(
      const AGPSRecorder: IGPSRecorder;
      const AGPSModule: IGPSModule
    );
  end;

  TSensorFromGPSRecorderGPSSatellites = class(TSensorGPSSatellitesValueFromGPSRecorder, ISensorGPSSatellites)
  protected
    function GetSensorTypeIID: TGUID; override;
    function GetCurrentValue: IGPSSatellitesInView; override;
  end;

implementation

{ TSensorFromGPSRecorderLastSpeed }

function TSensorFromGPSRecorderLastSpeed.GetCurrentValue: Double;
begin
  Result := GPSRecorder.LastSpeed;
end;

function TSensorFromGPSRecorderLastSpeed.GetSensorTypeIID: TGUID;
begin
  Result := ISensorSpeed;
end;

{ TSensorFromGPSRecorderAvgSpeed }

function TSensorFromGPSRecorderAvgSpeed.GetCurrentValue: Double;
begin
  Result := GPSRecorder.AvgSpeed;
end;

function TSensorFromGPSRecorderAvgSpeed.GetSensorTypeIID: TGUID;
begin
  Result := ISensorSpeed;
end;

procedure TSensorFromGPSRecorderAvgSpeed.Reset;
begin
  inherited;
  GPSRecorder.ResetAvgSpeed;
end;

{ TSensorFromGPSRecorderMaxSpeed }

function TSensorFromGPSRecorderMaxSpeed.GetCurrentValue: Double;
begin
  Result := GPSRecorder.MaxSpeed;
end;

function TSensorFromGPSRecorderMaxSpeed.GetSensorTypeIID: TGUID;
begin
  Result := ISensorSpeed;
end;

procedure TSensorFromGPSRecorderMaxSpeed.Reset;
begin
  inherited;
  GPSRecorder.ResetMaxSpeed;
end;

{ TSensorFromGPSRecorderDist }

function TSensorFromGPSRecorderDist.GetCurrentValue: Double;
begin
  Result := GPSRecorder.Dist;
end;

function TSensorFromGPSRecorderDist.GetSensorTypeIID: TGUID;
begin
  Result := ISensorDistance;
end;

procedure TSensorFromGPSRecorderDist.Reset;
begin
  inherited;
  GPSRecorder.ResetDist;
end;

{ TSensorFromGPSRecorderOdometer1 }

function TSensorFromGPSRecorderOdometer1.GetCurrentValue: Double;
begin
  Result := GPSRecorder.Odometer1;
end;

function TSensorFromGPSRecorderOdometer1.GetSensorTypeIID: TGUID;
begin
  Result := ISensorDistance;
end;

procedure TSensorFromGPSRecorderOdometer1.Reset;
begin
  inherited;
  GPSRecorder.ResetOdometer1;
end;

{ TSensorFromGPSRecorderOdometer2 }

function TSensorFromGPSRecorderOdometer2.GetCurrentValue: Double;
begin
  Result := GPSRecorder.Odometer2;
end;

function TSensorFromGPSRecorderOdometer2.GetSensorTypeIID: TGUID;
begin
  Result := ISensorDistance;
end;

procedure TSensorFromGPSRecorderOdometer2.Reset;
begin
  inherited;
  GPSRecorder.ResetOdometer2;
end;

{ TSensorFromGPSRecorderAltitude }

function TSensorFromGPSRecorderAltitude.GetCurrentValue: Double;
begin
  Result := GPSRecorder.LastAltitude;
end;

function TSensorFromGPSRecorderAltitude.GetSensorTypeIID: TGUID;
begin
  Result := ISensorDistance;
end;

{ TSensorFromGPSRecorderHeading }

function TSensorFromGPSRecorderHeading.GetCurrentValue: Double;
begin
  Result := GPSRecorder.LastHeading;
end;

function TSensorFromGPSRecorderHeading.GetSensorTypeIID: TGUID;
begin
  Result := ISensorDegrees;
end;

{ TSensorFromGPSRecorderHDOP }

function TSensorFromGPSRecorderHDOP.GetCurrentValue: Double;
var
  VPosition: IGPSPosition;
begin
  VPosition := GPSRecorder.CurrentPosition;
  Result := 0;
  if VPosition <> nil then begin
    Result := VPosition.HDOP;
  end;
end;

function TSensorFromGPSRecorderHDOP.GetSensorTypeIID: TGUID;
begin
  Result := ISensorDouble;
end;

{ TSensorFromGPSRecorderVDOP }

function TSensorFromGPSRecorderVDOP.GetCurrentValue: Double;
var
  VPosition: IGPSPosition;
begin
  VPosition := GPSRecorder.CurrentPosition;
  Result := 0;
  if VPosition <> nil then begin
    Result := VPosition.VDOP;
  end;
end;

function TSensorFromGPSRecorderVDOP.GetSensorTypeIID: TGUID;
begin
  Result := ISensorDouble;
end;

{ TSensorFromGPSRecorderUTCTime }

function TSensorFromGPSRecorderUTCTime.GetCurrentValue: TDateTime;
var
  VPosition: IGPSPosition;
begin
  VPosition := GPSRecorder.CurrentPosition;
  Result := 0;
  if VPosition <> nil then begin
    Result := VPosition.UTCTime;
  end;
end;

function TSensorFromGPSRecorderUTCTime.GetSensorTypeIID: TGUID;
begin
  Result := ISensorTime;
end;

{ TSensorFromGPSRecorderLocalTime }

constructor TSensorFromGPSRecorderLocalTime.Create(
  const ASystemTime: ISystemTimeProvider;
  const AGPSRecorder: IGPSRecorder;
  const AGPSModule: IGPSModule
);
begin
  inherited Create(AGPSRecorder);
  FGPSModule := AGPSModule;
  FSystemTime := ASystemTime;
end;

function TSensorFromGPSRecorderLocalTime.GetCurrentValue: TDateTime;
var
  VPosition: IGPSPosition;
begin
  VPosition := GPSRecorder.CurrentPosition;
  Result := 0;
  if VPosition <> nil then begin
    Result := VPosition.UTCTime;
  end;
  if (0 <> Result) then begin
    Result := FSystemTime.UTCToLocalTime(Result);
  end;
end;

function TSensorFromGPSRecorderLocalTime.GetSensorTypeIID: TGUID;
begin
  Result := ISensorTime;
end;

procedure TSensorFromGPSRecorderLocalTime.Reset;
begin
  inherited;
  FGPSModule.ApplyUTCDateTime;
end;

{ TSensorFromGPSRecorderDGPS }

constructor TSensorFromGPSRecorderDGPS.Create(const AGPSRecorder: IGPSRecorder;
  const AGPSModule: IGPSModule);
begin
  inherited Create(AGPSRecorder);
  FGPSModule := AGPSModule;
end;

function TSensorFromGPSRecorderDGPS.GetCurrentValue: string;
var
  VPosition: IGPSPosition;
begin
  VPosition := GPSRecorder.CurrentPosition;
  Result := '';
  if VPosition <> nil then begin
    Result := VPosition.DGPS;
  end;
end;

function TSensorFromGPSRecorderDGPS.GetSensorTypeIID: TGUID;
begin
  Result := ISensorText;
end;

procedure TSensorFromGPSRecorderDGPS.Reset;
begin
  inherited;
  FGPSModule.ResetDGPS;
end;

{ TSensorFromGPSRecorderGPSUnitInfo }

constructor TSensorFromGPSRecorderGPSUnitInfo.Create(
  const AGPSRecorder: IGPSRecorder; const AGPSModule: IGPSModule);
begin
  inherited Create(AGPSRecorder);
  FGPSModule := AGPSModule;
end;

function TSensorFromGPSRecorderGPSUnitInfo.GetCurrentValue: string;
begin
  Result := FGPSModule.GPSUnitInfo;
end;

function TSensorFromGPSRecorderGPSUnitInfo.GetSensorTypeIID: TGUID;
begin
  Result := ISensorText;
end;

procedure TSensorFromGPSRecorderGPSUnitInfo.Reset;
begin
  inherited;
  FGPSModule.ResetUnitInfo;
end;

{ TSensorFromGPSRecorderGPSSatellites }

function TSensorFromGPSRecorderGPSSatellites.GetCurrentValue: IGPSSatellitesInView;
var
  VPosition: IGPSPosition;
begin
  Result := nil;
  VPosition := GPSRecorder.CurrentPosition;
  if VPosition <> nil then begin
    Result := VPosition.Satellites;
  end;
end;

function TSensorFromGPSRecorderGPSSatellites.GetSensorTypeIID: TGUID;
begin
  Result := ISensorGPSSatellites;
end;

end.
