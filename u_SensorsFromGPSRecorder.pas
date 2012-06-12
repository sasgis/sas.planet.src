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

unit u_SensorsFromGPSRecorder;

interface

uses
  i_GPSRecorder,
  i_Sensor,
  u_SensorTextFromGPSRecorder;

type
  TSensorFromGPSRecorderLastSpeed = class(TSensorDoubeleValueFromGPSRecorder, ISensorSpeed)
  protected
    function GetCurrentValue: Double; override;
  public
    constructor Create(
      const AGPSRecorder: IGPSRecorder
    );
  end;

  TSensorFromGPSRecorderAvgSpeed = class(TSensorDoubeleValueFromGPSRecorder, ISensorSpeed)
  protected
    function GetCurrentValue: Double; override;
    procedure Reset; override;
  public
    constructor Create(
      const AGPSRecorder: IGPSRecorder
    );
  end;

  TSensorFromGPSRecorderMaxSpeed = class(TSensorDoubeleValueFromGPSRecorder, ISensorSpeed)
  protected
    function GetCurrentValue: Double; override;
    procedure Reset; override;
  public
    constructor Create(
      const AGPSRecorder: IGPSRecorder
    );
  end;

  TSensorFromGPSRecorderDist = class(TSensorDoubeleValueFromGPSRecorder, ISensorDistance)
  protected
    function GetCurrentValue: Double; override;
    procedure Reset; override;
  public
    constructor Create(
      const AGPSRecorder: IGPSRecorder
    );
  end;

  TSensorFromGPSRecorderOdometer1 = class(TSensorDoubeleValueFromGPSRecorder, ISensorDistance)
  protected
    function GetCurrentValue: Double; override;
    procedure Reset; override;
  public
    constructor Create(
      const AGPSRecorder: IGPSRecorder
    );
  end;

  TSensorFromGPSRecorderOdometer2 = class(TSensorDoubeleValueFromGPSRecorder, ISensorDistance)
  protected
    function GetCurrentValue: Double; override;
    procedure Reset; override;
  public
    constructor Create(
      const AGPSRecorder: IGPSRecorder
    );
  end;

  TSensorFromGPSRecorderAltitude = class(TSensorDoubeleValueFromGPSRecorder, ISensorDistance)
  protected
    function GetCurrentValue: Double; override;
  public
    constructor Create(
      const AGPSRecorder: IGPSRecorder
    );
  end;

  TSensorFromGPSRecorderHeading = class(TSensorDoubeleValueFromGPSRecorder, ISensorDegrees)
  protected
    function GetCurrentValue: Double; override;
  public
    constructor Create(
      const AGPSRecorder: IGPSRecorder
    );
  end;

  TSensorFromGPSRecorderHDOP = class(TSensorDoubeleValueFromGPSRecorder, ISensorDouble)
  protected
    function GetCurrentValue: Double; override;
  public
    constructor Create(
      const AGPSRecorder: IGPSRecorder
    );
  end;

  TSensorFromGPSRecorderVDOP = class(TSensorDoubeleValueFromGPSRecorder, ISensorDouble)
  protected
    function GetCurrentValue: Double; override;
  public
    constructor Create(
      const AGPSRecorder: IGPSRecorder
    );
  end;

  TSensorFromGPSRecorderUTCTime = class(TSensorDateTimeValueFromGPSRecorder, ISensorTime)
  protected
    function GetCurrentValue: TDateTime; override;
  public
    constructor Create(
      const AGPSRecorder: IGPSRecorder
    );
  end;

  TSensorFromGPSRecorderLocalTime = class(TSensorDateTimeValueFromGPSRecorder, ISensorTime)
  protected
    function GetCurrentValue: TDateTime; override;
    procedure Reset; override;
  public
    constructor Create(
      const AGPSRecorder: IGPSRecorder
    );
  end;

  TSensorFromGPSRecorderDGPS = class(TSensorTextValueFromGPSRecorder, ISensorText)
  protected
    function GetCurrentValue: string; override;
    procedure Reset; override;
  public
    constructor Create(
      const AGPSRecorder: IGPSRecorder
    );
  end;

  TSensorFromGPSRecorderGPSUnitInfo = class(TSensorTextValueFromGPSRecorder, ISensorText)
  protected
    function GetCurrentValue: string; override;
    procedure Reset; override;
  public
    constructor Create(
      const AGPSRecorder: IGPSRecorder
    );
  end;

implementation

uses
  SysUtils,
  i_GPS,
  u_GeoToStr,
  vsagps_public_base,
  vsagps_public_position,
  vsagps_public_time;

{ TSensorFromGPSRecorderLastSpeed }

constructor TSensorFromGPSRecorderLastSpeed.Create(
  const AGPSRecorder: IGPSRecorder
 );
begin
  inherited Create(False, AGPSRecorder);
end;

function TSensorFromGPSRecorderLastSpeed.GetCurrentValue: Double;
begin
  Result := GPSRecorder.LastSpeed;
end;

{ TSensorFromGPSRecorderAvgSpeed }

constructor TSensorFromGPSRecorderAvgSpeed.Create(
  const AGPSRecorder: IGPSRecorder
);
begin
  inherited Create(True, AGPSRecorder);
end;

function TSensorFromGPSRecorderAvgSpeed.GetCurrentValue: Double;
begin
  Result := GPSRecorder.AvgSpeed;
end;

procedure TSensorFromGPSRecorderAvgSpeed.Reset;
begin
  inherited;
  GPSRecorder.ResetAvgSpeed;
end;

{ TSensorFromGPSRecorderMaxSpeed }

constructor TSensorFromGPSRecorderMaxSpeed.Create(
  const AGPSRecorder: IGPSRecorder
);
begin
  inherited Create(True, AGPSRecorder);
end;

function TSensorFromGPSRecorderMaxSpeed.GetCurrentValue: Double;
begin
  Result := GPSRecorder.MaxSpeed;
end;

procedure TSensorFromGPSRecorderMaxSpeed.Reset;
begin
  inherited;
  GPSRecorder.ResetMaxSpeed;
end;

{ TSensorFromGPSRecorderDist }

constructor TSensorFromGPSRecorderDist.Create(
  const AGPSRecorder: IGPSRecorder
);
begin
  inherited Create(True, AGPSRecorder);
end;

function TSensorFromGPSRecorderDist.GetCurrentValue: Double;
begin
  Result := GPSRecorder.Dist;
end;

procedure TSensorFromGPSRecorderDist.Reset;
begin
  inherited;
  GPSRecorder.ResetDist;
end;

{ TSensorFromGPSRecorderOdometer1 }

constructor TSensorFromGPSRecorderOdometer1.Create(
  const AGPSRecorder: IGPSRecorder
);
begin
  inherited Create(True, AGPSRecorder);
end;

function TSensorFromGPSRecorderOdometer1.GetCurrentValue: Double;
begin
  Result := GPSRecorder.Odometer1;
end;

procedure TSensorFromGPSRecorderOdometer1.Reset;
begin
  inherited;
  GPSRecorder.ResetOdometer1;
end;

{ TSensorFromGPSRecorderOdometer2 }

constructor TSensorFromGPSRecorderOdometer2.Create(
  const AGPSRecorder: IGPSRecorder
);
begin
  inherited Create(True, AGPSRecorder);
end;

function TSensorFromGPSRecorderOdometer2.GetCurrentValue: Double;
begin
  Result := GPSRecorder.Odometer2;
end;

procedure TSensorFromGPSRecorderOdometer2.Reset;
begin
  inherited;
  GPSRecorder.ResetOdometer2;
end;

{ TSensorFromGPSRecorderAltitude }

constructor TSensorFromGPSRecorderAltitude.Create(
  const AGPSRecorder: IGPSRecorder
);
begin
  inherited Create(False, AGPSRecorder);
end;

function TSensorFromGPSRecorderAltitude.GetCurrentValue: Double;
begin
  Result := GPSRecorder.LastAltitude;
end;

{ TSensorFromGPSRecorderHeading }

constructor TSensorFromGPSRecorderHeading.Create(
  const AGPSRecorder: IGPSRecorder
);
begin
  inherited Create(False, AGPSRecorder);
end;

function TSensorFromGPSRecorderHeading.GetCurrentValue: Double;
begin
  Result := GPSRecorder.LastHeading;
end;

{ TSensorFromGPSRecorderHDOP }

constructor TSensorFromGPSRecorderHDOP.Create(
  const AGPSRecorder: IGPSRecorder
);
begin
  inherited Create(False, AGPSRecorder);
end;

function TSensorFromGPSRecorderHDOP.GetCurrentValue: Double;
var
  VPosition: IGPSPosition;
begin
  VPosition := GPSRecorder.CurrentPosition;
  Result := VPosition.GetPosParams^.HDOP;
end;

{ TSensorFromGPSRecorderVDOP }

constructor TSensorFromGPSRecorderVDOP.Create(
  const AGPSRecorder: IGPSRecorder
);
begin
  inherited Create(False, AGPSRecorder);
end;

function TSensorFromGPSRecorderVDOP.GetCurrentValue: Double;
var
  VPosition: IGPSPosition;
begin
  VPosition := GPSRecorder.CurrentPosition;
  Result := VPosition.GetPosParams^.VDOP;
end;

{ TSensorFromGPSRecorderUTCTime }

constructor TSensorFromGPSRecorderUTCTime.Create(
  const AGPSRecorder: IGPSRecorder
);
begin
  inherited Create(False, AGPSRecorder);
end;

function TSensorFromGPSRecorderUTCTime.GetCurrentValue: TDateTime;
var
  VPosition: IGPSPosition;
begin
  VPosition := GPSRecorder.CurrentPosition;
  with VPosition.GetPosParams^ do begin
    Result := (UTCDate + UTCTime);
  end;
end;

{ TSensorFromGPSRecorderLocalTime }

constructor TSensorFromGPSRecorderLocalTime.Create(
  const AGPSRecorder: IGPSRecorder
);
begin
  inherited Create(True, AGPSRecorder);
end;

function TSensorFromGPSRecorderLocalTime.GetCurrentValue: TDateTime;
var
  VPosition: IGPSPosition;
begin
  VPosition := GPSRecorder.CurrentPosition;
  with VPosition.GetPosParams^ do begin
    Result := (UTCDate + UTCTime);
  end;
  if (0 <> Result) then begin
    Result := SystemTimeToLocalTime(Result);
  end;
end;

procedure TSensorFromGPSRecorderLocalTime.Reset;
begin
  inherited;
  GPSRecorder.ExecuteGPSCommand(Self, cUnitIndex_ALL, gpsc_Apply_UTCDateTime, nil);
end;

{ TSensorFromGPSRecorderDGPS }

constructor TSensorFromGPSRecorderDGPS.Create(
  const AGPSRecorder: IGPSRecorder
);
begin
  inherited Create(True, AGPSRecorder);
end;

function TSensorFromGPSRecorderDGPS.GetCurrentValue: string;
var
  VPosition: IGPSPosition;
begin
  VPosition := GPSRecorder.CurrentPosition;
  with VPosition.GetPosParams^.DGPS do begin
    case Nmea23_Mode of
      'A': begin
        Result := 'A';
      end; //'Autonomous';
      'D': begin
        Result := 'DGPS';
      end; //'DGPS';
      'E': begin
        Result := 'DR';
      end; //'Dead Reckoning';
      'R': begin
        Result := 'CP';
      end; //'Coarse Position';
      'P': begin
        Result := 'PPS';
      end; //'PPS';
    else begin
      Result := 'N';
    end; //#0 if no data or 'N' = Not Valid
    end;

    if (Dimentions > 1) then begin
      Result := Result + ' (' + IntToStr(Dimentions) + 'D)';
    end;

    if (not NoData_Float32(DGPS_Age_Second)) then begin
      if (DGPS_Age_Second > 0) then begin
        Result := Result + ': ' + RoundEx(DGPS_Age_Second, 2);
      end;//+' '+SAS_UNITS_Secund;
      if (DGPS_Station_ID > 0) then begin
        Result := Result + ' #' + IntToStr(DGPS_Station_ID);
      end;
    end;
  end;
end;

procedure TSensorFromGPSRecorderDGPS.Reset;
begin
  inherited;
  GPSRecorder.ExecuteGPSCommand(Self, cUnitIndex_ALL, gpsc_Reset_DGPS, nil);
end;

{ TSensorFromGPSRecorderGPSUnitInfo }

constructor TSensorFromGPSRecorderGPSUnitInfo.Create(
  const AGPSRecorder: IGPSRecorder
);
begin
  inherited Create(True, AGPSRecorder);
end;

function TSensorFromGPSRecorderGPSUnitInfo.GetCurrentValue: string;
begin
  Result := GPSRecorder.GPSUnitInfo;
end;

procedure TSensorFromGPSRecorderGPSUnitInfo.Reset;
begin
  inherited;
  GPSRecorder.ExecuteGPSCommand(Self, cUnitIndex_ALL, gpsc_Refresh_GPSUnitInfo, nil);
end;

end.
