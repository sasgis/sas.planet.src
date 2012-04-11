{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
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
  i_ValueToStringConverter,
  i_LanguageManager,
  u_SensorTextFromGPSRecorder;

type
  TSensorFromGPSRecorderLastSpeed = class(TSensorTextFromGPSRecorder)
  protected
    function GetCaptionTranslated: string; override;
    function GetDescriptionTranslated: string; override;
    function GetMenuItemNameTranslated: string; override;

    function ValueToText(const AValue: Double): string; override;
    function GetValue: Double; override;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AGPSRecorder: IGPSRecorder;
      const AValueConverterConfig: IValueToStringConverterConfig
    );
  end;

  TSensorFromGPSRecorderAvgSpeed = class(TSensorTextFromGPSRecorder)
  protected
    function GetCaptionTranslated: string; override;
    function GetDescriptionTranslated: string; override;
    function GetMenuItemNameTranslated: string; override;

    function ValueToText(const AValue: Double): string; override;
    function GetValue: Double; override;
    procedure Reset; override;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AGPSRecorder: IGPSRecorder;
      const AValueConverterConfig: IValueToStringConverterConfig
    );
  end;

  TSensorFromGPSRecorderMaxSpeed = class(TSensorTextFromGPSRecorder)
  protected
    function GetCaptionTranslated: string; override;
    function GetDescriptionTranslated: string; override;
    function GetMenuItemNameTranslated: string; override;

    function ValueToText(const AValue: Double): string; override;
    function GetValue: Double; override;
    procedure Reset; override;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AGPSRecorder: IGPSRecorder;
      const AValueConverterConfig: IValueToStringConverterConfig
    );
  end;

  TSensorFromGPSRecorderDist = class(TSensorTextFromGPSRecorder)
  protected
    function GetCaptionTranslated: string; override;
    function GetDescriptionTranslated: string; override;
    function GetMenuItemNameTranslated: string; override;

    function ValueToText(const AValue: Double): string; override;
    function GetValue: Double; override;
    procedure Reset; override;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AGPSRecorder: IGPSRecorder;
      const AValueConverterConfig: IValueToStringConverterConfig
    );
  end;

  TSensorFromGPSRecorderOdometer1 = class(TSensorTextFromGPSRecorder)
  protected
    function GetCaptionTranslated: string; override;
    function GetDescriptionTranslated: string; override;
    function GetMenuItemNameTranslated: string; override;

    function ValueToText(const AValue: Double): string; override;
    function GetValue: Double; override;
    procedure Reset; override;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AGPSRecorder: IGPSRecorder;
      const AValueConverterConfig: IValueToStringConverterConfig
    );
  end;

  TSensorFromGPSRecorderOdometer2 = class(TSensorTextFromGPSRecorder)
  protected
    function GetCaptionTranslated: string; override;
    function GetDescriptionTranslated: string; override;
    function GetMenuItemNameTranslated: string; override;

    function ValueToText(const AValue: Double): string; override;
    function GetValue: Double; override;
    procedure Reset; override;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AGPSRecorder: IGPSRecorder;
      const AValueConverterConfig: IValueToStringConverterConfig
    );
  end;

  TSensorFromGPSRecorderAltitude = class(TSensorTextFromGPSRecorder)
  protected
    function GetCaptionTranslated: string; override;
    function GetDescriptionTranslated: string; override;
    function GetMenuItemNameTranslated: string; override;

    function ValueToText(const AValue: Double): string; override;
    function GetValue: Double; override;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AGPSRecorder: IGPSRecorder;
      const AValueConverterConfig: IValueToStringConverterConfig
    );
  end;

  TSensorFromGPSRecorderHeading = class(TSensorTextFromGPSRecorder)
  protected
    function GetCaptionTranslated: string; override;
    function GetDescriptionTranslated: string; override;
    function GetMenuItemNameTranslated: string; override;

    function ValueToText(const AValue: Double): string; override;
    function GetValue: Double; override;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AGPSRecorder: IGPSRecorder;
      const AValueConverterConfig: IValueToStringConverterConfig
    );
  end;

  TSensorFromGPSRecorderHDOP = class(TSensorTextFromGPSRecorder)
  protected
    function GetCaptionTranslated: string; override;
    function GetDescriptionTranslated: string; override;
    function GetMenuItemNameTranslated: string; override;

    function ValueToText(const AValue: Double): string; override;
    function GetValue: Double; override;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AGPSRecorder: IGPSRecorder;
      const AValueConverterConfig: IValueToStringConverterConfig
    );
  end;

  TSensorFromGPSRecorderVDOP = class(TSensorTextFromGPSRecorder)
  protected
    function GetCaptionTranslated: string; override;
    function GetDescriptionTranslated: string; override;
    function GetMenuItemNameTranslated: string; override;

    function ValueToText(const AValue: Double): string; override;
    function GetValue: Double; override;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AGPSRecorder: IGPSRecorder;
      const AValueConverterConfig: IValueToStringConverterConfig
    );
  end;

  TSensorFromGPSRecorderUTCTime = class(TSensorTimeFromGPSRecorder)
  protected
    function GetCaptionTranslated: string; override;
    function GetDescriptionTranslated: string; override;
    function GetMenuItemNameTranslated: string; override;
    function GetValue: Double; override;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AGPSRecorder: IGPSRecorder;
      const AValueConverterConfig: IValueToStringConverterConfig
    );
  end;

  TSensorFromGPSRecorderLocalTime = class(TSensorTimeFromGPSRecorder)
  protected
    function GetCaptionTranslated: string; override;
    function GetDescriptionTranslated: string; override;
    function GetMenuItemNameTranslated: string; override;
    function GetValue: Double; override;
    procedure Reset; override;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AGPSRecorder: IGPSRecorder;
      const AValueConverterConfig: IValueToStringConverterConfig
    );
  end;

  TSensorFromGPSRecorderDGPS = class(TSensorTextFromGPSRecorder)
  protected
    function GetCaptionTranslated: string; override;
    function GetDescriptionTranslated: string; override;
    function GetMenuItemNameTranslated: string; override;

    function ValueToText(const AValue: Double): string; override;
    function GetValue: Double; override;
    function ValueChanged(const AOld, ANew: Double): Boolean; override;
    procedure Reset; override;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AGPSRecorder: IGPSRecorder;
      const AValueConverterConfig: IValueToStringConverterConfig
    );
  end;

  TSensorFromGPSRecorderGPSUnitInfo = class(TSensorSimpleTextFromGPSRecorder)
  protected
    function GetCaptionTranslated: string; override;
    function GetDescriptionTranslated: string; override;
    function GetMenuItemNameTranslated: string; override;
    function ValueToText(const AValue: Double): string; override;
    procedure Reset; override;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AGPSRecorder: IGPSRecorder;
      const AValueConverterConfig: IValueToStringConverterConfig
    );
  end;


implementation

uses
  SysUtils,
  c_SensorsGUIDSimple,
  i_GPS,
  u_ResStrings,
  u_GeoToStr,
  vsagps_public_base,
  vsagps_public_position,
  vsagps_public_time;

{ TSensorFromGPSRecorderLastSpeed }

constructor TSensorFromGPSRecorderLastSpeed.Create(
  const ALanguageManager: ILanguageManager;
  const AGPSRecorder: IGPSRecorder;
  const AValueConverterConfig: IValueToStringConverterConfig
);
begin
  inherited Create(
    CSensorLastSpeedGUID,
    False,
    ALanguageManager,
    AGPSRecorder,
    AValueConverterConfig
  );
end;

function TSensorFromGPSRecorderLastSpeed.GetCaptionTranslated: string;
begin
  Result := SAS_STR_SensorGPSRecorderLastSpeedCaption;
end;

function TSensorFromGPSRecorderLastSpeed.GetDescriptionTranslated: string;
begin
  Result := SAS_STR_SensorGPSRecorderLastSpeedDescription;
end;

function TSensorFromGPSRecorderLastSpeed.GetMenuItemNameTranslated: string;
begin
  Result := SAS_STR_SensorGPSRecorderLastSpeedMenuItemName;
end;

function TSensorFromGPSRecorderLastSpeed.GetValue: Double;
begin
  Result := GPSRecorder.LastSpeed;
end;

function TSensorFromGPSRecorderLastSpeed.ValueToText(const AValue: Double): string;
begin
  Result := RoundEx(AValue, 2);
end;

{ TSensorFromGPSRecorderAvgSpeed }

constructor TSensorFromGPSRecorderAvgSpeed.Create(
  const ALanguageManager: ILanguageManager;
  const AGPSRecorder: IGPSRecorder;
  const AValueConverterConfig: IValueToStringConverterConfig
);
begin
  inherited Create(
    CSensorAvgSpeedGUID,
    True,
    ALanguageManager,
    AGPSRecorder,
    AValueConverterConfig
  );
end;

function TSensorFromGPSRecorderAvgSpeed.GetCaptionTranslated: string;
begin
  Result := SAS_STR_SensorGPSRecorderAvgSpeedCaption;
end;

function TSensorFromGPSRecorderAvgSpeed.GetDescriptionTranslated: string;
begin
  Result := SAS_STR_SensorGPSRecorderAvgSpeedDescription;
end;

function TSensorFromGPSRecorderAvgSpeed.GetMenuItemNameTranslated: string;
begin
  Result := SAS_STR_SensorGPSRecorderAvgSpeedMenuItemName;
end;

function TSensorFromGPSRecorderAvgSpeed.GetValue: Double;
begin
  Result := GPSRecorder.AvgSpeed;
end;

procedure TSensorFromGPSRecorderAvgSpeed.Reset;
begin
  inherited;
  GPSRecorder.ResetAvgSpeed;
end;

function TSensorFromGPSRecorderAvgSpeed.ValueToText(const AValue: Double): string;
begin
  Result := RoundEx(AValue, 2);
end;

{ TSensorFromGPSRecorderMaxSpeed }

constructor TSensorFromGPSRecorderMaxSpeed.Create(
  const ALanguageManager: ILanguageManager;
  const AGPSRecorder: IGPSRecorder;
  const AValueConverterConfig: IValueToStringConverterConfig
);
begin
  inherited Create(
    CSensorMaxSpeedGUID,
    True,
    ALanguageManager,
    AGPSRecorder,
    AValueConverterConfig
  );
end;

function TSensorFromGPSRecorderMaxSpeed.GetCaptionTranslated: string;
begin
  Result := SAS_STR_SensorGPSRecorderMaxSpeedCaption;
end;

function TSensorFromGPSRecorderMaxSpeed.GetDescriptionTranslated: string;
begin
  Result := SAS_STR_SensorGPSRecorderMaxSpeedDescription;
end;

function TSensorFromGPSRecorderMaxSpeed.GetMenuItemNameTranslated: string;
begin
  Result := SAS_STR_SensorGPSRecorderMaxSpeedMenuItemName;
end;

function TSensorFromGPSRecorderMaxSpeed.GetValue: Double;
begin
  Result := GPSRecorder.MaxSpeed;
end;

procedure TSensorFromGPSRecorderMaxSpeed.Reset;
begin
  inherited;
  GPSRecorder.ResetMaxSpeed;
end;

function TSensorFromGPSRecorderMaxSpeed.ValueToText(const AValue: Double): string;
begin
  Result := RoundEx(AValue, 2);
end;

{ TSensorFromGPSRecorderDist }

constructor TSensorFromGPSRecorderDist.Create(
  const ALanguageManager: ILanguageManager;
  const AGPSRecorder: IGPSRecorder;
  const AValueConverterConfig: IValueToStringConverterConfig
);
begin
  inherited Create(
    CSensorDistGUID,
    True,
    ALanguageManager,
    AGPSRecorder,
    AValueConverterConfig
  );
end;

function TSensorFromGPSRecorderDist.GetCaptionTranslated: string;
begin
  Result := SAS_STR_SensorGPSRecorderDistCaption;
end;

function TSensorFromGPSRecorderDist.GetDescriptionTranslated: string;
begin
  Result := SAS_STR_SensorGPSRecorderDistDescription;
end;

function TSensorFromGPSRecorderDist.GetMenuItemNameTranslated: string;
begin
  Result := SAS_STR_SensorGPSRecorderDistMenuItemName;
end;

function TSensorFromGPSRecorderDist.GetValue: Double;
begin
  Result := GPSRecorder.Dist;
end;

procedure TSensorFromGPSRecorderDist.Reset;
begin
  inherited;
  GPSRecorder.ResetDist;
end;

function TSensorFromGPSRecorderDist.ValueToText(const AValue: Double): string;
begin
  Result := ValueConverter.DistConvert(AValue)
end;

{ TSensorFromGPSRecorderOdometer1 }

constructor TSensorFromGPSRecorderOdometer1.Create(
  const ALanguageManager: ILanguageManager;
  const AGPSRecorder: IGPSRecorder;
  const AValueConverterConfig: IValueToStringConverterConfig
);
begin
  inherited Create(
    CSensorOdometer1GUID,
    True,
    ALanguageManager,
    AGPSRecorder,
    AValueConverterConfig
  );
end;

function TSensorFromGPSRecorderOdometer1.GetCaptionTranslated: string;
begin
  Result := SAS_STR_SensorGPSRecorderOdometer1Caption;
end;

function TSensorFromGPSRecorderOdometer1.GetDescriptionTranslated: string;
begin
  Result := SAS_STR_SensorGPSRecorderOdometer1Description;
end;

function TSensorFromGPSRecorderOdometer1.GetMenuItemNameTranslated: string;
begin
  Result := SAS_STR_SensorGPSRecorderOdometer1MenuItemName;
end;

function TSensorFromGPSRecorderOdometer1.GetValue: Double;
begin
  Result := GPSRecorder.Odometer1;
end;

procedure TSensorFromGPSRecorderOdometer1.Reset;
begin
  inherited;
  GPSRecorder.ResetOdometer1;
end;

function TSensorFromGPSRecorderOdometer1.ValueToText(const AValue: Double): string;
begin
  Result := ValueConverter.DistConvert(AValue)
end;

{ TSensorFromGPSRecorderOdometer2 }

constructor TSensorFromGPSRecorderOdometer2.Create(
  const ALanguageManager: ILanguageManager;
  const AGPSRecorder: IGPSRecorder;
  const AValueConverterConfig: IValueToStringConverterConfig
);
begin
  inherited Create(
    CSensorOdometer2GUID,
    True,
    ALanguageManager,
    AGPSRecorder,
    AValueConverterConfig
  );
end;

function TSensorFromGPSRecorderOdometer2.GetCaptionTranslated: string;
begin
  Result := SAS_STR_SensorGPSRecorderOdometer2Caption;
end;

function TSensorFromGPSRecorderOdometer2.GetDescriptionTranslated: string;
begin
  Result := SAS_STR_SensorGPSRecorderOdometer2Description;
end;

function TSensorFromGPSRecorderOdometer2.GetMenuItemNameTranslated: string;
begin
  Result := SAS_STR_SensorGPSRecorderOdometer2MenuItemName;
end;

function TSensorFromGPSRecorderOdometer2.GetValue: Double;
begin
  Result := GPSRecorder.Odometer2;
end;

procedure TSensorFromGPSRecorderOdometer2.Reset;
begin
  inherited;
  GPSRecorder.ResetOdometer2;
end;

function TSensorFromGPSRecorderOdometer2.ValueToText(const AValue: Double): string;
begin
  Result := ValueConverter.DistConvert(AValue)
end;

{ TSensorFromGPSRecorderAltitude }

constructor TSensorFromGPSRecorderAltitude.Create(
  const ALanguageManager: ILanguageManager;
  const AGPSRecorder: IGPSRecorder;
  const AValueConverterConfig: IValueToStringConverterConfig
);
begin
  inherited Create(
    CSensorLastAltitudeGUID,
    False,
    ALanguageManager,
    AGPSRecorder,
    AValueConverterConfig
  );
end;

function TSensorFromGPSRecorderAltitude.GetCaptionTranslated: string;
begin
  Result := SAS_STR_SensorGPSRecorderAltitudeCaption;
end;

function TSensorFromGPSRecorderAltitude.GetDescriptionTranslated: string;
begin
  Result := SAS_STR_SensorGPSRecorderAltitudeDescription;
end;

function TSensorFromGPSRecorderAltitude.GetMenuItemNameTranslated: string;
begin
  Result := SAS_STR_SensorGPSRecorderAltitudeMenuItemName;
end;

function TSensorFromGPSRecorderAltitude.GetValue: Double;
begin
  Result := GPSRecorder.LastAltitude;
end;

function TSensorFromGPSRecorderAltitude.ValueToText(const AValue: Double): string;
begin
  Result := RoundEx(AValue, 2);
end;

{ TSensorFromGPSRecorderHeading }

constructor TSensorFromGPSRecorderHeading.Create(
  const ALanguageManager: ILanguageManager;
  const AGPSRecorder: IGPSRecorder;
  const AValueConverterConfig: IValueToStringConverterConfig
);
begin
  inherited Create(
    CSensorHeadingGUID,
    False,
    ALanguageManager,
    AGPSRecorder,
    AValueConverterConfig
  );
end;

function TSensorFromGPSRecorderHeading.GetCaptionTranslated: string;
begin
  Result := SAS_STR_SensorGPSRecorderHeadingCaption;
end;

function TSensorFromGPSRecorderHeading.GetDescriptionTranslated: string;
begin
  Result := SAS_STR_SensorGPSRecorderHeadingDescription;
end;

function TSensorFromGPSRecorderHeading.GetMenuItemNameTranslated: string;
begin
  Result := SAS_STR_SensorGPSRecorderHeadingMenuItemName;
end;

function TSensorFromGPSRecorderHeading.GetValue: Double;
begin
  Result := GPSRecorder.LastHeading;
end;

function TSensorFromGPSRecorderHeading.ValueToText(const AValue: Double): string;
begin
  Result := RoundEx(AValue, 2) + '°';;
end;

{ TSensorFromGPSRecorderHDOP }

constructor TSensorFromGPSRecorderHDOP.Create(
  const ALanguageManager: ILanguageManager;
  const AGPSRecorder: IGPSRecorder;
  const AValueConverterConfig: IValueToStringConverterConfig
);
begin
  inherited Create(
    CSensorHDOPGUID,
    FALSE,
    ALanguageManager,
    AGPSRecorder,
    AValueConverterConfig
  );
end;

function TSensorFromGPSRecorderHDOP.GetCaptionTranslated: string;
begin
  Result := SAS_STR_SensorGPSRecorderHDOPCaption;
end;

function TSensorFromGPSRecorderHDOP.GetDescriptionTranslated: string;
begin
  Result := SAS_STR_SensorGPSRecorderHDOPDescription;
end;

function TSensorFromGPSRecorderHDOP.GetMenuItemNameTranslated: string;
begin
  Result := SAS_STR_SensorGPSRecorderHDOPMenuItemName;
end;

function TSensorFromGPSRecorderHDOP.GetValue: Double;
var VPosition: IGPSPosition;
begin
  VPosition := GPSRecorder.CurrentPosition;
  Result := VPosition.GetPosParams^.HDOP;
end;

function TSensorFromGPSRecorderHDOP.ValueToText(const AValue: Double): string;
begin
  Result := RoundEx(AValue, 1);
end;

{ TSensorFromGPSRecorderVDOP }

constructor TSensorFromGPSRecorderVDOP.Create(
  const ALanguageManager: ILanguageManager;
  const AGPSRecorder: IGPSRecorder;
  const AValueConverterConfig: IValueToStringConverterConfig
);
begin
  inherited Create(
    CSensorVDOPGUID,
    FALSE,
    ALanguageManager,
    AGPSRecorder,
    AValueConverterConfig
  );
end;

function TSensorFromGPSRecorderVDOP.GetCaptionTranslated: string;
begin
  Result := SAS_STR_SensorGPSRecorderVDOPCaption;
end;

function TSensorFromGPSRecorderVDOP.GetDescriptionTranslated: string;
begin
  Result := SAS_STR_SensorGPSRecorderVDOPDescription;
end;

function TSensorFromGPSRecorderVDOP.GetMenuItemNameTranslated: string;
begin
  Result := SAS_STR_SensorGPSRecorderVDOPMenuItemName;
end;

function TSensorFromGPSRecorderVDOP.GetValue: Double;
var VPosition: IGPSPosition;
begin
  VPosition := GPSRecorder.CurrentPosition;
  Result := VPosition.GetPosParams^.VDOP;
end;

function TSensorFromGPSRecorderVDOP.ValueToText(const AValue: Double): string;
begin
  Result := RoundEx(AValue, 1);
end;

{ TSensorFromGPSRecorderUTCTime }

constructor TSensorFromGPSRecorderUTCTime.Create(
  const ALanguageManager: ILanguageManager;
  const AGPSRecorder: IGPSRecorder;
  const AValueConverterConfig: IValueToStringConverterConfig
);
begin
  inherited Create(
    CSensorUTCTimeGUID,
    FALSE,
    ALanguageManager,
    AGPSRecorder,
    AValueConverterConfig
  );
end;

function TSensorFromGPSRecorderUTCTime.GetCaptionTranslated: string;
begin
  Result := SAS_STR_SensorGPSRecorderUTCTimeCaption;
end;

function TSensorFromGPSRecorderUTCTime.GetDescriptionTranslated: string;
begin
  Result := SAS_STR_SensorGPSRecorderUTCTimeDescription;
end;

function TSensorFromGPSRecorderUTCTime.GetMenuItemNameTranslated: string;
begin
  Result := SAS_STR_SensorGPSRecorderUTCTimeMenuItemName;
end;

function TSensorFromGPSRecorderUTCTime.GetValue: Double;
var VPosition: IGPSPosition;
begin
  VPosition := GPSRecorder.CurrentPosition;
  with VPosition.GetPosParams^ do
    Result := (UTCDate+UTCTime);
end;

{ TSensorFromGPSRecorderLocalTime }

constructor TSensorFromGPSRecorderLocalTime.Create(
  const ALanguageManager: ILanguageManager;
  const AGPSRecorder: IGPSRecorder;
  const AValueConverterConfig: IValueToStringConverterConfig
);
begin
  inherited Create(
    CSensorLocalTimeGUID,
    TRUE,
    ALanguageManager,
    AGPSRecorder,
    AValueConverterConfig
  );
end;

function TSensorFromGPSRecorderLocalTime.GetCaptionTranslated: string;
begin
  Result := SAS_STR_SensorGPSRecorderLocalTimeCaption;
end;

function TSensorFromGPSRecorderLocalTime.GetDescriptionTranslated: string;
begin
  Result := SAS_STR_SensorGPSRecorderLocalTimeDescription;
end;

function TSensorFromGPSRecorderLocalTime.GetMenuItemNameTranslated: string;
begin
  Result := SAS_STR_SensorGPSRecorderLocalTimeMenuItemName;
end;

function TSensorFromGPSRecorderLocalTime.GetValue: Double;
var VPosition: IGPSPosition;
begin
  VPosition := GPSRecorder.CurrentPosition;
  with VPosition.GetPosParams^ do
  Result := (UTCDate+UTCTime);
  if (0<>Result) then
    Result:=SystemTimeToLocalTime(Result);
end;

procedure TSensorFromGPSRecorderLocalTime.Reset;
begin
  inherited;
  GPSRecorder.ExecuteGPSCommand(Self, cUnitIndex_ALL, gpsc_Apply_UTCDateTime, nil);
end;

{ TSensorFromGPSRecorderDGPS }

constructor TSensorFromGPSRecorderDGPS.Create(
  const ALanguageManager: ILanguageManager;
  const AGPSRecorder: IGPSRecorder;
  const AValueConverterConfig: IValueToStringConverterConfig
);
begin
  inherited Create(
    CSensorDGPSGUID,
    TRUE,
    ALanguageManager,
    AGPSRecorder,
    AValueConverterConfig
  );
end;

function TSensorFromGPSRecorderDGPS.GetCaptionTranslated: string;
begin
  Result := SAS_STR_SensorGPSRecorderDGPSCaption;
end;

function TSensorFromGPSRecorderDGPS.GetDescriptionTranslated: string;
begin
  Result := SAS_STR_SensorGPSRecorderDGPSDescription;
end;

function TSensorFromGPSRecorderDGPS.GetMenuItemNameTranslated: string;
begin
  Result := SAS_STR_SensorGPSRecorderDGPSMenuItemName;
end;

function TSensorFromGPSRecorderDGPS.GetValue: Double;
var VPosition: IGPSPosition;
begin
  VPosition := GPSRecorder.CurrentPosition;
  Result := Double(VPosition.GetPosParams^.DGPS);
end;

procedure TSensorFromGPSRecorderDGPS.Reset;
begin
  inherited;
  GPSRecorder.ExecuteGPSCommand(Self, cUnitIndex_ALL, gpsc_Reset_DGPS, nil);
end;

function TSensorFromGPSRecorderDGPS.ValueChanged(const AOld, ANew: Double): Boolean;
begin
  Result := (not CompareMem(@AOld, @ANew, sizeof(Double)));
end;                                                              

function TSensorFromGPSRecorderDGPS.ValueToText(const AValue: Double): string;
begin
  with TSingleDGPSData(AValue) do begin
    case Nmea23_Mode of
    'A': Result:='A'; //'Autonomous';
    'D': Result:=SAS_STR_Yes ; //'DGPS';
    'E': Result:='DR'; //'Dead Reckoning';
    'R': Result:='CP'; //'Coarse Position';
    'P': Result:='PPS'; //'PPS';
    else Result:=SAS_STR_No; //#0 if no data or 'N' = Not Valid
    end;

    if (Dimentions>1) then
      Result := Result + ' ('+IntToStr(Dimentions)+'D)';

    if (not NoData_Float32(DGPS_Age_Second)) then begin
      if (DGPS_Age_Second>0) then
        Result := Result + ': ' + RoundEx(DGPS_Age_Second, 2);//+' '+SAS_UNITS_Secund;
      if (DGPS_Station_ID>0) then
        Result := Result + ' #' + IntToStr(DGPS_Station_ID);
    end;
  end;
end;

{ TSensorFromGPSRecorderGPSUnitInfo }

constructor TSensorFromGPSRecorderGPSUnitInfo.Create(
  const ALanguageManager: ILanguageManager;
  const AGPSRecorder: IGPSRecorder;
  const AValueConverterConfig: IValueToStringConverterConfig
);
begin
  inherited Create(
    CSensorGPSUnitInfoGUID,
    TRUE,
    ALanguageManager,
    AGPSRecorder,
    AValueConverterConfig
  );
end;

function TSensorFromGPSRecorderGPSUnitInfo.GetCaptionTranslated: string;
begin
  Result := SAS_STR_SensorGPSRecorderGPSUnitInfoCaption;
end;

function TSensorFromGPSRecorderGPSUnitInfo.GetDescriptionTranslated: string;
begin
  Result := SAS_STR_SensorGPSRecorderGPSUnitInfoDescription;
end;

function TSensorFromGPSRecorderGPSUnitInfo.GetMenuItemNameTranslated: string;
begin
  Result := SAS_STR_SensorGPSRecorderGPSUnitInfoMenuItemName;
end;

procedure TSensorFromGPSRecorderGPSUnitInfo.Reset;
begin
  inherited;
  GPSRecorder.ExecuteGPSCommand(Self, cUnitIndex_ALL, gpsc_Refresh_GPSUnitInfo, nil);
end;

function TSensorFromGPSRecorderGPSUnitInfo.ValueToText(const AValue: Double): string;
begin
  Result := GPSRecorder.GPSUnitInfo;
end;

end.
