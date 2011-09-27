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

    function ValueToText(AValue: Double): string; override;
    function GetValue: Double; override;
  public
    constructor Create(
      ALanguageManager: ILanguageManager;
      AGPSRecorder: IGPSRecorder;
      AValueConverterConfig: IValueToStringConverterConfig
    );
  end;

  TSensorFromGPSRecorderAvgSpeed = class(TSensorTextFromGPSRecorder)
  protected
    function GetCaptionTranslated: string; override;
    function GetDescriptionTranslated: string; override;
    function GetMenuItemNameTranslated: string; override;

    function ValueToText(AValue: Double): string; override;
    function GetValue: Double; override;
    procedure Reset; override;
  public
    constructor Create(
      ALanguageManager: ILanguageManager;
      AGPSRecorder: IGPSRecorder;
      AValueConverterConfig: IValueToStringConverterConfig
    );
  end;

  TSensorFromGPSRecorderMaxSpeed = class(TSensorTextFromGPSRecorder)
  protected
    function GetCaptionTranslated: string; override;
    function GetDescriptionTranslated: string; override;
    function GetMenuItemNameTranslated: string; override;

    function ValueToText(AValue: Double): string; override;
    function GetValue: Double; override;
    procedure Reset; override;
  public
    constructor Create(
      ALanguageManager: ILanguageManager;
      AGPSRecorder: IGPSRecorder;
      AValueConverterConfig: IValueToStringConverterConfig
    );
  end;

  TSensorFromGPSRecorderDist = class(TSensorTextFromGPSRecorder)
  protected
    function GetCaptionTranslated: string; override;
    function GetDescriptionTranslated: string; override;
    function GetMenuItemNameTranslated: string; override;

    function ValueToText(AValue: Double): string; override;
    function GetValue: Double; override;
    procedure Reset; override;
  public
    constructor Create(
      ALanguageManager: ILanguageManager;
      AGPSRecorder: IGPSRecorder;
      AValueConverterConfig: IValueToStringConverterConfig
    );
  end;

  TSensorFromGPSRecorderOdometer1 = class(TSensorTextFromGPSRecorder)
  protected
    function GetCaptionTranslated: string; override;
    function GetDescriptionTranslated: string; override;
    function GetMenuItemNameTranslated: string; override;

    function ValueToText(AValue: Double): string; override;
    function GetValue: Double; override;
    procedure Reset; override;
  public
    constructor Create(
      ALanguageManager: ILanguageManager;
      AGPSRecorder: IGPSRecorder;
      AValueConverterConfig: IValueToStringConverterConfig
    );
  end;

  TSensorFromGPSRecorderOdometer2 = class(TSensorTextFromGPSRecorder)
  protected
    function GetCaptionTranslated: string; override;
    function GetDescriptionTranslated: string; override;
    function GetMenuItemNameTranslated: string; override;

    function ValueToText(AValue: Double): string; override;
    function GetValue: Double; override;
    procedure Reset; override;
  public
    constructor Create(
      ALanguageManager: ILanguageManager;
      AGPSRecorder: IGPSRecorder;
      AValueConverterConfig: IValueToStringConverterConfig
    );
  end;

  TSensorFromGPSRecorderAltitude = class(TSensorTextFromGPSRecorder)
  protected
    function GetCaptionTranslated: string; override;
    function GetDescriptionTranslated: string; override;
    function GetMenuItemNameTranslated: string; override;

    function ValueToText(AValue: Double): string; override;
    function GetValue: Double; override;
  public
    constructor Create(
      ALanguageManager: ILanguageManager;
      AGPSRecorder: IGPSRecorder;
      AValueConverterConfig: IValueToStringConverterConfig
    );
  end;

  TSensorFromGPSRecorderHeading = class(TSensorTextFromGPSRecorder)
  protected
    function GetCaptionTranslated: string; override;
    function GetDescriptionTranslated: string; override;
    function GetMenuItemNameTranslated: string; override;

    function ValueToText(AValue: Double): string; override;
    function GetValue: Double; override;
  public
    constructor Create(
      ALanguageManager: ILanguageManager;
      AGPSRecorder: IGPSRecorder;
      AValueConverterConfig: IValueToStringConverterConfig
    );
  end;

implementation

uses
  c_SensorsGUIDSimple,
  u_ResStrings,
  u_GeoToStr;

{ TSensorFromGPSRecorderLastSpeed }

constructor TSensorFromGPSRecorderLastSpeed.Create(
  ALanguageManager: ILanguageManager;
  AGPSRecorder: IGPSRecorder;
  AValueConverterConfig: IValueToStringConverterConfig
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

function TSensorFromGPSRecorderLastSpeed.ValueToText(AValue: Double): string;
begin
  Result := RoundEx(AValue, 2);
end;

{ TSensorFromGPSRecorderAvgSpeed }

constructor TSensorFromGPSRecorderAvgSpeed.Create(
  ALanguageManager: ILanguageManager;
  AGPSRecorder: IGPSRecorder;
  AValueConverterConfig: IValueToStringConverterConfig
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

function TSensorFromGPSRecorderAvgSpeed.ValueToText(AValue: Double): string;
begin
  Result := RoundEx(AValue, 2);
end;

{ TSensorFromGPSRecorderMaxSpeed }

constructor TSensorFromGPSRecorderMaxSpeed.Create(
  ALanguageManager: ILanguageManager;
  AGPSRecorder: IGPSRecorder;
  AValueConverterConfig: IValueToStringConverterConfig
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

function TSensorFromGPSRecorderMaxSpeed.ValueToText(AValue: Double): string;
begin
  Result := RoundEx(AValue, 2);
end;

{ TSensorFromGPSRecorderDist }

constructor TSensorFromGPSRecorderDist.Create(
  ALanguageManager: ILanguageManager;
  AGPSRecorder: IGPSRecorder;
  AValueConverterConfig: IValueToStringConverterConfig
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

function TSensorFromGPSRecorderDist.ValueToText(AValue: Double): string;
begin
  Result := ValueConverter.DistConvert(AValue)
end;

{ TSensorFromGPSRecorderOdometer1 }

constructor TSensorFromGPSRecorderOdometer1.Create(
  ALanguageManager: ILanguageManager;
  AGPSRecorder: IGPSRecorder;
  AValueConverterConfig: IValueToStringConverterConfig
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

function TSensorFromGPSRecorderOdometer1.ValueToText(AValue: Double): string;
begin
  Result := ValueConverter.DistConvert(AValue)
end;

{ TSensorFromGPSRecorderOdometer2 }

constructor TSensorFromGPSRecorderOdometer2.Create(
  ALanguageManager: ILanguageManager;
  AGPSRecorder: IGPSRecorder;
  AValueConverterConfig: IValueToStringConverterConfig
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

function TSensorFromGPSRecorderOdometer2.ValueToText(AValue: Double): string;
begin
  Result := ValueConverter.DistConvert(AValue)
end;

{ TSensorFromGPSRecorderAltitude }

constructor TSensorFromGPSRecorderAltitude.Create(
  ALanguageManager: ILanguageManager;
  AGPSRecorder: IGPSRecorder;
  AValueConverterConfig: IValueToStringConverterConfig
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

function TSensorFromGPSRecorderAltitude.ValueToText(AValue: Double): string;
begin
  Result := RoundEx(AValue, 2);
end;

{ TSensorFromGPSRecorderHeading }

constructor TSensorFromGPSRecorderHeading.Create(
  ALanguageManager: ILanguageManager;
  AGPSRecorder: IGPSRecorder;
  AValueConverterConfig: IValueToStringConverterConfig
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

function TSensorFromGPSRecorderHeading.ValueToText(AValue: Double): string;
begin
  Result := RoundEx(AValue, 2) + '°';;
end;

end.
