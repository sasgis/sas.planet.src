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

unit u_SensorListStuped;

interface

uses
  i_NavigationToPoint,
  i_LocalCoordConverterChangeable,
  i_GPSRecorder,
  i_GpsSystem,
  i_BatteryStatus,
  i_SystemTimeProvider,
  i_LanguageManager,
  u_SensorListBase;

type
  TSensorListStuped = class(TSensorListBase)
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AViewPortState: ILocalCoordConverterChangeable;
      const ANavigationToPoint: INavigationToPoint;
      const ASystemTime: ISystemTimeProvider;
      const AGPSRecorder: IGPSRecorder;
      const AGPSModule: IGpsSystem;
      const ABatteryStatus: IBatteryStatus
    );
  end;

implementation

uses
  c_SensorsGUIDSimple,
  i_Sensor,
  i_StringConfigDataElement,
  i_SensorList,
  u_SensorListEntity,
  u_StringConfigDataElementWithDefByStringRec,
  u_ResStrings,
  u_SensorTextFromNavToPoint,
  u_SensorBatteryStatus,
  u_SensorsFromGPSRecorder;


{ TSensorListStuped }

constructor TSensorListStuped.Create(
  const ALanguageManager: ILanguageManager;
  const AViewPortState: ILocalCoordConverterChangeable;
  const ANavigationToPoint: INavigationToPoint;
  const ASystemTime: ISystemTimeProvider;
  const AGPSRecorder: IGPSRecorder;
  const AGPSModule: IGpsSystem;
  const ABatteryStatus: IBatteryStatus
);
var
  VCaption: IStringConfigDataElement;
  VDescription: IStringConfigDataElement;
  VMenuItemName: IStringConfigDataElement;
  VSensor: ISensor;
  VEntity: ISensorListEntity;
begin
  inherited Create;

  VSensor := TSensorFromGPSRecorderLastSpeed.Create(AGPSRecorder);
  VCaption :=
    TStringConfigDataElementWithDefByStringRec.Create(
      ALanguageManager, @SAS_STR_SensorGPSRecorderLastSpeedCaption
    );
  VDescription :=
    TStringConfigDataElementWithDefByStringRec.Create(
      ALanguageManager, @SAS_STR_SensorGPSRecorderLastSpeedDescription
    );
  VMenuItemName :=
    TStringConfigDataElementWithDefByStringRec.Create(
      ALanguageManager, @SAS_STR_SensorGPSRecorderLastSpeedMenuItemName
    );
  VEntity :=
    TSensorListEntity.Create(
      CSensorLastSpeedGUID,
      VCaption,
      VDescription,
      VMenuItemName,
      VSensor,
      VSensor.SensorTypeIID
    );
  Self.Add(VEntity);

  VSensor := TSensorFromGPSRecorderAvgSpeed.Create(AGPSRecorder);
  VCaption :=
    TStringConfigDataElementWithDefByStringRec.Create(
      ALanguageManager, @SAS_STR_SensorGPSRecorderAvgSpeedCaption
    );
  VDescription :=
    TStringConfigDataElementWithDefByStringRec.Create(
      ALanguageManager, @SAS_STR_SensorGPSRecorderAvgSpeedDescription
    );
  VMenuItemName :=
    TStringConfigDataElementWithDefByStringRec.Create(
      ALanguageManager, @SAS_STR_SensorGPSRecorderAvgSpeedMenuItemName
    );
  VEntity :=
    TSensorListEntity.Create(
      CSensorAvgSpeedGUID,
      VCaption,
      VDescription,
      VMenuItemName,
      VSensor,
      VSensor.SensorTypeIID
    );
  Self.Add(VEntity);

  VSensor := TSensorFromGPSRecorderMaxSpeed.Create(AGPSRecorder);
  VCaption :=
    TStringConfigDataElementWithDefByStringRec.Create(
      ALanguageManager, @SAS_STR_SensorGPSRecorderMaxSpeedCaption
    );
  VDescription :=
    TStringConfigDataElementWithDefByStringRec.Create(
      ALanguageManager, @SAS_STR_SensorGPSRecorderMaxSpeedDescription
    );
  VMenuItemName :=
    TStringConfigDataElementWithDefByStringRec.Create(
      ALanguageManager, @SAS_STR_SensorGPSRecorderMaxSpeedMenuItemName
    );
  VEntity :=
    TSensorListEntity.Create(
      CSensorMaxSpeedGUID,
      VCaption,
      VDescription,
      VMenuItemName,
      VSensor,
      VSensor.SensorTypeIID
    );
  Self.Add(VEntity);

  VSensor := TSensorFromGPSRecorderDist.Create(AGPSRecorder);
  VCaption :=
    TStringConfigDataElementWithDefByStringRec.Create(
      ALanguageManager, @SAS_STR_SensorGPSRecorderDistCaption
    );
  VDescription :=
    TStringConfigDataElementWithDefByStringRec.Create(
      ALanguageManager, @SAS_STR_SensorGPSRecorderDistDescription
    );
  VMenuItemName :=
    TStringConfigDataElementWithDefByStringRec.Create(
      ALanguageManager, @SAS_STR_SensorGPSRecorderDistMenuItemName
    );
  VEntity :=
    TSensorListEntity.Create(
      CSensorDistGUID,
      VCaption,
      VDescription,
      VMenuItemName,
      VSensor,
      VSensor.SensorTypeIID
    );
  Self.Add(VEntity);

  VSensor := TSensorFromGPSRecorderOdometer1.Create(AGPSRecorder);
  VCaption :=
    TStringConfigDataElementWithDefByStringRec.Create(
      ALanguageManager, @SAS_STR_SensorGPSRecorderOdometer1Caption
    );
  VDescription :=
    TStringConfigDataElementWithDefByStringRec.Create(
      ALanguageManager, @SAS_STR_SensorGPSRecorderOdometer1Description
    );
  VMenuItemName :=
    TStringConfigDataElementWithDefByStringRec.Create(
      ALanguageManager, @SAS_STR_SensorGPSRecorderOdometer1MenuItemName
    );
  VEntity :=
    TSensorListEntity.Create(
      CSensorOdometer1GUID,
      VCaption,
      VDescription,
      VMenuItemName,
      VSensor,
      VSensor.SensorTypeIID
    );
  Self.Add(VEntity);

  VSensor := TSensorFromGPSRecorderOdometer2.Create(AGPSRecorder);
  VCaption :=
    TStringConfigDataElementWithDefByStringRec.Create(
      ALanguageManager, @SAS_STR_SensorGPSRecorderOdometer2Caption
    );
  VDescription :=
    TStringConfigDataElementWithDefByStringRec.Create(
      ALanguageManager, @SAS_STR_SensorGPSRecorderOdometer2Description
    );
  VMenuItemName :=
    TStringConfigDataElementWithDefByStringRec.Create(
      ALanguageManager, @SAS_STR_SensorGPSRecorderOdometer2MenuItemName
    );
  VEntity :=
    TSensorListEntity.Create(
      CSensorOdometer2GUID,
      VCaption,
      VDescription,
      VMenuItemName,
      VSensor,
      VSensor.SensorTypeIID
    );
  Self.Add(VEntity);

  VSensor := TSensorTextFromNavToPoint.Create(AViewPortState, ANavigationToPoint);
  VCaption :=
    TStringConfigDataElementWithDefByStringRec.Create(
      ALanguageManager, @SAS_STR_SensorNavToPointCaption
    );
  VDescription :=
    TStringConfigDataElementWithDefByStringRec.Create(
      ALanguageManager, @SAS_STR_SensorNavToPointDescription
    );
  VMenuItemName :=
    TStringConfigDataElementWithDefByStringRec.Create(
      ALanguageManager, @SAS_STR_SensorNavToPointMenuItemName
    );
  VEntity :=
    TSensorListEntity.Create(
      CSensorDistToMarkGUID,
      VCaption,
      VDescription,
      VMenuItemName,
      VSensor,
      VSensor.SensorTypeIID
    );
  Self.Add(VEntity);

  VSensor := TSensorBatteryStatus.Create(ABatteryStatus);
  VCaption :=
    TStringConfigDataElementWithDefByStringRec.Create(
      ALanguageManager, @SAS_STR_SensorBatteryStatusCaption
    );
  VDescription :=
    TStringConfigDataElementWithDefByStringRec.Create(
      ALanguageManager, @SAS_STR_SensorBatteryStatusDescription
    );
  VMenuItemName :=
    TStringConfigDataElementWithDefByStringRec.Create(
      ALanguageManager, @SAS_STR_SensorBatteryStatusMenuItemName
    );
  VEntity :=
    TSensorListEntity.Create(
      CSensorBatteryGUID,
      VCaption,
      VDescription,
      VMenuItemName,
      VSensor,
      VSensor.SensorTypeIID
    );
  Self.Add(VEntity);

  VSensor := TSensorFromGPSRecorderAltitude.Create(AGPSRecorder);
  VCaption :=
    TStringConfigDataElementWithDefByStringRec.Create(
      ALanguageManager, @SAS_STR_SensorGPSRecorderAltitudeCaption
    );
  VDescription :=
    TStringConfigDataElementWithDefByStringRec.Create(
      ALanguageManager, @SAS_STR_SensorGPSRecorderAltitudeDescription
    );
  VMenuItemName :=
    TStringConfigDataElementWithDefByStringRec.Create(
      ALanguageManager, @SAS_STR_SensorGPSRecorderAltitudeMenuItemName
    );
  VEntity :=
    TSensorListEntity.Create(
      CSensorLastAltitudeGUID,
      VCaption,
      VDescription,
      VMenuItemName,
      VSensor,
      VSensor.SensorTypeIID
    );
  Self.Add(VEntity);

  VSensor := TSensorFromGPSRecorderHeading.Create(AGPSRecorder);
  VCaption :=
    TStringConfigDataElementWithDefByStringRec.Create(
      ALanguageManager, @SAS_STR_SensorGPSRecorderHeadingCaption
    );
  VDescription :=
    TStringConfigDataElementWithDefByStringRec.Create(
      ALanguageManager, @SAS_STR_SensorGPSRecorderHeadingDescription
    );
  VMenuItemName :=
    TStringConfigDataElementWithDefByStringRec.Create(
      ALanguageManager, @SAS_STR_SensorGPSRecorderHeadingMenuItemName
    );
  VEntity :=
    TSensorListEntity.Create(
      CSensorHeadingGUID,
      VCaption,
      VDescription,
      VMenuItemName,
      VSensor,
      VSensor.SensorTypeIID
    );
  Self.Add(VEntity);

  VSensor := TSensorFromGPSRecorderHDOP.Create(AGPSRecorder);
  VCaption :=
    TStringConfigDataElementWithDefByStringRec.Create(
      ALanguageManager, @SAS_STR_SensorGPSRecorderHDOPCaption
    );
  VDescription :=
    TStringConfigDataElementWithDefByStringRec.Create(
      ALanguageManager, @SAS_STR_SensorGPSRecorderHDOPDescription
    );
  VMenuItemName :=
    TStringConfigDataElementWithDefByStringRec.Create(
      ALanguageManager, @SAS_STR_SensorGPSRecorderHDOPMenuItemName
    );
  VEntity :=
    TSensorListEntity.Create(
      CSensorHDOPGUID,
      VCaption,
      VDescription,
      VMenuItemName,
      VSensor,
      VSensor.SensorTypeIID
    );
  Self.Add(VEntity);

  VSensor := TSensorFromGPSRecorderVDOP.Create(AGPSRecorder);
  VCaption :=
    TStringConfigDataElementWithDefByStringRec.Create(
      ALanguageManager, @SAS_STR_SensorGPSRecorderVDOPCaption
    );
  VDescription :=
    TStringConfigDataElementWithDefByStringRec.Create(
      ALanguageManager, @SAS_STR_SensorGPSRecorderVDOPDescription
    );
  VMenuItemName :=
    TStringConfigDataElementWithDefByStringRec.Create(
      ALanguageManager, @SAS_STR_SensorGPSRecorderVDOPMenuItemName
    );
  VEntity :=
    TSensorListEntity.Create(
      CSensorVDOPGUID,
      VCaption,
      VDescription,
      VMenuItemName,
      VSensor,
      VSensor.SensorTypeIID
    );
  Self.Add(VEntity);

  VSensor := TSensorFromGPSRecorderUTCTime.Create(AGPSRecorder);
  VCaption :=
    TStringConfigDataElementWithDefByStringRec.Create(
      ALanguageManager, @SAS_STR_SensorGPSRecorderUTCTimeCaption
    );
  VDescription :=
    TStringConfigDataElementWithDefByStringRec.Create(
      ALanguageManager, @SAS_STR_SensorGPSRecorderUTCTimeDescription
    );
  VMenuItemName :=
    TStringConfigDataElementWithDefByStringRec.Create(
      ALanguageManager, @SAS_STR_SensorGPSRecorderUTCTimeMenuItemName
    );
  VEntity :=
    TSensorListEntity.Create(
      CSensorUTCTimeGUID,
      VCaption,
      VDescription,
      VMenuItemName,
      VSensor,
      VSensor.SensorTypeIID
    );
  Self.Add(VEntity);

  VSensor := TSensorFromGPSRecorderSunRiseTime.Create(ASystemTime, AGPSRecorder);
  VCaption :=
    TStringConfigDataElementWithDefByStringRec.Create(
      ALanguageManager, @SAS_STR_SensorGPSRecorderSunRiseTimeCaption
    );
  VDescription :=
    TStringConfigDataElementWithDefByStringRec.Create(
      ALanguageManager, @SAS_STR_SensorGPSRecorderSunRiseTimeDescription
    );
  VMenuItemName :=
    TStringConfigDataElementWithDefByStringRec.Create(
      ALanguageManager, @SAS_STR_SensorGPSRecorderSunRiseTimeMenuItemName
    );
  VEntity :=
    TSensorListEntity.Create(
      CSensorSunRiseTimeGUID,
      VCaption,
      VDescription,
      VMenuItemName,
      VSensor,
      VSensor.SensorTypeIID
    );
  Self.Add(VEntity);

  VSensor := TSensorFromGPSRecorderSunSetTime.Create(ASystemTime, AGPSRecorder);
  VCaption :=
    TStringConfigDataElementWithDefByStringRec.Create(
      ALanguageManager, @SAS_STR_SensorGPSRecorderSunSetTimeCaption
    );
  VDescription :=
    TStringConfigDataElementWithDefByStringRec.Create(
      ALanguageManager, @SAS_STR_SensorGPSRecorderSunSetTimeDescription
    );
  VMenuItemName :=
    TStringConfigDataElementWithDefByStringRec.Create(
      ALanguageManager, @SAS_STR_SensorGPSRecorderSunSetTimeMenuItemName
    );
  VEntity :=
    TSensorListEntity.Create(
      CSensorSunSetTimeGUID,
      VCaption,
      VDescription,
      VMenuItemName,
      VSensor,
      VSensor.SensorTypeIID
    );
  Self.Add(VEntity);

  VSensor := TSensorFromGPSRecorderLocalTime.Create(ASystemTime, AGPSRecorder, AGPSModule);
  VCaption :=
    TStringConfigDataElementWithDefByStringRec.Create(
      ALanguageManager, @SAS_STR_SensorGPSRecorderLocalTimeCaption
    );
  VDescription :=
    TStringConfigDataElementWithDefByStringRec.Create(
      ALanguageManager, @SAS_STR_SensorGPSRecorderLocalTimeDescription
    );
  VMenuItemName :=
    TStringConfigDataElementWithDefByStringRec.Create(
      ALanguageManager, @SAS_STR_SensorGPSRecorderLocalTimeMenuItemName
    );
  VEntity :=
    TSensorListEntity.Create(
      CSensorLocalTimeGUID,
      VCaption,
      VDescription,
      VMenuItemName,
      VSensor,
      VSensor.SensorTypeIID
    );
  Self.Add(VEntity);

  VSensor := TSensorFromGPSRecorderDGPS.Create(AGPSRecorder, AGPSModule);
  VCaption :=
    TStringConfigDataElementWithDefByStringRec.Create(
      ALanguageManager, @SAS_STR_SensorGPSRecorderDGPSCaption
    );
  VDescription :=
    TStringConfigDataElementWithDefByStringRec.Create(
      ALanguageManager, @SAS_STR_SensorGPSRecorderDGPSDescription
    );
  VMenuItemName :=
    TStringConfigDataElementWithDefByStringRec.Create(
      ALanguageManager, @SAS_STR_SensorGPSRecorderDGPSMenuItemName
    );
  VEntity :=
    TSensorListEntity.Create(
      CSensorDGPSGUID,
      VCaption,
      VDescription,
      VMenuItemName,
      VSensor,
      VSensor.SensorTypeIID
    );
  Self.Add(VEntity);

  VSensor := TSensorFromGPSRecorderGPSUnitInfo.Create(AGPSRecorder, AGPSModule);
  VCaption :=
    TStringConfigDataElementWithDefByStringRec.Create(
      ALanguageManager, @SAS_STR_SensorGPSRecorderGPSUnitInfoCaption
    );
  VDescription :=
    TStringConfigDataElementWithDefByStringRec.Create(
      ALanguageManager, @SAS_STR_SensorGPSRecorderGPSUnitInfoDescription
    );
  VMenuItemName :=
    TStringConfigDataElementWithDefByStringRec.Create(
      ALanguageManager, @SAS_STR_SensorGPSRecorderGPSUnitInfoMenuItemName
    );
  VEntity :=
    TSensorListEntity.Create(
      CSensorGPSUnitInfoGUID,
      VCaption,
      VDescription,
      VMenuItemName,
      VSensor,
      VSensor.SensorTypeIID
    );
  Self.Add(VEntity);

  VSensor := TSensorFromGPSRecorderGPSSatellites.Create(AGPSRecorder);
  VCaption :=
    TStringConfigDataElementWithDefByStringRec.Create(
      ALanguageManager, @SAS_STR_SensorGPSRecorderGPSSatellitesCaption
    );
  VDescription :=
    TStringConfigDataElementWithDefByStringRec.Create(
      ALanguageManager, @SAS_STR_SensorGPSRecorderGPSSatellitesDescription
    );
  VMenuItemName :=
    TStringConfigDataElementWithDefByStringRec.Create(
      ALanguageManager, @SAS_STR_SensorGPSRecorderGPSSatellitesMenuItemName
    );
  VEntity :=
    TSensorListEntity.Create(
      CSensorGPSSatellitesGUID,
      VCaption,
      VDescription,
      VMenuItemName,
      VSensor,
      VSensor.SensorTypeIID
    );
  Self.Add(VEntity);
end;

end.
