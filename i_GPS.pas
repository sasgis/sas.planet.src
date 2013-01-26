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

unit i_GPS;

interface

uses
  t_GeoTypes,
  vsagps_public_base;

type
  IGPSSatelliteInfo = interface
    ['{38C3C77F-DAC8-4187-B243-0F7001A7DF9B}']
    procedure GetBaseSatelliteParams(AParams: PSingleSatFixibilityData); stdcall;
    procedure GetSkySatelliteParams(AParams: PSingleSatSkyData); stdcall;
  end;

  IGPSSatellitesInView = interface
    ['{D8744967-74EB-47A1-A8FD-4626B5CD2B20}']
    function GetCount(const ATalkerID: AnsiString): Byte; stdcall;
    function GetFixCount(const ATalkerID: AnsiString): Byte; stdcall;
    function GetItem(
      const ATalkerID: AnsiString;
      const AIndex: Byte
    ): IGPSSatelliteInfo; stdcall;

    procedure SetFixedSats(AFixSatsAll: PVSAGPS_FIX_ALL); stdcall;
    function GetFixedSats: PVSAGPS_FIX_ALL; stdcall;

    function GetAllSatelliteParams(
      const AIndex: Byte;
      const ATalkerID: AnsiString;
      var AFixed: Boolean;
      AParams: PSingleSatFixibilityData;
      ASky: PSingleSatSkyData = nil
    ): Boolean; stdcall;

    function EnumerateTalkerID(var ATalkerID: AnsiString): Boolean; stdcall;
    function GetCountForAllTalkerIDs(const AOnlyForFixed: Boolean): Byte; stdcall;

    property Count[const ATalkerID: AnsiString]: Byte read GetCount;
    property FixCount[const ATalkerID: AnsiString]: Byte read GetFixCount;
    property Item[const ATalkerID: AnsiString; const AIndex: Byte]: IGPSSatelliteInfo read GetItem;
  end;

  IGPSPosition = interface
    ['{B2422759-9B8B-4CC5-AAA5-46A7240759D0}']
    function GetLonLat: TDoublePoint;
    property LonLat: TDoublePoint read GetLonLat;

    function GetAltitude: Double;
    property Altitude: Double read GetAltitude;

    function GetGeoidHeight: Double;
    property GeoidHeight: Double read GetGeoidHeight;

    function GetSpeed_KMH: Double;   // in km/h
    property Speed_KMH: Double read GetSpeed_KMH;   // in km/h

    function GetHeading: Double;     // true
    property Heading: Double read GetHeading;     // true

    function GetUTCTime: TDateTime;
    property UTCTime: TDateTime read GetUTCTime;

    function GetHDOP: Double;
    property HDOP: Double read GetHDOP;

    function GetVDOP: Double;
    property VDOP: Double read GetVDOP;

    function GetPDOP: Double;
    property PDOP: Double read GetPDOP;

    function GetDGPS: string;
    property DGPS: string read GetDGPS;

    function GetPositionOK: Boolean;
    property PositionOK: Boolean read GetPositionOK;

    function GetUTCTimeOK: Boolean;
    property UTCTimeOK: Boolean read GetUTCTimeOK;

    function GetSpeedOK: Boolean;
    property SpeedOK: Boolean read GetSpeedOK;

    function GetSatellites: IGPSSatellitesInView; stdcall;
    property Satellites: IGPSSatellitesInView read GetSatellites;
  end;

implementation

end.
