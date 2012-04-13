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
  vsagps_public_base,
  vsagps_public_position;

type
  IGPSSatelliteInfo = interface
    ['{38C3C77F-DAC8-4187-B243-0F7001A7DF9B}']
    procedure GetBaseSatelliteParams(AParams: PSingleSatFixibilityData); stdcall;
    procedure GetSkySatelliteParams(AParams: PSingleSatSkyData); stdcall;
  end;

  IGPSSatellitesInView = interface
    ['{D8744967-74EB-47A1-A8FD-4626B5CD2B20}']
    function GetCount(const ATalkerID: String): Byte; stdcall;
    function GetFixCount(const ATalkerID: String): Byte; stdcall;
    function GetItem(const ATalkerID: String; const AIndex: Byte): IGPSSatelliteInfo; stdcall;

    procedure SetFixedSats(AFixSatsAll: PVSAGPS_FIX_ALL); stdcall;
    function GetFixedSats: PVSAGPS_FIX_ALL; stdcall;

    function GetAllSatelliteParams(const AIndex: Byte;
                                   const ATalkerID: String;
                                   var AFixed: Boolean;
                                   AParams: PSingleSatFixibilityData;
                                   ASky: PSingleSatSkyData = nil): Boolean; stdcall;

    function EnumerateTalkerID(var ATalkerID: String): Boolean; stdcall;
    function GetCountForAllTalkerIDs(const AOnlyForFixed: Boolean): Byte; stdcall;

    property Count[const ATalkerID: String]: Byte read GetCount;
    property FixCount[const ATalkerID: String]: Byte read GetFixCount;
    property Item[const ATalkerID: String; const AIndex: Byte]: IGPSSatelliteInfo read GetItem;
  end;

  IGPSPosition = interface
    ['{B2422759-9B8B-4CC5-AAA5-46A7240759D0}']
    function GetPosParams: PSingleGPSData; stdcall;

    function GetTracksParams(var pPos: PSingleGPSData;
                             var pSatFixAll: PVSAGPS_FIX_ALL): Boolean; stdcall;

    function GetSatellites: IGPSSatellitesInView; stdcall;
    property Satellites: IGPSSatellitesInView read GetSatellites;
  end;

implementation

end.
