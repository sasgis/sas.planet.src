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

unit i_GPSConfig;

interface

uses
  i_GPSModuleByCOMPortConfig,
  i_ConfigDataElement,
  vsagps_public_tracks;

type
  IGPSConfig = interface(IConfigDataElement)
    ['{336A93F1-8E9C-4704-8384-214018758354}']
    function GetGPSEnabled: Boolean;
    procedure SetGPSEnabled(const AValue: Boolean);
    property GPSEnabled: Boolean read GetGPSEnabled write SetGPSEnabled;

    function GetNoDataTimeOut: Integer;
    procedure SetNoDataTimeOut(const AValue: Integer);
    property NoDataTimeOut: Integer read GetNoDataTimeOut write SetNoDataTimeOut;

    function GetWriteLog(const ATrackType: TVSAGPS_TrackType): Boolean;
    procedure SetWriteLog(
      const ATrackType: TVSAGPS_TrackType;
      const AValue: Boolean
    );
    property WriteLog[const ATrackType: TVSAGPS_TrackType]: Boolean read GetWriteLog write SetWriteLog;

    function AllowWriteLog(out ATrackTypes: TVSAGPS_TrackTypes): Boolean;
    procedure AbortWriteLog(const ATrackTypes: TVSAGPS_TrackTypes);

    function GetLogPath: WideString;
    property LogPath: WideString read GetLogPath;

    function GetModuleConfig: IGPSModuleByCOMPortConfig;
    property ModuleConfig: IGPSModuleByCOMPortConfig read GetModuleConfig;
  end;

implementation

end.
