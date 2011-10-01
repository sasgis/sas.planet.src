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

unit i_GPSPositionFactory;

interface

uses
  ActiveX,
  t_GeoTypes,
  i_GPS;

type
  IGPSPositionFactory = interface
    ['{542F6E48-EC4E-4C8D-9A53-8B392B0E8EA6}']
    function BuildSatelliteInfo(
      const APseudoRandomCode: Integer;
      const AElevation: Integer;
      const AAzimuth: Integer;
      const ASignalToNoiseRatio: Integer;
      const AIsFix: Boolean
    ): IGPSSatelliteInfo;

    function BuildSatellitesInViewEmpty: IGPSSatellitesInView;
    function BuildSatellitesInView(
      const AFixCount: Integer;
      const AItemsCount: Integer;
      const AItems: PUnknownList
    ): IGPSSatellitesInView;

    function BuildPositionEmpty: IGPSPosition;
    function BuildPosition(
      const APosition: TDoublePoint;
      const AAltitude: Double;
      const ASpeed_KMH: Double;
      const AHeading: Double;
      const AUTCDateTime: TDateTime;
      const ALocalDateTime: TDateTime;
      const AIsFix: Word;
      const AHDOP: Double;
      const AVDOP: Double;
      const APDOP: Double;
      const ASatellites: IGPSSatellitesInView
    ): IGPSPosition;
  end;

implementation

end.
