{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2017, SAS.Planet development team.                      *}
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

unit i_SunCalcProvider;

interface

uses
  i_ConfigDataElement,
  t_GeoTypes;

type
  TSunCalcTzInfo = record
    TzName: string;
    TzTime: Extended;
    TzOffset: Extended;
  end;

  ISunCalcProvider = interface(IConfigDataElement)
    ['{8F946926-994D-42A1-8A34-B8699B78F0CE}']
    function GetLocation: TDoublePoint;
    procedure SetLocation(const AValue: TDoublePoint);
    property Location: TDoublePoint read GetLocation write SetLocation;

    function GetDateTime: TDateTime;
    procedure SetDateTime(const AValue: TDateTime);
    property UTCDateTime: TDateTime read GetDateTime write SetDateTime;

    procedure SetDateTimeFromLocalTime(const ALocalTime: TDateTime);
    property LocalDateTime: TDateTime write SetDateTimeFromLocalTime;

    function GetTzInfo(
      const AUTCTime: TDateTime;
      out ATzInfo: TSunCalcTzInfo
    ): Boolean;

    function GetTzOffset(
      const AUTCTime: TDateTime;
      out ATzOffset: Extended
    ): Boolean;

    procedure Reset;
  end;

implementation

end.
