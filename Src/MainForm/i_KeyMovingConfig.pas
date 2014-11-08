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

unit i_KeyMovingConfig;

interface

uses
  i_ConfigDataElement;

type
  IKeyMovingConfigStatic = interface
    ['{E82C27D5-CB77-4E56-8A09-B1B894FAF224}']
    function GetFirstKeyPressDelta: Double;
    property FirstKeyPressDelta: Double read GetFirstKeyPressDelta;

    function GetMinPixelPerSecond: Double;
    property MinPixelPerSecond: Double read GetMinPixelPerSecond;

    function GetMaxPixelPerSecond: Double;
    property MaxPixelPerSecond: Double read GetMaxPixelPerSecond;

    function GetSpeedChangeTime: Double;
    property SpeedChangeTime: Double read GetSpeedChangeTime;

    function GetStopTime: Double;
    property StopTime: Double read GetStopTime;
  end;

  IKeyMovingConfig = interface(IConfigDataElement)
    ['{87769678-9D11-4E47-AAE5-88F4809B7406}']
    function GetFirstKeyPressDelta: Double;
    procedure SetFirstKeyPressDelta(const AValue: Double);
    property FirstKeyPressDelta: Double read GetFirstKeyPressDelta write SetFirstKeyPressDelta;

    function GetMinPixelPerSecond: Double;
    procedure SetMinPixelPerSecond(const AValue: Double);
    property MinPixelPerSecond: Double read GetMinPixelPerSecond write SetMinPixelPerSecond;

    function GetMaxPixelPerSecond: Double;
    procedure SetMaxPixelPerSecond(const AValue: Double);
    property MaxPixelPerSecond: Double read GetMaxPixelPerSecond write SetMaxPixelPerSecond;

    function GetSpeedChangeTime: Double;
    procedure SetSpeedChangeTime(const AValue: Double);
    property SpeedChangeTime: Double read GetSpeedChangeTime write SetSpeedChangeTime;

    function GetStopTime: Double;
    procedure SetStopTime(const AValue: Double);
    property StopTime: Double read GetStopTime write SetStopTime;

    function GetStatic: IKeyMovingConfigStatic;
  end;

implementation

end.
