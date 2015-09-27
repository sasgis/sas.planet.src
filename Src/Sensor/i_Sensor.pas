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

unit i_Sensor;

interface

uses
  t_GeoTypes,
  i_GPS,
  i_Changeable;

type
  ISensor = interface(IChangeable)
    ['{EFD30054-5F65-49DF-8EB9-A4EF816D05D2}']
    function GetSensorTypeIID: TGUID;
    property SensorTypeIID: TGUID read GetSensorTypeIID;
  end;

  ISensorResetable = interface
    ['{C266BFCA-541A-44A0-B791-06F02973F4B2}']
    procedure Reset;
  end;

  ISensorText = interface(ISensor)
    ['{9FBEF687-7C1E-4BA6-85D7-ECD16E2F1A7A}']
    function GetText: string;
  end;

  ISensorDouble = interface(ISensor)
    ['{3E23DA37-C873-4A85-A968-DA632D272D31}']
    function GetValue: Double;
  end;

  ISensorBatteryLifePercent = interface(ISensor)
    ['{F686AF94-BC3B-46B7-8096-5A6DDDE18F11}']
    // 0 - 100 BattaryLifePercent
    // 101 BattaryStateCharge
    // 200 BattaryStateOnLine
    // 255 BattaryStateUnknown
    function GetValue: Byte;
  end;

  ISensorSpeed = interface(ISensor)
    ['{43055AE6-0FBA-47C8-B015-151BC383A7C5}']
    function GetValue: Double;
  end;

  ISensorDistance = interface(ISensor)
    ['{E868E131-E588-4342-A7C9-73FBCC96AE69}']
    function GetValue: Double;
  end;

  ISensorDegrees = interface(ISensor)
    ['{4F08BE78-6584-46AC-B446-4F8DA851BD29}']
    function GetValue: Double;
  end;

  ISensorTime = interface(ISensor)
    ['{54A35D81-DB40-44EE-993E-D64BA01A3FC1}']
    function GetValue: TDateTime;
  end;

  ISensorPosition = interface(ISensor)
    ['{43055AE6-0FBA-47C8-B015-151BC383A7C5}']
    function GetValue: TDoublePoint;
  end;

  ISensorGPSSatellites = interface(ISensor)
    ['{6A1BB26A-13DE-4533-BA3F-188769BF71D6}']
    function GetInfo: IGPSSatellitesInView;
    property Info: IGPSSatellitesInView read GetInfo;
  end;

implementation

end.
