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

unit i_Sensor;

interface

uses
  t_GeoTypes,
  i_JclNotify,
  i_Bitmap32Static,
  i_ConfigDataElement;

type
  ISensor = interface
    ['{EFD30054-5F65-49DF-8EB9-A4EF816D05D2}']
    function CanReset: Boolean;
    procedure Reset;
    function GetDataUpdateNotifier: IJclNotifier;
  end;

  ISensorText = interface(ISensor)
    ['{9FBEF687-7C1E-4BA6-85D7-ECD16E2F1A7A}']
    function GetText: string;
  end;

  ISensorSpeed = interface(ISensor)
    ['{43055AE6-0FBA-47C8-B015-151BC383A7C5}']
    function GetValue: Double;
  end;

  ISensorLength = interface(ISensor)
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

  ISensorBitmap = interface(ISensor)
    ['{6A1BB26A-13DE-4533-BA3F-188769BF71D6}']
    function GetBitmap: IBitmap32Static;
    property Bitmap: IBitmap32Static read GetBitmap;
  end;

  ISensorViewConfig = interface(IConfigDataElement)
    ['{ABA124E3-376F-495E-982C-F3D27F48F610}']
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);
    property Visible: Boolean read GetVisible write SetVisible;
  end;

  ISensorView = interface
    ['{3D7823AF-17D9-495E-901C-BF6435E5C0E1}']
    function GetConfig: ISensorViewConfig;
    function GetSensor: ISensor;
  end;

implementation

end.
