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

unit i_ValueToStringConverter;

interface

uses
  t_GeoTypes,
  t_CommonTypes,
  i_Changeable,
  i_ConfigDataElement;

type
  IValueToStringConverter = interface
    ['{9EC20437-48BD-4D18-BF95-D2390C6F26F5}']
    function DataSizeConvert(const ASizeInKb: Double): string;
    function DistConvert(const ADistInMeters: Double): string;
    function DistPerPixelConvert(const ADistPerPixelInMeters: Double): string;
    function AreaConvert(const AAreaInSqm: Double): string;
    function SpeedConvert(const AKmph: Double): string;
    function AltitudeConvert(const AMeters: Double): string;
    function LonLatConvert(const ALonLat: TDoublePoint): string;
    function LonConvert(
      const ALon: Double;
      ACutZero: boolean
    ): string;
    function LatConvert(
      const ALat: Double;
      ACutZero: boolean
    ): string;
  end;

  IValueToStringConverterChangeable = interface(IChangeable)
    ['{3204C996-689A-4F5D-92FE-7E942AF1822A}']
    function GetStatic: IValueToStringConverter;
  end;

  IValueToStringConverterConfigStatic = interface
    ['{DFD404AC-DB7D-4108-9822-A0DD2943A5C7}']
    function GetDistStrFormat: TDistStrFormat;
    property DistStrFormat: TDistStrFormat read GetDistStrFormat;

    function GetIsLatitudeFirst: Boolean;
    property IsLatitudeFirst: Boolean read GetIsLatitudeFirst;

    function GetDegrShowFormat: TDegrShowFormat;
    property DegrShowFormat: TDegrShowFormat read GetDegrShowFormat;

    function GetAreaShowFormat: TAreaStrFormat;
    property AreaShowFormat: TAreaStrFormat read GetAreaShowFormat;
  end;

  IValueToStringConverterConfig = interface(IConfigDataElement)
    ['{DDC4DF45-A387-43DC-AED7-33935241C718}']
    function GetDistStrFormat: TDistStrFormat;
    procedure SetDistStrFormat(AValue: TDistStrFormat);
    property DistStrFormat: TDistStrFormat read GetDistStrFormat write SetDistStrFormat;

    function GetIsLatitudeFirst: Boolean;
    procedure SetIsLatitudeFirst(AValue: Boolean);
    property IsLatitudeFirst: Boolean read GetIsLatitudeFirst write SetIsLatitudeFirst;

    function GetDegrShowFormat: TDegrShowFormat;
    procedure SetDegrShowFormat(AValue: TDegrShowFormat);
    property DegrShowFormat: TDegrShowFormat read GetDegrShowFormat write SetDegrShowFormat;

    function GetAreaShowFormat: TAreaStrFormat;
    procedure SetAreaShowFormat(AValue: TAreaStrFormat);
    property AreaShowFormat: TAreaStrFormat read GetAreaShowFormat write SetAreaShowFormat;

    function GetStatic: IValueToStringConverterConfigStatic;
  end;

implementation

end.
