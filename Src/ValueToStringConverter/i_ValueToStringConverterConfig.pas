{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit i_ValueToStringConverterConfig;

interface

uses
  t_CommonTypes,
  i_ConfigDataElement;

type
  IValueToStringConverterConfigStatic = interface
    ['{DFD404AC-DB7D-4108-9822-A0DD2943A5C7}']
    function GetDistStrFormat: TDistStrFormat;
    property DistStrFormat: TDistStrFormat read GetDistStrFormat;

    function GetAreaShowFormat: TAreaStrFormat;
    property AreaShowFormat: TAreaStrFormat read GetAreaShowFormat;
  end;

  IValueToStringConverterConfig = interface(IConfigDataElement)
    ['{DDC4DF45-A387-43DC-AED7-33935241C718}']
    function GetDistStrFormat: TDistStrFormat;
    procedure SetDistStrFormat(const AValue: TDistStrFormat);
    property DistStrFormat: TDistStrFormat read GetDistStrFormat write SetDistStrFormat;

    function GetAreaShowFormat: TAreaStrFormat;
    procedure SetAreaShowFormat(const AValue: TAreaStrFormat);
    property AreaShowFormat: TAreaStrFormat read GetAreaShowFormat write SetAreaShowFormat;

    function GetStatic: IValueToStringConverterConfigStatic;
  end;

implementation

end.
