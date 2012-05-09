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

unit i_GSMGeoCodeConfig;

interface

uses
  i_ConfigDataElement;

type
  IGSMGeoCodeConfig = interface(IConfigDataElement)
    ['{E60F1967-FE2D-4C3C-81D1-D99B50A0F21F}']
    function GetUseGSMByCOM: Boolean;
    procedure SetUseGSMByCOM(const AValue: Boolean);
    property UseGSMByCOM: Boolean read GetUseGSMByCOM write SetUseGSMByCOM;

    function GetPortName: string;
    procedure SetPortName(const AValue: string);
    property PortName: string read GetPortName write SetPortName;

    function GetBaudRate: Cardinal;
    procedure SetBaudRate(const AValue: Cardinal);
    property BaudRate: Cardinal read GetBaudRate write SetBaudRate;

    function GetWaitTime: Cardinal;
    procedure SetWaitTime(const AValue: Cardinal);
    property WaitTime: Cardinal read GetWaitTime write SetWaitTime;
  end;

implementation

end.
