{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2015, SAS.Planet development team.                      *}
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

unit i_CoordRepresentationConfig;

interface

uses
  t_CoordRepresentation,
  i_ConfigDataElement;

type
    ICoordRepresentationConfigStatic = interface
    ['{74F2A6F1-C15F-476E-8315-AF64A2B11DDB}']
    function GetIsLatitudeFirst: Boolean;
    property IsLatitudeFirst: Boolean read GetIsLatitudeFirst;

    function GetDegrShowFormat: TDegrShowFormat;
    property DegrShowFormat: TDegrShowFormat read GetDegrShowFormat;

    function GetCoordSysType: TCoordSysType;
    property CoordSysType: TCoordSysType read GetCoordSysType;
  end;

  ICoordRepresentationConfig = interface(IConfigDataElement)
    ['{E20335CA-5087-48BC-B2E2-640BB39BD547}']
    function GetIsLatitudeFirst: Boolean;
    procedure SetIsLatitudeFirst(const AValue: Boolean);
    property IsLatitudeFirst: Boolean read GetIsLatitudeFirst write SetIsLatitudeFirst;

    function GetDegrShowFormat: TDegrShowFormat;
    procedure SetDegrShowFormat(const AValue: TDegrShowFormat);
    property DegrShowFormat: TDegrShowFormat read GetDegrShowFormat write SetDegrShowFormat;

    function GetCoordSysType: TCoordSysType;
    procedure SetCoordSysType(const AValue: TCoordSysType);
    property CoordSysType: TCoordSysType read GetCoordSysType write SetCoordSysType;

    function GetStatic: ICoordRepresentationConfigStatic;
  end;

implementation

end.
