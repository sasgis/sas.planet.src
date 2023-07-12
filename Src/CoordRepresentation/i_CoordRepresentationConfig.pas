{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
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

    function GetGeogCoordShowFormat: TGeogCoordShowFormat;
    property GeogCoordShowFormat: TGeogCoordShowFormat read GetGeogCoordShowFormat;

    function GetProjCoordShowFormat: TProjCoordShowFormat;
    property ProjCoordShowFormat: TProjCoordShowFormat read GetProjCoordShowFormat;

    function GetCoordSysType: TCoordSysType;
    property CoordSysType: TCoordSysType read GetCoordSysType;

    function GetCoordSysInfoType: TCoordSysInfoType;
    property CoordSysInfoType: TCoordSysInfoType read GetCoordSysInfoType;
  end;

  ICoordRepresentationConfig = interface(IConfigDataElement)
    ['{E20335CA-5087-48BC-B2E2-640BB39BD547}']
    function GetIsLatitudeFirst: Boolean;
    procedure SetIsLatitudeFirst(const AValue: Boolean);
    property IsLatitudeFirst: Boolean read GetIsLatitudeFirst write SetIsLatitudeFirst;

    function GetGeogCoordShowFormat: TGeogCoordShowFormat;
    procedure SetGeogCoordShowFormat(const AValue: TGeogCoordShowFormat);
    property GeogCoordShowFormat: TGeogCoordShowFormat read GetGeogCoordShowFormat write SetGeogCoordShowFormat;

    function GetProjCoordShowFormat: TProjCoordShowFormat;
    procedure SetProjCoordShowFormat(const AValue: TProjCoordShowFormat);
    property ProjCoordShowFormat: TProjCoordShowFormat read GetProjCoordShowFormat write SetProjCoordShowFormat;

    function GetCoordSysType: TCoordSysType;
    procedure SetCoordSysType(const AValue: TCoordSysType);
    property CoordSysType: TCoordSysType read GetCoordSysType write SetCoordSysType;

    function GetCoordSysInfoType: TCoordSysInfoType;
    procedure SetCoordSysInfoType(const AValue: TCoordSysInfoType);
    property CoordSysInfoType: TCoordSysInfoType read GetCoordSysInfoType write SetCoordSysInfoType;

    function GetStatic: ICoordRepresentationConfigStatic;
  end;

implementation

end.
