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

unit i_UseTilePrevZoomConfig;

interface

uses
  i_ConfigDataElement;

type
  IUseTilePrevZoomTileConfigStatic = interface
    function GetUsePrevZoomAtMap: Boolean;
    property UsePrevZoomAtMap: Boolean read GetUsePrevZoomAtMap;

    function GetUsePrevZoomAtLayer: Boolean;
    property UsePrevZoomAtLayer: Boolean read GetUsePrevZoomAtLayer;

    function GetUsePrevZoomAtVectorLayer: Boolean;
    property UsePrevZoomAtVectorLayer: Boolean read GetUsePrevZoomAtVectorLayer;
  end;

  IUseTilePrevZoomConfig = interface(IConfigDataElement)
    ['{FAA51B31-55AA-4F65-AA91-20F8C1174187}']
    function GetUsePrevZoomAtMap: Boolean;
    procedure SetUsePrevZoomAtMap(const AValue: Boolean);
    property UsePrevZoomAtMap: Boolean read GetUsePrevZoomAtMap write SetUsePrevZoomAtMap;

    function GetUsePrevZoomAtLayer: Boolean;
    procedure SetUsePrevZoomAtLayer(const AValue: Boolean);
    property UsePrevZoomAtLayer: Boolean read GetUsePrevZoomAtLayer write SetUsePrevZoomAtLayer;

    function GetUsePrevZoomAtVectorLayer: Boolean;
    procedure SetUsePrevZoomAtVectorLayer(const AValue: Boolean);
    property UsePrevZoomAtVectorLayer: Boolean read GetUsePrevZoomAtVectorLayer write SetUsePrevZoomAtVectorLayer;

    function GetStatic: IUseTilePrevZoomTileConfigStatic;
  end;

implementation

end.
