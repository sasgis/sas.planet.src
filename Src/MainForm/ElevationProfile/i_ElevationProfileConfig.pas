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

unit i_ElevationProfileConfig;

interface

uses
  i_ConfigDataElement;

type
  TElevationSource = (
    esTrackMetadata,
    esTerrainProvider
  );

  IElevationProfileConfigStatic = interface
    ['{BFE69AB6-3549-4B80-9C8D-A130C0A38862}']
    function GetElevationSource: TElevationSource;
    property ElevationSource: TElevationSource read GetElevationSource;

    function GetShowElevation: Boolean;
    property ShowElevation: Boolean read GetShowElevation;

    function GetShowSpeed: Boolean;
    property ShowSpeed: Boolean read GetShowSpeed;

    function GetKeepAspectRatio: Boolean;
    property KeepAspectRatio: Boolean read GetKeepAspectRatio;

    function GetZoomWithMouseWheel: Boolean;
    property ZoomWithMouseWheel: Boolean read GetZoomWithMouseWheel;

    function GetUseDataFiltering: Boolean;
    property UseDataFiltering: Boolean read GetUseDataFiltering;

    function GetCenterMap: Boolean;
    property CenterMap: Boolean read GetCenterMap;
  end;

  IElevationProfileConfig = interface(IConfigDataElement)
  ['{F8E46007-F1AF-4650-887B-589FC05BB146}']
    function GetElevationSource: TElevationSource;
    procedure SetElevationSource(const AValue: TElevationSource);
    property ElevationSource: TElevationSource read GetElevationSource write SetElevationSource;

    function GetShowElevation: Boolean;
    procedure SetShowElevation(const AValue: Boolean);
    property ShowElevation: Boolean read GetShowElevation write SetShowElevation;

    function GetShowSpeed: Boolean;
    procedure SetShowSpeed(const AValue: Boolean);
    property ShowSpeed: Boolean read GetShowSpeed write SetShowSpeed;

    function GetKeepAspectRatio: Boolean;
    procedure SetKeepAspectRatio(const AValue: Boolean);
    property KeepAspectRatio: Boolean read GetKeepAspectRatio write SetKeepAspectRatio;

    function GetZoomWithMouseWheel: Boolean;
    procedure SetZoomWithMouseWheel(const AValue: Boolean);
    property ZoomWithMouseWheel: Boolean read GetZoomWithMouseWheel write SetZoomWithMouseWheel;

    function GetUseDataFiltering: Boolean;
    procedure SetUseDataFiltering(const AValue: Boolean);
    property UseDataFiltering: Boolean read GetUseDataFiltering write SetUseDataFiltering;

    function GetCenterMap: Boolean;
    procedure SetCenterMap(const AValue: Boolean);
    property CenterMap: Boolean read GetCenterMap write SetCenterMap;

    function GetStatic: IElevationProfileConfigStatic;
  end;

implementation

end.
