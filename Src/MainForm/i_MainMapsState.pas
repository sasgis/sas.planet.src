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

unit i_MainMapsState;

interface

uses
  i_MapType,
  i_MapTypeListChangeable,
  i_MapTypeSet,
  i_MapTypeSetChangeable;

type
  IMainMapsState = interface
    ['{0ECCE24C-F40E-43D2-ABF1-2249CD4CFDC0}']
    function GetMapsSet: IMapTypeSet;
    property MapsSet: IMapTypeSet read GetMapsSet;

    function GetLayersSet: IMapTypeSet;
    property LayersSet: IMapTypeSet read GetLayersSet;

    function GetAllMapsSet: IMapTypeSet;
    property AllMapsSet: IMapTypeSet read GetAllMapsSet;

    function GetActiveMap: IMapTypeChangeable;
    property ActiveMap: IMapTypeChangeable read GetActiveMap;

    function GetActiveLayersSet: IMapTypeSetChangeable;
    property ActiveLayersSet: IMapTypeSetChangeable read GetActiveLayersSet;

    function GetAllActiveMapsSet: IMapTypeSetChangeable;
    property AllActiveMapsSet: IMapTypeSetChangeable read GetAllActiveMapsSet;

    function GetActiveMapsSetLicenseNotEmpty: IMapTypeSetChangeable;
    property ActiveMapsSetLicenseNotEmpty: IMapTypeSetChangeable read GetActiveMapsSetLicenseNotEmpty;

    function GetActiveBitmapMapsSet: IMapTypeSetChangeable;
    property ActiveBitmapMapsSet: IMapTypeSetChangeable read GetActiveBitmapMapsSet;

    function GetActiveBitmapLayersSet: IMapTypeSetChangeable;
    property ActiveBitmapLayersSet: IMapTypeSetChangeable read GetActiveBitmapLayersSet;

    function GetActiveBitmapLayersList: IMapTypeListChangeable;
    property ActiveBitmapLayersList: IMapTypeListChangeable read GetActiveBitmapLayersList;

    function GetActiveKmlLayersSet: IMapTypeSetChangeable;
    property ActiveKmlLayersSet: IMapTypeSetChangeable read GetActiveKmlLayersSet;

    function GetMiniMapMapsSet: IMapTypeSet;
    property MiniMapMapsSet: IMapTypeSet read GetMiniMapMapsSet;

    function GetMiniMapLayersSet: IMapTypeSet;
    property MiniMapLayersSet: IMapTypeSet read GetMiniMapLayersSet;

    function GetMiniMapActiveMap: IMapTypeChangeable;
    property MiniMapActiveMap: IMapTypeChangeable read GetMiniMapActiveMap;

    function GetMiniMapActiveLayersSet: IMapTypeSetChangeable;
    property MiniMapActiveLayersSet: IMapTypeSetChangeable read GetMiniMapActiveLayersSet;

    function GetMiniMapActiveBitmapLayersList: IMapTypeListChangeable;
    property MiniMapActiveBitmapLayersList: IMapTypeListChangeable read GetMiniMapActiveBitmapLayersList;

    function GetFillingMapActiveMap: IMapTypeChangeable;
    property FillingMapActiveMap: IMapTypeChangeable read GetFillingMapActiveMap;
  end;

implementation

end.
