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

unit i_ActiveMapsConfig;

interface

uses
  i_MapTypes,
  i_MapTypeSet,
  i_MapTypeSetChangeable,
  i_Changeable,
  i_ConfigDataElement;

type
  IActiveMapSingle = interface(IChangeable)
    ['{12F47503-E574-4F4F-A30C-7304D38410C7}']
    function GetMapType: IMapType;
    function GetIsActive: Boolean;
  end;

  IActiveMapSingleSet = interface
    ['{4F39120B-56F6-4794-93D1-618287A72D59}']
    function GetMapSingle(const AMapGUID: TGUID): IActiveMapSingle;
  end;

  IMainActiveMap = interface(IConfigDataElement)
    procedure SelectMainByGUID(const AMapGUID: TGUID);
    function GetActiveMap: IMapTypeChangeable;

    function GetMapSingleSet: IActiveMapSingleSet;
    function GetMapsSet: IMapTypeSet;
  end;

  IActivMapWithLayers = interface(IMainActiveMap)
    ['{92B95280-7FD6-402A-8260-3FD83ED6BE36}']
    procedure InvertLayerSelectionByGUID(const AMapGUID: TGUID);
    procedure SelectLayerByGUID(const AMapGUID: TGUID);
    procedure UnSelectLayerByGUID(const AMapGUID: TGUID);

    function GetActiveLayersSet: IMapTypeSetChangeable;
    function GetAllActiveMapsSet: IMapTypeSetChangeable;

    function GetLayersSet: IMapTypeSet;
    function GetAllMapsSet: IMapTypeSet;
  end;

  IMainMapsConfig = interface(IActivMapWithLayers)
    ['{8A8A42A5-9252-4E85-812C-6A5EEEF98443}']
    function GetActiveBitmapLayersSet: IMapTypeSetChangeable;
    function GetActiveKmlLayersSet: IMapTypeSetChangeable;
  end;

implementation

end.
