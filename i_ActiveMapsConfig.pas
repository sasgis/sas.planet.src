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
  i_ConfigDataElement;

type
  IActiveMapSingle = interface(IConfigDataElement)
    ['{12F47503-E574-4F4F-A30C-7304D38410C7}']
    function GetMapType: IMapType;
    function GetIsActive: Boolean;
  end;

  IActiveMap = interface(IConfigDataElement)
    ['{6BAD8743-D50B-4342-9A68-DA5FBDDFDB04}']
    function GetSelectedGUID: TGUID;
    function GetMapSingle(const AMapGUID: TGUID): IActiveMapSingle;
    function GetMapsSet: IMapTypeSet;
  end;

  IActiveMapsSet = interface(IConfigDataElement)
    ['{09F8FEE4-984C-4D1F-A240-BD8FF3333F85}']
    function IsGUIDSelected(const AMapGUID: TGUID): Boolean;
    function GetMapSingle(const AMapGUID: TGUID): IActiveMapSingle;
    function GetSelectedMapsSet: IMapTypeSet;
    function GetMapsSet: IMapTypeSet;
  end;

  IMainActiveMap = interface(IConfigDataElement)
    procedure SelectMainByGUID(const AMapGUID: TGUID);
    function GetActiveMap: IActiveMap;
    function GetActiveMapsSet: IActiveMapsSet;
  end;

  IActivMapWithLayers = interface(IMainActiveMap)
    ['{92B95280-7FD6-402A-8260-3FD83ED6BE36}']
    procedure SelectLayerByGUID(const AMapGUID: TGUID);
    procedure UnSelectLayerByGUID(const AMapGUID: TGUID);

    function GetActiveLayersSet: IActiveMapsSet;
    function GetAllActiveMapsSet: IActiveMapsSet;
  end;

  IMainMapsConfig = interface(IActivMapWithLayers)
    ['{8A8A42A5-9252-4E85-812C-6A5EEEF98443}']
    function GetSelectedMapType: IMapType;
    function GetActiveBitmapLayersSet: IActiveMapsSet;
    function GetActiveKmlLayersSet: IActiveMapsSet;
  end;
  
implementation

end.
 