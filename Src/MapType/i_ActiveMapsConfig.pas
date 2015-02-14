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

unit i_ActiveMapsConfig;

interface

uses
  i_GUIDListStatic,
  i_ConfigDataElement;

type
  IActiveMapConfig = interface(IConfigDataElement)
    ['{12F3937E-E4A6-4751-AC5A-CE6CFBB10DAB}']
    function GetMainMapGUID: TGUID;
    procedure SetMainMapGUID(const AMapGUID: TGUID);
    property MainMapGUID: TGUID read GetMainMapGUID write SetMainMapGUID;
  end;

  IActiveLayersConfig = interface(IConfigDataElement)
    ['{92B95280-7FD6-402A-8260-3FD83ED6BE36}']
    function GetLayerGuids: IGUIDSetStatic;
    procedure SetLayerGuids(const AValue: IGUIDSetStatic);
    property LayerGuids: IGUIDSetStatic read GetLayerGuids write SetLayerGuids;

    procedure InvertLayerSelectionByGUID(const AMapGUID: TGUID);
    procedure SelectLayerByGUID(const AMapGUID: TGUID);
    procedure UnSelectLayerByGUID(const AMapGUID: TGUID);
  end;

implementation

end.
