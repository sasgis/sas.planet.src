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

unit i_TileStorageType;

interface

uses
  i_CoordConverter,
  i_ContentTypeInfo,
  i_TileStorageAbilities,
  i_MapVersionFactory,
  i_NotifierTilePyramidUpdate,
  i_TileInfoBasicMemCache,
  i_TileStorageTypeConfig,
  i_TileStorage;

type
  ITileStorageType = interface
    ['{EBB122FB-5382-49CA-A265-3BEA89694B0E}']
    function GetAbilities: ITileStorageTypeAbilities;
    property Abilities: ITileStorageTypeAbilities read GetAbilities;

    function GetConfig: ITileStorageTypeConfig;
    property Config: ITileStorageTypeConfig read GetConfig;

    function GetMapVersionFactory: IMapVersionFactory;
    property MapVersionFactory: IMapVersionFactory read GetMapVersionFactory;

    function BuildStorage(
      const AForceAbilities: ITileStorageAbilities;
      const AGeoConverter: ICoordConverter;
      const AMainContentType: IContentTypeInfoBasic;
      const ATileNotifier: INotifierTilePyramidUpdateInternal;
      const APath: string;
      const ACacheTileInfo: ITileInfoBasicMemCache
    ): ITileStorage;
  end;

implementation

end.
