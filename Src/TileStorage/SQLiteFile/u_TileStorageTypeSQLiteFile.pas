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

unit u_TileStorageTypeSQLiteFile;

interface

uses
  t_TileStorageSQLiteFile,
  i_ProjectionSet,
  i_ContentTypeInfo,
  i_ContentTypeManager,
  i_TileInfoBasicMemCache,
  i_MapVersionFactory,
  i_ConfigDataProvider,
  i_NotifierTilePyramidUpdate,
  i_TileStorage,
  i_TileStorageAbilities,
  i_TileStorageTypeConfig,
  u_TileStorageTypeBase;

type
  TTileStorageTypeSQLiteFile = class(TTileStorageTypeBase)
  private
    FFormatId: TTileStorageSQLiteFileFormatId;
    FContentTypeManager: IContentTypeManager;
  protected
    function BuildStorageInternal(
      const AStorageConfigData: IConfigDataProvider;
      const AForceAbilities: ITileStorageAbilities;
      const AProjectionSet: IProjectionSet;
      const AMainContentType: IContentTypeInfoBasic;
      const ATileNotifier: INotifierTilePyramidUpdateInternal;
      const APath: string;
      const ACacheTileInfo: ITileInfoBasicMemCache
    ): ITileStorage; override;
  public
    constructor Create(
      const AMapVersionFactory: IMapVersionFactory;
      const AContentTypeManager: IContentTypeManager;
      const AConfig: ITileStorageTypeConfig;
      const AFormatId: TTileStorageSQLiteFileFormatId
    );
  end;

implementation

uses
  t_CommonTypes,
  u_TileStorageAbilities,
  u_TileStorageSQLiteFile;

{ TTileStorageTypeSQLiteFile }

constructor TTileStorageTypeSQLiteFile.Create(
  const AMapVersionFactory: IMapVersionFactory;
  const AContentTypeManager: IContentTypeManager;
  const AConfig: ITileStorageTypeConfig;
  const AFormatId: TTileStorageSQLiteFileFormatId
);
var
  VAbilities: ITileStorageTypeAbilities;
  VAbilitiesTypes: TTileStorageAbilitiesTypes;
begin
  if AFormatId = sfOruxMaps then begin
    VAbilitiesTypes := CTileStorageReadOnly;
  end else begin
    VAbilitiesTypes := CTileStorageReadWrite;
  end;

  VAbilities :=
    TTileStorageTypeAbilities.Create(
      TTileStorageAbilities.Create(VAbilitiesTypes) as ITileStorageAbilities,
      tstvsVersionIgnored,
      False,
      stsUnicode,
      tstcOneFile
    );

  inherited Create(
    VAbilities,
    AMapVersionFactory,
    AConfig
  );

  FContentTypeManager := AContentTypeManager;
  FFormatId := AFormatId;
end;

function TTileStorageTypeSQLiteFile.BuildStorageInternal(
  const AStorageConfigData: IConfigDataProvider;
  const AForceAbilities: ITileStorageAbilities;
  const AProjectionSet: IProjectionSet;
  const AMainContentType: IContentTypeInfoBasic;
  const ATileNotifier: INotifierTilePyramidUpdateInternal;
  const APath: string;
  const ACacheTileInfo: ITileInfoBasicMemCache
): ITileStorage;
begin
  Result :=
    TTileStorageSQLiteFile.Create(
      GetAbilities,
      AForceAbilities,
      ACacheTileInfo,
      AProjectionSet,
      ATileNotifier,
      GetMapVersionFactory,
      AMainContentType,
      FContentTypeManager,
      APath,
      FFormatId
    );
end;

end.
