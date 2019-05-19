{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2019, SAS.Planet development team.                      *}
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

unit u_TileStorageTypeArchive;

interface

uses
  i_ProjectionSet,
  i_ContentTypeInfo,
  i_ContentTypeManager,
  i_TileFileNameGenerator,
  i_TileFileNameParser,
  i_ArchiveReadWrite,
  i_MapVersionFactory,
  i_ConfigDataProvider,
  i_NotifierTilePyramidUpdate,
  i_TileStorage,
  i_TileStorageAbilities,
  i_TileInfoBasicMemCache,
  i_TileStorageTypeConfig,
  u_TileStorageTypeBase;

type
  TTileStorageTypeArchive = class(TTileStorageTypeBase)
  private
    FContentTypeManager: IContentTypeManager;
    FArchiveReader: IArchiveReaderBase;
    FArchiveWriter: IArchiveWriterBase;
    FNameGenerator: ITileFileNameGenerator;
    FTileNameParser: ITileFileNameParser;
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
      const ATileStorageAbilities: ITileStorageAbilities;
      const AArchiveReader: IArchiveReaderBase;
      const AArchiveWriter: IArchiveWriterBase;
      const AContentTypeManager: IContentTypeManager;
      const ANameGenerator: ITileFileNameGenerator;
      const ATileNameParser: ITileFileNameParser;
      const AMapVersionFactory: IMapVersionFactory;
      const AConfig: ITileStorageTypeConfig
    );
  end;

implementation

uses
  t_CommonTypes,
  u_TileStorageAbilities,
  u_TileStorageArchive;

{ TTileStorageTypeArchive }

constructor TTileStorageTypeArchive.Create(
  const ATileStorageAbilities: ITileStorageAbilities;
  const AArchiveReader: IArchiveReaderBase;
  const AArchiveWriter: IArchiveWriterBase;
  const AContentTypeManager: IContentTypeManager;
  const ANameGenerator: ITileFileNameGenerator;
  const ATileNameParser: ITileFileNameParser;
  const AMapVersionFactory: IMapVersionFactory;
  const AConfig: ITileStorageTypeConfig
);
var
  VAbilities: ITileStorageTypeAbilities;
begin
  VAbilities :=
    TTileStorageTypeAbilities.Create(
      ATileStorageAbilities,
      tstvsVersionIgnored,
      True,
      stsUnicode,
      tstcOneFile
    );
  inherited Create(
    VAbilities,
    AMapVersionFactory,
    AConfig
  );
  FArchiveReader := AArchiveReader;
  FArchiveWriter := AArchiveWriter;
  FContentTypeManager := AContentTypeManager;
  FNameGenerator := ANameGenerator;
  FTileNameParser := ATileNameParser;
end;

function TTileStorageTypeArchive.BuildStorageInternal(
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
    TTileStorageArchive.Create(
      GetAbilities,
      APath,
      FArchiveReader,
      FArchiveWriter,
      AMainContentType,
      FContentTypeManager,
      AProjectionSet,
      FTileNameParser,
      FNameGenerator
    );
end;

end.
