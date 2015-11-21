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

unit u_TileStorageTypeDBMS;

interface

uses
  i_ProjectionSet,
  i_ContentTypeInfo,
  i_ContentTypeManager,
  i_NotifierTime,
  i_MapVersionFactory,
  i_ConfigDataProvider,
  i_NotifierTilePyramidUpdate,
  i_TileStorage,
  i_TileStorageAbilities,
  i_TileStorageTypeConfig,
  i_TileInfoBasicMemCache,
  u_TileStorageTypeBase;

type
  TTileStorageTypeDBMS = class(TTileStorageTypeBase)
  private
    FGCNotifier: INotifierTime;
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
      const AGCNotifier: INotifierTime;
      const AContentTypeManager: IContentTypeManager;
      const AMapVersionFactory: IMapVersionFactory;
      const AConfig: ITileStorageTypeConfig
    );
  end;

implementation

uses
  SysUtils,
  t_CommonTypes,
  u_TileStorageAbilities,
  u_TileStorageDBMS;

{ TTileStorageTypeDBMS }

constructor TTileStorageTypeDBMS.Create(
  const AGCNotifier: INotifierTime;
  const AContentTypeManager: IContentTypeManager;
  const AMapVersionFactory: IMapVersionFactory;
  const AConfig: ITileStorageTypeConfig
);
var
  VAbilities: ITileStorageTypeAbilities;
begin
  VAbilities :=
    TTileStorageTypeAbilities.Create(
      TTileStorageAbilities.Create(False, True, True, True, True, True),
      tstvsMultiVersions,
      True,
      stsUnicode,
      tstcOther
    );
  inherited Create(
    VAbilities,
    AMapVersionFactory,
    AConfig
  );
  FGCNotifier := AGCNotifier;
  FContentTypeManager := AContentTypeManager;
end;

function TTileStorageTypeDBMS.BuildStorageInternal(
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
    TTileStorageDBMS.Create(
      GetAbilities,
      AForceAbilities,
      AProjectionSet,
      ATileNotifier,
      GetConfig.BasePath.Path,
      ExcludeTrailingPathDelimiter(APath),
      FGCNotifier,
      ACacheTileInfo,
      FContentTypeManager,
      GetMapVersionFactory,
      AMainContentType
    );
end;

end.
