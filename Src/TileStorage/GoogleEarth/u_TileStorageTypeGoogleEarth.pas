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

unit u_TileStorageTypeGoogleEarth;

interface

uses
  i_ProjectionSet,
  i_ContentTypeInfo,
  i_MapVersionFactory,
  i_ConfigDataProvider,
  i_NotifierTilePyramidUpdate,
  i_TileStorage,
  i_TileStorageAbilities,
  i_TileInfoBasicMemCache,
  i_TileStorageTypeConfig,
  u_TileStorageTypeBase;

type
  TTileStorageTypeGoogleEarth = class(TTileStorageTypeBase)
  private
    FIsTerrainStorage: Boolean;
    FIsGeoCacherStorage: Boolean;
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
      const AIsTerrainStorage: Boolean;
      const AIsGeoCacherStorage: Boolean;
      const AConfig: ITileStorageTypeConfig
    );
  end;

implementation

uses
  SysUtils,
  t_CommonTypes,
  c_CoordConverter,
  u_TileStorageAbilities,
  u_TileStorageGoogleEarth;

{ TTileStorageTypeGoogleEarth }

constructor TTileStorageTypeGoogleEarth.Create(
  const AMapVersionFactory: IMapVersionFactory;
  const AIsTerrainStorage: Boolean;
  const AIsGeoCacherStorage: Boolean;
  const AConfig: ITileStorageTypeConfig
);
var
  VAbilities: ITileStorageTypeAbilities;
begin
  VAbilities :=
    TTileStorageTypeAbilities.Create(
      TTileStorageAbilities.Create(True, True, False, False, False, False),
      tstvsMultiVersions,
      True,
      stsAnsi,
      tstcFolder
    );
  inherited Create(
    VAbilities,
    AMapVersionFactory,
    AConfig
  );
  FIsTerrainStorage := AIsTerrainStorage;
  FIsGeoCacherStorage := AIsGeoCacherStorage;
end;

function TTileStorageTypeGoogleEarth.BuildStorageInternal(
  const AStorageConfigData: IConfigDataProvider;
  const AForceAbilities: ITileStorageAbilities;
  const AProjectionSet: IProjectionSet;
  const AMainContentType: IContentTypeInfoBasic;
  const ATileNotifier: INotifierTilePyramidUpdateInternal;
  const APath: string;
  const ACacheTileInfo: ITileInfoBasicMemCache
): ITileStorage;
var
  VNameInCache: string;
begin
  Result := nil;
  if
    (AProjectionSet.Zooms[0].ProjectionType.ProjectionEPSG = CGELonLatProjectionEPSG) and
    (AProjectionSet.Zooms[0].GetTileSplitCode = CTileSplitQuadrate256x256)
  then begin
    VNameInCache := ExtractFileName(APath);
    if SameText(VNameInCache, 'earth') then begin
      VNameInCache := 'earth';
    end else if SameText(VNameInCache, 'mars') then begin
      VNameInCache := 'mars';
    end else if SameText(VNameInCache, 'moon') then begin
      VNameInCache := 'moon';
    end else if SameText(VNameInCache, 'sky') then begin
      VNameInCache := 'sky';
    end else begin
      VNameInCache := '';
    end;

    Result :=
      TTileStorageGoogleEarth.Create(
        GetAbilities,
        AForceAbilities,
        AProjectionSet,
        ATileNotifier,
        GetConfig.BasePath.FullPath,
        VNameInCache,
        FIsTerrainStorage,
        FIsGeoCacherStorage,
        ACacheTileInfo,
        GetMapVersionFactory,
        AMainContentType
      );
  end;
end;

end.
