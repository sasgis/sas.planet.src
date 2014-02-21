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

unit u_TileStorageTypeBase;

interface

uses
  i_CoordConverter,
  i_ContentTypeInfo,
  i_TileStorageAbilities,
  i_ConfigDataProvider,
  i_MapVersionFactory,
  i_TileInfoBasicMemCache,
  i_TileStorageTypeConfig,
  i_TileStorage,
  i_TileStorageType,
  u_BaseInterfacedObject;

type
  TTileStorageTypeBase = class(TBaseInterfacedObject, ITileStorageType)
  private
    FAbilities: ITileStorageTypeAbilities;
    FMapVersionFactory: IMapVersionFactory;
    FConfig: ITileStorageTypeConfig;
  protected
    function BuildStorageInternal(
      const AStorageConfigData: IConfigDataProvider;
      const AForceAbilities: ITileStorageAbilities;
      const AGeoConverter: ICoordConverter;
      const AMainContentType: IContentTypeInfoBasic;
      const APath: string;
      const ACacheTileInfo: ITileInfoBasicMemCache
    ): ITileStorage; virtual; abstract;
  protected
    function GetAbilities: ITileStorageTypeAbilities;
    function GetConfig: ITileStorageTypeConfig;
    function GetMapVersionFactory: IMapVersionFactory;
    function BuildStorage(
      const AForceAbilities: ITileStorageAbilities;
      const AGeoConverter: ICoordConverter;
      const AMainContentType: IContentTypeInfoBasic;
      const APath: string;
      const ACacheTileInfo: ITileInfoBasicMemCache
    ): ITileStorage;
  public
    constructor Create(
      const AAbilities: ITileStorageTypeAbilities;
      const AMapVersionFactory: IMapVersionFactory;
      const AConfig: ITileStorageTypeConfig
    );
  end;

implementation

uses
  SysUtils,
  IniFiles,
  u_ConfigDataProviderByIniFile,
  u_TileStorageAbilities;

const
  CStorageConfFileName = 'StorageConfig.ini';

{ TTileStorageTypeBase }

function TTileStorageTypeBase.BuildStorage(
  const AForceAbilities: ITileStorageAbilities;
  const AGeoConverter: ICoordConverter;
  const AMainContentType: IContentTypeInfoBasic;
  const APath: string;
  const ACacheTileInfo: ITileInfoBasicMemCache
): ITileStorage;
var
  VAbilities: ITileStorageAbilities;
  VConfigFileName: string;
  VConfigData: IConfigDataProvider;
  VIniFile: TMemIniFile;
begin
  if Assigned(AForceAbilities) then begin
    VAbilities :=
      TTileStorageAbilities.Create(
        FAbilities.BaseStorageAbilities.IsReadOnly or AForceAbilities.IsReadOnly,
        FAbilities.BaseStorageAbilities.AllowAdd and AForceAbilities.AllowAdd,
        FAbilities.BaseStorageAbilities.AllowDelete and AForceAbilities.AllowDelete,
        FAbilities.BaseStorageAbilities.AllowReplace and AForceAbilities.AllowReplace
      );
  end else begin
    VAbilities := FAbilities.BaseStorageAbilities;
  end;
  VConfigFileName := APath + CStorageConfFileName;
  VConfigData := nil;
  if FileExists(VConfigFileName) then begin
    VIniFile := TMemIniFile.Create(VConfigFileName);
    try
      VConfigData := TConfigDataProviderByIniFile.CreateWithOwn(VIniFile);
      VIniFile :=  nil;
    finally
      VIniFile.Free;
    end;
  end;

  Result :=
    BuildStorageInternal(
      VConfigData,
      VAbilities,
      AGeoConverter,
      AMainContentType,
      APath,
      ACacheTileInfo
    );
end;

constructor TTileStorageTypeBase.Create(
  const AAbilities: ITileStorageTypeAbilities;
  const AMapVersionFactory: IMapVersionFactory;
  const AConfig: ITileStorageTypeConfig
);
begin
  inherited Create;
  FAbilities := AAbilities;
  FMapVersionFactory := AMapVersionFactory;
  FConfig := AConfig;
end;

function TTileStorageTypeBase.GetConfig: ITileStorageTypeConfig;
begin
  Result := FConfig;
end;

function TTileStorageTypeBase.GetAbilities: ITileStorageTypeAbilities;
begin
  Result := FAbilities;
end;

function TTileStorageTypeBase.GetMapVersionFactory: IMapVersionFactory;
begin
  Result := FMapVersionFactory;
end;

end.
