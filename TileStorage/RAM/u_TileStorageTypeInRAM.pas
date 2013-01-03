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

unit u_TileStorageTypeInRAM;

interface

uses
  i_CoordConverter,
  i_ContentTypeInfo,
  i_ContentTypeManager,
  i_NotifierTTLCheck,
  i_SimpleTileStorageConfig,
  i_TileStorage,
  i_TileStorageTypeConfig,
  u_TileStorageTypeBase;

type
  TTileStorageTypeInRAM = class(TTileStorageTypeBase)
  private
    FStorageConfig: ISimpleTileStorageConfigStatic;
    FGCList: INotifierTTLCheck;
    FContentTypeManager: IContentTypeManager;
  protected
    function BuildStorage(
      const AGeoConverter: ICoordConverter;
      const AMainContentType: IContentTypeInfoBasic;
      const APath: string
    ): ITileStorage; override;
  public
    constructor Create(
      const AGCList: INotifierTTLCheck;
      const AContentTypeManager: IContentTypeManager;
      const AConfig: ITileStorageTypeConfig;
      const AStorageConfig: ISimpleTileStorageConfigStatic
    );
  end;

implementation

uses
  u_TileStorageTypeAbilities,
  u_TileStorageInRAM,
  u_MapVersionFactorySimpleString;

{ TTileStorageTypeInRAM }

constructor TTileStorageTypeInRAM.Create(
  const AGCList: INotifierTTLCheck;
  const AContentTypeManager: IContentTypeManager;
  const AConfig: ITileStorageTypeConfig;
  const AStorageConfig: ISimpleTileStorageConfigStatic
);
begin
  inherited Create(
    TTileStorageTypeAbilitiesFileFolder.Create,
    TMapVersionFactorySimpleString.Create,
    AConfig
  );
  FStorageConfig := AStorageConfig;
  FGCList := AGCList;
  FContentTypeManager := AContentTypeManager;
end;

function TTileStorageTypeInRAM.BuildStorage(
  const AGeoConverter: ICoordConverter;
  const AMainContentType: IContentTypeInfoBasic;
  const APath: string
): ITileStorage;
begin
  Result :=
    TTileStorageInRAM.Create(
      FStorageConfig,
      AGeoConverter,
      FGCList,
      FContentTypeManager,
      GetMapVersionFactory,
      AMainContentType
    );
end;

end.
