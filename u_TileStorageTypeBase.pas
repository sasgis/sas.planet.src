{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
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
  i_StorageTypeAbilities,
  i_MapVersionConfig,
  i_TileStorageTypeConfig,
  i_TileStorage,
  i_TileStorageType;

type
  TTileStorageTypeBase = class(TInterfacedObject, ITileStorageType)
  private
    FGUID: TGUID;
    FCaption: string;
    FInfo: IStorageTypeAbilities;
    FMapVersionFactory: IMapVersionFactory;
    FConfig: ITileStorageTypeConfig;
  protected
    function GetGUID: TGUID;
    function GetInfo: IStorageTypeAbilities;
    function GetConfig: ITileStorageTypeConfig;
    function GetMapVersionFactory: IMapVersionFactory;
    function BuildStorage(const APath: string): ITileStorage; virtual; abstract;
    function GetCaption: string;
  public
    constructor Create(
      const AGUID: TGUID;
      const ACaption: string;
      const AInfo: IStorageTypeAbilities;
      const AMapVersionFactory: IMapVersionFactory;
      const AConfig: ITileStorageTypeConfig
    );
  end;

implementation

{ TTileStorageTypeBase }

constructor TTileStorageTypeBase.Create(
  const AGUID: TGUID;
  const ACaption: string;
  const AInfo: IStorageTypeAbilities;
  const AMapVersionFactory: IMapVersionFactory;
  const AConfig: ITileStorageTypeConfig
);
begin
  FGUID := AGUID;
  FCaption := ACaption;
  FInfo := AInfo;
  FMapVersionFactory := AMapVersionFactory;
  FConfig := AConfig;
end;

function TTileStorageTypeBase.GetCaption: string;
begin
  Result := FCaption;
end;

function TTileStorageTypeBase.GetConfig: ITileStorageTypeConfig;
begin
  Result := FConfig;
end;

function TTileStorageTypeBase.GetGUID: TGUID;
begin
  Result := FGUID;
end;

function TTileStorageTypeBase.GetInfo: IStorageTypeAbilities;
begin
  Result := FInfo;
end;

function TTileStorageTypeBase.GetMapVersionFactory: IMapVersionFactory;
begin
  Result := FMapVersionFactory;
end;

end.
