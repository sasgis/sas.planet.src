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

unit u_TileStorageTypeListItem;

interface

uses
  i_TileStorageType,
  i_TileStorageTypeListItem,
  u_BaseInterfacedObject;

type
  TTileStorageTypeListItem = class(TBaseInterfacedObject, ITileStorageTypeListItem)
  private
    FGUID: TGUID;
    FIntCode: Integer;
    FCaption: string;
    FStorageType: ITileStorageType;
    FCanUseAsDefault: Boolean;
    FIsChangeable: Boolean;
  private
    function GetGUID: TGUID;
    function GetIntCode: Integer;
    function GetCaption: string;
    function GetStorageType: ITileStorageType;
    function GetCanUseAsDefault: Boolean;
    function GetIsChangeable: Boolean;
  public
    constructor Create(
      const AGUID: TGUID;
      const AIntCode: Integer;
      const ACaption: string;
      const AStorageType: ITileStorageType;
      ACanUseAsDefault: Boolean;
      AIsChangeable: Boolean
    );
  end;

implementation

{ TTileStorageTypeListItem }

constructor TTileStorageTypeListItem.Create(
  const AGUID: TGUID;
  const AIntCode: Integer;
  const ACaption: string;
  const AStorageType: ITileStorageType;
  ACanUseAsDefault: Boolean;
  AIsChangeable: Boolean
);
begin
  inherited Create;
  FGUID := AGUID;
  FIntCode := AIntCode;
  FCaption := ACaption;
  FStorageType := AStorageType;
  FCanUseAsDefault := ACanUseAsDefault;
  FIsChangeable := AIsChangeable;
end;

function TTileStorageTypeListItem.GetCanUseAsDefault: Boolean;
begin
  Result := FCanUseAsDefault;
end;

function TTileStorageTypeListItem.GetCaption: string;
begin
  Result := FCaption;
end;

function TTileStorageTypeListItem.GetGUID: TGUID;
begin
  Result := FGUID;
end;

function TTileStorageTypeListItem.GetIntCode: Integer;
begin
  Result := FIntCode;
end;

function TTileStorageTypeListItem.GetIsChangeable: Boolean;
begin
  Result := FIsChangeable;
end;

function TTileStorageTypeListItem.GetStorageType: ITileStorageType;
begin
  Result := FStorageType;
end;

end.
