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

unit u_TileStorageTypeListItem;

interface

uses
  i_TileStorageType,
  i_TileStorageTypeListItem;

type
  TTileStorageTypeListItem = class(TInterfacedObject, ITileStorageTypeListItem)
  private
    FGUID: TGUID;
    FStorageType: ITileStorageType;
    FCanUseAsDefault: Boolean;
  protected
    function GetGUID: TGUID;
    function GetStorageType: ITileStorageType;
    function GetCanUseAsDefault: Boolean;
  public
    constructor Create(
      const AGUID: TGUID;
      const AStorageType: ITileStorageType;
      ACanUseAsDefault: Boolean
    );
  end;

implementation

{ TTileStorageTypeListItem }

constructor TTileStorageTypeListItem.Create(
  const AGUID: TGUID;
  const AStorageType: ITileStorageType;
  ACanUseAsDefault: Boolean
);
begin
  FGUID := AGUID;
  FStorageType := AStorageType;
  FCanUseAsDefault := ACanUseAsDefault;
end;

function TTileStorageTypeListItem.GetCanUseAsDefault: Boolean;
begin
  Result := FCanUseAsDefault;
end;

function TTileStorageTypeListItem.GetGUID: TGUID;
begin
  Result := FGUID;
end;

function TTileStorageTypeListItem.GetStorageType: ITileStorageType;
begin
  Result := FStorageType;
end;

end.
