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

unit u_TileStorageTypeListSimple;

interface

uses
  i_TileStorageTypeConfig,
  i_TileStorageType,
  i_TileStorageTypeListItem,
  u_TileStorageTypeList;

type
  TTileStorageTypeListSimple = class(TTileStorageTypeList)
  public
    constructor Create;
  end;

implementation

uses
  u_TileStorageTypeListItem;

{ TTileStorageTypeListSimple }

constructor TTileStorageTypeListSimple.Create;
var
  VItem: ITileStorageTypeListItem;
begin
//  VItem := TTileStorageTypeListItem.Create(
//    ['{C66C56C5-43AC-490A-B3A1-34898B13BD79}'],
//    
//  );
  inherited Create(VItem);
end;

end.
