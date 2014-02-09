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

unit u_TileStorageTypeList;

interface

uses
  ActiveX,
  i_GUIDSet,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_ConfigDataElement,
  i_TileStorageTypeConfig,
  i_TileStorageType,
  i_TileStorageTypeList,
  i_TileStorageTypeListItem,
  i_InterfaceListStatic,
  u_BaseInterfacedObject;

type
  TTileStorageTypeList = class(TBaseInterfacedObject, ITileStorageTypeListStatic)
  private
    FList: IInterfaceListStatic;
  private
    function GetCount: Integer;
    function GetItem(AIndex: Integer): ITileStorageTypeListItem;
  public
    constructor Create(const AList: IInterfaceListStatic);
  end;

implementation

{ TTileStorageTypeList }

constructor TTileStorageTypeList.Create(
  const AList: IInterfaceListStatic
);
begin
  inherited Create;
  FList := AList;
end;

function TTileStorageTypeList.GetCount: Integer;
begin
  if Assigned(FList) then begin
    Result := FList.Count;
  end else begin
    Result := 0;
  end;
end;

function TTileStorageTypeList.GetItem(
  AIndex: Integer
): ITileStorageTypeListItem;
begin
  Result := ITileStorageTypeListItem(FList.Items[AIndex]);
end;

end.
