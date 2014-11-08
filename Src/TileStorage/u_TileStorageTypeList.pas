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

unit u_TileStorageTypeList;

interface

uses
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
    function GetItemByCode(const ACode: Integer): ITileStorageTypeListItem;
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

function TTileStorageTypeList.GetItemByCode(
  const ACode: Integer
): ITileStorageTypeListItem;
var
  i: Integer;
  VItem: ITileStorageTypeListItem;
begin
  Result := nil;
  if Assigned(FList) then begin
    for i := 0 to FList.Count - 1 do begin
      VItem := ITileStorageTypeListItem(FList.Items[i]);
      if VItem.IntCode = ACode then begin
        Result := VItem;
        Break;
      end;
    end;
  end;
end;

end.
