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

unit u_MapTypeHotKeyListStatic;

interface

uses
  Classes,
  i_MapType,
  i_MapTypeSet,
  i_MapTypeHotKeyListStatic,
  i_IDList,
  u_BaseInterfacedObject;

type
  TMapTypeHotKeyListStatic = class(TBaseInterfacedObject, IMapTypeHotKeyListStatic)
  private
    FList: IIDInterfaceList;
  private
    function GetMapTypeGUIDByHotKey(AHotKey: TShortCut): IMapType;
  public
    constructor Create(
      const AMapsSet: IMapTypeSet
    );
  end;

implementation

uses
  u_IDInterfaceList;

{ TMapTypeHotKeyListStatic }

constructor TMapTypeHotKeyListStatic.Create(const AMapsSet: IMapTypeSet);
var
  VMap: IMapType;
  VHotKey: TShortCut;
  i: Integer;
begin
  inherited Create;
  FList := TIDInterfaceList.Create(False);
  for i := 0 to AMapsSet.Count - 1 do begin
    VMap := AMapsSet.Items[i];
    VHotKey := VMap.GUIConfig.HotKey;
    if VHotKey <> 0 then begin
      FList.Add(VHotKey, VMap);
    end;
  end;
end;

function TMapTypeHotKeyListStatic.GetMapTypeGUIDByHotKey(
  AHotKey: TShortCut
): IMapType;
begin
  Result := IMapType(FList.GetByID(AHotKey));
end;

end.
