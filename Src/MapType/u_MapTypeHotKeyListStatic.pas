{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2021, SAS.Planet development team.                      *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
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
    { IMapTypeHotKeyListStatic }
    function GetMapTypeByHotKey(const AHotKey: TShortCut): IMapType;
  public
    constructor Create(const AMapsSet: IMapTypeSet);
  end;

implementation

uses
  i_MapTypeGUIConfig,
  u_IDInterfaceList;

{ TMapTypeHotKeyListStatic }

constructor TMapTypeHotKeyListStatic.Create(const AMapsSet: IMapTypeSet);
var
  I: Integer;
  VMap: IMapType;
  VConfig: IMapTypeGUIConfig;
  VHotKey: TShortCut;
begin
  inherited Create;

  FList := TIDInterfaceList.Create(False);

  for I := 0 to AMapsSet.Count - 1 do begin
    VMap := AMapsSet.Items[I];
    VConfig := VMap.GUIConfig;
    if VConfig.Enabled then begin
      VHotKey := VConfig.HotKey;
      if VHotKey <> 0 then begin
        FList.Add(VHotKey, VMap);
      end;
    end;
  end;
end;

function TMapTypeHotKeyListStatic.GetMapTypeByHotKey(
  const AHotKey: TShortCut
): IMapType;
begin
  Result := IMapType(FList.GetByID(AHotKey));
end;

end.
