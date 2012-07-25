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

unit u_MapTypeMenuItemsGeneratorBasic;

interface

uses
  Classes,
  TB2Item,
  TBX,
  i_MapTypes,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  i_MapTypeIconsList,
  u_MapType;

type
  TMapMenuGeneratorBasic = class
  private
    FGUIConfigList: IMapTypeGUIConfigList;
    FIconsList: IMapTypeIconsList;
    FRootMenu: TTBCustomItem;
    FMapsSet: IMapTypeSet;
    FSingleSet: IActiveMapSingleSet;
    FOnClick: TNotifyEvent;
    procedure ClearLists; virtual;
    procedure ProcessSubItemsCreate; virtual;
    procedure ProcessSubItemGUID(const AGUID: TGUID); virtual;
    function CreateSubMenuItem(const AName: string): TTBCustomItem; virtual;
    function GetParentMenuItem(const AName: string): TTBCustomItem; virtual;
    function CreateMenuItem(const AMapActive: IActiveMapSingle): TTBXCustomItem; virtual;
  public
    constructor Create(
      const AGUIConfigList: IMapTypeGUIConfigList;
      const AMapsSet: IMapTypeSet;
      const ASingleSet: IActiveMapSingleSet;
      ARootMenu: TTBCustomItem;
      AOnClick: TNotifyEvent;
      const AIconsList: IMapTypeIconsList
    );
    procedure BuildControls;
  end;

implementation

uses
  SysUtils,
  c_ZeroGUID,
  i_GUIDListStatic,
  u_TBXSubmenuItemWithIndicator,
  u_ActiveMapTBXItem,
  u_ResStrings;

{ TMapMenuGeneratorBasic }

constructor TMapMenuGeneratorBasic.Create(
  const AGUIConfigList: IMapTypeGUIConfigList;
  const AMapsSet: IMapTypeSet;
  const ASingleSet: IActiveMapSingleSet;
  ARootMenu: TTBCustomItem;
  AOnClick: TNotifyEvent;
  const AIconsList: IMapTypeIconsList
);
begin
  Assert(AGUIConfigList <> nil);
  Assert(AMapsSet <> nil);
  Assert(ASingleSet <> nil);
  Assert(AIconsList <> nil);
  inherited Create;
  FGUIConfigList := AGUIConfigList;
  FMapsSet := AMapsSet;
  FSingleSet := ASingleSet;
  FRootMenu := ARootMenu;
  FIconsList := AIconsList;
  FOnClick := AOnClick;
end;

function TMapMenuGeneratorBasic.CreateMenuItem(
  const AMapActive: IActiveMapSingle
): TTBXCustomItem;
var
  VGUID: TGUID;
  VMapType: TMapType;
begin
  Result := TActiveMapTBXItem.Create(FRootMenu, AMapActive);
  VMapType := nil;
  if AMapActive.GetMapType <> nil then begin
    VMapType := AMapActive.GetMapType.MapType;
  end;
  if VMapType <> nil then begin
    VGUID := VMapType.Zmp.GUID;
    Result.Caption := VMapType.GUIConfig.Name.Value;
  end else begin
    VGUID := CGUID_Zero;
    Result.Caption := SAS_STR_MiniMapAsMainMap;
  end;
  Result.ImageIndex := FIconsList.GetIconIndexByGUID(VGUID);
  Result.Tag := Integer(AMapActive);
  Result.OnClick := FOnClick;
end;

function TMapMenuGeneratorBasic.CreateSubMenuItem(
  const AName: string
): TTBCustomItem;
begin
  Result := TTBXSubmenuItemWithIndicator.Create(FRootMenu);
  Result.Caption := AName;
  Result.Images := FIconsList.GetImageList;
  Result.tag := -1;
end;

function TMapMenuGeneratorBasic.GetParentMenuItem(
  const AName: string
): TTBCustomItem;
var
  i: Integer;
begin
  if (AName = '') then begin
    Result := FRootMenu;
  end else begin
    Result := nil;
    for i := 0 to FRootMenu.Count - 1 do begin
      if SameText(FRootMenu.Items[i].Caption, AName) then begin
        Result := FRootMenu.Items[i];
      end;
    end;
    if Result = nil then begin
      Result := CreateSubMenuItem(AName);
      FRootMenu.Add(Result);
    end;
  end;
end;

procedure TMapMenuGeneratorBasic.BuildControls;
begin
  ClearLists;
  ProcessSubItemsCreate;
end;

procedure TMapMenuGeneratorBasic.ClearLists;
var
  i: integer;
begin
  for i := FRootMenu.Count - 1 downto 0 do begin
    if FRootMenu.Items[i].Tag <> 0 then begin
      FRootMenu.Items[i].Free;
    end;
  end;
end;

procedure TMapMenuGeneratorBasic.ProcessSubItemGUID(const AGUID: TGUID);
var
  VActiveMap: IActiveMapSingle;
  VSubMenu: TTBCustomItem;
  VMenuItem: TTBXCustomItem;
  VSubMenuName: string;
  VMapType: TMapType;
  VEnabled: Boolean;
begin
  if FMapsSet.GetMapTypeByGUID(AGUID) <> nil then begin
    VActiveMap := FSingleSet.GetMapSingle(AGUID);
    if VActiveMap <> nil then begin
      VSubMenuName := '';
      VEnabled := True;
      if VActiveMap.GetMapType <> nil then begin
        if VActiveMap.GetMapType.MapType <> nil then begin
          VEnabled := VActiveMap.GetMapType.MapType.GUIConfig.Enabled;
          VSubMenuName := VActiveMap.GetMapType.MapType.GUIConfig.ParentSubMenu.Value;
        end;
      end;
      if VEnabled then begin
        VSubMenu := GetParentMenuItem(VSubMenuName);
        Assert(VSubMenu <> nil);
        VMenuItem := CreateMenuItem(VActiveMap);
        VSubMenu.Add(VMenuItem);
        VMapType := nil;
        if VActiveMap.GetMapType <> nil then begin
          VMapType := VActiveMap.GetMapType.MapType;
        end;
        if (VMapType <> nil) and (VActiveMap.GetMapType.MapType.GUIConfig.Separator) then begin
          VSubMenu.Add(TTBSeparatorItem.Create(FRootMenu));
        end;
      end;
    end;
  end;
end;

procedure TMapMenuGeneratorBasic.ProcessSubItemsCreate;
var
  i: Integer;
  VStaticList: IGUIDListStatic;
begin
  ProcessSubItemGUID(CGUID_Zero);
  VStaticList := FGUIConfigList.OrderedMapGUIDList;
  for i := 0 to VStaticList.Count - 1 do begin
    ProcessSubItemGUID(VStaticList.Items[i]);
  end;
end;

end.
