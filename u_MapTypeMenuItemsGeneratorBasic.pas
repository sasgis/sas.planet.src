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

unit u_MapTypeMenuItemsGeneratorBasic;

interface

uses
  Classes,
  TB2Item,
  TBX,
  i_MapTypeSet,
  i_MapType,
  i_MapTypeSetChangeable,
  i_MapTypeGUIConfigList,
  i_MapTypeIconsList;

type
  TMapMenuGeneratorBasic = class
  private
    FGUIConfigList: IMapTypeGUIConfigList;
    FIconsList: IMapTypeIconsList;
    FRootMenu: TTBCustomItem;
    FMapsSet: IMapTypeSet;
    FActiveMap: IMapTypeChangeable;
    FActiveLayers: IMapTypeSetChangeable;
    FOnClick: TNotifyEvent;
    procedure ClearLists; virtual;
    procedure ProcessSubItemsCreate; virtual;
    procedure ProcessSubItemGUID(const AGUID: TGUID); virtual;
    function CreateSubMenuItem(const AName: string): TTBCustomItem; virtual;
    function GetParentMenuItem(const AName: string): TTBCustomItem; virtual;
    function CreateMenuItem(const AMapType: IMapType): TTBXCustomItem; virtual;
  public
    constructor Create(
      const AGUIConfigList: IMapTypeGUIConfigList;
      const AMapsSet: IMapTypeSet;
      const AActiveMap: IMapTypeChangeable;
      const AActiveLayers: IMapTypeSetChangeable;
      ARootMenu: TTBCustomItem;
      AOnClick: TNotifyEvent;
      const AIconsList: IMapTypeIconsList
    );
    procedure BuildControls;
  end;

implementation

uses
  SysUtils,
  i_GUIDListStatic,
  u_TBXSubmenuItemWithIndicator,
  u_ActiveMapTBXItem;

{ TMapMenuGeneratorBasic }

constructor TMapMenuGeneratorBasic.Create(
  const AGUIConfigList: IMapTypeGUIConfigList;
  const AMapsSet: IMapTypeSet;
  const AActiveMap: IMapTypeChangeable;
  const AActiveLayers: IMapTypeSetChangeable;
  ARootMenu: TTBCustomItem;
  AOnClick: TNotifyEvent;
  const AIconsList: IMapTypeIconsList
);
begin
  Assert(AGUIConfigList <> nil);
  Assert(AMapsSet <> nil);
  Assert(Assigned(AActiveLayers) or Assigned(AActiveMap));
  Assert(AIconsList <> nil);
  inherited Create;
  FGUIConfigList := AGUIConfigList;
  FMapsSet := AMapsSet;
  FActiveMap := AActiveMap;
  FActiveLayers := AActiveLayers;
  FRootMenu := ARootMenu;
  FIconsList := AIconsList;
  FOnClick := AOnClick;
end;

function TMapMenuGeneratorBasic.CreateMenuItem(
  const AMapType: IMapType
): TTBXCustomItem;
var
  VGUID: TGUID;
begin
  if Assigned(FActiveMap) then begin
    Result := TActiveMapTBXItem.Create(FRootMenu, AMapType, FActiveMap);
  end else begin
    Result := TActiveLayerTBXItem.Create(FRootMenu, AMapType, FActiveLayers);
  end;
  VGUID := AMapType.GUID;
  Result.Caption := AMapType.GUIConfig.Name.Value;
  Result.ImageIndex := FIconsList.GetIconIndexByGUID(VGUID);
  Result.Tag := Integer(AMapType);
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
const
  CLineBreak = '\';
var
  i, j: Integer;
  VName: string;
  VParent, Vmenu: TTBCustomItem;
  VNameParts: TStringList;
begin
  if (AName = '') then begin
    Result := FRootMenu;
  end else begin
    if Pos(CLineBreak, AName) <> 0 then begin
      VNameParts := TStringList.Create;
      try
        VNameParts.LineBreak := CLineBreak;
        VNameParts.Text := AName;
        Vmenu := FRootMenu;
        for j := 0 to VNameParts.Count - 1 do begin
          VParent := Vmenu;
          VName := VNameParts.Strings[j];
          if Vname = '' then begin
            Vmenu := VParent; // Fix for multiple LineBreak
          end else begin
            Vmenu := nil;
            for i := 0 to VParent.Count - 1 do begin
              if SameText(VParent.Items[i].Caption, VName) then begin
                Vmenu := VParent.Items[i];
                Break;
              end;
            end;
          end;
          if Vmenu = nil then begin
            Vmenu := CreateSubMenuItem(VName);
            VParent.Add(Vmenu);
          end;
        end;
        Result :=  Vmenu;
      finally
        FreeAndNil(VNameParts);
      end;
    end else begin
      VName := AName;
      Result := nil;
      for i := 0 to FRootMenu.Count - 1 do begin
        if SameText(FRootMenu.Items[i].Caption, VName) then begin
          Result := FRootMenu.Items[i];
          Break;
        end;
      end;
      if Result = nil then begin
        Result := CreateSubMenuItem(VName);
        FRootMenu.Add(Result);
      end;
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
  VMapType: IMapType;
  VSubMenu: TTBCustomItem;
  VMenuItem: TTBXCustomItem;
  VSubMenuName: string;
  VEnabled: Boolean;
begin
  VMapType := FMapsSet.GetMapTypeByGUID(AGUID);
  if VMapType <> nil then begin
    VEnabled := VMapType.GUIConfig.Enabled;
    VSubMenuName := VMapType.GUIConfig.ParentSubMenu.Value;
    if VEnabled then begin
      VSubMenu := GetParentMenuItem(VSubMenuName);
      Assert(Assigned(VSubMenu));
      VMenuItem := CreateMenuItem(VMapType);
      Assert(Assigned(VMenuItem));
      VSubMenu.Add(VMenuItem);
      if VMapType.GUIConfig.Separator then begin
        VSubMenu.Add(TTBSeparatorItem.Create(FRootMenu));
      end;
    end;
  end;
end;

procedure TMapMenuGeneratorBasic.ProcessSubItemsCreate;
var
  i: Integer;
  VStaticList: IGUIDListStatic;
begin
  VStaticList := FGUIConfigList.OrderedMapGUIDList;
  for i := 0 to VStaticList.Count - 1 do begin
    ProcessSubItemGUID(VStaticList.Items[i]);
  end;
end;

end.
