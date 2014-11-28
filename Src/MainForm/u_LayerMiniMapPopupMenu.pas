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

unit u_LayerMiniMapPopupMenu;

interface

uses
  Classes,
  GR32_Image,
  TBX,
  TB2Item,
  i_MapTypeIconsList,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  i_PopUp,
  u_BaseInterfacedObject;

type
  TLayerMiniMapPopupMenu = class(TBaseInterfacedObject, IPopUp)
  private
    FParentMap: TImage32;
    FIconsList: IMapTypeIconsList;
    FMapsConfig: IActivMapWithLayers;
    FGUIConfigList: IMapTypeGUIConfigList;

    FPopup: TTBXPopupMenu;
    procedure BuildPopUpMenu;
    procedure BuildMapsListUI(AMapssSubMenu, ALayersSubMenu: TTBCustomItem);
    procedure OnClickMapItem(Sender: TObject);
    procedure OnClickLayerItem(Sender: TObject);
  private
    procedure PopUp;
  public
    constructor Create(
      const AParentMap: TImage32;
      const AMapsConfig: IActivMapWithLayers;
      const AGUIConfigList: IMapTypeGUIConfigList;
      const AIconsList: IMapTypeIconsList
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  c_ZeroGUID,
  i_MapType,
  u_ActiveMapTBXItem,
  u_MapTypeMenuItemsGeneratorBasic,
  u_ResStrings;

{ TLayerMiniMapPopupMenu }

constructor TLayerMiniMapPopupMenu.Create(
  const AParentMap: TImage32;
  const AMapsConfig: IActivMapWithLayers;
  const AGUIConfigList: IMapTypeGUIConfigList;
  const AIconsList: IMapTypeIconsList
);
begin
  inherited Create;
  FParentMap := AParentMap;
  FMapsConfig := AMapsConfig;
  FIconsList := AIconsList;
  FGUIConfigList := AGUIConfigList;

  BuildPopUpMenu;
end;

destructor TLayerMiniMapPopupMenu.Destroy;
begin
  FreeAndNil(FPopup);
  inherited;
end;

procedure TLayerMiniMapPopupMenu.BuildMapsListUI(
  AMapssSubMenu, ALayersSubMenu: TTBCustomItem
);
var
  VGenerator: TMapMenuGeneratorBasic;
begin
  VGenerator := TMapMenuGeneratorBasic.Create(
    FGUIConfigList,
    FMapsConfig.GetMapsSet,
    FMapsConfig.GetActiveMap,
    nil,
    AMapssSubMenu,
    Self.OnClickMapItem,
    FIconsList
  );
  try
    VGenerator.BuildControls;
  finally
    VGenerator.Free;
  end;
  VGenerator := TMapMenuGeneratorBasic.Create(
    FGUIConfigList,
    FMapsConfig.GetLayersSet,
    nil,
    FMapsConfig.GetActiveLayersSet,
    ALayersSubMenu,
    Self.OnClickLayerItem,
    FIconsList
  );
  try
    VGenerator.BuildControls;
  finally
    VGenerator.Free;
  end;
end;

procedure TLayerMiniMapPopupMenu.BuildPopUpMenu;
var
  VSubMenuItem: TTBXSubmenuItem;
  VLayersSubMenu: TTBXSubmenuItem;
  VMenuItemAsMainMap: TTBXCustomItem;
begin
  FPopup := TTBXPopupMenu.Create(nil);
  FPopup.Name := 'PopupMiniMap';
  FPopup.Images := FIconsList.GetImageList;

  VSubMenuItem := TTBXSubmenuItem.Create(FPopup);
  VSubMenuItem.Name := 'MiniMapLayers';
  VSubMenuItem.Caption := SAS_STR_Layers;
  VSubMenuItem.Hint := '';
  VSubMenuItem.SubMenuImages := FPopup.Images;
  FPopup.Items.Add(VSubMenuItem);
  VLayersSubMenu := VSubMenuItem;

  VMenuItemAsMainMap := TActiveMapTBXItem.Create(FPopup, nil, FMapsConfig.GetActiveMap);
  VMenuItemAsMainMap.Name := 'MapAsMainLayer';
  VMenuItemAsMainMap.Caption := SAS_STR_MiniMapAsMainMap;
  VMenuItemAsMainMap.Hint := '';
  VMenuItemAsMainMap.OnClick := Self.OnClickMapItem;
  FPopup.Items.Add(VMenuItemAsMainMap);

  BuildMapsListUI(FPopup.Items, VLayersSubMenu);
end;

procedure TLayerMiniMapPopupMenu.OnClickLayerItem(Sender: TObject);
var
  VSender: TTBCustomItem;
  VMapType: IMapType;
begin
  if Sender is TTBCustomItem then begin
    VSender := TTBCustomItem(Sender);
    VMapType := IMapType(VSender.Tag);
    if VMapType <> nil then begin
      FMapsConfig.InvertLayerSelectionByGUID(VMapType.GUID);
    end;
  end;
end;

procedure TLayerMiniMapPopupMenu.OnClickMapItem(Sender: TObject);
var
  VSender: TComponent;
  VMapType: IMapType;
  VGUID: TGUID;
begin
  if Sender is TComponent then begin
    VGUID := CGUID_Zero;
    VSender := TComponent(Sender);
    VMapType := IMapType(VSender.Tag);
    if VMapType <> nil then begin
      VGUID := VMapType.GUID;
    end;
    FMapsConfig.SelectMainByGUID(VGUID);
  end;
end;

procedure TLayerMiniMapPopupMenu.PopUp;
begin
  FParentMap.PopupMenu := FPopup;
end;

end.
