{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2015, SAS.Planet development team.                      *}
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

unit u_FavoriteMapSetMenu;

interface

uses
  TB2Item,
  TBX,
  i_Listener,
  i_MapTypeSet,
  i_ViewPortState,
  i_ActiveMapsConfig,
  i_InterfaceListStatic,
  i_FavoriteMapSetConfig,
  i_FavoriteMapSetItemStatic;

type
  TFavoriteMapSetMenu = class
  private
    FMapsSet: IMapTypeSet;
    FMainMapConfig: IActiveMapConfig;
    FMapLayersConfig: IActiveLayersConfig;
    FViewPortState: IViewPortState;
    FRootMenu: TTBXCustomItem;
    FFavoriteMapSetConfig: IFavoriteMapSetConfig;
    FFavoriteMapSetChangeListener: IListener;
    procedure ClearMenu;
    procedure OnMenuItemClick(Sender: TObject);
    procedure OnFavoriteMapSetChanged;
    function CreateMenuItem(const AItem: IFavoriteMapSetItemStatic): TTBXCustomItem;
  public
    constructor Create(
      const AFavoriteMapSetConfig: IFavoriteMapSetConfig;
      const AMapsSet: IMapTypeSet;
      const AMainMapConfig: IActiveMapConfig;
      const AMapLayersConfig: IActiveLayersConfig;
      const AViewPortState: IViewPortState;
      ARootMenu: TTBXCustomItem
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  c_ZeroGUID,
  i_GUIDListStatic,
  u_ListenerByEvent;

{ TFavoriteMapSetMenu }

constructor TFavoriteMapSetMenu.Create(
  const AFavoriteMapSetConfig: IFavoriteMapSetConfig;
  const AMapsSet: IMapTypeSet;
  const AMainMapConfig: IActiveMapConfig;
  const AMapLayersConfig: IActiveLayersConfig;
  const AViewPortState: IViewPortState;
  ARootMenu: TTBXCustomItem
);
begin
  inherited Create;

  FFavoriteMapSetConfig := AFavoriteMapSetConfig;
  FMapsSet := AMapsSet;
  FMainMapConfig := AMainMapConfig;
  FMapLayersConfig := AMapLayersConfig;
  FViewPortState := AViewPortState;
  FRootMenu := ARootMenu;

  OnFavoriteMapSetChanged;

  FFavoriteMapSetChangeListener :=
    TNotifyNoMmgEventListener.Create(Self.OnFavoriteMapSetChanged);

  FFavoriteMapSetConfig.ChangeNotifier.Add(FFavoriteMapSetChangeListener);
end;

destructor TFavoriteMapSetMenu.Destroy;
begin
  FFavoriteMapSetConfig.ChangeNotifier.Remove(FFavoriteMapSetChangeListener);
  ClearMenu;
  inherited Destroy;
end;

procedure TFavoriteMapSetMenu.OnFavoriteMapSetChanged;
var
  I: Integer;
  VStatic: IInterfaceListStatic;
  VItem: IFavoriteMapSetItemStatic;
  VMenuItem: TTBXCustomItem;
begin
  ClearMenu;
  VStatic := FFavoriteMapSetConfig.GetStatic;
  if Assigned(VStatic) and (VStatic.Count > 0) then begin
    for I := 0 to VStatic.Count - 1 do begin
      VItem := IFavoriteMapSetItemStatic(VStatic.Items[I]);
      VMenuItem := CreateMenuItem(VItem);
      FRootMenu.Add(VMenuItem);
    end;
  end;
end;

function TFavoriteMapSetMenu.CreateMenuItem(
  const AItem: IFavoriteMapSetItemStatic
): TTBXCustomItem;
begin
  Assert(AItem <> nil);
  AItem._AddRef;
  Result := TTBXItem.Create(FRootMenu);
  Result.Tag := LongInt(AItem);
  Result.Caption := AItem.Name;
  Result.OnClick := Self.OnMenuItemClick;
end;

procedure TFavoriteMapSetMenu.OnMenuItemClick(Sender: TObject);
var
  I: Integer;
  VLayers: IGUIDSetStatic;
  VMenuItem: TTBXCustomItem;
  VItem: IFavoriteMapSetItemStatic;
begin
  VMenuItem := Sender as TTBXCustomItem;
  if Assigned(VMenuItem) and (VMenuItem.Tag > 0) then begin
    VItem := IFavoriteMapSetItemStatic(VMenuItem.Tag);

    if not IsEqualGUID(VItem.BaseMap, CGUID_Zero) then begin
      if FMapsSet.IsExists(VItem.BaseMap) then begin
        FMainMapConfig.MainMapGUID := VItem.BaseMap;
      end else begin
        raise Exception.Create(
          'Can''t switch BaseMap - unknown GUID: ' + GUIDToString(VItem.BaseMap)
        );
      end;
    end;

    VLayers := VItem.Layers;
    if Assigned(VLayers) then begin
      for I := 0 to VLayers.Count - 1 do begin
        if not FMapsSet.IsExists(VLayers.Items[I]) then begin
          raise Exception.Create(
            'Can''t switch Layers - unknown GUID: ' + GUIDToString(VLayers.Items[I])
          );
        end;
      end;
    end;

    if not VItem.MergeLayers then begin
      FMapLayersConfig.LayerGuids := VLayers;
    end else begin
      for I := 0 to VLayers.Count - 1 do begin
        FMapLayersConfig.SelectLayerByGUID(VLayers.Items[I]);
      end;
    end;

    if VItem.Zoom >= 0 then begin
      FViewPortState.ChangeZoomWithFreezeAtCenter(VItem.Zoom);
    end;
  end;
end;

procedure TFavoriteMapSetMenu.ClearMenu;
var
  I: Integer;
  VMenuItem: TTBCustomItem;
  VItem: IFavoriteMapSetItemStatic;
begin
  for I := FRootMenu.Count - 1 downto 0 do begin
    VMenuItem := FRootMenu.Items[I];
    if VMenuItem.Tag > 0 then begin
      VItem := IFavoriteMapSetItemStatic(VMenuItem.Tag);
      VItem._Release;
      VMenuItem.Tag := 0;
      FRootMenu.Remove(VMenuItem);
    end;
  end;
end;

end.
