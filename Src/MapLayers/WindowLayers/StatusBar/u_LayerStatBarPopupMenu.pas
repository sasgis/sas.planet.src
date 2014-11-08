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

unit u_LayerStatBarPopupMenu;

interface

uses
  Classes,
  GR32_Image,
  TBX,
  TB2Item,
  i_Notifier,
  i_Listener,
  i_LanguageManager,
  i_TerrainProviderList,
  i_TerrainConfig,
  i_StatBarConfig;

type
  TLayerStatBarPopupMenu = class(TObject)
  private
    FParentMap: TImage32;
    FPopup: TTBXPopupMenu;
    FStatBarConfig: IStatBarConfig;
    FTerrainConfig: ITerrainConfig;
    FTerrainProviderList: ITerrainProviderList;
    FOnOptionsClick: TNotifyEvent;
    FLanguageManager: ILanguageManager;
    FListener: IListener;
    procedure BuildPopUpMenu;
    procedure InitItemsState;
    procedure OnMenuItemClick(Sender: TObject);
    procedure OnTerrainItemClick(Sender: TObject);
    procedure OnTerrainCustomizeItemClick(Sender: TObject);
    procedure OnLangChange;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AParentMap: TImage32;
      const AStatBarConfig: IStatBarConfig;
      const ATerrainConfig: ITerrainConfig;
      const ATerrainProviderList: ITerrainProviderList;
      const AOnOptionsClick: TNotifyEvent
    );
    destructor Destroy; override;
    procedure PopUp;
  end;

implementation

uses
  ActiveX,
  SysUtils,
  i_TerrainProviderListElement,
  u_ResStrings,
  u_TimeZoneInfo,
  u_ListenerByEvent;

resourcestring
  rsShowZoomInfo = 'Show Zoom Info';
  rsShowLonLatInfo = 'Show LonLat Info';
  rsShowMeterPerPixelInfo = 'Show Meter Per Pixel Info';
  rsShowElevationInfo = 'Show Elevation Info';
  rsShowTimeZoneInfo = 'Show Time Zone Info';
  rsShowDownloadInfo = 'Show Download Info';
  rsShowQueueInfo = 'Show Queue Info';
  rsShowTilePathInfo = 'Show Tile Path Info';
  rsHideStatusBar = 'Hide Status Bar';
  rsOptions = 'Options...';
  rsAnyAvailableElevationSource = 'Any Available Source';
  rsDisableElevationInfo = 'Disable';

type
  TMenuItemTag = (
    tagZoomInfo = 0,
    tagLonLatInfo,
    tagMetrPerPixInfo,
    tagElevationInfo,
    tagTimeZoneInfo,
    tagDownloadInfo,
    tagQueueInfo,
    tagTilePathInfo,
    tagHide,
    tagOptions
  );

  TMenuItemList = array [TMenuItemTag] of string;

const
  cDisableElevationInfoItemTag = 0;
  cAnyAvailableElevationSourceItemTag = 1;

function GetMenuItemList: TMenuItemList; inline;
begin
  Result[TMenuItemTag(0)] := rsShowZoomInfo;
  Result[TMenuItemTag(1)] := rsShowLonLatInfo;
  Result[TMenuItemTag(2)] := rsShowMeterPerPixelInfo;
  Result[TMenuItemTag(3)] := rsShowElevationInfo;
  Result[TMenuItemTag(4)] := rsShowTimeZoneInfo;
  Result[TMenuItemTag(5)] := rsShowDownloadInfo;
  Result[TMenuItemTag(6)] := rsShowQueueInfo;
  Result[TMenuItemTag(7)] := rsShowTilePathInfo;
  Result[TMenuItemTag(8)] := rsHideStatusBar;
  Result[TMenuItemTag(9)] := rsOptions;
end;

{ TLayerStatBarPopupMenu }

constructor TLayerStatBarPopupMenu.Create(
  const ALanguageManager: ILanguageManager;
  const AParentMap: TImage32;
  const AStatBarConfig: IStatBarConfig;
  const ATerrainConfig: ITerrainConfig;
  const ATerrainProviderList: ITerrainProviderList;
  const AOnOptionsClick: TNotifyEvent
);
begin
  inherited Create;
  FLanguageManager := ALanguageManager;
  FParentMap := AParentMap;
  FStatBarConfig := AStatBarConfig;
  FTerrainConfig := ATerrainConfig;
  FTerrainProviderList := ATerrainProviderList;
  FOnOptionsClick := AOnOptionsClick;

  FPopup := TTBXPopupMenu.Create(FParentMap);
  FPopup.Name := 'PopupStatusBar';

  BuildPopUpMenu;

  FListener := TNotifyNoMmgEventListener.Create(Self.OnLangChange);
  FLanguageManager.GetChangeNotifier.Add(FListener);
end;

destructor TLayerStatBarPopupMenu.Destroy;
begin
  if Assigned(FLanguageManager) and Assigned(FListener) then begin
    FLanguageManager.GetChangeNotifier.Remove(FListener);
    FListener := nil;
    FLanguageManager := nil;
  end;
  FStatBarConfig := nil;
  FTerrainConfig := nil;
  FTerrainProviderList := nil;
  inherited;
end;

procedure TLayerStatBarPopupMenu.BuildPopUpMenu;
var
  I: TMenuItemTag;
  J: Integer;
  VMenuItem: TTBXItem;
  VMenuSubItem: TTBXSubmenuItem;
  VMenuItemList: TMenuItemList;
  VMenuSeparator: TTBSeparatorItem;
  VGUID: TGUID;
  VTmp: Cardinal;
  VEnum: IEnumGUID;
  VItem: ITerrainProviderListElement;
begin
  VMenuItemList := GetMenuItemList;
  for I := Low(TMenuItemTag) to High(TMenuItemTag) do begin
    if I = tagElevationInfo then begin
      VMenuSubItem := TTBXSubmenuItem.Create(FPopup);
      VMenuSubItem.Caption := VMenuItemList[I];
      VMenuSubItem.Tag := Integer(I);
      J := 0;

      VEnum := FTerrainProviderList.GetGUIDEnum;
      while VEnum.Next(1, VGUID, VTmp) = S_OK do begin
        VItem := FTerrainProviderList.Get(VGUID);
        VMenuItem := TTBXItem.Create(FPopup);
        VMenuItem.RadioItem := True;
        VMenuItem.AutoCheck := True;
        VMenuItem.GroupIndex := 1;
        VMenuItem.Caption := VItem.Caption;
        VMenuItem.Tag := Integer(VItem);
        VMenuItem.OnClick := OnTerrainItemClick;
        VMenuSubItem.Add(VMenuItem);
        Inc(J);
      end;

      VMenuSubItem.Enabled := (J > 0);

      if VMenuSubItem.Enabled then begin
        VMenuSeparator := TTBSeparatorItem.Create(FPopup);
        VMenuSubItem.Add(VMenuSeparator);

        VMenuItem := TTBXItem.Create(FPopup);
        VMenuItem.AutoCheck := True;
        VMenuItem.Caption := rsAnyAvailableElevationSource;
        VMenuItem.Tag := cAnyAvailableElevationSourceItemTag;
        VMenuItem.OnClick := OnTerrainCustomizeItemClick;
        VMenuSubItem.Add(VMenuItem);

        VMenuItem := TTBXItem.Create(FPopup);
        VMenuItem.RadioItem := True;
        VMenuItem.AutoCheck := True;
        VMenuItem.GroupIndex := 1;
        VMenuItem.Caption := rsDisableElevationInfo;
        VMenuItem.Tag := cDisableElevationInfoItemTag;
        VMenuItem.OnClick := OnTerrainCustomizeItemClick;
        VMenuSubItem.Add(VMenuItem);

        VMenuSubItem.Checked := VMenuItem.Checked;
      end;

      FPopup.Items.Add(VMenuSubItem);
    end else begin
      VMenuItem := TTBXItem.Create(FPopup);
      if not (I in [tagHide, tagOptions]) then begin
        VMenuItem.AutoCheck := True;
      end;
      VMenuItem.Caption := VMenuItemList[I];
      VMenuItem.Tag := Integer(I);
      VMenuItem.OnClick := OnMenuItemClick;
      FPopup.Items.Add(VMenuItem);
    end;
  end;
  InitItemsState;
end;

procedure TLayerStatBarPopupMenu.InitItemsState;
var
  I, J: Integer;
  VMenuItem: TTBCustomItem;
  VMenuSubItem: TTBCustomItem;
  VItem: ITerrainProviderListElement;
begin
  for I := 0 to FPopup.Items.Count - 1 do begin
    VMenuItem := FPopup.Items[I];
    case TMenuItemTag(VMenuItem.Tag) of
      tagZoomInfo: VMenuItem.Checked := FStatBarConfig.ViewZoomInfo;
      tagLonLatInfo: VMenuItem.Checked := FStatBarConfig.ViewLonLatInfo;
      tagMetrPerPixInfo: VMenuItem.Checked := FStatBarConfig.ViewMetrPerPixInfo;

      tagElevationInfo: begin
        VMenuSubItem := FPopup.Items[I];
        for J := 0 to FPopup.Items[I].Count - 1 do begin
          VMenuItem := VMenuSubItem.Items[J];
          if VMenuItem.Tag = cDisableElevationInfoItemTag then begin
            VMenuItem.Checked :=
              not FTerrainConfig.ShowInStatusBar or
              not FTerrainConfig.ElevationInfoAvailable;
          end else if VMenuItem.Tag = cAnyAvailableElevationSourceItemTag then begin
            VMenuItem.Enabled := FTerrainConfig.ShowInStatusBar;
            VMenuItem.Checked := FTerrainConfig.TrySecondaryElevationProviders;
          end else begin
            VItem := ITerrainProviderListElement(VMenuItem.Tag);
            if (VItem <> nil) then begin
              VMenuItem.Enabled := VItem.Provider.Available;
              VMenuItem.Checked :=
                VMenuItem.Enabled and
                FTerrainConfig.ShowInStatusBar and
                IsEqualGUID(FTerrainConfig.ElevationPrimaryProvider, VItem.GUID);
              if IsEqualGUID(FTerrainConfig.LastActualProviderWithElevationData, VItem.GUID) then begin
                TTBXItem(VMenuItem).FontSettings.Bold := tsTrue;
              end else begin
                TTBXItem(VMenuItem).FontSettings.Bold := tsFalse;
              end;
            end;
          end;
        end;
        VMenuSubItem.Checked :=
          FTerrainConfig.ShowInStatusBar and
          FTerrainConfig.ElevationInfoAvailable;
      end;

      tagTimeZoneInfo: begin
         VMenuItem.Checked :=
          FStatBarConfig.ViewTimeZoneTimeInfo and
          FStatBarConfig.TimeZoneInfoAvailable;
         VMenuItem.Enabled := FStatBarConfig.TimeZoneInfoAvailable;
         if not FStatBarConfig.TimeZoneInfoAvailable then begin
           if not (Pos(cTimeZoneDllName, VMenuItem.Caption) > 0) then begin
             VMenuItem.Caption := VMenuItem.Caption + ' ' +
               Format(SAS_ERR_TimeZoneInfoDisabled, [cTimeZoneDllName]);
           end;
         end;
      end;
      tagDownloadInfo: VMenuItem.Checked := FStatBarConfig.ViewDownloadedInfo;
      tagQueueInfo: VMenuItem.Checked := FStatBarConfig.ViewHttpQueueInfo;
      tagTilePathInfo: VMenuItem.Checked := FStatBarConfig.ViewTilePathInfo;
    end;
  end;
end;

procedure TLayerStatBarPopupMenu.PopUp;
begin
  InitItemsState;
  FParentMap.PopupMenu := FPopup;
end;

procedure TLayerStatBarPopupMenu.OnMenuItemClick(Sender: TObject);
var
  VMenuItem: TTBXItem;
begin
  if Sender is TTBXItem then begin
    VMenuItem := Sender as TTBXItem;
    case TMenuItemTag(VMenuItem.Tag) of
      tagZoomInfo: FStatBarConfig.ViewZoomInfo := VMenuItem.Checked;
      tagLonLatInfo: FStatBarConfig.ViewLonLatInfo := VMenuItem.Checked;
      tagMetrPerPixInfo: FStatBarConfig.ViewMetrPerPixInfo := VMenuItem.Checked;
      tagTimeZoneInfo: FStatBarConfig.ViewTimeZoneTimeInfo := VMenuItem.Checked;
      tagDownloadInfo: FStatBarConfig.ViewDownloadedInfo := VMenuItem.Checked;
      tagQueueInfo: FStatBarConfig.ViewHttpQueueInfo := VMenuItem.Checked;
      tagTilePathInfo: FStatBarConfig.ViewTilePathInfo := VMenuItem.Checked;
      tagHide: FStatBarConfig.Visible := False;
      tagOptions: if Assigned(FOnOptionsClick) then FOnOptionsClick(Self);
    end;
  end;
end;

procedure TLayerStatBarPopupMenu.OnTerrainItemClick(Sender: TObject);
var
  VMenuItem: TTBXItem;
  VItem: ITerrainProviderListElement;
begin
  if Sender is TTBXItem then begin
    VMenuItem := Sender as TTBXItem;
    VItem := ITerrainProviderListElement(VMenuItem.Tag);
    if (VItem <> nil) then begin
      if VMenuItem.Checked then begin
        FTerrainConfig.ElevationPrimaryProvider := VItem.GUID;
        FTerrainConfig.ShowInStatusBar := True;
      end;
    end;
  end;
end;

procedure TLayerStatBarPopupMenu.OnTerrainCustomizeItemClick(Sender: TObject);
var
  VMenuItem: TTBXItem;
begin
  if Sender is TTBXItem then begin
    VMenuItem := Sender as TTBXItem;
    case VMenuItem.Tag of
      cDisableElevationInfoItemTag: begin
        if VMenuItem.Checked then begin
          FTerrainConfig.ShowInStatusBar := False;
        end;
      end;
      cAnyAvailableElevationSourceItemTag: begin
        FTerrainConfig.TrySecondaryElevationProviders := VMenuItem.Checked;
      end;
    end;
  end;
end;

procedure TLayerStatBarPopupMenu.OnLangChange;
var
  I: Integer;
  VMenuItemList: TMenuItemList;
begin
  VMenuItemList := GetMenuItemList;
  for I := 0 to FPopup.Items.Count - 1 do begin
    FPopup.Items[I].Caption := VMenuItemList[TMenuItemTag(I)];
  end;
end;

end.
