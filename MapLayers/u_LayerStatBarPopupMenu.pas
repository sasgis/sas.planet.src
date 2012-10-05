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
  i_StatBarConfig;

type
  TLayerStatBarPopupMenu = class(TObject)
  private
    FParentMap: TImage32;
    FPopup: TTBXPopupMenu;
    FStatBarConfig: IStatBarConfig;
    FOnOptionsClick: TNotifyEvent;
    FLanguageManager: ILanguageManager;
    FListener: IListener;
    procedure BuildPopUpMenu;
    procedure InitItemsState;
    procedure OnMenuItemClick(Sender: TObject);
    procedure OnLangChange;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AParentMap: TImage32;
      const AStatBarConfig: IStatBarConfig;
      const AOnOptionsClick: TNotifyEvent
    );
    destructor Destroy; override;
    procedure PopUp;
  end;

implementation

uses
  u_ListenerByEvent;

resourcestring
  rsShowZoomInfo = 'Show Zoom Info';
  rsShowLonLatInfo = 'Show LonLat Info';
  rsShowMeterPerPixelInfo = 'Show Meter Per Pixel Info';
  rsShowTimeZoneInfo = 'Show Time Zone Info';
  rsShowDownloadInfo = 'Show Download Info';
  rsShowQueueInfo = 'Show Queue Info';
  rsShowTilePathInfo = 'Show Tile Path Info';
  rsHideStatusBar = 'Hide Status Bar';
  rsOptions = 'Options...';

type
  TMenuItemTag = (
    tagZoomInfo = 0,
    tagLonLatInfo,
    tagMetrPerPixInfo,
    tagTimeZoneInfo,
    tagDownloadInfo,
    tagQueueInfo,
    tagTilePathInfo,
    tagHide,
    tagOptions
  );

  TMenuItemList = array [TMenuItemTag] of string;

function GetMenuItemList: TMenuItemList; inline;
begin
  Result[TMenuItemTag(0)] := rsShowZoomInfo;
  Result[TMenuItemTag(1)] := rsShowLonLatInfo;
  Result[TMenuItemTag(2)] := rsShowMeterPerPixelInfo;
  Result[TMenuItemTag(3)] := rsShowTimeZoneInfo;
  Result[TMenuItemTag(4)] := rsShowDownloadInfo;
  Result[TMenuItemTag(5)] := rsShowQueueInfo;
  Result[TMenuItemTag(6)] := rsShowTilePathInfo;
  Result[TMenuItemTag(7)] := rsHideStatusBar;
  Result[TMenuItemTag(8)] := rsOptions;
end;

{ TLayerStatBarPopupMenu }

constructor TLayerStatBarPopupMenu.Create(
  const ALanguageManager: ILanguageManager;
  const AParentMap: TImage32;
  const AStatBarConfig: IStatBarConfig;
  const AOnOptionsClick: TNotifyEvent
);
begin
  inherited Create;
  FLanguageManager := ALanguageManager;
  FParentMap := AParentMap;
  FStatBarConfig := AStatBarConfig;
  FOnOptionsClick := AOnOptionsClick;

  FPopup := TTBXPopupMenu.Create(FParentMap);
  FPopup.Name := 'PopupStatusBar';

  BuildPopUpMenu;

  FListener := TNotifyNoMmgEventListener.Create(Self.OnLangChange);
  FLanguageManager.GetChangeNotifier.Add(FListener);
end;

destructor TLayerStatBarPopupMenu.Destroy;
begin
  FLanguageManager.GetChangeNotifier.Remove(FListener);
  FListener := nil;
  FLanguageManager := nil;
  FStatBarConfig := nil;
  inherited Destroy;
end;

procedure TLayerStatBarPopupMenu.BuildPopUpMenu;
var
  I: TMenuItemTag;
  VMenuItem: TTBXItem;
  VMenuItemList: TMenuItemList;
begin
  VMenuItemList := GetMenuItemList;
  for I := Low(TMenuItemTag) to High(TMenuItemTag) do begin
    VMenuItem := TTBXItem.Create(FPopup);
    if not (I in [tagHide, tagOptions]) then begin
      VMenuItem.AutoCheck := True;
    end;
    VMenuItem.Caption := VMenuItemList[I];
    VMenuItem.Tag := Integer(I);
    VMenuItem.OnClick := OnMenuItemClick;
    FPopup.Items.Add(VMenuItem);
  end;
  InitItemsState;
end;

procedure TLayerStatBarPopupMenu.InitItemsState;
var
  I: Integer;
  VMenuItem: TTBCustomItem;
begin
  for I := 0 to FPopup.Items.Count - 1 do begin
    VMenuItem := FPopup.Items[I];
    case TMenuItemTag(VMenuItem.Tag) of
      tagZoomInfo: VMenuItem.Checked := FStatBarConfig.ViewZoomInfo;
      tagLonLatInfo: VMenuItem.Checked := FStatBarConfig.ViewLonLatInfo;
      tagMetrPerPixInfo: VMenuItem.Checked := FStatBarConfig.ViewMetrPerPixInfo;
      tagTimeZoneInfo: VMenuItem.Checked := FStatBarConfig.ViewTimeZoneTimeInfo;
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
