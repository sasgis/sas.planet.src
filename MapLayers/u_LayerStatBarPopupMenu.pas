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
  i_StatBarConfig;

type
  TLayerStatBarPopupMenu = class(TObject)
  private
    FParentMap: TImage32;
    FPopup: TTBXPopupMenu;
    FStatBarConfig: IStatBarConfig;
    FOnOptionsClick: TNotifyEvent;
    procedure BuildPopUpMenu;
    procedure InitItemsState;
    procedure OnMenuItemClick(Sender: TObject);
  public
    constructor Create(
      const AParentMap: TImage32;
      const AStatBarConfig: IStatBarConfig;
      const AOnOptionsClick: TNotifyEvent
    );
    destructor Destroy; override;
    procedure PopUp;
  end;

implementation

type
  TMenuItemTag = (
    tagZoomInfo,
    tagLonLatInfo,
    tagMetrPerPixInfo,
    tagTimeZoneInfo,
    tagDownloadInfo,
    tagQueueInfo,
    tagTilePathInfo,
    tagHide,
    tagOptions
  );

const
  cMenuItemList: array [TMenuItemTag] of string = (
    'Show Zoom Info',
    'Show LonLat Info',
    'Show Meter Per Pixel Info',
    'Show Time Zone Info',
    'Show Download Info',
    'Show Queue Info',
    'Show Tile Path Info',
    'Hide Status Bar',
    'Options...'
  );

{ TLayerStatBarPopupMenu }

constructor TLayerStatBarPopupMenu.Create(
  const AParentMap: TImage32;
  const AStatBarConfig: IStatBarConfig;
  const AOnOptionsClick: TNotifyEvent
);
begin
  inherited Create;
  FParentMap := AParentMap;
  FStatBarConfig := AStatBarConfig;
  FOnOptionsClick := AOnOptionsClick;
  FPopup := TTBXPopupMenu.Create(FParentMap);
  FPopup.Name := 'PopupStatusBar';
  BuildPopUpMenu;
end;

destructor TLayerStatBarPopupMenu.Destroy;
begin
  inherited Destroy;
end;

procedure TLayerStatBarPopupMenu.BuildPopUpMenu;
var
  I: TMenuItemTag;
  VMenuItem: TTBXItem;
begin
  for I := Low(TMenuItemTag) to High(TMenuItemTag) do begin
    VMenuItem := TTBXItem.Create(FPopup);
    if not (I in [tagHide, tagOptions]) then begin
      VMenuItem.AutoCheck := True;
    end;
    VMenuItem.Caption := cMenuItemList[I];
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

end.
