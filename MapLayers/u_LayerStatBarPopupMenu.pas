unit u_LayerStatBarPopupMenu;

interface

uses
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
    procedure BuildPopUpMenu;
    procedure InitItemsState;
    procedure OnMenuItemClick(Sender: TObject);
  public
    constructor Create(
      const AParentMap: TImage32;
      const AStatBarConfig: IStatBarConfig
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
    tagHide
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
    'Hide Status Bar'
  );

{ TLayerStatBarPopupMenu }

constructor TLayerStatBarPopupMenu.Create(
  const AParentMap: TImage32;
  const AStatBarConfig: IStatBarConfig
);
begin
  inherited Create;
  FParentMap := AParentMap;
  FStatBarConfig := AStatBarConfig;
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
    if I <> tagHide then begin
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
    end;
  end;
end;

end.
