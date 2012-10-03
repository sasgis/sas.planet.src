unit u_LayerScaleLinePopupMenu;

interface

uses
  Menus,
  GR32_Image,
  TBX,
  TB2Item,
  i_ScaleLineConfig;

type
  TLayerScaleLinePopupMenu = class(TObject)
    private
    FParentMap: TImage32;
    FPopup: TTBXPopupMenu;
    FConfig: IScaleLineConfig;
    procedure BuildPopUpMenu;
    procedure InitItemsState;
    procedure OnMenuItemClick(Sender: TObject);
  public
    constructor Create(
      const AParentMap: TImage32;
      const AConfig: IScaleLineConfig
    );
    destructor Destroy; override;
    procedure PopUp;
  end;

implementation

type
  TMenuItemTag = (
    tagExtended,
    tagNumbersFormat,
    tagNice,
    tagScienceRound,
    tagScience,
    tagHide
  );

const
  cMenuItemList: array [TMenuItemTag] of string = (
    'Show Vertical Scale Legend',
    'Numbers Format',
    'Nice',
    'ScienceRound',
    'Science',
    'Hide Scale Legend'
  );

{ TLayerScaleLinePopupMenu }

constructor TLayerScaleLinePopupMenu.Create(
  const AParentMap: TImage32;
  const AConfig: IScaleLineConfig
);
begin
  inherited Create;
  FParentMap := AParentMap;
  FConfig := AConfig;
  FPopup := TTBXPopupMenu.Create(FParentMap);
  FPopup.Name := 'PopupScaleLine';
  BuildPopUpMenu;
end;

destructor TLayerScaleLinePopupMenu.Destroy;
begin
  inherited Destroy;
end;

procedure TLayerScaleLinePopupMenu.BuildPopUpMenu;
var
  I: TMenuItemTag;
  VMenuItem: TTBXItem;
  VMenuSubItem: TTBXSubmenuItem;
begin
  VMenuItem := TTBXItem.Create(FPopup);
  VMenuItem.AutoCheck := True;
  VMenuItem.Caption := cMenuItemList[tagExtended];
  VMenuItem.Tag := Integer(tagExtended);
  VMenuItem.OnClick := OnMenuItemClick;
  FPopup.Items.Add(VMenuItem);

  VMenuSubItem := TTBXSubmenuItem.Create(FPopup);
  VMenuSubItem.Caption := cMenuItemList[tagNumbersFormat];
  for I := tagNice to tagScience do begin
    VMenuItem := TTBXItem.Create(FPopup);
    VMenuItem.RadioItem := True;
    VMenuItem.AutoCheck := True;
    VMenuItem.GroupIndex := 1;
    VMenuItem.Caption := cMenuItemList[I];
    VMenuItem.Tag := Integer(I);
    VMenuItem.OnClick := OnMenuItemClick;
    VMenuSubItem.Add(VMenuItem);
  end;
  FPopup.Items.Add(VMenuSubItem);

  VMenuItem := TTBXItem.Create(FPopup);
  VMenuItem.Caption := cMenuItemList[tagHide];
  VMenuItem.Tag := Integer(tagHide);
  VMenuItem.OnClick := OnMenuItemClick;
  FPopup.Items.Add(VMenuItem);

  InitItemsState;
end;

procedure TLayerScaleLinePopupMenu.InitItemsState;
var
  I: Integer;
  VMenuItem: TTBCustomItem;
begin
  for I := 0 to FPopup.Items.Count - 1 do begin
    VMenuItem := FPopup.Items[I];
    case TMenuItemTag(VMenuItem.Tag) of
      tagExtended: VMenuItem.Checked := FConfig.Extended;
      tagNice: VMenuItem.Checked := FConfig.NumbersFormat = slnfNice;
      tagScienceRound: VMenuItem.Checked := FConfig.NumbersFormat = slnfScienceRound;
      tagScience: VMenuItem.Checked := FConfig.NumbersFormat = slnfScience;
    end;
  end;
end;

procedure TLayerScaleLinePopupMenu.PopUp;
begin
  FParentMap.PopupMenu := FPopup;
end;

procedure TLayerScaleLinePopupMenu.OnMenuItemClick(Sender: TObject);
var
  VMenuItem: TTBXItem;
begin
  if Sender is TTBXItem then begin
    VMenuItem := Sender as TTBXItem;
    case TMenuItemTag(VMenuItem.Tag) of
      tagExtended: FConfig.Extended := VMenuItem.Checked;
      tagNice: FConfig.NumbersFormat := slnfNice;
      tagScienceRound: FConfig.NumbersFormat := slnfScienceRound;
      tagScience: FConfig.NumbersFormat := slnfScience;
      tagHide: FConfig.Visible := False;
    end;
  end;
end;

end.
