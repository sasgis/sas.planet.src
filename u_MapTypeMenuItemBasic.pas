unit u_MapTypeMenuItemBasic;

interface

uses
  TB2Item,
  i_JclNotify,
  UMapType,
  i_IMapChangeMessage,
  i_IHybrChangeMessage,
  i_IActiveMapsConfig,
  i_IMapTypeMenuItem;

type
  TMapTypeMenuItemBasic = class(TInterfacedObject, IMapTypeMenuItem)
  private
    FMapsActive: IActiveMapWithHybrConfig;
    FListener: IJclListener;
  private
    FMapType: TMapType;
    FMenuItem: TTBCustomItem;
    function GetMapType: TMapType;
    function GetMenuItem: TTBCustomItem;
    procedure OnNotifyMapChange(msg: IMapChangeMessage); virtual;
    procedure OnNotifyHybrChange(msg: IHybrChangeMessage); virtual;
    procedure OnItemClick(Sender: TObject);
  public
    constructor Create(AMapsActive: IActiveMapWithHybrConfig; AMapType: TMapType; AMenuItem: TTBCustomItem);
    destructor Destroy; override;
  end;

implementation

uses
  u_JclNotify;

type
  TMapTypeMenuItemListener = class(TJclBaseListener)
  private
    FOwnerItem: TMapTypeMenuItemBasic;
  public
    constructor Create(AOwnerItem: TMapTypeMenuItemBasic);
  end;

{ TMapTypeMenuItemListener }

constructor TMapTypeMenuItemListener.Create(
  AOwnerItem: TMapTypeMenuItemBasic);
begin
  FOwnerItem := AOwnerItem;
end;

type
  TMapTypeMenuItemMapChangeListener = class(TMapTypeMenuItemListener)
  public
    procedure Notification(msg: IJclNotificationMessage); override;
  end;

{ TMapTypeMenuItemMapChangeListener }

procedure TMapTypeMenuItemMapChangeListener.Notification(
  msg: IJclNotificationMessage);
begin
  FOwnerItem.OnNotifyMapChange(msg as IMapChangeMessage);
end;

type
  TMapTypeMenuItemHybrChangeListener = class(TMapTypeMenuItemListener)
  public
    procedure Notification(msg: IJclNotificationMessage); override;
  end;

{ TMapTypeMenuItemHybrChangeListener }

procedure TMapTypeMenuItemHybrChangeListener.Notification(
  msg: IJclNotificationMessage);
begin
  FOwnerItem.OnNotifyHybrChange(msg as IHybrChangeMessage);
end;


{ TMapTypeMenuItemBasic }

constructor TMapTypeMenuItemBasic.Create(AMapsActive: IActiveMapWithHybrConfig; AMapType: TMapType; AMenuItem: TTBCustomItem);
begin
  FMapType := AMapType;
  FMenuItem := AMenuItem;
  FMapsActive := AMapsActive;
  FMenuItem.OnClick := OnItemClick;
  if FMapType.IsHybridLayer then begin
    FListener := TMapTypeMenuItemHybrChangeListener.Create(Self);
    FMapsActive.HybrChangeNotifier.Add(FListener);
  end else begin
    FListener := TMapTypeMenuItemMapChangeListener.Create(Self);
    FMapsActive.MapChangeNotifier.Add(FListener);
  end;
end;

destructor TMapTypeMenuItemBasic.Destroy;
begin
  FMenuItem.OnClick := nil;
  FMapsActive.MapChangeNotifier.Remove(FListener);
  FMapsActive.HybrChangeNotifier.Remove(FListener);
  FListener := nil;
  inherited;
end;

function TMapTypeMenuItemBasic.GetMapType: TMapType;
begin
  Result := FMapType;
end;

function TMapTypeMenuItemBasic.GetMenuItem: TTBCustomItem;
begin
  Result := FMenuItem;
end;

procedure TMapTypeMenuItemBasic.OnItemClick(Sender: TObject);
begin
  if FMapType.IsHybridLayer then begin
    if FMapsActive.IsHybrGUIDSelected(FMapType.GUID) then begin
      FMapsActive.UnSelectHybrByGUID(FMapType.GUID);
    end else begin
      FMapsActive.SelectHybrByGUID(FMapType.GUID);
    end;
  end else begin
    if FMapType <> nil then begin
      FMapsActive.SelectMapByGUID(FMapType.GUID);
    end else begin
      FMapsActive.SelectMapByGUID(CGUID_Zero);
    end;
  end;
end;

procedure TMapTypeMenuItemBasic.OnNotifyHybrChange(
  msg: IHybrChangeMessage);
begin
  if msg.GetMap = FMapType then begin
    if msg.GetAction = hcaSelect then begin
      FMenuItem.Checked := True;
    end else begin
      FMenuItem.Checked := False;
    end;
  end;
end;

procedure TMapTypeMenuItemBasic.OnNotifyMapChange(msg: IMapChangeMessage);
begin
  if msg.GetSorurceMap = FMapType then begin
    FMenuItem.Checked := False;
  end;
  if msg.GetNewMap = FMapType then begin
    FMenuItem.Checked := True;
  end;
end;

end.
