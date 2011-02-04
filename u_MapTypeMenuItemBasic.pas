unit u_MapTypeMenuItemBasic;

interface

uses
  Graphics,
  Classes,
  TB2Item,
  TBX,
  i_JclNotify,
  i_MapTypes,
  UMapType,
  i_IMapChangeMessage,
  i_IHybrChangeMessage,
  i_IActiveMapsConfig,
  i_IMapTypeMenuItem;

type
  TActiveMapTBXItem = class(TTBXCustomItem)
  private
    FMapActive: IActiveMapSingle;
    FListener: IJclListener;
    procedure OnMapChangeState(Sender: TObject);
    procedure AdjustFont(Item: TTBCustomItem;
      Viewer: TTBItemViewer; Font: TFont; StateFlags: Integer);
  public
    constructor Create(AOwner: TComponent; AMapActive: IActiveMapSingle); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  c_ZeroGUID,
  u_NotifyEventListener;

{ TMiniMapTBXITem }

constructor TActiveMapTBXItem.Create(AOwner: TComponent;
  AMapActive: IActiveMapSingle);
begin
  inherited Create(AOwner);
  FMapActive := AMapActive;
  OnAdjustFont := Self.AdjustFont;
  FListener := TNotifyEventListener.Create(Self.OnMapChangeState);
  FMapActive.GetChangeNotifier.Add(FListener);
  OnMapChangeState(nil);
end;

destructor TActiveMapTBXItem.Destroy;
begin
  FMapActive.GetChangeNotifier.Remove(FListener);
  FListener := nil;
  inherited;
end;

procedure TActiveMapTBXItem.AdjustFont(Item: TTBCustomItem; Viewer: TTBItemViewer;
  Font: TFont; StateFlags: Integer);
begin
  if Self.Checked then begin
    Self.FontSettings.Bold := tsTrue;
  end else begin
    Self.FontSettings.Bold := tsDefault;
  end;
end;

procedure TActiveMapTBXItem.OnMapChangeState(Sender: TObject);
begin
  Self.Checked := FMapActive.GetIsActive;
end;

end.
