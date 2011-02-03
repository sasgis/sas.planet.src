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
  TMiniMapTBXITem = class(TTBXItem)
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

constructor TMiniMapTBXITem.Create(AOwner: TComponent;
  AMapActive: IActiveMapSingle);
begin
  inherited Create(AOwner);
  FMapActive := AMapActive;
  Self.OnAdjustFont := Self.AdjustFont;
  FListener := TNotifyEventListener.Create(Self.OnMapChangeState);
  FMapActive.GetChangeNotifier.Add(FListener);
  OnMapChangeState(nil);
end;

destructor TMiniMapTBXITem.Destroy;
begin
  FMapActive.GetChangeNotifier.Remove(FListener);
  FListener := nil;
  inherited;
end;

procedure TMiniMapTBXITem.AdjustFont(Item: TTBCustomItem; Viewer: TTBItemViewer;
  Font: TFont; StateFlags: Integer);
begin
  if Item.Checked then begin
    TTBXItem(Item).FontSettings.Bold := tsTrue;
  end else begin
    TTBXItem(Item).FontSettings.Bold := tsDefault;
  end;
end;

procedure TMiniMapTBXITem.OnMapChangeState(Sender: TObject);
begin
  Self.Checked := FMapActive.GetIsActive;
end;

end.
