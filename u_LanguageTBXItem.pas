unit u_LanguageTBXItem;

interface

uses
  Classes,
  TB2Item,
  TBX,
  i_JclNotify,
  i_LanguageManager;

type
  TLanguageTBXItem = class(TTBXCustomItem)
  private
    FParentMenu: TTBCustomItem;
    FLangIndex: Integer;
    FLanguageManager: ILanguageManager;
    FListener: IJclListener;
    procedure OnLangChange(Sender: TObject);
    procedure OnClickItem(Sender: TObject);
  public
    constructor Create(AOwner: TComponent; AParentMenu: TTBCustomItem; ALanguageManager: ILanguageManager; ALangIndex: Integer); reintroduce;
    destructor Destroy; override;
  end;


implementation

uses
  u_NotifyEventListener;

{ TLanguageTBXItem }

constructor TLanguageTBXItem.Create(
  AOwner: TComponent;
  AParentMenu: TTBCustomItem;
  ALanguageManager: ILanguageManager;
  ALangIndex: Integer
);
begin
  inherited Create(AOwner);
  Assert(ALangIndex < ALanguageManager.GetCount);
  FLanguageManager := ALanguageManager;
  FLangIndex := ALangIndex;
  FParentMenu := AParentMenu;

  Self.OnClick := Self.OnClickItem;
  Self.Caption := FLanguageManager.GetLangNameByIndex(FLangIndex);
  Self.RadioItem := True;

  FListener := TNotifyEventListener.Create(Self.OnLangChange);
  FLanguageManager.GetChangeNotifier.Add(FListener);

  FParentMenu.Add(Self);
  OnLangChange(nil);
end;

destructor TLanguageTBXItem.Destroy;
begin
  FLanguageManager.GetChangeNotifier.Remove(FListener);
  FListener := nil;
  FLanguageManager := nil;
  inherited;
end;

procedure TLanguageTBXItem.OnClickItem(Sender: TObject);
begin
  FLanguageManager.SetCurrentLangIndex(FLangIndex);
end;

procedure TLanguageTBXItem.OnLangChange(Sender: TObject);
begin
  if FLangIndex = FLanguageManager.GetCurrentLangIndex then begin
    Self.Checked := True;
  end else begin
    Self.Checked := False;
  end;
end;

end.
