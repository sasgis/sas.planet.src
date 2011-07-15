unit u_UserInterfaceItemBase;

interface

uses
  i_JclNotify,
  i_JclListenerNotifierLinksList,
  i_LanguageManager,
  u_ConfigDataElementBase;

type
  TUserInterfaceItemBase = class(TConfigDataElementBaseEmptySaveLoad)
  private
    FGUID: TGUID;
    FCaption: string;
    FDescription: string;
    FMenuItemName: string;

    FLinksList: IJclListenerNotifierLinksList;
    procedure OnLangChange(Sender: TObject);
  protected
    function GetCaptionTranslated: string; virtual; abstract;
    function GetDescriptionTranslated: string; virtual; abstract;
    function GetMenuItemNameTranslated: string; virtual; abstract;

    property LinksList: IJclListenerNotifierLinksList read FLinksList;
  protected
    function GetGUID: TGUID;
    function GetCaption: string;
    function GetDescription: string;
    function GetMenuItemName: string;
  public
    constructor Create(
      AGUID: TGUID;
      ALanguageManager: ILanguageManager
    );
  end;

implementation

uses
  u_JclNotify,
  u_JclListenerNotifierLinksList,
  u_NotifyEventListener;

{ TUserInterfaceItemBase }

constructor TUserInterfaceItemBase.Create(AGUID: TGUID;
  ALanguageManager: ILanguageManager);
begin
  inherited Create;
  FGUID := AGUID;

  FLinksList := TJclListenerNotifierLinksList.Create;
  FLinksList.ActivateLinks;
  LinksList.Add(
    TNotifyEventListener.Create(Self.OnLangChange),
    ALanguageManager.GetChangeNotifier
  );
  OnLangChange(nil);
end;

function TUserInterfaceItemBase.GetCaption: string;
begin
  LockRead;
  try
    Result := FCaption;
  finally
    UnlockRead;
  end;
end;

function TUserInterfaceItemBase.GetDescription: string;
begin
  LockRead;
  try
    Result := FDescription;
  finally
    UnlockRead;
  end;
end;

function TUserInterfaceItemBase.GetGUID: TGUID;
begin
  Result := FGUID;
end;

function TUserInterfaceItemBase.GetMenuItemName: string;
begin
  LockRead;
  try
    Result := FMenuItemName;
  finally
    UnlockRead;
  end;
end;

procedure TUserInterfaceItemBase.OnLangChange(Sender: TObject);
begin
  LockWrite;
  try
    FCaption := GetCaptionTranslated;
    FDescription := GetDescriptionTranslated;
    FMenuItemName := GetMenuItemNameTranslated;
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

end.
