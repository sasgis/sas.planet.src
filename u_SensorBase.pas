unit u_SensorBase;

interface

uses
  i_JclNotify,
  i_JclListenerNotifierLinksList,
  i_LanguageManager,
  i_Sensor,
  u_ConfigDataElementBase;

type
  TSensorBase = class(TConfigDataElementBaseEmptySaveLoad, ISensor)
  private
    FGUID: TGUID;
    FCaption: string;
    FDescription: string;
    FMenuItemName: string;
    FCanReset: Boolean;
    FSensorTypeIID: TGUID;

    FDataUpdateNotifier: IJclNotifier;
    FLinksList: IJclListenerNotifierLinksList;
    procedure OnLangChange(Sender: TObject);
  protected
    function GetCaptionTranslated: string; virtual; abstract;
    function GetDescriptionTranslated: string; virtual; abstract;
    function GetMenuItemNameTranslated: string; virtual; abstract;
    procedure NotifyDataUpdate;
    property LinksList: IJclListenerNotifierLinksList read FLinksList;
  protected
    function GetGUID: TGUID;
    function GetCaption: string;
    function GetDescription: string;
    function GetMenuItemName: string;
    function CanReset: Boolean;
    procedure Reset; virtual;
    function GetSensorTypeIID: TGUID;
    function GetDataUpdateNotifier: IJclNotifier;
  public
    constructor Create(
      AGUID: TGUID;
      ACanReset: Boolean;
      ASensorTypeIID: TGUID;
      ALanguageManager: ILanguageManager
    );
  end;

implementation

uses
  u_JclNotify,
  u_JclListenerNotifierLinksList,
  u_NotifyEventListener;

{ TSensorBase }

constructor TSensorBase.Create(
  AGUID: TGUID;
  ACanReset: Boolean;
  ASensorTypeIID: TGUID;
  ALanguageManager: ILanguageManager
);
begin
  inherited Create;
  FGUID := AGUID;
  FCanReset := ACanReset;
  FSensorTypeIID := ASensorTypeIID;

  FDataUpdateNotifier := TJclBaseNotifier.Create;
  FLinksList := TJclListenerNotifierLinksList.Create;
  FLinksList.ActivateLinks;
  LinksList.Add(
    TNotifyEventListener.Create(Self.OnLangChange),
    ALanguageManager.GetChangeNotifier
  );
  OnLangChange(nil);
end;

function TSensorBase.CanReset: Boolean;
begin
  Result := FCanReset;
end;

function TSensorBase.GetCaption: string;
begin
  LockRead;
  try
    Result := FCaption;
  finally
    UnlockRead;
  end;
end;

function TSensorBase.GetDataUpdateNotifier: IJclNotifier;
begin
  Result := FDataUpdateNotifier;
end;

function TSensorBase.GetDescription: string;
begin
  LockRead;
  try
    Result := FDescription;
  finally
    UnlockRead;
  end;
end;

function TSensorBase.GetGUID: TGUID;
begin
  Result := FGUID;
end;

function TSensorBase.GetMenuItemName: string;
begin
  LockRead;
  try
    Result := FMenuItemName;
  finally
    UnlockRead;
  end;
end;

function TSensorBase.GetSensorTypeIID: TGUID;
begin
  Result := FSensorTypeIID;
end;

procedure TSensorBase.NotifyDataUpdate;
begin
  FDataUpdateNotifier.Notify(nil);
end;

procedure TSensorBase.OnLangChange(Sender: TObject);
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

procedure TSensorBase.Reset;
begin
end;

end.
