unit u_StringConfigDataElementWithDefByGetText;

interface

uses
  gnugettext,
  i_LanguageManager,
  u_StringConfigDataElementWithDefBase;

type
  TStringConfigDataElementWithDefByGetText = class(TStringConfigDataElementWithDefBase)
  private
    FGetTextMsgId: MsgIdString;
  protected
    function GetDefValueForCurrentLang: string; override;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      AUseSotre: Boolean;
      const AStoreIdentifier: string;
      AIsStoreDefault: Boolean;
      const AGetTextMsgId: MsgIdString
    ); overload;
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AGetTextMsgId: MsgIdString
    ); overload;
  end;


implementation

{ TStringConfigDataElementWithDefByGetText }

constructor TStringConfigDataElementWithDefByGetText.Create(
  const ALanguageManager: ILanguageManager;
  AUseSotre: Boolean;
  const AStoreIdentifier: string;
  AIsStoreDefault: Boolean;
  const AGetTextMsgId: MsgIdString
);
begin
  inherited Create(ALanguageManager, AUseSotre, AStoreIdentifier, AIsStoreDefault);
  FGetTextMsgId := AGetTextMsgId;
end;

constructor TStringConfigDataElementWithDefByGetText.Create(
  const ALanguageManager: ILanguageManager;
  const AGetTextMsgId: MsgIdString
);
begin
  inherited Create(ALanguageManager, False, '', False);
  FGetTextMsgId := AGetTextMsgId;
end;

function TStringConfigDataElementWithDefByGetText.GetDefValueForCurrentLang: string;
begin
  Result := gettext_NoExtract(FGetTextMsgId);
end;

end.
