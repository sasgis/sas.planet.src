unit u_StringConfigDataElementWithDefByGetText;

interface

uses
  i_JclNotify,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_StringConfigDataElement,
  i_LanguageManager,
  u_StringConfigDataElementWithDefBase;

type
  TStringConfigDataElementWithDefByGetText = class(TStringConfigDataElementWithDefBase)
  private
    FGetTextMsgId: WideString;
  protected
    function GetDefValueForCurrentLang: string; override;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      AUseSotre: Boolean;
      const AStoreIdentifier: string;
      AIsStoreDefault: Boolean;
      const AGetTextMsgId: WideString
    );
  end;


implementation

uses
  gnugettext,
  u_NotifyEventListener;

{ TStringConfigDataElementWithDefByGetText }

constructor TStringConfigDataElementWithDefByGetText.Create(
  const ALanguageManager: ILanguageManager;
  AUseSotre: Boolean;
  const AStoreIdentifier: string;
  AIsStoreDefault: Boolean;
  const AGetTextMsgId: WideString
);
begin
  inherited Create(ALanguageManager, AUseSotre, AStoreIdentifier, AIsStoreDefault);
  FGetTextMsgId := AGetTextMsgId;
end;

function TStringConfigDataElementWithDefByGetText.GetDefValueForCurrentLang: string;
begin
  Result := gettext_NoExtract(FGetTextMsgId);
end;

end.
