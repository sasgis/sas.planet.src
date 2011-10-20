unit u_InternalBrowserByForm;

interface

uses
  i_ProxySettings,
  i_InternalBrowser,
  i_LanguageManager,
  frm_IntrnalBrowser;

type
  TInternalBrowserByForm = class(TInterfacedObject, IInternalBrowser)
  private
    FLanguageManager: ILanguageManager;
    FProxyConfig: IProxyConfig;
    FfrmInternalBrowser: TfrmIntrnalBrowser;
  protected
    procedure ShowMessage(ACaption, AText: string);
    procedure Navigate(ACaption, AUrl: string);
  public
    constructor Create(
      ALanguageManager: ILanguageManager;
      AProxyConfig: IProxyConfig
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils;

{ TInternalBrowserByForm }

constructor TInternalBrowserByForm.Create(
  ALanguageManager: ILanguageManager;
  AProxyConfig: IProxyConfig
);
begin
  FLanguageManager := ALanguageManager;
  FProxyConfig := AProxyConfig;
end;

destructor TInternalBrowserByForm.Destroy;
begin
  if FfrmInternalBrowser <> nil then begin
    FreeAndNil(FfrmInternalBrowser);
  end;
  inherited;
end;

procedure TInternalBrowserByForm.Navigate(ACaption, AUrl: string);
begin
  if FfrmInternalBrowser = nil then begin
    FfrmInternalBrowser := TfrmIntrnalBrowser.Create(FLanguageManager, FProxyConfig);
  end;
  FfrmInternalBrowser.Navigate(ACaption, AUrl);
end;

procedure TInternalBrowserByForm.ShowMessage(ACaption, AText: string);
begin
  if FfrmInternalBrowser = nil then begin
    FfrmInternalBrowser := TfrmIntrnalBrowser.Create(FLanguageManager, FProxyConfig);
  end;
  FfrmInternalBrowser.showmessage(ACaption, AText);
end;

end.
