unit u_InternalBrowserByForm;

interface

uses
  i_ProxySettings,
  i_InternalBrowser,
  frm_IntrnalBrowser;

type
  TInternalBrowserByForm = class(TInterfacedObject, IInternalBrowser)
  private
    FProxyConfig: IProxyConfig;
    FfrmInternalBrowser: TfrmIntrnalBrowser;
  protected
    procedure ShowMessage(ACaption, AText: string);
    procedure Navigate(ACaption, AUrl: string);
  public
    constructor Create(AProxyConfig: IProxyConfig);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils;

{ TInternalBrowserByForm }

constructor TInternalBrowserByForm.Create(AProxyConfig: IProxyConfig);
begin
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
    FfrmInternalBrowser := TfrmIntrnalBrowser.Create(nil, FProxyConfig);
  end;
  FfrmInternalBrowser.Navigate(ACaption, AUrl);
end;

procedure TInternalBrowserByForm.ShowMessage(ACaption, AText: string);
begin
  if FfrmInternalBrowser = nil then begin
    FfrmInternalBrowser := TfrmIntrnalBrowser.Create(nil, FProxyConfig);
  end;
  FfrmInternalBrowser.showmessage(ACaption, AText);
end;

end.
