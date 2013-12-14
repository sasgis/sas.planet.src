unit u_InternalBrowserByForm;

interface

uses
  i_ProxySettings,
  i_InternalBrowser,
  i_LanguageManager,
  i_InternalBrowserLastContent,
  i_WindowPositionConfig,
  i_ContentTypeManager,
  u_BaseInterfacedObject,
  frm_IntrnalBrowser;

type
  TInternalBrowserByForm = class(TBaseInterfacedObject, IInternalBrowser)
  private
    FLanguageManager: ILanguageManager;
    FProxyConfig: IProxyConfig;
    FContentTypeManager: IContentTypeManager;
    FConfig: IWindowPositionConfig;
    FContent: IInternalBrowserLastContent;
    FfrmInternalBrowser: TfrmIntrnalBrowser;
  private
    procedure SafeCreateInternal;
  private
    { IInternalBrowser }
    procedure ShowMessage(const ACaption, AText: string);
    procedure Navigate(const ACaption, AUrl: string);
    procedure NavigatePost(const ACaption, AUrl, AReferer, APostData: string);
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AContent: IInternalBrowserLastContent;
      const AConfig: IWindowPositionConfig;
      const AProxyConfig: IProxyConfig;
      const AContentTypeManager: IContentTypeManager
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  c_InternalBrowser;

{ TInternalBrowserByForm }

constructor TInternalBrowserByForm.Create(
  const ALanguageManager: ILanguageManager;
  const AContent: IInternalBrowserLastContent;
  const AConfig: IWindowPositionConfig;
  const AProxyConfig: IProxyConfig;
  const AContentTypeManager: IContentTypeManager
);
begin
  inherited Create;
  FLanguageManager := ALanguageManager;
  FContent := AContent;
  FConfig := AConfig;
  FProxyConfig := AProxyConfig;
  FContentTypeManager := AContentTypeManager;
end;

destructor TInternalBrowserByForm.Destroy;
begin
  if FfrmInternalBrowser <> nil then begin
    FreeAndNil(FfrmInternalBrowser);
  end;
  inherited;
end;

procedure TInternalBrowserByForm.Navigate(const ACaption, AUrl: string);
begin
  SafeCreateInternal;
  FfrmInternalBrowser.Navigate(ACaption, AUrl);
end;

procedure TInternalBrowserByForm.NavigatePost(const ACaption, AUrl, AReferer, APostData: string);
begin
  SafeCreateInternal;
  FfrmInternalBrowser.NavigatePost(ACaption, AUrl, AReferer, APostData);
end;

procedure TInternalBrowserByForm.SafeCreateInternal;
begin
  if FfrmInternalBrowser = nil then begin
    FfrmInternalBrowser :=
      TfrmIntrnalBrowser.Create(
        FLanguageManager,
        FConfig,
        FProxyConfig,
        FContentTypeManager
      );
  end;
end;

procedure TInternalBrowserByForm.ShowMessage(const ACaption, AText: string);
begin
  SafeCreateInternal;
  FContent.Content := AText;
  FfrmInternalBrowser.Navigate(ACaption, CShowMessageInternalURL);
end;

end.
