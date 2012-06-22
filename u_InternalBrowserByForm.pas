unit u_InternalBrowserByForm;

interface

uses
  i_ProxySettings,
  i_InternalBrowser,
  i_LanguageManager,
  i_WindowPositionConfig,
  i_ContentTypeManager,
  i_MapAttachmentsInfo,
  frm_IntrnalBrowser;

type
  TInternalBrowserByForm = class(TInterfacedObject, IInternalBrowser)
  private
    FLanguageManager: ILanguageManager;
    FProxyConfig: IProxyConfig;
    FContentTypeManager: IContentTypeManager;
    FConfig: IWindowPositionConfig;
    FfrmInternalBrowser: TfrmIntrnalBrowser;
  private
    procedure SafeCreateInternal;
  protected
    { IInternalBrowser }
    procedure ShowMessage(const ACaption, AText: string);
    procedure Navigate(const ACaption, AUrl: string);
    procedure ShowHTMLDescrWithParser(
      const ACaption, AText: string;
      const AParserProc: TMapAttachmentsInfoParserProc
    );
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AConfig: IWindowPositionConfig;
      const AProxyConfig: IProxyConfig;
      const AContentTypeManager: IContentTypeManager
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils;

{ TInternalBrowserByForm }

constructor TInternalBrowserByForm.Create(
  const ALanguageManager: ILanguageManager;
  const AConfig: IWindowPositionConfig;
  const AProxyConfig: IProxyConfig;
  const AContentTypeManager: IContentTypeManager
);
begin
  inherited Create;
  FLanguageManager := ALanguageManager;
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

procedure TInternalBrowserByForm.ShowHTMLDescrWithParser(
  const ACaption, AText: string;
  const AParserProc: TMapAttachmentsInfoParserProc
);
begin
  SafeCreateInternal;
  FfrmInternalBrowser.ShowHTMLDescrWithParser(ACaption, AText, AParserProc);
end;

procedure TInternalBrowserByForm.ShowMessage(const ACaption, AText: string);
begin
  SafeCreateInternal;
  FfrmInternalBrowser.ShowHTMLDescrWithParser(ACaption, AText, nil);
end;

end.
