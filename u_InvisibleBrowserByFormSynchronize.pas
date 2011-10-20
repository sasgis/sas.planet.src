unit u_InvisibleBrowserByFormSynchronize;

interface

uses
  SyncObjs,
  i_InvisibleBrowser,
  i_LanguageManager,
  i_ProxySettings,
  frm_InvisibleBrowser;

type
  TInvisibleBrowserByFormSynchronize = class(TInterfacedObject, IInvisibleBrowser)
  private
    FCS: TCriticalSection;
    FProxyConfig: IProxyConfig;
    FLanguageManager: ILanguageManager;
    FfrmInvisibleBrowser: TfrmInvisibleBrowser;
  protected
    procedure NavigateAndWait(AUrl: WideString);
  public
    constructor Create(
      ALanguageManager: ILanguageManager;
      AProxyConfig: IProxyConfig
    );
    destructor Destroy; override;
  end;

implementation

uses
  Classes,
  SysUtils;

type
  TSyncNavigate = class
  private
    FUrl: WideString;
    FfrmInvisibleBrowser: TfrmInvisibleBrowser;
    procedure SyncNavigate;
  public
    constructor Create(AUrl: WideString; AfrmInvisibleBrowser: TfrmInvisibleBrowser);
    procedure Navigate;
  end;

{ TSyncNavigate }

constructor TSyncNavigate.Create(AUrl: WideString;
  AfrmInvisibleBrowser: TfrmInvisibleBrowser);
begin
  FUrl := AUrl;
  FfrmInvisibleBrowser := AfrmInvisibleBrowser;
end;

procedure TSyncNavigate.Navigate;
begin
  TThread.Synchronize(nil, SyncNavigate);
end;

procedure TSyncNavigate.SyncNavigate;
begin
  FfrmInvisibleBrowser.NavigateAndWait(FUrl);
end;

{ TInvisibleBrowserByFormSynchronize }

constructor TInvisibleBrowserByFormSynchronize.Create(
  ALanguageManager: ILanguageManager;
  AProxyConfig: IProxyConfig
);
begin
  FCS := TCriticalSection.Create;
  FProxyConfig := AProxyConfig;
  FLanguageManager := ALanguageManager;
end;

destructor TInvisibleBrowserByFormSynchronize.Destroy;
begin
  FreeAndNil(FCS);
  if FfrmInvisibleBrowser <> nil then begin
    FreeAndNil(FfrmInvisibleBrowser);
  end;
  inherited;
end;

procedure TInvisibleBrowserByFormSynchronize.NavigateAndWait(AUrl: WideString);
var
  VSyncNav: TSyncNavigate;
begin
  FCS.Acquire;
  try
    if FfrmInvisibleBrowser = nil then begin
      FfrmInvisibleBrowser := TfrmInvisibleBrowser.Create(FLanguageManager, FProxyConfig);
    end;
  finally
    FCS.Release;
  end;
  VSyncNav :=  TSyncNavigate.Create(AUrl, FfrmInvisibleBrowser);
  try
    VSyncNav.Navigate;
  finally
    VSyncNav.Free;
  end;
end;

end.
