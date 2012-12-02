unit u_InvisibleBrowserByFormSynchronize;

interface

uses
  SysUtils,
  i_InvisibleBrowser,
  i_LanguageManager,
  i_ProxySettings,
  u_BaseInterfacedObject,
  frm_InvisibleBrowser;

type
  TInvisibleBrowserByFormSynchronize = class(TBaseInterfacedObject, IInvisibleBrowser)
  private
    FCS: IReadWriteSync;
    FProxyConfig: IProxyConfig;
    FLanguageManager: ILanguageManager;
    FfrmInvisibleBrowser: TfrmInvisibleBrowser;
  private
    procedure NavigateAndWait(const AUrl: WideString);
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AProxyConfig: IProxyConfig
    );
    destructor Destroy; override;
  end;

implementation

uses
  Classes,
  u_Synchronizer;

type
  TSyncNavigate = class
  private
    FUrl: WideString;
    FfrmInvisibleBrowser: TfrmInvisibleBrowser;
    procedure SyncNavigate;
  public
    constructor Create(
      const AUrl: WideString;
      AfrmInvisibleBrowser: TfrmInvisibleBrowser
    );
    procedure Navigate;
  end;

{ TSyncNavigate }

constructor TSyncNavigate.Create(
  const AUrl: WideString;
  AfrmInvisibleBrowser: TfrmInvisibleBrowser
);
begin
  inherited Create;
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
  const ALanguageManager: ILanguageManager;
  const AProxyConfig: IProxyConfig
);
begin
  inherited Create;
  FCS := MakeSyncRW_Var(Self, FALSE);
  FProxyConfig := AProxyConfig;
  FLanguageManager := ALanguageManager;
end;

destructor TInvisibleBrowserByFormSynchronize.Destroy;
begin
  if FfrmInvisibleBrowser <> nil then begin
    FreeAndNil(FfrmInvisibleBrowser);
  end;
  FCS := nil;
  inherited;
end;

procedure TInvisibleBrowserByFormSynchronize.NavigateAndWait(const AUrl: WideString);
var
  VSyncNav: TSyncNavigate;
begin
  FCS.BeginWrite;
  try
    if FfrmInvisibleBrowser = nil then begin
      FfrmInvisibleBrowser := TfrmInvisibleBrowser.Create(FLanguageManager, FProxyConfig);
    end;
  finally
    FCS.EndWrite;
  end;

  VSyncNav := TSyncNavigate.Create(AUrl, FfrmInvisibleBrowser);
  try
    VSyncNav.Navigate;
  finally
    VSyncNav.Free;
  end;
end;

end.
