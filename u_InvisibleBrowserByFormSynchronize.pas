unit u_InvisibleBrowserByFormSynchronize;

interface

uses
  SyncObjs,
  i_InvisibleBrowser,
  i_ProxySettings,
  frm_InvisibleBrowser;

type
  TInvisibleBrowserByFormSynchronize = class(TInterfacedObject, IInvisibleBrowser)
  private
    FCS: TCriticalSection;
    FProxyConfig: IProxyConfig;
    FfrmInvisibleBrowser: TfrmInvisibleBrowser;
  protected
    procedure NavigateAndWait(AUrl: WideString);
  public
    constructor Create(AProxyConfig: IProxyConfig);
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

constructor TInvisibleBrowserByFormSynchronize.Create(AProxyConfig: IProxyConfig);
begin
  FCS := TCriticalSection.Create;
  FProxyConfig := AProxyConfig;
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
      FfrmInvisibleBrowser := TfrmInvisibleBrowser.Create(nil, FProxyConfig);
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
