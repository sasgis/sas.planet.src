{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_InternalBrowserImplByEdge;

interface

uses
  ActiveX,
  Classes,
  Controls,
  SysUtils,
  Forms,
  Types,
  Windows,
  uWVLoader,
  uWVBrowser,
  uWVTypes,
  uWVTypeLibrary,
  uWVWindowParent,
  i_ProxySettings,
  i_DownloadRequest,
  i_InternalDomainUrlHandler,
  i_InternalDomainInfoProvider,
  u_InternalBrowserImpl;

type
  TEdgeBrowserEnvironmentLoaderGlobal = class;

  TInternalBrowserImplByEdge = class(TInternalBrowserImpl)
  private type
    TBrowserState = (bsNone, bsStarting, bsReady, bsError);
  private
    FOnKeyDown: TOnKeyDown;
    FOnTitleChange: TOnTitleChange;
    FIsInvisible: Boolean;
    FEnvironmentLoader: TEdgeBrowserEnvironmentLoaderGlobal;
    FInternalDomainUrlHandler: IInternalDomainUrlHandler;
    FDomainList: IInternalDomainInfoProviderList;
    FProxyConfig: IProxyConfigStatic;

    FParent: TWVWindowParent;
    FBrowser: TWVBrowser;

    FKeyPressedHandler: ICoreWebView2AcceleratorKeyPressedEventHandler;
    FKeyPressedEventToken: EventRegistrationToken;

    FNavigationSuccess: Boolean;
    FNavigationCompleted: Boolean;

    FBrowserState: TBrowserState;
    function DoInitialize: Boolean;

    procedure SetKeyHandler;
    procedure RemoveKeyHandler;

    procedure OnCreateWebViewCompleted(Sender: TObject);
    procedure OnCreateWebViewFailed(Sender: TObject; aErrorCode: HRESULT; const aErrorMessage: wvstring);

    procedure OnNavigationStarting(Sender: TObject; const aWebView: ICoreWebView2; const aArgs: ICoreWebView2NavigationStartingEventArgs);
    procedure OnNavigationCompleted(Sender: TObject; const aWebView: ICoreWebView2; const aArgs: ICoreWebView2NavigationCompletedEventArgs);

    procedure OnBasicAuthenticationRequested(Sender: TObject; const aWebView: ICoreWebView2; const aArgs: ICoreWebView2BasicAuthenticationRequestedEventArgs);
    procedure OnWebResourceRequested(Sender: TObject; const aWebView: ICoreWebView2; const aArgs: ICoreWebView2WebResourceRequestedEventArgs);
    procedure OnDocumentTitleChanged(Sender: TObject);
 public
    { TInternalBrowserImpl }
    function Initialize: Boolean; override;
    procedure AssignEmptyDocument; override;
    procedure Navigate(const AUrl: string); overload; override;
    procedure Navigate(const ARequest: IDownloadRequest); overload; override;
    function NavigateWait(const AUrl: string; const ATimeOut: Cardinal): Boolean; override;
    procedure SetHtmlText(const AText: string); override;
    procedure Stop; override;
    procedure SetVisible(const AIsVisible: Boolean); override;
    function GetVisible: Boolean; override;
    function GetCurrentAddress: string; override;
  public
    constructor Create(
      const AParent: TWinControl;
      const AIsInvisible: Boolean;
      const AEnvironmentLoader: TEdgeBrowserEnvironmentLoaderGlobal;
      const AInternalDomainUrlHandler: IInternalDomainUrlHandler;
      const ADomainList: IInternalDomainInfoProviderList;
      const AOnKeyDown: TOnKeyDown = nil;
      const AOnTitleChange: TOnTitleChange = nil
    );
    destructor Destroy; override;
  end;

  TEdgeBrowserEnvironmentLoaderGlobal = class
  strict private
    FBlackList: TStringDynArray;
    FProxyConfig: IProxyConfigStatic;
    procedure SetupProxySettings;
  public
    function TryStartWebView2: Boolean;
    property BlackList: TStringDynArray read FBlackList;
    property ProxyConfig: IProxyConfigStatic read FProxyConfig;
  public
    constructor Create(
      const AProxyConfig: IProxyConfigStatic = nil;
      const ARuntimePath: string = '';
      const AUserDataPath: string = '';
      const ABlackList: TStringDynArray = nil
    );
    destructor Destroy; override;
  end;

implementation

uses
  StrUtils,
  c_InternalBrowser,
  i_BinaryData,
  u_Dialogs,
  u_HtmlDoc,
  u_InternalDomainInfoProviderFunc;

type
  TEdgeBrowserKeyPressedHandler = class(TInterfacedObject, ICoreWebView2AcceleratorKeyPressedEventHandler)
  private
    FOnKeyDown: TOnKeyDown;
  private
    { ICoreWebView2AcceleratorKeyPressedEventHandler }
    function Invoke(
      const sender: ICoreWebView2Controller;
      const args: ICoreWebView2AcceleratorKeyPressedEventArgs
    ): HResult; stdcall;
  public
    constructor Create(const AOnKeyDown: TOnKeyDown);
  end;

procedure DoProcessMessages(const ASleepTimeout: Cardinal); inline;
begin
  Application.ProcessMessages;
  Sleep(ASleepTimeout);
end;

{ TInternalBrowserImplByEdge }

constructor TInternalBrowserImplByEdge.Create(
  const AParent: TWinControl;
  const AIsInvisible: Boolean;
  const AEnvironmentLoader: TEdgeBrowserEnvironmentLoaderGlobal;
  const AInternalDomainUrlHandler: IInternalDomainUrlHandler;
  const ADomainList: IInternalDomainInfoProviderList;
  const AOnKeyDown: TOnKeyDown;
  const AOnTitleChange: TOnTitleChange
);
begin
  Assert(AEnvironmentLoader <> nil);

  inherited Create;

  FIsInvisible := AIsInvisible;
  FEnvironmentLoader := AEnvironmentLoader;
  FInternalDomainUrlHandler := AInternalDomainUrlHandler;
  FDomainList := ADomainList;
  FOnKeyDown := AOnKeyDown;
  FOnTitleChange := AOnTitleChange;

  FBrowser := TWVBrowser.Create(nil);

  FBrowser.OnAfterCreated := Self.OnCreateWebViewCompleted;
  FBrowser.OnInitializationError := Self.OnCreateWebViewFailed;

  if Assigned(FInternalDomainUrlHandler) then begin
    FBrowser.OnNavigationStarting := Self.OnNavigationStarting;
  end;

  if Assigned(FOnTitleChange) then begin
    FBrowser.OnDocumentTitleChanged := Self.OnDocumentTitleChanged;
  end;

  FProxyConfig := FEnvironmentLoader.ProxyConfig;
  if Assigned(FProxyConfig) and FProxyConfig.UseProxy and FProxyConfig.UseLogin then begin
    FBrowser.OnBasicAuthenticationRequested := Self.OnBasicAuthenticationRequested;
  end;

  FParent := TWVWindowParent.Create(nil);
  FParent.Parent := AParent;
  FParent.Left := 0;
  FParent.Top := 0;
  FParent.Align := alClient;
  FParent.Visible := not FIsInvisible;
  FParent.Browser := FBrowser;

  FKeyPressedEventToken.value := 0;
end;

destructor TInternalBrowserImplByEdge.Destroy;
begin
  RemoveKeyHandler;
  FreeAndNil(FParent);
  FreeAndNil(FBrowser);
  FEnvironmentLoader := nil;
  inherited;
end;

function TInternalBrowserImplByEdge.Initialize: Boolean;
begin
  Result := DoInitialize;
  if Result then begin
    FBrowser.Navigate(CEmptyDocument);
  end;
end;

function TInternalBrowserImplByEdge.DoInitialize: Boolean;
begin
  if FBrowserState = bsReady then begin
    Exit(True);
  end;

  if FBrowserState = bsError then begin
    Exit(False);
  end;

  if not FEnvironmentLoader.TryStartWebView2 then begin
    FBrowserState := bsError;
    ShowErrorMessage(Self.ClassName + ': StartWebView2 failed!');
    Exit(False);
  end;

  FBrowserState := bsStarting;

  if not FBrowser.CreateBrowser(FParent.Handle, True) then begin
    FBrowserState := bsError;
    ShowErrorMessage(Self.ClassName + ': CreateBrowser failed!');
    Exit(False);
  end;

  while FBrowserState = bsStarting do begin
    DoProcessMessages(10);
  end;

  Exit(FBrowserState = bsReady);
end;

procedure TInternalBrowserImplByEdge.OnCreateWebViewCompleted(Sender: TObject);
var
  VMatch: string;
begin
  FBrowserState := bsReady;

  if Assigned(FOnKeyDown) then begin
    SetKeyHandler;
  end;

  FBrowser.AddWebResourceRequestedFilterWithRequestSourceKinds(
    CSASInternalURLPrefix + '*',
    COREWEBVIEW2_WEB_RESOURCE_CONTEXT_ALL,
    COREWEBVIEW2_WEB_RESOURCE_REQUEST_SOURCE_KINDS_ALL
  );

  for VMatch in FEnvironmentLoader.BlackList do begin
    if Trim(VMatch) = '' then Continue;
    FBrowser.AddWebResourceRequestedFilterWithRequestSourceKinds(
      VMatch,
      COREWEBVIEW2_WEB_RESOURCE_CONTEXT_ALL,
      COREWEBVIEW2_WEB_RESOURCE_REQUEST_SOURCE_KINDS_ALL
    );
  end;

  FBrowser.OnWebResourceRequested := Self.OnWebResourceRequested;

  // disable SmartScreen
  FBrowser.CoreWebView2Settings.IsReputationCheckingRequired := False;

  if FIsInvisible then begin
    FBrowser.DevToolsEnabled := False;
    FBrowser.StatusBarEnabled := False;
    FBrowser.DefaultContextMenusEnabled := False;
    FBrowser.BuiltInErrorPageEnabled := False;
  end;
end;

procedure TInternalBrowserImplByEdge.OnCreateWebViewFailed(
  Sender: TObject;
  aErrorCode: HRESULT;
  const aErrorMessage: wvstring
);
begin
  FBrowserState := bsError;
  ShowErrorMessage(aErrorMessage);
end;

procedure TInternalBrowserImplByEdge.SetKeyHandler;
var
  VController: ICoreWebView2Controller;
begin
  RemoveKeyHandler;
  VController := FBrowser.CoreWebView2Controller.BaseIntf;
  if Assigned(VController) then begin
    Assert(FKeyPressedEventToken.value = 0);
    FKeyPressedHandler := TEdgeBrowserKeyPressedHandler.Create(FOnKeyDown);
    VController.add_AcceleratorKeyPressed(FKeyPressedHandler, FKeyPressedEventToken);
  end;
end;

procedure TInternalBrowserImplByEdge.RemoveKeyHandler;
var
  VController: ICoreWebView2Controller;
begin
  if Assigned(FBrowser) and (FKeyPressedEventToken.value <> 0) then begin
    VController := FBrowser.CoreWebView2Controller.BaseIntf;
    if Assigned(VController) then begin
      VController.remove_AcceleratorKeyPressed(FKeyPressedEventToken);
      FKeyPressedEventToken.value := 0;
    end;
  end;
end;

procedure TInternalBrowserImplByEdge.OnNavigationStarting(
  Sender: TObject;
  const aWebView: ICoreWebView2;
  const aArgs: ICoreWebView2NavigationStartingEventArgs
);
var
  VUrl: string;
  VUrlPtr: PWideChar;
begin
  if not Succeeded(aArgs.Get_uri(VUrlPtr)) then begin
    Exit;
  end;

  VUrl := string(VUrlPtr);
  CoTaskMemFree(VUrlPtr);

  if (VUrl = '') or SameText(VUrl, CEmptyDocument) then begin
    Exit;
  end;

  if Assigned(FInternalDomainUrlHandler) then
  try
    if FInternalDomainUrlHandler.Process(VUrl) then begin
      aArgs.Set_Cancel(1);
    end;
  except
    on E: Exception do begin
      aArgs.Set_Cancel(1);
      if not FIsInvisible then begin
        ShowErrorMessage(E.ClassName + ': ' + E.Message);
      end;
    end;
  end;
end;

procedure TInternalBrowserImplByEdge.OnNavigationCompleted(
  Sender: TObject;
  const aWebView: ICoreWebView2;
  const aArgs: ICoreWebView2NavigationCompletedEventArgs
);
var
  VIsSuccess: Integer;
begin
  FNavigationCompleted := True;
  FNavigationSuccess := Succeeded(aArgs.Get_IsSuccess(VIsSuccess)) and LongBool(VIsSuccess);
end;

procedure TInternalBrowserImplByEdge.OnWebResourceRequested(
  Sender: TObject;
  const aWebView: ICoreWebView2;
  const aArgs: ICoreWebView2WebResourceRequestedEventArgs
);

  procedure _SetResponse(const AStatusCode: Integer; const AReasonPhrase: string);
  var
    VResp: ICoreWebView2WebResourceResponse;
  begin
    if FBrowser.CoreWebView2Environment.CreateWebResourceResponse(nil, AStatusCode, AReasonPhrase, '', VResp) then begin
      aArgs.Set_Response(VResp);
    end;
  end;

var
  VReq: ICoreWebView2WebResourceRequest;
  VResp: ICoreWebView2WebResourceResponse;
  VUrl: string;
  VUrlPtr: PWideChar;
  VDomainName, VFilePath: string;
  VDomainProvider: IInternalDomainInfoProvider;
  VData: IBinaryData;
  VContentType: AnsiString;
  VHeaders: string;
  VRespBody: IStream;
  VMemStream: TMemoryStream;
begin
  if not Succeeded(aArgs.Get_Request(VReq)) then begin
    Exit;
  end;

  if not Succeeded(VReq.Get_uri(VUrlPtr)) then begin
    Exit;
  end;

  VUrl := string(VUrlPtr);
  CoTaskMemFree(VUrlPtr);

  if not StartsText(CSASInternalURLPrefix, VUrl) then begin
    _SetResponse(403, 'Blocked');
    Exit;
  end;

  if not TInternalDomainInfoProviderFunc.ParseUrl(VUrl, VDomainName, VFilePath) then begin
    _SetResponse(404, 'Not Found');
    Exit;
  end;

  if Assigned(FDomainList) then begin
    VDomainProvider := FDomainList.GetByName(VDomainName);
  end else begin
    VDomainProvider := nil;
  end;

  if VDomainProvider = nil then begin
    _SetResponse(404, 'Not Found');
    Exit;
  end;

  VData := VDomainProvider.LoadBinaryByFilePath(VFilePath, VContentType);
  if VData = nil then begin
    _SetResponse(404, 'Not Found');
    Exit;
  end;

  VHeaders :=
    'Content-Type: ' + string(VContentType) + #13#10 +
    'Access-Control-Allow-Origin: *';

  VMemStream := TMemoryStream.Create;
  try
    if (VData.Buffer <> nil) and (VData.Size > 0) then begin
      VMemStream.WriteBuffer(VData.Buffer^, VData.Size);
      VMemStream.Position := 0;
    end;
    VRespBody := TStreamAdapter.Create(VMemStream, soOwned);
    VMemStream := nil;
  finally
    VMemStream.Free;
  end;

  if FBrowser.CoreWebView2Environment.CreateWebResourceResponse(VRespBody, 200, 'OK', VHeaders, VResp) then begin
    aArgs.Set_Response(VResp);
  end;
end;

procedure TInternalBrowserImplByEdge.OnBasicAuthenticationRequested(
  Sender: TObject;
  const aWebView: ICoreWebView2;
  const aArgs: ICoreWebView2BasicAuthenticationRequestedEventArgs
);
var
  VUri: string;
  VUriPtr: PWideChar;
  VResponse: ICoreWebView2BasicAuthenticationResponse;
begin
  if not Assigned(FProxyConfig) or
     not FProxyConfig.UseProxy or
     not FProxyConfig.UseLogin or
     (FProxyConfig.Login = '')
  then begin
    Exit;
  end;

  if not Succeeded(aArgs.Get_Uri(VUriPtr)) then begin
    Exit;
  end;

  VUri := string(VUriPtr);
  CoTaskMemFree(VUriPtr);

  if Pos(string(FProxyConfig.Host), VUri) = 0 then begin
    Exit;
  end;

  if Succeeded(aArgs.Get_Response(VResponse)) and Assigned(VResponse) then begin
    VResponse.Set_UserName(PWideChar(FProxyConfig.Login));
    VResponse.Set_Password(PWideChar(FProxyConfig.Password));
  end;
end;

procedure TInternalBrowserImplByEdge.OnDocumentTitleChanged(Sender: TObject);
begin
  if Assigned(FOnTitleChange) then begin
    FOnTitleChange(Sender, FBrowser.DocumentTitle);
  end;
end;

procedure TInternalBrowserImplByEdge.Navigate(const AUrl: string);
begin
  if DoInitialize then begin
    FBrowser.Navigate(AUrl);
  end;
end;

procedure TInternalBrowserImplByEdge.Navigate(const ARequest: IDownloadRequest);
var
  VRequest: ICoreWebView2WebResourceRequest;
  VPostRequest: IDownloadPostRequest;
  VData: IBinaryData;
  VPostData: IStream;
  VUrl: string;
  VHeaders: string;
  VMethod: string;
  VResult: Boolean;
  VMemStream: TMemoryStream;
begin
  if not DoInitialize then begin
    Exit;
  end;

  if Supports(ARequest, IDownloadPostRequest, VPostRequest) then begin
    VMethod := 'POST';
    VMemStream := TMemoryStream.Create;
    try
      VData := VPostRequest.PostData;
      if (VData <> nil) and (VData.Buffer <> nil) and (VData.Size > 0) then begin
        VMemStream.WriteBuffer(VData.Buffer^, VData.Size);
      end;
      VPostData := TStreamAdapter.Create(VMemStream, soOwned);
      VMemStream := nil;
    finally
      VMemStream.Free;
    end;
  end else begin
    VMethod := 'GET';
    VPostData := nil;
  end;

  VUrl := string(ARequest.Url);
  VHeaders := string(ARequest.RequestHeader);

  VResult := FBrowser.CoreWebView2Environment.CreateWebResourceRequest(
    VUrl, VMethod, VPostData, VHeaders, VRequest
  );

  if VResult then begin
    FBrowser.NavigateWithWebResourceRequest(VRequest);
  end;
end;

function TInternalBrowserImplByEdge.NavigateWait(
  const AUrl: string;
  const ATimeOut: Cardinal
): Boolean;
begin
  if not DoInitialize then begin
    Exit(False);
  end;

  FNavigationSuccess := False;
  FNavigationCompleted := False;

  FBrowser.OnNavigationCompleted := Self.OnNavigationCompleted;
  try
    FBrowser.Navigate(AUrl);

    var VStartTime := GetTickCount;
    while not FNavigationCompleted and (GetTickCount - VStartTime < ATimeOut) do begin
      DoProcessMessages(10);
    end;
  finally
    FBrowser.OnNavigationCompleted := nil;
  end;

  if FNavigationCompleted then begin
    Result := FNavigationSuccess;
  end else begin
    Result := False;
    FBrowser.Stop;
  end;
end;

procedure TInternalBrowserImplByEdge.AssignEmptyDocument;
begin
  if DoInitialize then begin
    FBrowser.Navigate(CEmptyDocument);
    FBrowser.ClearBrowsingData(COREWEBVIEW2_BROWSING_DATA_KINDS_BROWSING_HISTORY);
  end;
end;

procedure TInternalBrowserImplByEdge.SetHtmlText(const AText: string);
begin
  if DoInitialize then begin
    FBrowser.NavigateToString(THtmlDoc.FormattedTextToHtml(AText));
  end;
end;

procedure TInternalBrowserImplByEdge.SetVisible(const AIsVisible: Boolean);
begin
  FBrowser.IsVisible := AIsVisible;
  if AIsVisible then begin
    FParent.UpdateSize;
    FParent.SetFocus;
  end;
end;

function TInternalBrowserImplByEdge.GetVisible: Boolean;
begin
  Result := FBrowser.IsVisible;
end;

function TInternalBrowserImplByEdge.GetCurrentAddress: string;
begin
  Result := FBrowser.Source;
end;

procedure TInternalBrowserImplByEdge.Stop;
begin
  FBrowser.Stop;
end;

{ TEdgeBrowserKeyPressedHandler }

constructor TEdgeBrowserKeyPressedHandler.Create(const AOnKeyDown: TOnKeyDown);
begin
  Assert(Assigned(AOnKeyDown));
  inherited Create;
  FOnKeyDown := AOnKeyDown;
end;

function TEdgeBrowserKeyPressedHandler.Invoke(
  const sender: ICoreWebView2Controller;
  const args: ICoreWebView2AcceleratorKeyPressedEventArgs
): HResult;
var
  VKey: SYSUINT;
  VHandled: Boolean;
  VEventKind: COREWEBVIEW2_KEY_EVENT_KIND;
begin
  Result := S_OK;
  if Succeeded(args.Get_KeyEventKind(VEventKind)) and (VEventKind = COREWEBVIEW2_KEY_EVENT_KIND_KEY_DOWN) then begin
    if Succeeded(args.Get_VirtualKey(VKey)) then begin
      VHandled := False;
      FOnKeyDown(nil, VKey, VHandled);
      args.Set_Handled(Integer(LongBool(VHandled)));
    end;
  end;
end;

{ TEdgeBrowserEnvironmentLoaderGlobal }

procedure _OnGetCustomSchemes(Sender: TObject; var aCustomSchemes: TWVCustomSchemeInfoArray);
begin
  SetLength(aCustomSchemes, 1);

  with aCustomSchemes[0] do begin
    SchemeName := CSASProtocolName;
    TreatAsSecure := False;
    AllowedDomains := '';
    HasAuthorityComponent := False;
  end;
end;

constructor TEdgeBrowserEnvironmentLoaderGlobal.Create(
  const AProxyConfig: IProxyConfigStatic;
  const ARuntimePath: string;
  const AUserDataPath: string;
  const ABlackList: TStringDynArray
);
begin
  inherited Create;

  if Assigned(GlobalWebView2Loader) then begin
    raise Exception.Create('GlobalWebView2Loader already assigned!');
  end;

  FProxyConfig := AProxyConfig;
  FBlackList := ABlackList;

  GlobalWebView2Loader := TWVLoader.Create(nil);

  if Assigned(FProxyConfig) then begin
    SetupProxySettings;
  end;

  if ARuntimePath <> '' then begin
    GlobalWebView2Loader.BrowserExecPath := ARuntimePath;
  end;

  if AUserDataPath <> '' then begin
    GlobalWebView2Loader.UserDataFolder := AUserDataPath;
  end;

  GlobalWebView2Loader.AdditionalBrowserArguments :=
    '--disable-background-networking ' +
    '--disable-telemetry ' +
    '--disable-component-update ' +
    '--metrics-recording-only ' +
    '--disable-crash-reporter ' +
    '--disable-breakpad ' +
    '--no-pings ' +
    '--disable-domain-reliability ' +
    '--no-first-run ' +
    '--disable-default-apps ' +
    '--disable-logging ' +
    '--safebrowsing-disable-auto-update ' +
    '--disable-client-side-phishing-detection ' +
    '--disable-features=msEdgeTelemetry,msSmartScreenProtection,MediaRouter';

  // disable Crash Reporter
  GlobalWebView2Loader.CustomCrashReportingEnabled := True;

  GlobalWebView2Loader.OnGetCustomSchemes := _OnGetCustomSchemes;
end;

destructor TEdgeBrowserEnvironmentLoaderGlobal.Destroy;
begin
  FreeAndNil(GlobalWebView2Loader);
  inherited;
end;

procedure TEdgeBrowserEnvironmentLoaderGlobal.SetupProxySettings;
var
  VProxyType: string;
begin
  if FProxyConfig.UseProxy then begin
    case FProxyConfig.ProxyType of
      ptHttp:              VProxyType := 'http://';
      ptHttps:             VProxyType := 'https://';
      ptSocks4, ptSocks4a: VProxyType := 'socks4://';
      ptSocks5, ptSocks5h: VProxyType := 'socks5://';
    else
      raise Exception.CreateFmt('Unexpected ProxyType value: %d', [Integer(FProxyConfig.ProxyType)]);
    end;
    GlobalWebView2Loader.ProxySettings.Server := VProxyType + string(FProxyConfig.Host);
  end else
  if FProxyConfig.UseIESettings then begin
    // nothing to do: this is default
  end else begin
    GlobalWebView2Loader.ProxySettings.NoProxyServer := True;
  end;
end;

function TEdgeBrowserEnvironmentLoaderGlobal.TryStartWebView2: Boolean;
begin
  Result := False;

  if GlobalWebView2Loader.Status in [wvlsCreated, wvlsUnloaded] then begin
    if not GlobalWebView2Loader.StartWebView2 then begin
      Exit;
    end;
  end;

  repeat
    if GlobalWebView2Loader.InitializationError then begin
      Exit;
    end;

    if GlobalWebView2Loader.Initialized then begin
      Result := GlobalWebView2Loader.Environment <> nil;
      Exit;
    end;

    DoProcessMessages(10);

  until False;
end;

end.
