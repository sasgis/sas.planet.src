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

unit u_InternalBrowserFactory;

interface

uses
  Forms,
  Controls,
  i_Listener,
  i_InetConfig,
  i_ProxyConfig,
  i_InternalBrowserFactory,
  i_InternalDomainUrlHandler,
  i_InternalDomainInfoProvider,
  u_ConfigDataElementBase,
  u_InternalBrowserImpl,
  u_InternalBrowserImplByEdge,
  u_InternalBrowserImplByIe,
  u_IeEmbeddedProtocolRegistration,
  u_BaseInterfacedObject;

type
  TInternalBrowserFactory = class(TConfigDataElementBaseEmptySaveLoad, IInternalBrowserFactory)
  private const
    CEdgeRuntimePath       = 'Edge\Runtime\';
    CEdgeWebView2Exe       = 'Edge\Runtime\msedgewebview2.exe';
    CEdgeUserDataPath      = 'Edge\UserData\';
    CEdgeBlackListFileName = 'Edge\BlackList.txt';
    CEdgeBlackListMaxCount = 100;
  private
    FInetConfig: IInetConfig;
    FInetConfigStatic: IInetConfigStatic;
    FInternalDomainUrlHandler: IInternalDomainUrlHandler;
    FInternalDomainInfoProviderList: IInternalDomainInfoProviderList;

    FIeProtocol: TIeEmbeddedProtocolRegistration;
    FEdgeEnvironmentLoader: TEdgeBrowserEnvironmentLoaderGlobal;

    FOnInetConfigChangeListener: IListener;
    FOnProxyConfigChangeListener: IListener;

    procedure OnConfigChange;
  protected
    procedure DoInChangeNotify; override;
  private
    { InternalBrowserFactory }
    function CreateBrowserImpl(
      const AParent: TWinControl;
      const AIsInvisible: Boolean;
      const AOnKeyDown: TOnKeyDown;
      const AOnTitleChange: TOnTitleChange
    ): TInternalBrowserImpl;
  public
    constructor Create(
      const AInetConfig: IInetConfig;
      const AInternalDomainUrlHandler: IInternalDomainUrlHandler;
      const AInternalDomainInfoProviderList: IInternalDomainInfoProviderList
    );
    destructor Destroy; override;
  end;

implementation

uses
  Types,
  Classes,
  SysUtils,
  IOUtils,
  UrlMon,
  WinInet,
  c_InternalBrowser,
  u_ListenerByEvent,
  u_IeEmbeddedProtocolFactory;

{ TInternalBrowserFactory }

constructor TInternalBrowserFactory.Create(
  const AInetConfig: IInetConfig;
  const AInternalDomainUrlHandler: IInternalDomainUrlHandler;
  const AInternalDomainInfoProviderList: IInternalDomainInfoProviderList
);
begin
  inherited Create;

  FInetConfig := AInetConfig;
  FInetConfigStatic := FInetConfig.GetStatic;
  FInternalDomainUrlHandler := AInternalDomainUrlHandler;
  FInternalDomainInfoProviderList := AInternalDomainInfoProviderList;

  FOnInetConfigChangeListener := TNotifyNoMmgEventListener.Create(Self.OnConfigChange);
  FInetConfig.ChangeNotifier.Add(FOnInetConfigChangeListener);

  FOnProxyConfigChangeListener := TNotifyNoMmgEventListener.Create(Self.OnConfigChange);
  FInetConfig.ProxyConfig.ChangeNotifier.Add(FOnProxyConfigChangeListener);
end;

destructor TInternalBrowserFactory.Destroy;
begin
  if Assigned(FInetConfig) and Assigned(FOnInetConfigChangeListener) then begin
    FInetConfig.ChangeNotifier.Remove(FOnInetConfigChangeListener);
    FOnInetConfigChangeListener := nil;
  end;

  if Assigned(FInetConfig) and Assigned(FOnProxyConfigChangeListener) then begin
    FInetConfig.ProxyConfig.ChangeNotifier.Remove(FOnProxyConfigChangeListener);
    FOnProxyConfigChangeListener := nil;
  end;

  FreeAndNil(FIeProtocol);
  FreeAndNil(FEdgeEnvironmentLoader);

  inherited Destroy;
end;

procedure SetIeProxy(const AProxyConfig: IProxyConfigStatic);
var
  VInfo: TInternetProxyInfo;
  VUseSystemProxy: Boolean;
  VUseCustomProxy: Boolean;
  VCustomProxyHost: AnsiString;
  VProxyType: TProxyServerType;
begin
  VUseSystemProxy := AProxyConfig.UseIESettings;
  VUseCustomProxy := AProxyConfig.UseProxy;
  VCustomProxyHost := AProxyConfig.GetHost;
  VProxyType := AProxyConfig.ProxyType;

  if VUseCustomProxy then begin
    if VProxyType = ptSocks4 then begin
      VCustomProxyHost := 'socks=' + VCustomProxyHost;
    end else
    if VProxyType <> ptHttp then begin
      VUseCustomProxy := False;
    end;
  end;

  FillChar(VInfo, SizeOf(VInfo), 0);

  if VUseSystemProxy then begin
    VInfo.dwAccessType := INTERNET_OPEN_TYPE_PRECONFIG;
    VInfo.lpszProxy := nil;
    VInfo.lpszProxyBypass := nil;
    UrlMkSetSessionOption(INTERNET_OPTION_PROXY, @VInfo, SizeOf(VInfo), 0);
    UrlMkSetSessionOption(INTERNET_OPTION_REFRESH, nil, 0, 0);
  end else begin
    if VUseCustomProxy then begin
      VInfo.dwAccessType := INTERNET_OPEN_TYPE_PROXY;
      VInfo.lpszProxy := PAnsiChar(VCustomProxyHost);
      VInfo.lpszProxyBypass := nil;
    end else begin
      VInfo.dwAccessType := INTERNET_OPEN_TYPE_DIRECT;
      VInfo.lpszProxy := nil;
      VInfo.lpszProxyBypass := nil;
    end;
    UrlMkSetSessionOption(INTERNET_OPTION_PROXY, @VInfo, SizeOf(VInfo), 0);
    UrlMkSetSessionOption(INTERNET_OPTION_SETTINGS_CHANGED, nil, 0, 0);
  end;
end;

function TInternalBrowserFactory.CreateBrowserImpl(
  const AParent: TWinControl;
  const AIsInvisible: Boolean;
  const AOnKeyDown: TOnKeyDown;
  const AOnTitleChange: TOnTitleChange
): TInternalBrowserImpl;
var
  VEngine: TBrowserEngineType;
  VAppPath: string;
  VEdgeRuntimePath: string;
  VEdgeUserDataPath: string;
  VEdgeBlackListFile: string;
  VEdgeBlackList: TStringDynArray;
begin
  VEngine := FInetConfigStatic.BrowserEngineType;

  case VEngine of
    beInternetExplorer: begin
      if FIeProtocol = nil then begin
        FIeProtocol :=
          TIeEmbeddedProtocolRegistration.Create(
            CSASProtocolName,
            TIeEmbeddedProtocolFactory.Create(FInternalDomainInfoProviderList)
          );
        SetIeProxy(FInetConfigStatic.ProxyConfigStatic);
      end;

      Result :=
        TInternalBrowserImplByIe.Create(
          AParent,
          AIsInvisible,
          FInetConfigStatic.ProxyConfigStatic,
          FInternalDomainUrlHandler,
          FInetConfigStatic.UserAgentString,
          AOnKeyDown,
          AOnTitleChange
        );
    end;

    beEdge: begin
      if FEdgeEnvironmentLoader = nil then begin
        VAppPath := ExtractFilePath(ParamStr(0));

        // Runtime
        if FileExists(VAppPath + CEdgeWebView2Exe) then begin
          VEdgeRuntimePath := VAppPath + CEdgeRuntimePath;
        end else begin
          VEdgeRuntimePath := '';
        end;

        // UserData
        VEdgeUserDataPath := VAppPath + CEdgeUserDataPath;

        // BlackList
        VEdgeBlackListFile := VAppPath + CEdgeBlackListFileName;
        if FileExists(VEdgeBlackListFile) then begin
          VEdgeBlackList := TFile.ReadAllLines(VEdgeBlackListFile);
          if Length(VEdgeBlackList) > CEdgeBlackListMaxCount then begin
            SetLength(VEdgeBlackList, CEdgeBlackListMaxCount);
          end;
        end else begin
          VEdgeBlackList := nil;
        end;

        FEdgeEnvironmentLoader :=
          TEdgeBrowserEnvironmentLoaderGlobal.Create(
            FInetConfigStatic.ProxyConfigStatic,
            VEdgeRuntimePath,
            VEdgeUserDataPath,
            VEdgeBlackList
          );
      end;

      Result :=
        TInternalBrowserImplByEdge.Create(
          AParent,
          AIsInvisible,
          FEdgeEnvironmentLoader,
          FInternalDomainUrlHandler,
          FInternalDomainInfoProviderList,
          AOnKeyDown,
          AOnTitleChange
        );
    end;
  else
    raise Exception.CreateFmt('Unexpected BrowserEngineType value: %d', [Integer(VEngine)]);
  end;

  if not FInetConfigStatic.PreInitBrowserEngine or AIsInvisible then begin
    Exit;
  end;

  if not Result.Initialize and (VEngine <> beInternetExplorer) then begin
    FInetConfig.BrowserEngineType := beInternetExplorer;
    Result := Self.CreateBrowserImpl(AParent, AIsInvisible, AOnKeyDown, AOnTitleChange);
  end;
end;

procedure TInternalBrowserFactory.OnConfigChange;
var
  VIsChanged: Boolean;
  VStatic: IInetConfigStatic;
begin
  LockWrite;
  try
    VStatic := FInetConfig.GetStatic;

    VIsChanged :=
      (VStatic.BrowserEngineType <> FInetConfigStatic.BrowserEngineType) or
      (VStatic.UserAgentString   <> FInetConfigStatic.UserAgentString) or
      (VStatic.ProxyConfigStatic <> FInetConfigStatic.ProxyConfigStatic);

    if VIsChanged then begin
      FInetConfigStatic := VStatic;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TInternalBrowserFactory.DoInChangeNotify;
begin
  FreeAndNil(FIeProtocol);
  if FEdgeEnvironmentLoader <> nil then begin
    FreeAndNil(FEdgeEnvironmentLoader);
    Application.ProcessMessages;
    Sleep(250);
  end;
  inherited DoInChangeNotify;
end;

end.
