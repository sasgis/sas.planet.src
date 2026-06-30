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
  Controls,
  i_InetConfig,
  i_InternalBrowserFactory,
  i_InternalDomainUrlHandler,
  i_InternalDomainInfoProvider,
  u_InternalBrowserImpl,
  u_InternalBrowserImplByEdge,
  u_InternalBrowserImplByIe,
  u_IeEmbeddedProtocolRegistration,
  u_BaseInterfacedObject;

type
  TInternalBrowserFactory = class(TBaseInterfacedObject, IInternalBrowserFactory)
  private const
    CEdgePotableRuntimePath = 'Edge\Runtime\';
    CEdgePotableUserDataPath = 'Edge\UserData\';
  private
    FInetConfig: IInetConfig;
    FInternalDomainUrlHandler: IInternalDomainUrlHandler;
    FInternalDomainInfoProviderList: IInternalDomainInfoProviderList;

    FIeProtocol: TIeEmbeddedProtocolRegistration;
    FEdgeEnvironmentLoader: TEdgeBrowserEnvironmentLoaderGlobal;

    function DoCreateBrowser(
      const AParent: TWinControl;
      const AIsInvisible: Boolean;
      const AOnKeyDown: TOnKeyDown;
      const AOnTitleChange: TOnTitleChange
    ): TInternalBrowserImpl;
  private
    { InternalBrowserFactory }
    function CreateBrowser(
      const AParent: TWinControl;
      const AOnKeyDown: TOnKeyDown = nil;
      const AOnTitleChange: TOnTitleChange = nil
    ): TInternalBrowserImpl;

    function CreateInvisibleBrowser(
      const AParent: TWinControl;
      const AOnKeyDown: TOnKeyDown = nil;
      const AOnTitleChange: TOnTitleChange = nil
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
  SysUtils,
  c_InternalBrowser,
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
  FInternalDomainUrlHandler := AInternalDomainUrlHandler;
  FInternalDomainInfoProviderList := AInternalDomainInfoProviderList;
end;

destructor TInternalBrowserFactory.Destroy;
begin
  FreeAndNil(FIeProtocol);
  FreeAndNil(FEdgeEnvironmentLoader);
  inherited Destroy;
end;

function TInternalBrowserFactory.CreateBrowser(
  const AParent: TWinControl;
  const AOnKeyDown: TOnKeyDown;
  const AOnTitleChange: TOnTitleChange
): TInternalBrowserImpl;
begin
  Result := DoCreateBrowser(AParent, False, AOnKeyDown, AOnTitleChange);
end;

function TInternalBrowserFactory.CreateInvisibleBrowser(
  const AParent: TWinControl;
  const AOnKeyDown: TOnKeyDown;
  const AOnTitleChange: TOnTitleChange
): TInternalBrowserImpl;
begin
  Result := DoCreateBrowser(AParent, True, AOnKeyDown, AOnTitleChange);
end;

function TInternalBrowserFactory.DoCreateBrowser(
  const AParent: TWinControl;
  const AIsInvisible: Boolean;
  const AOnKeyDown: TOnKeyDown;
  const AOnTitleChange: TOnTitleChange
): TInternalBrowserImpl;
var
  VEngine: TBrowserEngineType;
  VPreInit: Boolean;
  VUserAgent: string;
  VAppPath: string;
  VEdgeRuntimePath: string;
  VEdgeUserDataPath: string;
begin
  FInetConfig.LockRead;
  try
    VEngine := FInetConfig.BrowserEngineType;
    VPreInit := FInetConfig.PreInitBrowserEngine;
  finally
    FInetConfig.UnlockRead;
  end;

  if False then begin
    // TODO: Add an explicit option to override the engine's User-Agent value
    VUserAgent := FInetConfig.UserAgentString;
  end else begin
    VUserAgent := '';
  end;

  // Internet Explorer
  if VEngine = beInternetExplorer then begin

    if FIeProtocol = nil then begin
      FIeProtocol :=
        TIeEmbeddedProtocolRegistration.Create(
          CSASProtocolName,
          TIeEmbeddedProtocolFactory.Create(FInternalDomainInfoProviderList)
        );
    end;

    Result :=
      TInternalBrowserImplByIe.Create(
        AParent,
        AIsInvisible,
        FInetConfig.ProxyConfig,
        FInternalDomainUrlHandler,
        FInetConfig.UserAgentString, // TODO
        AOnKeyDown,
        AOnTitleChange
      );
  end else
  // Edge WebView2
  if VEngine in [beEdgePortable, beEdgeSystem] then begin

    if FEdgeEnvironmentLoader = nil then begin
      if VEngine = beEdgePortable then begin
        VAppPath := ExtractFilePath(ParamStr(0));
        VEdgeRuntimePath := VAppPath + CEdgePotableRuntimePath;
        VEdgeUserDataPath := VAppPath + CEdgePotableUserDataPath;
      end else begin
        VEdgeRuntimePath := '';
        VEdgeUserDataPath := '';
      end;
      FEdgeEnvironmentLoader :=
        TEdgeBrowserEnvironmentLoaderGlobal.Create(
          FInetConfig.ProxyConfig,
          VUserAgent,
          VEdgeRuntimePath,
          VEdgeUserDataPath
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
  end else begin
    raise Exception.CreateFmt('Unexpected BrowserEngineType value: %d', [Integer(VEngine)]);
  end;

  if not VPreInit or AIsInvisible then begin
    Exit;
  end;

  if not Result.Initialize and (VEngine <> beInternetExplorer) then begin
    FInetConfig.BrowserEngineType := beInternetExplorer;
    Result := DoCreateBrowser(AParent, AIsInvisible, AOnKeyDown, AOnTitleChange);
  end;
end;

end.
