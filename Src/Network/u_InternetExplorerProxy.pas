{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2019, SAS.Planet development team.                      *}
{* This program is free software: you can redistribute it and/or modify       *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* This program is distributed in the hope that it will be useful,            *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *}
{*                                                                            *}
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

unit u_InternetExplorerProxy;

interface

type
  TInternetExplorerProxyInfo = record
    AutoDetect: Boolean;
    AutoConfigUrl: string;
    Proxy: string;
    ProxyBypass: string;
  end;

  TInternetExplorerProxy = record
    class function GetInfo(
      var AInfo: TInternetExplorerProxyInfo
    ): Boolean; static;

    class function GetInfoForUrl(
      const AUrl: string;
      var AInfo: TInternetExplorerProxyInfo
    ): Boolean; static;

    class function ShowConnectionDialog: Cardinal; static;
  end;

implementation

{$IF CompilerVersion >= 29}
  // WinHttp unit was added in Delphi XE8
  {$DEFINE USE_WINHTTP_UNIT}
{$IFEND}

uses
  Windows,
  ShellAPI,
  {$IFDEF USE_WINHTTP_UNIT}
  WinHttp,
  {$ENDIF}
  SysUtils;

{$IFNDEF UNICODE}
type
  UnicodeString = WideString;
{$ENDIF}

{$IFNDEF USE_WINHTTP_UNIT}
const
  winhttp_dll = 'winhttp.dll';

const
  WINHTTP_ACCESS_TYPE_DEFAULT_PROXY = 0;
  WINHTTP_ACCESS_TYPE_NO_PROXY = 1;
  WINHTTP_ACCESS_TYPE_NAMED_PROXY = 3;

  WINHTTP_NO_PROXY_NAME = nil;
  WINHTTP_NO_PROXY_BYPASS = nil;

  WINHTTP_AUTOPROXY_AUTO_DETECT = $00000001;
  WINHTTP_AUTOPROXY_CONFIG_URL = $00000002;

  WINHTTP_AUTO_DETECT_TYPE_DHCP = $00000001;
  WINHTTP_AUTO_DETECT_TYPE_DNS_A = $00000002;

  WINHTTP_ERROR_BASE = 12000;
  ERROR_WINHTTP_LOGIN_FAILURE = WINHTTP_ERROR_BASE + 15;

type
  HINTERNET = Pointer;

  WINHTTP_PROXY_INFO = record
    dwAccessType: DWORD;
    lpszProxy: LPWSTR;
    lpszProxyBypass: LPWSTR;
  end;
  TWinHttpProxyInfo = WINHTTP_PROXY_INFO;

  WINHTTP_CURRENT_USER_IE_PROXY_CONFIG = record
    fAutoDetect: BOOL;
    lpszAutoConfigUrl: LPWSTR;
    lpszProxy: LPWSTR;
    lpszProxyBypass: LPWSTR;
  end;
  TWinHttpCurrentUserIEProxyConfig = WINHTTP_CURRENT_USER_IE_PROXY_CONFIG;

  WINHTTP_AUTOPROXY_OPTIONS = record
    dwFlags: DWORD;
    dwAutoDetectFlags: DWORD;
    lpszAutoConfigUrl: LPCWSTR;
    lpvReserved: Pointer;
    dwReserved: DWORD;
    fAutoLogonIfChallenged: BOOL;
  end;
  TWinHttpAutoProxyOptions = WINHTTP_AUTOPROXY_OPTIONS;

function WinHttpGetIEProxyConfigForCurrentUser(var pProxyInfo: TWinHttpCurrentUserIEProxyConfig): BOOL; stdcall; external winhttp_dll;

function WinHttpOpen(pszAgentW: LPCWSTR; dwAccessType: DWORD; pszProxyW: LPCWSTR; pszProxyBypassW: LPCWSTR;
  dwFlags: DWORD): HINTERNET; stdcall; external winhttp_dll;

function WinHttpSetTimeouts(hInternet: HINTERNET; nResolveTimeout: Integer; nConnectTimeout: Integer;
  nSendTimeout: Integer; nReceiveTimeout: Integer): BOOL; stdcall; external winhttp_dll;

function WinHttpGetProxyForUrl(hSession: HINTERNET; lpcwszUrl: LPCWSTR; var pAutoProxyOptions: TWinHttpAutoProxyOptions;
  var pProxyInfo: TWinHttpProxyInfo): BOOL; stdcall; external winhttp_dll;

function WinHttpCloseHandle(hInternet: HINTERNET): BOOL; stdcall; external winhttp_dll;
{$ENDIF}

procedure GlobalFreeStr(var AStr: PWideChar); inline;
begin
  if AStr <> nil then begin
    GlobalFree(HGLOBAL(AStr));
    AStr := nil;
  end;
end;

{ TInternetExplorerProxy }

class function TInternetExplorerProxy.GetInfo(
  var AInfo: TInternetExplorerProxyInfo
): Boolean;
var
  VConfig: TWinHttpCurrentUserIEProxyConfig;
begin
  FillChar(VConfig, SizeOf(VConfig), 0);

  Result := WinHttpGetIEProxyConfigForCurrentUser(VConfig);

  if Result then begin
    AInfo.AutoConfigUrl := VConfig.lpszAutoConfigUrl;
    AInfo.AutoDetect := VConfig.fAutoDetect;
    AInfo.Proxy := VConfig.lpszProxy;
    AInfo.ProxyBypass := VConfig.lpszProxyBypass;

    GlobalFreeStr(VConfig.lpszAutoConfigUrl);
    GlobalFreeStr(VConfig.lpszProxy);
    GlobalFreeStr(VConfig.lpszProxyBypass);
  end else begin
    FillChar(AInfo, SizeOf(AInfo), 0);
    Result := GetLastError = ERROR_FILE_NOT_FOUND;
    if Result then begin
      // Internet Explorer's proxy configuration is not available
      // fallback to autodetect
      AInfo.AutoDetect := True;
    end;
  end;
end;

class function TInternetExplorerProxy.GetInfoForUrl(
  const AUrl: string;
  var AInfo: TInternetExplorerProxyInfo
): Boolean;
var
  VUrl: PWideChar;
  VProxy: TWinHttpProxyInfo;
  VOptions: TWinHttpAutoProxyOptions;
  VHttpSession: HINTERNET;
begin
  FillChar(VProxy, SizeOf(VProxy), 0);
  FillChar(VOptions, SizeOf(VOptions), 0);

  VHttpSession := WinHttpOpen(nil, WINHTTP_ACCESS_TYPE_DEFAULT_PROXY,
    WINHTTP_NO_PROXY_NAME, WINHTTP_NO_PROXY_BYPASS, 0);

  if VHttpSession = nil then begin
    Result := False;
    Exit;
  end;

  if AInfo.AutoConfigUrl <> '' then begin
    VOptions.dwFlags := WINHTTP_AUTOPROXY_CONFIG_URL;
    VOptions.lpszAutoConfigUrl := PWideChar(UnicodeString(AInfo.AutoConfigUrl));
  end else begin
    VOptions.dwFlags := WINHTTP_AUTOPROXY_AUTO_DETECT;
    VOptions.dwAutoDetectFlags := WINHTTP_AUTO_DETECT_TYPE_DNS_A or WINHTTP_AUTO_DETECT_TYPE_DHCP;
  end;

  WinHttpSetTimeouts(VHttpSession, 10000, 10000, 5000, 5000);

  VUrl := PWideChar(UnicodeString(AUrl));
  VOptions.fAutoLogonIfChallenged := False;

  // Per http://msdn.microsoft.com/en-us/library/aa383153(VS.85).aspx, it is
  // necessary to first try resolving with fAutoLogonIfChallenged set to false.
  // Otherwise, we fail over to trying it with a value of true.  This way we
  // get good performance in the case where WinHTTP uses an out-of-process
  // resolver.  This is important for Vista and Win2k3

  Result := WinHttpGetProxyForUrl(VHttpSession, VUrl, VOptions, VProxy);

  if not Result and (GetLastError() = ERROR_WINHTTP_LOGIN_FAILURE) then begin
    VOptions.fAutoLogonIfChallenged := True;
    Result := WinHttpGetProxyForUrl(VHttpSession, VUrl, VOptions, VProxy);
  end;

  WinHttpCloseHandle(VHttpSession);

  if not Result then begin
    Exit;
  end;

  if VProxy.dwAccessType = WINHTTP_ACCESS_TYPE_NO_PROXY then begin
    AInfo.Proxy := '';
    AInfo.ProxyBypass := '';
  end else begin
    AInfo.Proxy := VProxy.lpszProxy;
    AInfo.ProxyBypass := VProxy.lpszProxyBypass;
  end;

  GlobalFreeStr(VProxy.lpszProxy);
  GlobalFreeStr(VProxy.lpszProxyBypass);
end;

class function TInternetExplorerProxy.ShowConnectionDialog: Cardinal;
begin
  Result :=
    ShellExecute(
      0,
      'open',
      'rundll32.exe',
      'inetcpl.cpl,LaunchConnectionDialog',
      nil,
      SW_SHOWNORMAL
    );
end;

end.
