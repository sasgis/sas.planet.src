{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
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

unit u_InetConfigStatic;

interface

uses
  i_ProxySettings,
  i_InetConfig,
  i_WinInetConfig,
  u_BaseInterfacedObject;

type
  TInetConfigStatic = class(TBaseInterfacedObject, IInetConfigStatic)
  private
    FWinInetConfigStatic: IWinInetConfigStatic;
    FProxyConfigStatic: IProxyConfigStatic;
    FUserAgentString: AnsiString;
    FTimeOut: Cardinal;
    FSleepOnResetConnection: Cardinal;
    FDownloadTryCount: Integer;
    FNetworkEngineType: TNetworkEngineType;
  private
    function GetWinInetConfigStatic: IWinInetConfigStatic;
    function GetProxyConfigStatic: IProxyConfigStatic;
    function GetUserAgentString: AnsiString;
    function GetTimeOut: Cardinal;
    function GetSleepOnResetConnection: Cardinal;
    function GetDownloadTryCount: Integer;
    function GetNetworkEngineType: TNetworkEngineType;
  public
    constructor Create(
      const AWinInetConfigStatic: IWinInetConfigStatic;
      const AProxyConfigStatic: IProxyConfigStatic;
      const AUserAgentString: AnsiString;
      const ATimeOut: Cardinal;
      const ASleepOnResetConnection: Cardinal;
      const ADownloadTryCount: Integer;
      const ANetworkEngineType: TNetworkEngineType
    );
  end;

implementation

{ TInetConfigStatic }

constructor TInetConfigStatic.Create(
  const AWinInetConfigStatic: IWinInetConfigStatic;
  const AProxyConfigStatic: IProxyConfigStatic;
  const AUserAgentString: AnsiString;
  const ATimeOut, ASleepOnResetConnection: Cardinal;
  const ADownloadTryCount: Integer;
  const ANetworkEngineType: TNetworkEngineType
);
begin
  inherited Create;
  FWinInetConfigStatic := AWinInetConfigStatic;
  FProxyConfigStatic := AProxyConfigStatic;
  FTimeOut := ATimeOut;
  FSleepOnResetConnection := ASleepOnResetConnection;
  FDownloadTryCount := ADownloadTryCount;
  FUserAgentString := AUserAgentString;
  FNetworkEngineType := ANetworkEngineType;
end;

function TInetConfigStatic.GetDownloadTryCount: Integer;
begin
  Result := FDownloadTryCount;
end;

function TInetConfigStatic.GetProxyConfigStatic: IProxyConfigStatic;
begin
  Result := FProxyConfigStatic;
end;

function TInetConfigStatic.GetSleepOnResetConnection: Cardinal;
begin
  Result := FSleepOnResetConnection;
end;

function TInetConfigStatic.GetTimeOut: Cardinal;
begin
  Result := FTimeOut;
end;

function TInetConfigStatic.GetUserAgentString: AnsiString;
begin
  Result := FUserAgentString;
end;

function TInetConfigStatic.GetWinInetConfigStatic: IWinInetConfigStatic;
begin
  Result := FWinInetConfigStatic;
end;

function TInetConfigStatic.GetNetworkEngineType: TNetworkEngineType;
begin
  Result := FNetworkEngineType;
end;

end.
