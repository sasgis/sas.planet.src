{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_InetConfigStatic;

interface

uses
  i_ProxySettings,
  i_InetConfig,
  u_BaseInterfacedObject;

type
  TInetConfigStatic = class(TBaseInterfacedObject, IInetConfigStatic)
  private
    FProxyConfigStatic: IProxyConfigStatic;
    FUserAgentString: AnsiString;
    FTimeOut: Cardinal;
    FSleepOnResetConnection: Cardinal;
    FDownloadTryCount: Integer;
  private
    function GetProxyConfigStatic: IProxyConfigStatic;
    function GetUserAgentString: AnsiString;
    function GetTimeOut: Cardinal;
    function GetSleepOnResetConnection: Cardinal;
    function GetDownloadTryCount: Integer;
  public
    constructor Create(
      const AProxyConfigStatic: IProxyConfigStatic;
      const AUserAgentString: AnsiString;
      ATimeOut: Cardinal;
      ASleepOnResetConnection: Cardinal;
      ADownloadTryCount: Integer
    );
  end;

implementation

{ TTileDownloaderConfigStatic }

constructor TInetConfigStatic.Create(
  const AProxyConfigStatic: IProxyConfigStatic;
  const AUserAgentString: AnsiString;
  ATimeOut, ASleepOnResetConnection: Cardinal;
  ADownloadTryCount: Integer
);
begin
  inherited Create;
  FProxyConfigStatic := AProxyConfigStatic;
  FTimeOut := ATimeOut;
  FSleepOnResetConnection := ASleepOnResetConnection;
  FDownloadTryCount := ADownloadTryCount;
  FUserAgentString := AUserAgentString;
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

end.
