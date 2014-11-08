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

unit i_InetConfig;

interface

uses
  i_ConfigDataElement,
  i_ProxySettings;

type
  IInetConfigStatic = interface
    ['{5608C1CA-91D5-43CB-BAF0-8C76351EC1D7}']
    function GetProxyConfigStatic: IProxyConfigStatic;
    property ProxyConfigStatic: IProxyConfigStatic read GetProxyConfigStatic;

    function GetUserAgentString: AnsiString;
    property UserAgentString: AnsiString read GetUserAgentString;

    function GetTimeOut: Cardinal;
    property TimeOut: Cardinal read GetTimeOut;

    function GetSleepOnResetConnection: Cardinal;
    property SleepOnResetConnection: Cardinal read GetSleepOnResetConnection;

    function GetDownloadTryCount: Integer;
    property DownloadTryCount: Integer read GetDownloadTryCount;
  end;

  IInetConfig = interface(IConfigDataElement)
    ['{D025A3CE-2CC7-4DB3-BBF6-53DF14A2A2E7}']
    function GetProxyConfig: IProxyConfig;
    property ProxyConfig: IProxyConfig read GetProxyConfig;

    function GetUserAgentString: AnsiString;
    procedure SetUserAgentString(const AValue: AnsiString);
    property UserAgentString: AnsiString read GetUserAgentString write SetUserAgentString;

    function GetTimeOut: Cardinal;
    procedure SetTimeOut(AValue: Cardinal);
    property TimeOut: Cardinal read GetTimeOut write SetTimeOut;

    function GetSleepOnResetConnection: Cardinal;
    procedure SetSleepOnResetConnection(AValue: Cardinal);
    property SleepOnResetConnection: Cardinal read GetSleepOnResetConnection write SetSleepOnResetConnection;

    function GetDownloadTryCount: Integer;
    procedure SetDownloadTryCount(AValue: Integer);
    property DownloadTryCount: Integer read GetDownloadTryCount write SetDownloadTryCount;

    function GetStatic: IInetConfigStatic;
  end;

implementation

end.
