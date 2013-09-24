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

unit i_TileDownloaderConfig;

interface

uses
  Types,
  i_ConfigDataElement,
  i_InetConfig;

type
  ITileDownloaderConfigStatic = interface
    ['{FECD40CF-A0AF-479A-8CCC-E3363037773E}']
    function GetInetConfigStatic: IInetConfigStatic;
    property InetConfigStatic: IInetConfigStatic read GetInetConfigStatic;

    function GetEnabled: Boolean;
    property Enabled: Boolean read GetEnabled;

    function GetAllowUseCookie: Boolean;
    property AllowUseCookie: Boolean read GetAllowUseCookie;

    function GetWaitInterval: Cardinal;
    property WaitInterval: Cardinal read GetWaitInterval;

    function GetMaxConnectToServerCount: Cardinal;
    property MaxConnectToServerCount: Cardinal read GetMaxConnectToServerCount;

    function GetIgnoreMIMEType: Boolean;
    property IgnoreMIMEType: Boolean read GetIgnoreMIMEType;

    function GetExpectedMIMETypes: AnsiString;
    property ExpectedMIMETypes: AnsiString read GetExpectedMIMETypes;

    function GetDefaultMIMEType: AnsiString;
    property DefaultMIMEType: AnsiString read GetDefaultMIMEType;

    function GetIteratorSubRectSize: TPoint;
    property IteratorSubRectSize: TPoint read GetIteratorSubRectSize;

    function GetRestartDownloadOnMemCacheTTL: Boolean;
    property RestartDownloadOnMemCacheTTL: Boolean read GetRestartDownloadOnMemCacheTTL;
  end;

  ITileDownloaderConfig = interface(IConfigDataElement)
    ['{FECD40CF-A0AF-479A-8CCC-E3363037773E}']
    function GetInetConfigStatic: IInetConfigStatic;
    property InetConfigStatic: IInetConfigStatic read GetInetConfigStatic;

    function GetEnabled: Boolean;
    procedure SetEnabled(AValue: Boolean);
    property Enabled: Boolean read GetEnabled write SetEnabled;

    function GetAllowUseCookie: Boolean;
    procedure SetAllowUseCookie(const AValue: Boolean);
    property AllowUseCookie: Boolean read GetAllowUseCookie write SetAllowUseCookie;

    function GetWaitInterval: Cardinal;
    procedure SetWaitInterval(AValue: Cardinal);
    property WaitInterval: Cardinal read GetWaitInterval write SetWaitInterval;

    function GetMaxConnectToServerCount: Cardinal;
    procedure SetMaxConnectToServerCount(AValue: Cardinal);
    property MaxConnectToServerCount: Cardinal read GetMaxConnectToServerCount write SetMaxConnectToServerCount;

    function GetIgnoreMIMEType: Boolean;
    procedure SetIgnoreMIMEType(AValue: Boolean);
    property IgnoreMIMEType: Boolean read GetIgnoreMIMEType write SetIgnoreMIMEType;

    function GetExpectedMIMETypes: AnsiString;
    procedure SetExpectedMIMETypes(const AValue: AnsiString);
    property ExpectedMIMETypes: AnsiString read GetExpectedMIMETypes write SetExpectedMIMETypes;

    function GetDefaultMIMEType: AnsiString;
    procedure SetDefaultMIMEType(const AValue: AnsiString);
    property DefaultMIMEType: AnsiString read GetDefaultMIMEType write SetDefaultMIMEType;

    function GetIteratorSubRectSize: TPoint;
    procedure SetIteratorSubRectSize(const AValue: TPoint);
    property IteratorSubRectSize: TPoint read GetIteratorSubRectSize write SetIteratorSubRectSize;

    function GetRestartDownloadOnMemCacheTTL: Boolean;
    procedure SetRestartDownloadOnMemCacheTTL(const AValue: Boolean);
    property RestartDownloadOnMemCacheTTL: Boolean read GetRestartDownloadOnMemCacheTTL write SetRestartDownloadOnMemCacheTTL;

    function GetStatic: ITileDownloaderConfigStatic;
  end;

implementation

end.
