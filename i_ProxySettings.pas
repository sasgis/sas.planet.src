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

unit i_ProxySettings;

interface

uses
  i_ConfigDataElement;

type
  IProxyConfigStatic = interface(IInterface)
    ['{DD723CA2-3A8F-4350-B04E-284B00AC47EA}']
    function GetUseIESettings: Boolean;
    property UseIESettings: Boolean read GetUseIESettings;

    function GetUseProxy: boolean;
    property UseProxy: boolean read GetUseProxy;

    function GetHost: WideString;
    property Host: WideString read GetHost;

    function GetUseLogin: boolean;
    property UseLogin: boolean read GetUseLogin;

    function GetLogin: WideString;
    property Login: WideString read GetLogin;

    function GetPassword: WideString;
    property Password: WideString read GetPassword;
  end;

  IProxyConfig = interface(IConfigDataElement)
    ['{0CE5A97E-471D-4A3E-93E3-D130DD1F50F5}']
    function GetUseIESettings: Boolean; safecall;
    procedure SetUseIESettings(AValue: Boolean);
    property UseIESettings: Boolean read GetUseIESettings write SetUseIESettings;

    function GetUseProxy: Boolean; safecall;
    procedure SetUseProxy(AValue: Boolean);
    property UseProxy: boolean read GetUseProxy write SetUseProxy;

    function GetHost: WideString; safecall;
    procedure SetHost(const AValue: WideString);
    property Host: WideString read GetHost write SetHost;

    function GetUseLogin: boolean; safecall;
    procedure SetUseLogin(AValue: Boolean);
    property UseLogin: boolean read GetUseLogin write SetUseLogin;

    function GetLogin: WideString; safecall;
    procedure SetLogin(const AValue: WideString);
    property Login: WideString read GetLogin write SetLogin;

    function GetPassword: WideString; safecall;
    procedure SetPassword(const AValue: WideString);
    property Password: WideString read GetPassword write SetPassword;

    function GetStatic: IProxyConfigStatic;
  end;

implementation

end.
