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

unit i_ZmpConfig;

interface

uses
//  Windows,
  i_ConfigDataElement;

type
  IZmpConfig = interface(IConfigDataElement)
    ['{57185941-5902-4843-9E24-54CBEA4A67FD}']
//    function GetIgnoreMIMEType: Boolean;
//    procedure SetIgnoreMIMEType(AValue: Boolean);
//    property IgnoreMIMEType: Boolean read GetIgnoreMIMEType write SetIgnoreMIMEType;
//
//    function GetDefaultMIMEType: string;
//    procedure SetDefaultMIMEType(AValue: string);
//    property DefaultMIMEType: string read GetDefaultMIMEType write SetDefaultMIMEType;
//
//    function GetExpectedMIMETypes: string;
//    procedure SetExpectedMIMETypes(AValue: string);
//    property ExpectedMIMETypes: string read GetExpectedMIMETypes write SetExpectedMIMETypes;
//
//    function GetWaitInterval: Cardinal;
//    procedure SetWaitInterval(AValue: Cardinal);
//    property WaitInterval: Cardinal read GetWaitInterval write SetWaitInterval;

    function GetMaxConnectToServerCount: Cardinal;
    procedure SetMaxConnectToServerCount(AValue: Cardinal);
    property MaxConnectToServerCount: Cardinal read GetMaxConnectToServerCount write SetMaxConnectToServerCount;

//    function GetIteratorSubRectSize: TPoint;
//    procedure SetIteratorSubRectSize(AValue: TPoint);
//    property IteratorSubRectSize: TPoint read GetIteratorSubRectSize write SetIteratorSubRectSize;
//
//    function GetUrlBase: string;
//    procedure SetUrlBase(AValue: string);
//    property UrlBase: string read GetUrlBase write SetUrlBase;
//
//    function GetRequestHead: string;
//    procedure SetRequestHead(AValue: string);
//    property RequestHead: string read GetRequestHead write SetRequestHead;
//
//    function GetVersion: Variant;
//    procedure SetVersion(AValue: Variant);
//    property Version: Variant read GetVersion write SetVersion;
  end;

implementation

end.
