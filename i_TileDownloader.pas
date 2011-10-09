{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
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

unit i_TileDownloader;

interface

uses
  Windows,
  Classes,
  i_OperationNotifier,
  i_DownloadResult,
  i_MapVersionInfo,
  i_LastResponseInfo,
  i_TileDownloadRequest,
  i_TileRequestBuilderConfig,
  i_TileDownloaderConfig;

type
  ITileDownloaderEvent = interface;

  POnDownloadCallBack = ^TOnDownloadCallBack;
  TOnDownloadCallBack = procedure (AEvent: ITileDownloaderEvent) of object;

  ITileDownloader = interface
    ['{EAF443E6-FC84-46A3-95AA-8217117A2A6B}']
    procedure Download(AEvent: ITileDownloaderEvent);
    function GetIsEnabled: Boolean;
    property Enabled: Boolean read GetIsEnabled;
  end;
  
  ITileDownloaderEvent = interface
    ['{6AF695C6-FBCF-49FD-BDDE-04C4568D31F7}']
    procedure ProcessEvent;

    procedure AddToCallBackList(ACallBack: TOnDownloadCallBack);
    procedure ExecCallBackList;
    
    function GetRequest: ITileDownloadRequest;
    procedure SetRequest(Value: ITileDownloadRequest);
    property Request: ITileDownloadRequest read GetRequest write SetRequest;

    function GetLastResponseInfo: ILastResponseInfo;
    procedure SetLastResponseInfo(AValue: ILastResponseInfo);
    property LastResponseInfo: ILastResponseInfo read GetLastResponseInfo write SetLastResponseInfo;

    function GetVersionInfo: IMapVersionInfo;
    procedure SetVersionInfo(AValue: IMapVersionInfo);
    property VersionInfo: IMapVersionInfo read GetVersionInfo write SetVersionInfo;

    function GetTileXY: TPoint;
    procedure SetTileXY(Value: TPoint);
    property TileXY: TPoint read GetTileXY write SetTileXY;

    function GetTileZoom: Byte;
    procedure SetTileZoom(AValue: Byte);
    property TileZoom: Byte read GetTileZoom write SetTileZoom;

    function GetCheckTileSize: Boolean;
    procedure SetCheckTileSize(AValue: Boolean);
    property CheckTileSize: Boolean read GetCheckTileSize write SetCheckTileSize;

    function GetOldTileSize: Cardinal;
    procedure SetOldTileSize(AValue: Cardinal);
    property OldTileSize: Cardinal read GetOldTileSize write SetOldTileSize;

    function GetDownloadResult: IDownloadResult;
    procedure SetDownloadResult(AValue: IDownloadResult);
    property DownloadResult: IDownloadResult read GetDownloadResult write SetDownloadResult;

    function GetCancelNotifier: IOperationNotifier;
    property CancelNotifier: IOperationNotifier read GetCancelNotifier;
  end;

implementation

end.
