{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2016, SAS.Planet development team.                      *}
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

unit i_DownloadSession;

interface

uses
  Types,
  i_GeometryLonLat,
  i_GeometryLonLatFactory,
  i_MapType,
  i_MapTypeSet,
  i_MapVersionInfo,
  i_MapVersionRequest,
  i_GlobalDownloadConfig,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider;

type
  IDownloadSession = interface
    ['{68A191EF-241C-4EA7-A0AC-9695B9610567}']
    function GetMapType: IMapType;
    property MapType: IMapType read GetMapType;

    function GetVersionForCheck: IMapVersionRequest;
    property VersionForCheck: IMapVersionRequest read GetVersionForCheck;

    function GetVersionForDownload: IMapVersionInfo;
    property VersionForDownload: IMapVersionInfo read GetVersionForDownload;

    function GetZoom: Byte;
    procedure SetZoom(const Value: Byte);
    property Zoom: Byte read GetZoom write SetZoom;

    function GetZoomArr: TByteDynArray;
    property ZoomArr: TByteDynArray read GetZoomArr;

    function GetPolygon: IGeometryLonLatPolygon;
    property Polygon: IGeometryLonLatPolygon read GetPolygon;

    function GetSecondLoadTNE: Boolean;
    property SecondLoadTNE: Boolean read GetSecondLoadTNE;

    function GetReplaceTneOlderDate: TDateTime;
    property ReplaceTneOlderDate: TDateTime read GetReplaceTneOlderDate;

    function GetReplaceExistTiles: Boolean;
    property ReplaceExistTiles: Boolean read GetReplaceExistTiles;

    function GetCheckExistTileSize: Boolean;
    property CheckExistTileSize: Boolean read GetCheckExistTileSize;

    function GetCheckExistTileDate: Boolean;
    property CheckExistTileDate: Boolean read GetCheckExistTileDate;

    function GetCheckTileDate: TDateTime;
    property CheckTileDate: TDateTime read GetCheckTileDate;

    function GetProcessed: Int64;
    procedure SetProcessed(const Value: Int64);
    property Processed: Int64 read GetProcessed write SetProcessed;

    function GetDownloadedSize: UInt64;
    procedure SetDownloadedSize(const Value: UInt64);
    property DownloadedSize: UInt64 read GetDownloadedSize write SetDownloadedSize;

    function GetDownloadedCount: Int64;
    procedure SetDownloadedCount(const Value: Int64);
    property DownloadedCount: Int64 read GetDownloadedCount write SetDownloadedCount;

    function GetLastProcessedPoint: TPoint;
    procedure SetLastProcessedPoint(const Value: TPoint);
    property LastProcessedPoint: TPoint read GetLastProcessedPoint write SetLastProcessedPoint;

    function GetLastSuccessfulPoint: TPoint;
    procedure SetLastSuccessfulPoint(const Value: TPoint);
    property LastSuccessfulPoint: TPoint read GetLastSuccessfulPoint write SetLastSuccessfulPoint;

    function GetElapsedTime: TDateTime;
    procedure SetElapsedTime(const Value: TDateTime);
    property ElapsedTime: TDateTime read GetElapsedTime write SetElapsedTime;

    procedure Save(
      const ASessionSection: IConfigDataWriteProvider
    );

    procedure Load(
      const ASessionSection: IConfigDataProvider;
      const AFullMapsSet: IMapTypeSet;
      const ADownloadConfig: IGlobalDownloadConfig;
      const AVectorGeometryLonLatFactory: IGeometryLonLatFactory
    );

    procedure Reset(
      const AMapType: IMapType;
      const AVersionForCheck: IMapVersionRequest;
      const AVersionForDownload: IMapVersionInfo;
      const AZoom: Byte;
      const AZoomArr: TByteDynArray;
      const APolygon: IGeometryLonLatPolygon;
      const ASecondLoadTNE: Boolean;
      const AReplaceTneOlderDate: TDateTime;
      const AReplaceExistTiles: Boolean;
      const ACheckExistTileSize: Boolean;
      const ACheckExistTileDate: Boolean;
      const ACheckTileDate: TDateTime;
      const AProcessed: Int64;
      const ADownloadedSize: UInt64;
      const ADownloadedCount: Int64;
      const ALastProcessedPoint: TPoint;
      const ALastSuccessfulPoint: TPoint;
      const AElapsedTime: TDateTime
    );
  end;

implementation

end.
