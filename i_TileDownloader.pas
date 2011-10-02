unit i_TileDownloader;

interface

uses
  Windows,
  Classes,
  i_OperationNotifier,
  i_DownloadResult,
  i_DownloadResultFactory,
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

    procedure OnBeforeRequest(AConfig: ITileDownloaderConfigStatic);
    procedure OnAfterResponse(const ARawResponseHeader: string);

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

    function GetTileSize: Cardinal;
    procedure SetTileSize(AValue: Cardinal);
    property TileSize: Cardinal read GetTileSize write SetTileSize;

    function GetCheckTileSize: Boolean;
    procedure SetCheckTileSize(AValue: Boolean);
    property CheckTileSize: Boolean read GetCheckTileSize write SetCheckTileSize;

    function GetOldTileSize: Cardinal;
    procedure SetOldTileSize(AValue: Cardinal);
    property OldTileSize: Cardinal read GetOldTileSize write SetOldTileSize;

    function GetTileMIME: string;
    procedure SetTileMIME(AValue: string);
    property TileMIME: string read GetTileMIME write SetTileMIME;

    function GetTileStream: TMemoryStream;
    procedure SetTileStream(AValue: TMemoryStream);
    property TileStream: TMemoryStream read GetTileStream write SetTileStream;

    function GetHttpStatusCode: Cardinal;
    procedure SetHttpStatusCode(AValue: Cardinal);
    property HttpStatusCode: Cardinal read GetHttpStatusCode write SetHttpStatusCode;

    function GetDownloadResult: IDownloadResult;
    procedure SetDownloadResult(AValue: IDownloadResult);
    property DownloadResult: IDownloadResult read GetDownloadResult write SetDownloadResult;

    function GetResultFactory: IDownloadResultFactory;
    property ResultFactory: IDownloadResultFactory read GetResultFactory;

    function GetCancelNotifier: IOperationNotifier;
    property CancelNotifier: IOperationNotifier read GetCancelNotifier;
  end;

implementation

end.
