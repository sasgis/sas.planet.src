unit i_TileDownloader;

interface

uses
  Windows,
  Classes,
  i_JclNotify,
  i_DownloadResult,
  i_DownloadResultFactory,
  i_TileRequestBuilderConfig,
  i_TileDownloaderConfig;

type
  ITileDownloaderEvent = interface;

  POnDownloadCallBack = ^TOnDownloadCallBack;
  TOnDownloadCallBack = procedure (AEvent: ITileDownloaderEvent) of object;

  ITileDownloader = interface
    ['{EAF443E6-FC84-46A3-95AA-8217117A2A6B}']
    function GetTileUrl(ATileXY: TPoint; AZoom: Byte): string;
    procedure Download(AEvent: ITileDownloaderEvent);

    function GetTileRequestBuilderConfig: ITileRequestBuilderConfig;
    property TileRequestBuilderConfig: ITileRequestBuilderConfig read GetTileRequestBuilderConfig;

    function GetTileDownloaderConfig: ITileDownloaderConfig;
    property TileDownloaderConfig: ITileDownloaderConfig read GetTileDownloaderConfig;

    function GetIsEnabled: Boolean;
    property Enabled: Boolean read GetIsEnabled;
  end;
  
  ITileDownloaderEvent = interface
    ['{6AF695C6-FBCF-49FD-BDDE-04C4568D31F7}']
    procedure ProcessEvent;

    procedure AddToCallBackList(ACallBack: TOnDownloadCallBack);
    procedure ExecCallBackList;

    procedure OnBeforeRequest(AConfig: ITileDownloaderConfigStatic);
    procedure OnAfterResponse();

    function GetUrl: string;
    procedure SetUrl(Value: string);
    property Url: string read GetUrl write SetUrl;

    function GetRawRequestHeader: string;
    procedure SetRawRequestHeader(Value: string);
    property RawRequestHeader: string read GetRawRequestHeader write SetRawRequestHeader;

    function GetRawResponseHeader: string;
    procedure SetRawResponseHeader(Value: string);
    property RawResponseHeader: string read GetRawResponseHeader write SetRawResponseHeader;

    function GetTileXY: TPoint;
    procedure SetTileXY(Value: TPoint);
    property TileXY: TPoint read GetTileXY write SetTileXY;

    function GetTileZoom: Byte;
    procedure SetTileZoom(Value: Byte);
    property TileZoom: Byte read GetTileZoom write SetTileZoom;

    function GetTileSize: Cardinal;
    procedure SetTileSize(Value: Cardinal);
    property TileSize: Cardinal read GetTileSize write SetTileSize;

    function GetCheckTileSize: Boolean;
    procedure SetCheckTileSize(Value: Boolean);
    property CheckTileSize: Boolean read GetCheckTileSize write SetCheckTileSize;

    function GetOldTileSize: Cardinal;
    procedure SetOldTileSize(Value: Cardinal);
    property OldTileSize: Cardinal read GetOldTileSize write SetOldTileSize;

    function GetTileMIME: string;
    procedure SetTileMIME(Value: string);
    property TileMIME: string read GetTileMIME write SetTileMIME;

    function GetTileStream: TMemoryStream;
    procedure SetTileStream(Value: TMemoryStream);
    property TileStream: TMemoryStream read GetTileStream write SetTileStream;

    function GetHttpStatusCode: Cardinal;
    procedure SetHttpStatusCode(Value: Cardinal);
    property HttpStatusCode: Cardinal read GetHttpStatusCode write SetHttpStatusCode;

    function GetDownloadResult: IDownloadResult;
    procedure SetDownloadResult(Value: IDownloadResult);
    property DownloadResult: IDownloadResult read GetDownloadResult write SetDownloadResult;

    function GetResultFactory: IDownloadResultFactory;
    property ResultFactory: IDownloadResultFactory read GetResultFactory;

    function GetCancelNotifier: IJclNotifier;
    property CancelNotifier: IJclNotifier read GetCancelNotifier;
  end;

implementation

end.
