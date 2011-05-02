unit i_TileDownloader;

interface

uses
  Windows,
  Classes;

type
  TDownloadTileResult = (dtrOK, dtrSameTileSize, dtrErrorInternetOpen, dtrErrorInternetOpenURL, dtrProxyAuthError, dtrErrorMIMEType, dtrDownloadError, dtrTileNotExists, dtrBanError, dtrUnknownError);
  
  ITileDownloaderEvent = interface;

  TOnDownloadCallBack = procedure (AEvent: ITileDownloaderEvent) of object;

  ITileDownloader = interface
    ['{EAF443E6-FC84-46A3-95AA-8217117A2A6B}']
    procedure Download(AEvent: ITileDownloaderEvent);
    function  GetWaitInterval: Cardinal;
    procedure SetWaitInterval(AValue: Cardinal);
    property  WaitInterval: Cardinal read GetWaitInterval write SetWaitInterval;
  end;
  
  ITileDownloaderEvent = interface
    ['{6AF695C6-FBCF-49FD-BDDE-04C4568D31F7}']
    procedure ProcessEvent;

    procedure AddToCallBackList(ACallBack: TOnDownloadCallBack);
    procedure ExecCallBackList;

    function  GetTileXY: TPoint;
    procedure SetTileXY(Value: TPoint);
    function  GetTileZoom: Byte;
    procedure SetTileZoom(Value: Byte);
    function  GetTileSize: Cardinal;
    procedure SetTileSize(Value: Cardinal);
    function  GetCheckTileSize: Boolean;
    procedure SetCheckTileSize(Value: Boolean);
    function  GetOldTileSize: Cardinal;
    procedure SetOldTileSize(Value: Cardinal);
    function  GetTileMIME: string;
    procedure SetTileMIME(Value: string);
    function  GetTileStream: TMemoryStream;
    procedure SetTileStream(Value: TMemoryStream);
    function  GetRawResponseHeader: string;
    procedure SetRawResponseHeader(Value: string);
    function  GetDwnlResult: TDownloadTileResult;
    procedure SetDwnlResult(Value: TDownloadTileResult);

    property TileXY: TPoint read GetTileXY write SetTileXY;
    property TileZoom: Byte read GetTileZoom write SetTileZoom;
    property TileSize: Cardinal read GetTileSize write SetTileSize;
    property CheckTileSize: Boolean read GetCheckTileSize write SetCheckTileSize;
    property OldTileSize: Cardinal read GetOldTileSize write SetOldTileSize;
    property TileMIME: string read GetTileMIME write SetTileMIME;
    property TileStream: TMemoryStream read GetTileStream write SetTileStream;
    property RawResponseHeader: string read GetRawResponseHeader write SetRawResponseHeader;
    property DownloadResult: TDownloadTileResult read GetDwnlResult write SetDwnlResult;
  end;

implementation

end.
