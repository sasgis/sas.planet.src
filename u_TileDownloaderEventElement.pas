unit u_TileDownloaderEventElement;

interface

uses
  Windows,
  Classes,
  SysUtils,
  i_TileDownloader,
  i_TileError,
  u_MapType;

type
  TTileDownloaderEventElement = class(TInterfacedObject, ITileDownloaderEvent)
  private
    FProcessed: Boolean;
    FMapTileUpdateEvent: TMapTileUpdateEvent;
    FErrorLogger: ITileErrorLogger;
    FMapType: TMapType;

    FTileXY: TPoint;
    FTileZoom: Byte;
    FTileSize: Cardinal;
    FCheckTileSize: Boolean;
    FOldTileSize: Cardinal;
    FTileMIME: string;
    FTileStream: TMemoryStream;
    FRawResponseHeader: string;
    FDownloadResult: TDownloadTileResult;
    FErrorString: string;

    FRES_Authorization: string;
    FRES_Ban: string;
    FRES_TileNotExists: string;
    FRES_Noconnectionstointernet: string;
    FRES_TileDownloadContentTypeUnexpcted: string;
    FRES_TileDownloadUnexpectedError: string;

    FCallBackList: TList;

    procedure GuiSync;
    function  GetErrStr(AErr: TDownloadTileResult): string;
  public
    constructor Create(AMapTileUpdateEvent: TMapTileUpdateEvent; AErrorLogger: ITileErrorLogger; AMapType: TMapType);
    destructor Destroy; override;

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
    function  GetErrorString: string;
    procedure SetErrorString(Value: string);

    property TileXY: TPoint read GetTileXY write SetTileXY;
    property TileZoom: Byte read GetTileZoom write SetTileZoom;
    property TileSize: Cardinal read GetTileSize write SetTileSize;
    property CheckTileSize: Boolean read GetCheckTileSize write SetCheckTileSize;
    property OldTileSize: Cardinal read GetOldTileSize write SetOldTileSize;
    property TileMIME: string read GetTileMIME write SetTileMIME;
    property TileStream: TMemoryStream read GetTileStream write SetTileStream;
    property RawResponseHeader: string read GetRawResponseHeader write SetRawResponseHeader;
    property DownloadResult: TDownloadTileResult read GetDwnlResult write SetDwnlResult;
    property ErrorString: string read GetErrorString write SetErrorString;
  end;

implementation

uses
  u_GlobalState,
  u_TileErrorInfo,
  u_ResStrings;

{ TTileDownloaderEventElement }

constructor TTileDownloaderEventElement.Create(AMapTileUpdateEvent: TMapTileUpdateEvent; AErrorLogger: ITileErrorLogger; AMapType: TMapType);
begin
  inherited Create;
  FProcessed := False;
  FMapTileUpdateEvent := AMapTileUpdateEvent;
  FErrorLogger := AErrorLogger;
  FMapType := AMapType;

  FTileXY.X := 0;
  FTileXY.Y := 0;
  FTileZoom := 0;
  FTileSize := 0;
  FCheckTileSize := False;
  FOldTileSize := 0;
  FTileMIME := '';
  FTileStream := TMemoryStream.Create;
  FDownloadResult := dtrUnknownError;
  FErrorString := '';

  FCallBackList := TList.Create;

  FRES_Authorization := SAS_ERR_Authorization;
  FRES_Ban := SAS_ERR_Ban;
  FRES_TileNotExists := SAS_ERR_TileNotExists;
  FRES_Noconnectionstointernet := SAS_ERR_Noconnectionstointernet;
  FRES_TileDownloadContentTypeUnexpcted := SAS_ERR_TileDownloadContentTypeUnexpcted;
  FRES_TileDownloadUnexpectedError := SAS_ERR_TileDownloadUnexpectedError;
end;

destructor TTileDownloaderEventElement.Destroy;
begin
  try
    try
      if not FProcessed then
        ProcessEvent;
      FCallBackList.Clear;
      FreeAndNil(FCallBackList);
    finally
      FreeAndNil(FTileStream);
    end; 
  finally
    inherited Destroy;
  end;
end;

procedure TTileDownloaderEventElement.GuiSync;
begin
  if Addr(FMapTileUpdateEvent) <> nil then
    FMapTileUpdateEvent(FMapType, FTileZoom, FTileXY);
end;

procedure TTileDownloaderEventElement.ProcessEvent;
begin
  try
    try
      TileSize := TileStream.Size;
      ExecCallBackList;
    except
      on E: Exception do
        FErrorString := E.Message;
    end;
    try
      if FErrorString <> '' then begin
        if FErrorLogger <> nil then
            FErrorLogger.LogError( TTileErrorInfo.Create(FMapType, FTileZoom, FTileXY, FErrorString) );
      end else begin
        FErrorString := GetErrStr(FDownloadResult);
        if (FDownloadResult = dtrOK) or (FDownloadResult = dtrSameTileSize) then
          GState.DownloadInfo.Add(1, FTileSize);
        if FErrorString <> '' then begin
          if FErrorLogger <> nil then
            FErrorLogger.LogError( TTileErrorInfo.Create(FMapType, FTileZoom, FTileXY, FErrorString) );
        end else begin
          TThread.Synchronize(nil, GuiSync);
        end;
      end;
    except

    end;
  finally
    FProcessed := True;
  end;
end;

procedure TTileDownloaderEventElement.AddToCallBackList(ACallBack: TOnDownloadCallBack);
var
  VCallBack: POnDownloadCallBack;
begin
  if Assigned(ACallBack) then begin
    if not Assigned(FCallBackList) then
      FCallBackList := TList.Create;
    New(VCallBack);
    TOnDownloadCallBack(VCallBack^) := ACallBack;
    FCallBackList.Add(VCallBack);
  end;
end;

procedure TTileDownloaderEventElement.ExecCallBackList;
var
  i: Integer;
  VCallBack: POnDownloadCallBack;
begin
  if Assigned(FCallBackList) then
  try
    for i := FCallBackList.Count - 1 downto 0 do // !!! FILO
    try
      VCallBack := FCallBackList.Items[i];
      if Assigned(VCallBack) then
      try
        VCallBack^(Self);
      finally
        Dispose(VCallBack);
        VCallBack := nil;
      end;
    except
      // ignore all
    end;
  finally
    FCallBackList.Clear;
  end;
end;

procedure TTileDownloaderEventElement.SetTileXY(Value: TPoint);
begin
  FTileXY := Value;
end;

function TTileDownloaderEventElement.GetTileXY: TPoint;
begin
  Result := FTileXY;
end;

procedure TTileDownloaderEventElement.SetTileZoom(Value: Byte);
begin
  FTileZoom := Value;
end;

function TTileDownloaderEventElement.GetTileZoom: Byte;
begin
  Result := FTileZoom;
end;

procedure TTileDownloaderEventElement.SetTileSize(Value: Cardinal);
begin
  FTileSize := Value;
end;

function TTileDownloaderEventElement.GetTileSize: Cardinal;
begin
  Result := FTileSize;
end;

procedure TTileDownloaderEventElement.SetCheckTileSize(Value: Boolean);
begin
  FCheckTileSize := Value;
end;

function TTileDownloaderEventElement.GetCheckTileSize: Boolean;
begin
  Result := FCheckTileSize;
end;

procedure TTileDownloaderEventElement.SetOldTileSize(Value: Cardinal);
begin
  FOldTileSize := Value;
end;

function TTileDownloaderEventElement.GetOldTileSize: Cardinal;
begin
  Result := FOldTileSize;
end;

procedure TTileDownloaderEventElement.SetTileMIME(Value: string);
begin
  FTileMIME := Value;
end;

function TTileDownloaderEventElement.GetTileMIME: string;
begin
  Result := FTileMIME;
end;

procedure TTileDownloaderEventElement.SetTileStream(Value: TMemoryStream);
begin
  FTileStream := Value;
end;

function TTileDownloaderEventElement.GetTileStream: TMemoryStream;
begin
  Result := FTileStream;
end;

procedure TTileDownloaderEventElement.SetRawResponseHeader(Value: string);
begin
  FRawResponseHeader := Value;
end;

function TTileDownloaderEventElement.GetRawResponseHeader: string;
begin
  Result := FRawResponseHeader;
end;

procedure TTileDownloaderEventElement.SetDwnlResult(Value: TDownloadTileResult);
begin
  FDownloadResult := Value;
end;

function TTileDownloaderEventElement.GetDwnlResult: TDownloadTileResult;
begin
  Result := FDownloadResult;
end;

procedure TTileDownloaderEventElement.SetErrorString(Value: string);
begin
  FErrorString := Value;
end;

function TTileDownloaderEventElement.GetErrorString: string;
begin
  Result := FErrorString;
end;

function TTileDownloaderEventElement.GetErrStr(AErr: TDownloadTileResult): string;
begin
  Result := '';
  case AErr of
    dtrProxyAuthError:
      result := FRES_Authorization;

    dtrBanError:
      result := FRES_Ban;

    dtrTileNotExists:
      result := FRES_TileNotExists;

    dtrDownloadError,
    dtrErrorInternetOpen,
    dtrErrorInternetOpenURL:
      result := FRES_Noconnectionstointernet;

    dtrErrorMIMEType:
      result := FRES_TileDownloadContentTypeUnexpcted;

    dtrUnknownError:
      Result := FRES_TileDownloadUnexpectedError;
  end;
end;

end.
