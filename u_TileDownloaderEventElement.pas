unit u_TileDownloaderEventElement;

interface

uses
  Windows,
  i_TileDownlodSession,
  u_MapLayerShowError,
  u_MapType;

type
  TTileDownloaderEventElement = class
  private
    FMapTileUpdateEvent: TMapTileUpdateEvent;
    FErrorShowLayer: TTileErrorInfoLayer;
    FMapType: TMapType;
    FTileXY: TPoint;
    FTileZoom: Byte;
    FTileSize: Int64;
    FDownloadResult: TDownloadTileResult;

    function GetErrStr(AErr: TDownloadTileResult): string;
  public
    constructor Create(AMapTileUpdateEvent: TMapTileUpdateEvent; AErrorShowLayer: TTileErrorInfoLayer);
    procedure Process;

    property MapType: TMapType write FMapType;
    property TileXY: TPoint write FTileXY;
    property TileZoom: Byte write FTileZoom;
    property TileSize: Int64 write FTileSize;
    property DownloadResult: TDownloadTileResult write FDownloadResult;
  end;

implementation

uses
  u_GlobalState,
  u_ResStrings;

{ TTileDownloaderEventElement }

constructor TTileDownloaderEventElement.Create(AMapTileUpdateEvent: TMapTileUpdateEvent; AErrorShowLayer: TTileErrorInfoLayer);
begin
  inherited Create;
  FMapTileUpdateEvent := AMapTileUpdateEvent;
  FErrorShowLayer := AErrorShowLayer;
end;

procedure TTileDownloaderEventElement.Process;
var
  VErrorString: string;
begin
  VErrorString := GetErrStr(FDownloadResult);
  if (FDownloadResult = dtrOK) or (FDownloadResult = dtrSameTileSize) then begin
    GState.DownloadInfo.Add(1, FTileSize);
  end;
  if VErrorString <> '' then begin
    if FErrorShowLayer <> nil then begin
      FErrorShowLayer.ShowError(FTileXY, FTileZoom, FMapType, VErrorString);
    end;
  end else begin
    if FErrorShowLayer <> nil then begin
      FErrorShowLayer.Visible := False;
    end;
    if Addr(FMapTileUpdateEvent) <> nil then begin
      FMapTileUpdateEvent(FMapType, FTileZoom, FTileXY);
    end;
  end;
end;

function TTileDownloaderEventElement.GetErrStr(AErr: TDownloadTileResult): string;
begin
  case AErr of
    dtrProxyAuthError:
    begin
      result := SAS_ERR_Authorization;
    end;
    dtrBanError:
    begin
      result := SAS_ERR_Ban;
    end;
    dtrTileNotExists:
    begin
      result := SAS_ERR_TileNotExists;
    end;
    dtrDownloadError,
    dtrErrorInternetOpen,
    dtrErrorInternetOpenURL:
    begin
      result := SAS_ERR_Noconnectionstointernet;
    end;
    dtrErrorMIMEType:
    begin
      result := SAS_ERR_TileDownloadContentTypeUnexpcted;
    end;
    dtrUnknownError:
    begin
      Result := SAS_ERR_TileDownloadUnexpectedError;
    end else begin
    result := '';
  end;
  end;
end;

end.
