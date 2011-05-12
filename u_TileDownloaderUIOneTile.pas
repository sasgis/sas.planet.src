unit u_TileDownloaderUIOneTile;

interface

uses
  Windows,
  Classes,
  Types,
  i_TileError,
  u_MapType;

type
  TTileDownloaderUIOneTile = class(TThread)
  private
    //FErrorString: string;
    FMapTileUpdateEvent: TMapTileUpdateEvent;
    FErrorLogger: ITileErrorLogger;
    FMapType: TMapType;
    FLoadXY: TPoint;
    FZoom: Byte;
    //procedure AfterWriteToFile;
  protected
    procedure Execute; override;
  public
    constructor Create(
      AXY: TPoint;
      AZoom: byte;
      AMapType: TMapType;
      AMapTileUpdateEvent: TMapTileUpdateEvent;
      AErrorLogger: ITileErrorLogger
    ); overload;
  end;

implementation

uses
  SysUtils,
  u_GlobalState,
  u_TileErrorInfo,
  u_ResStrings;

constructor TTileDownloaderUIOneTile.Create(
  AXY: TPoint;
  AZoom: byte;
  AMapType: TMapType;
  AMapTileUpdateEvent: TMapTileUpdateEvent;
  AErrorLogger: ITileErrorLogger
);
begin
  inherited Create(False);
  FMapTileUpdateEvent := AMapTileUpdateEvent;
  FErrorLogger := AErrorLogger;
  FLoadXY := AXY;
  FZoom := AZoom;
  FMapType := AMapType;

  Priority := tpLower;
  FreeOnTerminate := true;
  randomize;
end;
{
procedure TTileDownloaderUIOneTile.AfterWriteToFile;
begin
  if Addr(FMapTileUpdateEvent) <> nil then begin
    FMapTileUpdateEvent(FMapType, FZoom, FLoadXY);
  end;
end;
}

procedure TTileDownloaderUIOneTile.Execute;
{
var
  ty: string;
  fileBuf: TMemoryStream;
  res: TDownloadTileResult;
}
begin
  {
  if FMapType.UseDwn then begin
    FileBuf := TMemoryStream.Create;
    try
      try
        res := FMapType.DownloadTile(Self, FLoadXY, FZoom, false, 0, FLoadUrl, ty, fileBuf);
        VErrorString := GetErrStr(res);
        if (res = dtrOK) or (res = dtrSameTileSize) then begin
          GState.DownloadInfo.Add(1, fileBuf.Size);
        end;
      except
        on E: Exception do begin
          VErrorString := E.Message;
        end;
      end;
    finally
      FileBuf.Free;
    end;
  end else begin
    VErrorString := SAS_ERR_NotLoads;
  end;
  if VErrorString = '' then begin
    Synchronize(AfterWriteToFile);
  end else begin
    FErrorLogger.LogError(
        TTileErrorInfo.Create(
          FMapType,
          FZoom,
          FLoadXY,
          VErrorString
        )
      );
  end;
  }

end;

end.
