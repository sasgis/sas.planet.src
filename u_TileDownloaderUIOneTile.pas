unit u_TileDownloaderUIOneTile;

interface

uses
  Windows,
  Classes,
  Types,
  u_TileDownloaderThreadBase,
  u_MapLayerShowError,
  u_MapType;

type
  TTileDownloaderUIOneTile = class(TTileDownloaderThreadBase)
  private
    FErrorString: string;

    FMapTileUpdateEvent: TMapTileUpdateEvent;
    FErrorShowLayer: TTileErrorInfoLayer;

    procedure AfterWriteToFile;
  protected
    procedure Execute; override;
  public
    constructor Create(
      AXY: TPoint;
      AZoom: byte;
      AMapType: TMapType;
      AMapTileUpdateEvent: TMapTileUpdateEvent;
      AErrorShowLayer: TTileErrorInfoLayer
    ); overload;
  end;

implementation

uses
  SysUtils,
  u_GlobalState,
  i_TileDownlodSession,
  u_ResStrings;

constructor TTileDownloaderUIOneTile.Create(
  AXY: TPoint;
  AZoom: byte;
  AMapType: TMapType;
  AMapTileUpdateEvent: TMapTileUpdateEvent;
  AErrorShowLayer: TTileErrorInfoLayer
);
begin
  inherited Create(False);
  FMapTileUpdateEvent := AMapTileUpdateEvent;
  FErrorShowLayer := AErrorShowLayer;
  FLoadXY := AXY;
  FZoom := AZoom;
  FMapType := AMapType;

  Priority := tpLower;
  FreeOnTerminate := true;
  randomize;
end;

procedure TTileDownloaderUIOneTile.AfterWriteToFile;
begin
  if FErrorString <> '' then begin
    if FErrorShowLayer <> nil then begin
      FErrorShowLayer.ShowError(FLoadXY, FZoom, FMapType, FErrorString);
    end;
  end else begin
    if FErrorShowLayer <> nil then begin
      FErrorShowLayer.SetNoError(FLoadXY, FZoom, FMapType);
    end;
    if Addr(FMapTileUpdateEvent) <> nil then begin
      FMapTileUpdateEvent(FMapType, FZoom, FLoadXY);
    end;
  end;
end;

procedure TTileDownloaderUIOneTile.Execute;
var
  ty: string;
  fileBuf: TMemoryStream;
  res: TDownloadTileResult;
begin
  if FMapType.UseDwn then begin
    FileBuf := TMemoryStream.Create;
    try
      try
        res := FMapType.DownloadTile(Self, FLoadXY, FZoom, false, 0, FLoadUrl, ty, fileBuf);
        FErrorString := GetErrStr(res);
        if (res = dtrOK) or (res = dtrSameTileSize) then begin
          GState.DownloadInfo.Add(1, fileBuf.Size);
        end;
      except
        on E: Exception do begin
          FErrorString := E.Message;
        end;
      end;
    finally
      FileBuf.Free;
    end;
  end else begin
    FErrorString := SAS_ERR_NotLoads;
  end;
  Synchronize(AfterWriteToFile);
end;

end.
