unit u_TileDownloaderUIOneTile;

interface

uses
  Windows,
  Classes,
  Types,
  t_LoadEvent,
  u_TileDownloaderThreadBase,
  UMapType;

type
  TTileDownloaderUIOneTile = class(TTileDownloaderThreadBase)
  private
    FLastLoad: TlastLoad;
    FErrorString: string;
    procedure AfterWriteToFile;
  protected
    procedure Execute; override;
  public
    constructor Create(AXY: TPoint; AZoom: byte; AMapType: TMapType); overload;
  end;

implementation

uses
  SysUtils,
  u_GlobalState,
  i_ITileDownlodSession,
  UResStrings,
  Unit1;

constructor TTileDownloaderUIOneTile.Create(AXY: TPoint; AZoom: byte; AMapType: TMapType);
begin
  inherited Create(False);
  FLoadXY := AXY;
  FZoom := AZoom;
  FMapType := AMapType;

  Priority := tpLower;
  FreeOnTerminate := true;
  randomize;
end;

procedure TTileDownloaderUIOneTile.AfterWriteToFile;
begin
  Fmain.generate_im(FLastLoad, FErrorString);
end;

procedure TTileDownloaderUIOneTile.Execute;
var
  ty: string;
  fileBuf: TMemoryStream;
  res: TDownloadTileResult;
begin
  Flastload.TilePos.X := FLoadXY.X;
  Flastload.TilePos.Y := FLoadXY.Y;
  Flastload.Zoom := Fzoom;
  FlastLoad.mt := FMapType;
  FlastLoad.use := true;
  if FMapType.UseDwn then begin
    FileBuf := TMemoryStream.Create;
    try
      try
        res := FMapType.DownloadTile(Self, FLoadXY, FZoom, false, 0, FLoadUrl, ty, fileBuf);
        FErrorString := GetErrStr(res);
        if (res = dtrOK) or (res = dtrSameTileSize) then begin
          GState.IncrementDownloaded(fileBuf.Size / 1024, 1);
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
