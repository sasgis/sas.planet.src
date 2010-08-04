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
    constructor Create(AXY: TPoint; AZoom: byte; MT:TMapType);overload;
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  u_GlobalState,
  i_ITileDownlodSession,
  UResStrings,
  Unit1;

constructor TTileDownloaderUIOneTile.Create(AXY: TPoint; AZoom: byte; MT:TMapType);
begin
  inherited Create(False);
  FLoadXY := AXY;
  FZoom := AZoom;
  FTypeMap := MT;

  Priority := tpLower;
  FreeOnTerminate := true;
  randomize;
end;

destructor TTileDownloaderUIOneTile.Destroy;
begin
  inherited;
end;

procedure TTileDownloaderUIOneTile.AfterWriteToFile;
begin
  if (Fmain.Enabled)and(not(Fmain.MapMoving))and(not(FMain.MapZoomAnimtion=1)) then begin
    Fmain.generate_im(FLastLoad, FErrorString);
  end;
end;

procedure TTileDownloaderUIOneTile.Execute;
var
  ty: string;
  fileBuf:TMemoryStream;
  res: TDownloadTileResult;
begin
  Flastload.TilePos.X := FLoadXY.X;
  Flastload.TilePos.Y := FLoadXY.Y;
  Flastload.Zoom := Fzoom;
  FlastLoad.mt := Ftypemap;
  FlastLoad.use :=true;
  if FTypeMap.UseDwn then begin
    FileBuf:=TMemoryStream.Create;
    try
      try
        res :=FTypeMap.DownloadTile(Self, FLoadXY.X shl 8, FLoadXY.Y shl 8, FZoom + 1, false, 0, FLoadUrl, ty, fileBuf);
        FErrorString:=GetErrStr(res);
        if (res = dtrOK) or (res = dtrSameTileSize) then begin
          GState.IncrementDownloaded(fileBuf.Size/1024, 1);
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
    FErrorString:=SAS_ERR_NotLoads;
  end;
  Synchronize(AfterWriteToFile);
end;

end.
