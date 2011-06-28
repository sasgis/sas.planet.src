unit u_TileDownloaderFrontEnd;

interface

uses
  Windows,
  SysUtils,
  i_ConfigDataProvider,
  i_InetConfig,
  i_TileRequestBuilderConfig,
  i_TileDownloader,
  i_TileDownloaderConfig,
  i_ZmpInfo;

type
  TTileDownloaderFrontEnd = class
  private
    FDownloader: ITileDownloader;
    FTileRequestBuilderConfig: ITileRequestBuilderConfig;
    FTileDownloaderConfig: ITileDownloaderConfig;
    FUseDwn: Boolean;
  public
    constructor Create(
      AConfig: IConfigDataProvider;
      AInetConfig: IInetConfig;
      AZmp: IZmpInfo
    );
    destructor Destroy; override;
    function GetTileUrl(ATileXY: TPoint; AZoom: Byte): string;
    procedure Download(AEvent: ITileDownloaderEvent);
    property TileRequestBuilderConfig: ITileRequestBuilderConfig read FTileRequestBuilderConfig;
    property TileDownloaderConfig: ITileDownloaderConfig read FTileDownloaderConfig;
    property UseDwn: Boolean read FUseDwn;
  end;

implementation

uses
  u_TileDownloaderBaseCore;

{ TTileDownloaderFrontEnd }

constructor TTileDownloaderFrontEnd.Create(
  AConfig: IConfigDataProvider;
  AInetConfig: IInetConfig;
  AZmp: IZmpInfo
);
var
  VParams: IConfigDataProvider;
  VDownloaderStr: string;
begin
  inherited Create;
  FDownloader := nil;
  FTileRequestBuilderConfig := nil;
  FTileDownloaderConfig := nil;
  FUseDwn := False;
  try
    VParams := AConfig.GetSubItem('params.txt').GetSubItem('PARAMS');
    VDownloaderStr := VParams.ReadString('Downloader', 'sasplanet');
    if LowerCase(VDownloaderStr) = 'sasplanet' then begin
      FDownloader := TTileDownloaderBaseCore.Create(AConfig, AInetConfig, AZmp);
      if Assigned(FDownloader) then begin
        FTileDownloaderConfig := FDownloader.TileDownloaderConfig;
        FTileRequestBuilderConfig := FDownloader.TileRequestBuilderConfig;
        FUseDwn := FDownloader.Enabled;
      end;
    end;
  finally
    if FDownloader = nil then begin
      FUseDwn := False;
      FTileDownloaderConfig := nil;
      FTileRequestBuilderConfig := nil;
    end;
  end;
end;

destructor TTileDownloaderFrontEnd.Destroy;
begin
  inherited Destroy;
end;

function TTileDownloaderFrontEnd.GetTileUrl(ATileXY: TPoint; AZoom: Byte): string;
begin
  if FUseDwn and Assigned(FDownloader) then begin
    Result := FDownloader.GetTileUrl(ATileXY, AZoom);
  end else begin
    Result := '';
  end;
end;

procedure TTileDownloaderFrontEnd.Download(AEvent: ITileDownloaderEvent);
begin
  if FUseDwn and Assigned(AEvent) then begin
    if Assigned(FDownloader) then begin
      FDownloader.Download(AEvent)
    end else begin
      FUseDwn := False;
      raise Exception.Create('Downloader not Assigned!');
    end;
  end;
end;

end.
