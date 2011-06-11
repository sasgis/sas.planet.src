unit u_TileDownloaderFrontEnd;

interface

uses
  SysUtils,
  i_ConfigDataProvider,
  i_RequestBuilderScript,
  i_TileDownloader,
  i_TileDownloaderConfig,
  u_TileDownloaderBaseCore;

type
  TTileDownloaderFrontEnd = class
  private
    FDownloader: ITileDownloader;
    FRequestBuilderScript: IRequestBuilderScript;
    FTileDownloaderConfig: ITileDownloaderConfig;
    FUseDwn: Boolean;
  public
    constructor Create(AConfig: IConfigDataProvider; AZmpFileName: string);
    destructor Destroy; override;
    procedure Download(AEvent: ITileDownloaderEvent);
    property RequestBuilderScript: IRequestBuilderScript read FRequestBuilderScript;
    property TileDownloaderConfig: ITileDownloaderConfig read FTileDownloaderConfig;
    property UseDwn: Boolean read FUseDwn;
  end;

implementation

{ TTileDownloaderFrontEnd }

constructor TTileDownloaderFrontEnd.Create(AConfig: IConfigDataProvider; AZmpFileName: string);
var
  VParams: IConfigDataProvider;
  VDownloaderStr: string;
begin
  inherited Create;
  FDownloader := nil;
  FRequestBuilderScript := nil;
  FTileDownloaderConfig := nil;
  VParams := AConfig.GetSubItem('params.txt').GetSubItem('PARAMS');
  FUseDwn := VParams.ReadBool('UseDwn', True);
  try
    if FUseDwn then
    begin
      VDownloaderStr := VParams.ReadString('Downloader', 'sasplanet');
      if LowerCase(VDownloaderStr) = 'sasplanet' then
      begin
        FDownloader := TTileDownloaderBaseCore.Create(AConfig, AZmpFileName);
        if Assigned(FDownloader) then
        begin
          FTileDownloaderConfig := FDownloader.TileDownloaderConfig;
          FRequestBuilderScript := FDownloader.RequestBuilderScript;
          FUseDwn := FDownloader.Enabled;
        end
      end;
    end;
  finally
    if FDownloader = nil then
    begin
      FUseDwn := False;
      FTileDownloaderConfig := nil;
      FRequestBuilderScript := nil;
    end;
  end;
end;

destructor TTileDownloaderFrontEnd.Destroy;
begin
  inherited Destroy;
end;

procedure TTileDownloaderFrontEnd.Download(AEvent: ITileDownloaderEvent);
begin
  if FUseDwn and Assigned(AEvent) then
  begin
    if Assigned(FDownloader) then
      FDownloader.Download(AEvent)
    else
    begin
      FUseDwn := False;
      AEvent.ErrorString := 'Downloader not Assigned!';
    end;
  end;
end;

end.
