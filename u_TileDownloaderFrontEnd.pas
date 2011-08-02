unit u_TileDownloaderFrontEnd;

interface

uses
  Windows,
  SysUtils,
  i_ConfigDataProvider,
  i_CoordConverterFactory,
  i_LanguageManager,
  i_TileRequestBuilderConfig,
  i_TileDownloader,
  i_TileDownloaderConfig,
  i_ZmpInfo;

type
  TTileDownloaderFrontEnd = class
  private
    FDownloader: ITileDownloader;
    FUseDwn: Boolean;
  public
    constructor Create(
      AConfig: IConfigDataProvider;
      ATileDownloaderConfig: ITileDownloaderConfig;
      ATileRequestBuilderConfig: ITileRequestBuilderConfig;
      AZmp: IZmpInfo;
      ACoordConverterFactory: ICoordConverterFactory;
      ALangManager: ILanguageManager
    );
    destructor Destroy; override;
    procedure Download(AEvent: ITileDownloaderEvent);
    property UseDwn: Boolean read FUseDwn;
  end;

implementation

uses
  u_TileDownloaderBaseCore;

{ TTileDownloaderFrontEnd }

constructor TTileDownloaderFrontEnd.Create(
  AConfig: IConfigDataProvider;
  ATileDownloaderConfig: ITileDownloaderConfig;
  ATileRequestBuilderConfig: ITileRequestBuilderConfig;
  AZmp: IZmpInfo;
  ACoordConverterFactory: ICoordConverterFactory;
  ALangManager: ILanguageManager
);
var
  VParams: IConfigDataProvider;
  VDownloaderStr: string;
begin
  inherited Create;
  FDownloader := nil;
  FUseDwn := False;
  try
    VParams := AConfig.GetSubItem('params.txt').GetSubItem('PARAMS');
    VDownloaderStr := VParams.ReadString('Downloader', 'sasplanet');
    if LowerCase(VDownloaderStr) = 'sasplanet' then begin
      FDownloader := TTileDownloaderBaseCore.Create(
        AConfig,
        ATileDownloaderConfig,
        ATileRequestBuilderConfig,
        AZmp,
        ACoordConverterFactory,
        ALangManager
      );
      if Assigned(FDownloader) then begin
        FUseDwn := FDownloader.Enabled;
      end;
    end;
  finally
    if FDownloader = nil then begin
      FUseDwn := False;
    end;
  end;
end;

destructor TTileDownloaderFrontEnd.Destroy;
begin
  inherited Destroy;
end;

procedure TTileDownloaderFrontEnd.Download(AEvent: ITileDownloaderEvent);
begin
  if FUseDwn and Assigned(AEvent) then begin
    if Assigned(FDownloader) then begin
      FDownloader.Download(AEvent)
    end else begin
      FUseDwn := False;
    end;
  end;
end;

end.
