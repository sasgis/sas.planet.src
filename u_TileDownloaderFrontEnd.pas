unit u_TileDownloaderFrontEnd;

interface

uses
  SysUtils,
  i_ConfigDataProvider,
  i_RequestBuilderScript,
  i_TileDownloader,
  u_TileDownloaderBaseCore;

type
  TTileDownloaderFrontEnd = class
  private
    FDownloader: ITileDownloader;
    FRequestBuilderScript: IRequestBuilderScript;
    FDownloaderStr: string;
    FUseDwn: Boolean;
    function GetWaitInterval: Cardinal;
    procedure SetWaitInterval(Value: Cardinal);
  public
    constructor Create(AConfig: IConfigDataProvider; AZmpFileName: string);
    destructor Destroy; override;
    procedure Download(AEvent: ITileDownloaderEvent);
    property RequestBuilderScript: IRequestBuilderScript read FRequestBuilderScript;
    property WaitInterval: Cardinal read GetWaitInterval write SetWaitInterval;
    property UseDwn: Boolean read FUseDwn;
  end;

implementation

{ TTileDownloaderFrontEnd }

constructor TTileDownloaderFrontEnd.Create(AConfig: IConfigDataProvider; AZmpFileName: string);
var
  VParams: IConfigDataProvider;
begin
  inherited Create;
  FDownloader := nil;
  FRequestBuilderScript := nil;
  VParams := AConfig.GetSubItem('params.txt').GetSubItem('PARAMS');
  FUseDwn := VParams.ReadBool('UseDwn', True);
  try
    if FUseDwn then
    begin
      FDownloaderStr := VParams.ReadString('Downloader', 'sasplanet');
      if LowerCase(FDownloaderStr) = 'sasplanet' then
      begin
        FDownloader := TTileDownloaderBaseCore.Create(AConfig, AZmpFileName);
        FRequestBuilderScript := FDownloader.RequestBuilderScript;
        FUseDwn := FDownloader.Enabled;
      end;
    end;
  finally
    if FDownloader = nil then
      FUseDwn := False;
  end;
end;

destructor TTileDownloaderFrontEnd.Destroy;
begin
  FRequestBuilderScript := nil;
  FDownloader := nil;
  inherited Destroy;
end;

procedure TTileDownloaderFrontEnd.Download(AEvent: ITileDownloaderEvent);
begin
  if Assigned(FDownloader) then
    FDownloader.Download(AEvent);
end;

procedure TTileDownloaderFrontEnd.SetWaitInterval(Value: Cardinal);
begin
  if Assigned(FDownloader) then
    FDownloader.WaitInterval := Value;
end;

function TTileDownloaderFrontEnd.GetWaitInterval: Cardinal;
begin
  if Assigned(FDownloader) then
    Result := FDownloader.WaitInterval
  else
    Result := 0;
end;

end.
