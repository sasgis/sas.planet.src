unit u_TileDownloaderFrontEnd;

interface

uses
  SysUtils,
  i_ConfigDataProvider,
  i_TileDownloader,
  u_TileDownloaderBaseCore;

type
  TTileDownloaderFrontEnd = class
  private
    FDownloader: ITileDownloader;
    FUseDwn: Boolean;
    FMaxConnectToServerCount: Cardinal;
    function GetWaitInterval: Cardinal;
    procedure SetWaitInterval(Value: Cardinal);
  public
    constructor Create(AConfig: IConfigDataProvider; AZMPFileName: string);
    destructor Destroy; override;
    procedure Download(AEvent: ITileDownloaderEvent);
    property WaitInterval: Cardinal read GetWaitInterval write SetWaitInterval;
  end;

implementation

{ TTileDownloaderFrontEnd }

constructor TTileDownloaderFrontEnd.Create(AConfig: IConfigDataProvider; AZMPFileName: string);
var
  VParams: IConfigDataProvider;
  VDownloaderStr: string;
begin
  inherited Create;
  VParams := AConfig.GetSubItem('params.txt').GetSubItem('PARAMS');
  FMaxConnectToServerCount := VParams.ReadInteger('MaxConnectToServerCount', 1);
  if FMaxConnectToServerCount > 64 then
    FMaxConnectToServerCount := 64;
  FUseDwn := VParams.ReadBool('UseDwn', True);
  VDownloaderStr := VParams.ReadString('Downloader', 'sasplanet');
  FDownloader := nil;
  if LowerCase(VDownloaderStr) = 'sasplanet' then
  begin
    FDownloader := TTileDownloaderBaseCore.Create(AConfig, AZMPFileName);
  end
end;

destructor TTileDownloaderFrontEnd.Destroy;
begin
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
