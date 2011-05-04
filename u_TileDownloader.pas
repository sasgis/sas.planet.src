unit u_TileDownloader;

interface

uses
  SysUtils,
  SyncObjs,
  i_ConfigDataProvider,
  i_RequestBuilderScript,
  i_TileDownloader;

type
  TTileDownloader = class(TInterfacedObject, ITileDownloader)
  protected
    FEnabled: Boolean;
    FMapName: string;
    FZmpFileName: string;
    FMaxConnectToServerCount: Cardinal;
    FWaitInterval: Cardinal;
    FTimeOut: Cardinal;
    FDownloadTryCount: Byte;
    //FSleepOnResetConnection
    FRequestBuilderScript: IRequestBuilderScript;
    FCS: TCriticalSection;
    procedure Lock;
    procedure UnLock;
  public
    constructor Create(AConfig: IConfigDataProvider; AZmpFileName: string);
    destructor Destroy; override;
    procedure Download(AEvent: ITileDownloaderEvent); virtual;
    function  GetWaitInterval: Cardinal;
    procedure SetWaitInterval(AValue: Cardinal);
    function  GetRequestBuilderScript: IRequestBuilderScript;
    function  GetIsEnabled: Boolean;                          
    property  WaitInterval: Cardinal read GetWaitInterval write SetWaitInterval;
    property  RequestBuilderScript: IRequestBuilderScript read GetRequestBuilderScript;
    property  Enabled: Boolean read GetIsEnabled;
  end;

implementation

uses
  u_GlobalState;

{ TTileDownloader }

constructor TTileDownloader.Create(AConfig: IConfigDataProvider; AZmpFileName: string);
var
  VParams: IConfigDataProvider;
begin
  inherited Create;
  FRequestBuilderScript := nil;
  FZmpFileName := AZmpFileName;
  FCS := TCriticalSection.Create;
  FEnabled := False;
  FTimeOut := GState.InetConfig.GetTimeOut;
  if GState.TwoDownloadAttempt then
    FDownloadTryCount := 2
  else
    FDownloadTryCount := 1;
  VParams := AConfig.GetSubItem('params.txt').GetSubItem('PARAMS');
  FWaitInterval := VParams.ReadInteger('Sleep', 0);
  FMapName := VParams.ReadString('name', '');
  FMapName:=VParams.ReadString('name_'+GState.LanguageManager.GetCurrentLanguageCode, FMapName);
  FMaxConnectToServerCount := VParams.ReadInteger('MaxConnectToServerCount', 1);
  if FMaxConnectToServerCount > 64 then
    FMaxConnectToServerCount := 64;
end;

destructor TTileDownloader.Destroy;
begin
  FRequestBuilderScript := nil;
  FreeAndNil(FCS);
  inherited Destroy;
end;

procedure TTileDownloader.Download(AEvent: ITileDownloaderEvent);
begin
  // virtual
end;

procedure TTileDownloader.SetWaitInterval(AValue: Cardinal);
begin
  Lock;
  try
    FWaitInterval := AValue;
  finally
    Unlock;
  end;
end;

function TTileDownloader.GetWaitInterval: Cardinal;
begin
  Lock;
  try
    Result := FWaitInterval;
  finally
    Unlock;
  end;
end;

function TTileDownloader.GetRequestBuilderScript: IRequestBuilderScript;
begin
  Lock;
  try
    Result := FRequestBuilderScript;
  finally
    Unlock;
  end;
end;

function TTileDownloader.GetIsEnabled: Boolean;
begin
  Lock;
  try
    Result := FEnabled;
  finally
    Unlock;
  end;
end;

procedure TTileDownloader.Lock;
begin
  FCS.Acquire;
end;

procedure TTileDownloader.UnLock;
begin
  FCS.Release;
end;

end.
