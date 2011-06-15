unit u_TileDownloaderConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_ProxySettings,
  i_InetConfig,
  i_TileDownloaderConfig,
  u_ConfigDataElementComplexBase;

type
  TTileDownloaderConfig = class(TConfigDataElementComplexBase, ITileDownloaderConfig)
  private
    FIntetConfig: IInetConfig;
    FWaitInterval: Cardinal;
    FIgnoreMIMEType: Boolean;
    FExpectedMIMETypes: string;
    FDefaultMIMEType: string;
    FUserAgentString: string;

    FStatic: ITileDownloaderConfigStatic;
    function CreateStatic: ITileDownloaderConfigStatic;
  protected
    procedure SetChanged; override;
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetProxyConfig: IProxyConfig;
    function GetTimeOut: Cardinal;
    function GetSleepOnResetConnection: Cardinal;
    function GetDownloadTryCount: Integer;

    function GetWaitInterval: Cardinal;
    procedure SetWaitInterval(AValue: Cardinal);

    function GetIgnoreMIMEType: Boolean;
    procedure SetIgnoreMIMEType(AValue: Boolean);

    function GetExpectedMIMETypes: string;
    procedure SetExpectedMIMETypes(AValue: string);

    function GetDefaultMIMEType: string;
    procedure SetDefaultMIMEType(AValue: string);

    function GetUserAgentString: string;
    procedure SetUserAgentString(AValue: string);

    function GetStatic: ITileDownloaderConfigStatic;
  public
    constructor Create(AIntetConfig: IInetConfig);
  end;

implementation

uses
  u_TileDownloaderConfigStatic;

{ TTileDownloaderConfig }

constructor TTileDownloaderConfig.Create(AIntetConfig: IInetConfig);
begin
  inherited Create;
  FIntetConfig := AIntetConfig;
  FWaitInterval := 0;
  FIgnoreMIMEType := False;
  FDefaultMIMEType := 'image/jpg';
  FExpectedMIMETypes := 'image/jpg';
  FUserAgentString := 'Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.1; .NET CLR 2.0.50727)';

  Add(FIntetConfig, nil, False, False, False, True);
end;

function TTileDownloaderConfig.CreateStatic: ITileDownloaderConfigStatic;
begin
  FIntetConfig.LockRead;
  try
  Result :=
    TTileDownloaderConfigStatic.Create(
      FIntetConfig.ProxyConfig.GetStatic,
      FIntetConfig.TimeOut,
      FWaitInterval,
      FIntetConfig.SleepOnResetConnection,
      FIntetConfig.DownloadTryCount,
      FIgnoreMIMEType,
      FExpectedMIMETypes,
      FDefaultMIMEType,
      FUserAgentString
    );
  finally
    FIntetConfig.UnlockRead;
  end;
end;

procedure TTileDownloaderConfig.DoReadConfig(AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FIgnoreMIMEType := AConfigData.ReadBool('IgnoreContentType', FIgnoreMIMEType);
    FDefaultMIMEType := AConfigData.ReadString('DefaultContentType', FDefaultMIMEType);
    FExpectedMIMETypes := AConfigData.ReadString('ContentType', FExpectedMIMETypes);
    FWaitInterval := AConfigData.ReadInteger('Sleep', FWaitInterval);
    FUserAgentString := AConfigData.ReadString('UserAgentString', FUserAgentString);
    SetChanged;
  end;
end;

procedure TTileDownloaderConfig.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  inherited;
end;

function TTileDownloaderConfig.GetDefaultMIMEType: string;
begin
  LockRead;
  try
    Result := FDefaultMIMEType;
  finally
    UnlockRead;
  end;
end;

function TTileDownloaderConfig.GetDownloadTryCount: Integer;
begin
  Result := FStatic.DownloadTryCount;
end;

function TTileDownloaderConfig.GetExpectedMIMETypes: string;
begin
  LockRead;
  try
    Result := FExpectedMIMETypes;
  finally
    UnlockRead;
  end;
end;

function TTileDownloaderConfig.GetIgnoreMIMEType: Boolean;
begin
  LockRead;
  try
    Result := FIgnoreMIMEType;
  finally
    UnlockRead;
  end;
end;

function TTileDownloaderConfig.GetProxyConfig: IProxyConfig;
begin
  Result := FIntetConfig.ProxyConfig;
end;

function TTileDownloaderConfig.GetSleepOnResetConnection: Cardinal;
begin
  Result := FStatic.SleepOnResetConnection;
end;

function TTileDownloaderConfig.GetStatic: ITileDownloaderConfigStatic;
begin
  Result := FStatic;
end;

function TTileDownloaderConfig.GetTimeOut: Cardinal;
begin
  Result := FStatic.TimeOut;
end;

function TTileDownloaderConfig.GetUserAgentString: string;
begin
  LockRead;
  try
    Result := FUserAgentString;
  finally
    UnlockRead;
  end;
end;

function TTileDownloaderConfig.GetWaitInterval: Cardinal;
begin
  LockRead;
  try
    Result := FWaitInterval;
  finally
    UnlockRead;
  end;
end;

procedure TTileDownloaderConfig.SetChanged;
begin
  inherited;
  LockWrite;
  try
    FStatic := CreateStatic;
  finally
    UnlockWrite;
  end;
end;

procedure TTileDownloaderConfig.SetDefaultMIMEType(AValue: string);
begin
  LockWrite;
  try
    if FDefaultMIMEType <> AValue then begin
      FDefaultMIMEType := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TTileDownloaderConfig.SetExpectedMIMETypes(AValue: string);
begin
  LockWrite;
  try
    if FExpectedMIMETypes <> AValue then begin
      FExpectedMIMETypes := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TTileDownloaderConfig.SetIgnoreMIMEType(AValue: Boolean);
begin
  LockWrite;
  try
    if FIgnoreMIMEType <> AValue then begin
      FIgnoreMIMEType := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TTileDownloaderConfig.SetUserAgentString(AValue: string);
begin
  LockWrite;
  try
    if FUserAgentString <> AValue then begin
      FUserAgentString := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TTileDownloaderConfig.SetWaitInterval(AValue: Cardinal);
begin
  LockWrite;
  try
    if FWaitInterval <> AValue then begin
      FWaitInterval := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
