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

    FStatic: ITileDownloaderConfigStatic;
    function CreateStatic: ITileDownloaderConfigStatic;
  protected
    procedure SetChanged; override;
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetInetConfigStatic: IInetConfigStatic;

    function GetWaitInterval: Cardinal;
    procedure SetWaitInterval(AValue: Cardinal);

    function GetIgnoreMIMEType: Boolean;
    procedure SetIgnoreMIMEType(AValue: Boolean);

    function GetExpectedMIMETypes: string;
    procedure SetExpectedMIMETypes(AValue: string);

    function GetDefaultMIMEType: string;
    procedure SetDefaultMIMEType(AValue: string);

    function GetStatic: ITileDownloaderConfigStatic;
  public
    constructor Create(AIntetConfig: IInetConfig; ADefault: ITileDownloaderConfigStatic);
  end;

implementation

uses
  u_TileDownloaderConfigStatic;

{ TTileDownloaderConfig }

constructor TTileDownloaderConfig.Create(AIntetConfig: IInetConfig; ADefault: ITileDownloaderConfigStatic);
begin
  inherited Create;
  FIntetConfig := AIntetConfig;
  FWaitInterval := ADefault.WaitInterval;
  FIgnoreMIMEType := ADefault.IgnoreMIMEType;
  FDefaultMIMEType := ADefault.DefaultMIMEType;
  FExpectedMIMETypes := ADefault.ExpectedMIMETypes;

  Add(FIntetConfig, nil, False, False, False, True);
end;

function TTileDownloaderConfig.CreateStatic: ITileDownloaderConfigStatic;
begin
  FIntetConfig.LockRead;
  try
  Result :=
    TTileDownloaderConfigStatic.Create(
      FIntetConfig.GetStatic,
      FWaitInterval,
      FIgnoreMIMEType,
      FExpectedMIMETypes,
      FDefaultMIMEType
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

function TTileDownloaderConfig.GetInetConfigStatic: IInetConfigStatic;
begin
  Result := FIntetConfig.GetStatic;
end;

function TTileDownloaderConfig.GetStatic: ITileDownloaderConfigStatic;
begin
  Result := FStatic;
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
