unit u_TileDownloaderConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_InetConfig,
  i_TileDownloaderConfig,
  u_ConfigDataElementComplexBase;

type
  TTileDownloaderConfig = class(TConfigDataElementComplexBase, ITileDownloaderConfig)
  private
    FDefConfig: ITileDownloaderConfigStatic;
    FIntetConfig: IInetConfig;
    FWaitInterval: Cardinal;
    FMaxConnectToServerCount: Cardinal;
    FIgnoreMIMEType: Boolean;
    FExpectedMIMETypes: string;
    FDefaultMIMEType: string;

    FStatic: ITileDownloaderConfigStatic;
    function CreateStatic: ITileDownloaderConfigStatic;
  protected
    procedure DoBeforeChangeNotify; override;
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetInetConfigStatic: IInetConfigStatic;

    function GetWaitInterval: Cardinal;
    procedure SetWaitInterval(AValue: Cardinal);

    function GetMaxConnectToServerCount: Cardinal;
    procedure SetMaxConnectToServerCount(AValue: Cardinal);

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
  FDefConfig := ADefault;
  FIntetConfig := AIntetConfig;
  FWaitInterval := FDefConfig.WaitInterval;
  FMaxConnectToServerCount := FDefConfig.MaxConnectToServerCount;
  FIgnoreMIMEType := FDefConfig.IgnoreMIMEType;
  FDefaultMIMEType := FDefConfig.DefaultMIMEType;
  FExpectedMIMETypes := FDefConfig.ExpectedMIMETypes;

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
      FMaxConnectToServerCount,
      FIgnoreMIMEType,
      FExpectedMIMETypes,
      FDefaultMIMEType
    );
  finally
    FIntetConfig.UnlockRead;
  end;
end;

procedure TTileDownloaderConfig.DoBeforeChangeNotify;
begin
  inherited;
  LockWrite;
  try
    FStatic := CreateStatic;
  finally
    UnlockWrite;
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
    SetMaxConnectToServerCount(AConfigData.ReadInteger('MaxConnectToServerCount', FMaxConnectToServerCount));
    SetChanged;
  end;
end;

procedure TTileDownloaderConfig.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  if FWaitInterval <> FDefConfig.WaitInterval then begin
    AConfigData.WriteInteger('Sleep', FWaitInterval);
  end else begin
    AConfigData.DeleteValue('Sleep');
  end;

  if FMaxConnectToServerCount <> FDefConfig.MaxConnectToServerCount then begin
    AConfigData.WriteInteger('MaxConnectToServerCount', FMaxConnectToServerCount);
  end else begin
    AConfigData.DeleteValue('MaxConnectToServerCount');
  end;
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

function TTileDownloaderConfig.GetMaxConnectToServerCount: Cardinal;
begin
  LockRead;
  try
    Result := FMaxConnectToServerCount;
  finally
    UnlockRead;
  end;
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

procedure TTileDownloaderConfig.SetMaxConnectToServerCount(AValue: Cardinal);
var
  VValue: Cardinal;
begin
  VValue := AValue;
  if VValue > 64 then begin
    VValue := 64;
  end;
  if VValue <= 0 then begin
    VValue := 1;
  end;

  LockWrite;
  try
    if FMaxConnectToServerCount <> VValue then begin
      FMaxConnectToServerCount := VValue;
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
