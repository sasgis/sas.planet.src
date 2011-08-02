unit u_GSMGeoCodeConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_GSMGeoCodeConfig,
  u_ConfigDataElementBase;

type
  TGSMGeoCodeConfig = class(TConfigDataElementBase, IGSMGeoCodeConfig)
  private
    FUseGSMByCOM: Boolean;
    FPortName: string;
    FBaudRate: Cardinal;
    FWaitTime: Cardinal;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetUseGSMByCOM: Boolean;
    procedure SetUseGSMByCOM(const AValue: Boolean);

    function GetPortName: string;
    procedure SetPortName(const AValue: string);

    function GetBaudRate: Cardinal;
    procedure SetBaudRate(const AValue: Cardinal);

    function GetWaitTime: Cardinal;
    procedure SetWaitTime(const AValue: Cardinal);
  public
    constructor Create();
  end;

implementation

{ TGSMGeoCodeConfig }

constructor TGSMGeoCodeConfig.Create;
begin
  inherited;
  FUseGSMByCOM := True;
  FPortName := 'COM1';
  FBaudRate := 4800;
  FWaitTime := 200;
end;

procedure TGSMGeoCodeConfig.DoReadConfig(AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FUseGSMByCOM := AConfigData.ReadBool('UseGSMByCOM', FUseGSMByCOM);
    FPortName := AConfigData.ReadString('Port', FPortName);
    FBaudRate := AConfigData.ReadInteger('BaudRate', FBaudRate);
    FWaitTime := AConfigData.ReadInteger('WaitTime', FWaitTime);
    SetChanged;
  end;
end;

procedure TGSMGeoCodeConfig.DoWriteConfig(AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteBool('UseGSMByCOM', FUseGSMByCOM);
  AConfigData.WriteString('Port', FPortName);
  AConfigData.WriteInteger('BaudRate', FBaudRate);
  AConfigData.WriteInteger('WaitTime', FWaitTime);
end;

function TGSMGeoCodeConfig.GetBaudRate: Cardinal;
begin
  LockRead;
  try
    Result := FBaudRate;
  finally
    UnlockRead;
  end;
end;

function TGSMGeoCodeConfig.GetPortName: string;
begin
  LockRead;
  try
    Result := FPortName;
  finally
    UnlockRead;
  end;
end;

function TGSMGeoCodeConfig.GetUseGSMByCOM: Boolean;
begin
  LockRead;
  try
    Result := FUseGSMByCOM;
  finally
    UnlockRead;
  end;
end;

function TGSMGeoCodeConfig.GetWaitTime: Cardinal;
begin
  LockRead;
  try
    Result := FWaitTime;
  finally
    UnlockRead;
  end;
end;

procedure TGSMGeoCodeConfig.SetBaudRate(const AValue: Cardinal);
begin
  LockWrite;
  try
    if FBaudRate <> AValue then begin
      FBaudRate := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TGSMGeoCodeConfig.SetPortName(const AValue: string);
begin
  LockWrite;
  try
    if FPortName <> AValue then begin
      FPortName := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TGSMGeoCodeConfig.SetUseGSMByCOM(const AValue: Boolean);
begin
  LockWrite;
  try
    if FUseGSMByCOM <> AValue then begin
      FUseGSMByCOM := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TGSMGeoCodeConfig.SetWaitTime(const AValue: Cardinal);
begin
  LockWrite;
  try
    if FWaitTime <> AValue then begin
      FWaitTime := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
