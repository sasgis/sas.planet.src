unit u_MapSvcScanConfig;

interface

uses
  i_PathConfig,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_MapSvcScanConfig,
  u_ConfigDataElementBase;

type
  TMapSvcScanConfig = class(TConfigDataElementBaseEmptySaveLoad, IMapSvcScanConfig)
  private
    FMapSvcScanPath: IPathConfig;
    FUseStorage: Boolean;
    FShowOnlyNew: Boolean;
    FMakeOnlyNew: Boolean;
    FOldAfterDays: Integer;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetPath: IPathConfig;

    function GetUseStorage: Boolean;
    procedure SetUseStorage(const AValue: Boolean);

    function GetShowOnlyNew: Boolean;
    procedure SetShowOnlyNew(const AValue: Boolean);

    function GetMakeOnlyNew: Boolean;
    procedure SetMakeOnlyNew(const AValue: Boolean);

    function GetOldAfterDays: Integer;
    procedure SetOldAfterDays(const AValue: Integer);
  public
    constructor Create(const AMapSvcScanPath: IPathConfig);
  end;

implementation

{ TMapSvcScanConfig }

constructor TMapSvcScanConfig.Create(const AMapSvcScanPath: IPathConfig);
begin
  inherited Create;
  FMapSvcScanPath := AMapSvcScanPath;
  FUseStorage := TRUE;
  FShowOnlyNew := FALSE;
  FMakeOnlyNew := TRUE;
  FOldAfterDays := 1;
end;

procedure TMapSvcScanConfig.DoReadConfig(
  const AConfigData: IConfigDataProvider
);
begin
  inherited;
  if AConfigData <> nil then begin
    SetUseStorage(AConfigData.ReadBool('UseStorage', FUseStorage));
    SetShowOnlyNew(AConfigData.ReadBool('ShowOnlyNew', FShowOnlyNew));
    SetMakeOnlyNew(AConfigData.ReadBool('MakeOnlyNew', FMakeOnlyNew));
    SetOldAfterDays(AConfigData.ReadInteger('OldAfterDays', FOldAfterDays));
  end;
end;

procedure TMapSvcScanConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
begin
  inherited;
  AConfigData.WriteBool('UseStorage', FUseStorage);
  AConfigData.WriteBool('ShowOnlyNew', FShowOnlyNew);
  AConfigData.WriteBool('MakeOnlyNew', FMakeOnlyNew);
  AConfigData.WriteInteger('OldAfterDays', FOldAfterDays);
end;

function TMapSvcScanConfig.GetMakeOnlyNew: Boolean;
begin
  LockRead;
  try
    Result := FMakeOnlyNew;
  finally
    UnlockRead;
  end;
end;

function TMapSvcScanConfig.GetOldAfterDays: Integer;
begin
  LockRead;
  try
    Result := FOldAfterDays;
  finally
    UnlockRead;
  end;
end;

function TMapSvcScanConfig.GetPath: IPathConfig;
begin
  // never changed!
  Result := FMapSvcScanPath;
end;

function TMapSvcScanConfig.GetShowOnlyNew: Boolean;
begin
  LockRead;
  try
    Result := FShowOnlyNew;
  finally
    UnlockRead;
  end;
end;

function TMapSvcScanConfig.GetUseStorage: Boolean;
begin
  LockRead;
  try
    Result := FUseStorage;
  finally
    UnlockRead;
  end;
end;

procedure TMapSvcScanConfig.SetMakeOnlyNew(const AValue: Boolean);
begin
  LockWrite;
  try
    if FMakeOnlyNew <> AValue then begin
      FMakeOnlyNew := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMapSvcScanConfig.SetOldAfterDays(const AValue: Integer);
begin
  LockWrite;
  try
    if FOldAfterDays <> AValue then begin
      FOldAfterDays := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMapSvcScanConfig.SetShowOnlyNew(const AValue: Boolean);
begin
  LockWrite;
  try
    if FShowOnlyNew <> AValue then begin
      FShowOnlyNew := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMapSvcScanConfig.SetUseStorage(const AValue: Boolean);
begin
  LockWrite;
  try
    if FUseStorage <> AValue then begin
      FUseStorage := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
