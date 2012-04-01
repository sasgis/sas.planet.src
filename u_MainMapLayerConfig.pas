unit u_MainMapLayerConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_ThreadConfig,
  i_MainMapLayerConfig,
  u_ConfigDataElementComplexBase;

type
  TMainMapLayerConfig = class(TConfigDataElementComplexBase, IMainMapLayerConfig)
  private
    FUsePrevZoomAtMap: Boolean;
    FUsePrevZoomAtLayer: Boolean;
    FThreadConfig: IThreadConfig;
  private
    function GetUsePrevZoomAtMap: Boolean;
    procedure SetUsePrevZoomAtMap(const AValue: Boolean);

    function GetUsePrevZoomAtLayer: Boolean;
    procedure SetUsePrevZoomAtLayer(const AValue: Boolean);

    function GetThreadConfig: IThreadConfig;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  public
    constructor Create();
  end;

implementation

uses
  Classes,
  SysUtils,
  u_ConfigSaveLoadStrategyBasicUseProvider,
  u_ThreadConfig;

{ TMainMapLayerConfig }

constructor TMainMapLayerConfig.Create;
begin
  inherited Create;

  FUsePrevZoomAtMap := True;
  FUsePrevZoomAtLayer := True;

  FThreadConfig := TThreadConfig.Create(tpNormal);
  Add(FThreadConfig, TConfigSaveLoadStrategyBasicUseProvider.Create);
end;

procedure TMainMapLayerConfig.DoReadConfig(AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FUsePrevZoomAtMap := AConfigData.ReadBool('UsePrevZoomAtMap', FUsePrevZoomAtMap);
    FUsePrevZoomAtLayer := AConfigData.ReadBool('UsePrevZoomAtLayer', FUsePrevZoomAtLayer);
    SetChanged;
  end;
end;

procedure TMainMapLayerConfig.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteBool('UsePrevZoomAtMap', FUsePrevZoomAtMap);
  AConfigData.WriteBool('UsePrevZoomAtLayer', FUsePrevZoomAtLayer);
end;

function TMainMapLayerConfig.GetThreadConfig: IThreadConfig;
begin
  Result := FThreadConfig;
end;

function TMainMapLayerConfig.GetUsePrevZoomAtLayer: Boolean;
begin
  LockRead;
  try
    Result := FUsePrevZoomAtLayer;
  finally
    UnlockRead;
  end;
end;

function TMainMapLayerConfig.GetUsePrevZoomAtMap: Boolean;
begin
  LockRead;
  try
    Result := FUsePrevZoomAtMap;
  finally
    UnlockRead;
  end;
end;

procedure TMainMapLayerConfig.SetUsePrevZoomAtLayer(const AValue: Boolean);
begin
  LockWrite;
  try
    if FUsePrevZoomAtLayer <> AValue then begin
      FUsePrevZoomAtLayer := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMainMapLayerConfig.SetUsePrevZoomAtMap(const AValue: Boolean);
begin
  LockWrite;
  try
    if FUsePrevZoomAtMap <> AValue then begin
      FUsePrevZoomAtMap := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
