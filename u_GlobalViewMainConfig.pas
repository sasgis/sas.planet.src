unit u_GlobalViewMainConfig;

interface

uses
  GR32,
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider,
  i_IGlobalViewMainConfig,
  u_ConfigDataElementBase;

type
  TGlobalViewMainConfig = class(TConfigDataElementBase, IGlobalViewMainConfig)
  private
    FBackGroundColor: TColor32;
    FUsePrevZoomAtMap: Boolean;
    FUsePrevZoomAtLayer: Boolean;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetBackGroundColor: TColor32;
    procedure SetBackGroundColor(AValue: TColor32);

    function GetUsePrevZoomAtMap: Boolean;
    procedure SetUsePrevZoomAtMap(AValue: Boolean);

    function GetUsePrevZoomAtLayer: Boolean;
    procedure SetUsePrevZoomAtLayer(AValue: Boolean);
  public
    constructor Create;
  end;

implementation

uses
  u_ConfigProviderHelpers;

{ TGlobalViewMainConfig }

constructor TGlobalViewMainConfig.Create;
begin
  inherited;
  FBackGroundColor := TColor32($FFC0C0C0);
  FUsePrevZoomAtMap := True;
  FUsePrevZoomAtLayer := True;
end;

procedure TGlobalViewMainConfig.DoReadConfig(AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FUsePrevZoomAtMap := AConfigData.ReadBool('UsePrevZoomAtMap', FUsePrevZoomAtMap);
    FUsePrevZoomAtLayer := AConfigData.ReadBool('UsePrevZoomAtLayer', FUsePrevZoomAtLayer);
    FBackGroundColor := LoadColor32(AConfigData, 'BackgroundColor', FBackGroundColor);
    SetChanged;
  end;
end;

procedure TGlobalViewMainConfig.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteBool('UsePrevZoomAtMap', FUsePrevZoomAtMap);
  AConfigData.WriteBool('UsePrevZoomAtLayer', FUsePrevZoomAtLayer);
  WriteColor32(AConfigData, 'BackgroundColor', FBackGroundColor);
end;

function TGlobalViewMainConfig.GetBackGroundColor: TColor32;
begin
  LockRead;
  try
    Result := FBackGroundColor;
  finally
    UnlockRead;
  end;
end;

function TGlobalViewMainConfig.GetUsePrevZoomAtLayer: Boolean;
begin
  LockRead;
  try
    Result := FUsePrevZoomAtLayer;
  finally
    UnlockRead;
  end;
end;

function TGlobalViewMainConfig.GetUsePrevZoomAtMap: Boolean;
begin
  LockRead;
  try
    Result := FUsePrevZoomAtMap;
  finally
    UnlockRead;
  end;
end;

procedure TGlobalViewMainConfig.SetBackGroundColor(AValue: TColor32);
begin
  LockWrite;
  try
    if FBackGroundColor <> AValue then begin
      FBackGroundColor := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TGlobalViewMainConfig.SetUsePrevZoomAtLayer(AValue: Boolean);
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

procedure TGlobalViewMainConfig.SetUsePrevZoomAtMap(AValue: Boolean);
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
