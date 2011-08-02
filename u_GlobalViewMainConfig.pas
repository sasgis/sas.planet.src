unit u_GlobalViewMainConfig;

interface

uses
  Graphics,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_GlobalViewMainConfig,
  u_ConfigDataElementBase;

type
  TGlobalViewMainConfig = class(TConfigDataElementBase, IGlobalViewMainConfig)
  private
    FBackGroundColor: TColor;
    FUsePrevZoomAtMap: Boolean;
    FUsePrevZoomAtLayer: Boolean;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetBackGroundColor: TColor;
    procedure SetBackGroundColor(const AValue: TColor);

    function GetUsePrevZoomAtMap: Boolean;
    procedure SetUsePrevZoomAtMap(const AValue: Boolean);

    function GetUsePrevZoomAtLayer: Boolean;
    procedure SetUsePrevZoomAtLayer(const AValue: Boolean);
  public
    constructor Create;
  end;

implementation

{ TGlobalViewMainConfig }

constructor TGlobalViewMainConfig.Create;
begin
  inherited;
  FBackGroundColor := clSilver;
  FUsePrevZoomAtMap := True;
  FUsePrevZoomAtLayer := True;
end;

procedure TGlobalViewMainConfig.DoReadConfig(AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FUsePrevZoomAtMap := AConfigData.ReadBool('UsePrevZoomAtMap', FUsePrevZoomAtMap);
    FUsePrevZoomAtLayer := AConfigData.ReadBool('UsePrevZoomAtLayer', FUsePrevZoomAtLayer);
    FBackGroundColor := TColor(AConfigData.ReadInteger('BackgroundColor', FBackGroundColor));
    SetChanged;
  end;
end;

procedure TGlobalViewMainConfig.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteBool('UsePrevZoomAtMap', FUsePrevZoomAtMap);
  AConfigData.WriteBool('UsePrevZoomAtLayer', FUsePrevZoomAtLayer);
  AConfigData.WriteInteger('BackgroundColor', Integer(FBackGroundColor));
end;

function TGlobalViewMainConfig.GetBackGroundColor: TColor;
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

procedure TGlobalViewMainConfig.SetBackGroundColor(const AValue: TColor);
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

procedure TGlobalViewMainConfig.SetUsePrevZoomAtLayer(const AValue: Boolean);
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

procedure TGlobalViewMainConfig.SetUsePrevZoomAtMap(const AValue: Boolean);
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
