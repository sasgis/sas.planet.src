unit u_MainGeoCoderConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_IGeoCoderList,
  i_IMainGeoCoderConfig,
  u_ConfigDataElementBase;

type
  TMainGeoCoderConfig = class(TConfigDataElementBase, IMainGeoCoderConfig)
  private
    FList: IGeoCoderList;
    FActiveGeoCoderGUID: TGUID;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetList: IGeoCoderList;
    function GetActiveGeoCoderGUID: TGUID;
    procedure SetActiveGeoCoderGUID(AValue: TGUID);
    function GetActiveGeoCoder: IGeoCoderListEntity;
  public
    constructor Create(AList: IGeoCoderList);
  end;

implementation

uses
  SysUtils,
  c_ZeroGUID;

{ TMainGeoCoderConfig }

constructor TMainGeoCoderConfig.Create(AList: IGeoCoderList);
var
  i: Cardinal;
begin
  inherited Create;
  FList := AList;
  if FList.GetGUIDEnum.Next(1, FActiveGeoCoderGUID, i) <> S_OK then begin
    raise Exception.Create('В списке геокодеров пусто');
  end;
end;

procedure TMainGeoCoderConfig.DoReadConfig(AConfigData: IConfigDataProvider);
var
  VGUID: TGUID;
  VGUIDStr: string;
begin
  inherited;
  if AConfigData <> nil then begin
    VGUID := CGUID_Zero;
    VGUIDStr := AConfigData.ReadString('GeoCoderGUID', '');
    if VGUIDStr <> '' then begin
      try
        VGUID := StringToGUID(VGUIDStr);
      except
        VGUID := CGUID_Zero;
      end;
    end;
    SetActiveGeoCoderGUID(VGUID);
  end;
end;

procedure TMainGeoCoderConfig.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteString('GeoCoderGUID', GUIDToString(FActiveGeoCoderGUID));
end;

function TMainGeoCoderConfig.GetActiveGeoCoder: IGeoCoderListEntity;
begin
  LockRead;
  try
    Result := FList.Get(FActiveGeoCoderGUID);
  finally
    UnlockRead;
  end;
end;

function TMainGeoCoderConfig.GetActiveGeoCoderGUID: TGUID;
begin
  LockRead;
  try
    Result := FActiveGeoCoderGUID;
  finally
    UnlockRead;
  end;
end;

function TMainGeoCoderConfig.GetList: IGeoCoderList;
begin
  Result := FList;
end;

procedure TMainGeoCoderConfig.SetActiveGeoCoderGUID(AValue: TGUID);
begin
  if not IsEqualGUID(AValue, CGUID_Zero) then begin
    LockWrite;
    try
      if FList.Get(AValue) <> nil then begin
        if not IsEqualGUID(FActiveGeoCoderGUID, AValue) then begin
          FActiveGeoCoderGUID := AValue;
          SetChanged;
        end;
      end;
    finally
      UnlockWrite;
    end;
  end;
end;

end.
