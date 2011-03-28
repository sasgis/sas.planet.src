unit u_MainMemCacheConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_MainMemCacheConfig,
  u_ConfigDataElementBase;

type
  TMainMemCacheConfig = class(TConfigDataElementBase, IMainMemCacheConfig)
  private
    FMaxSize: Integer;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetMaxSize: Integer;
    procedure SetMaxSize(AValue: Integer);
  public
    constructor Create;
  end;

implementation

{ TMainMemCacheConfig }

constructor TMainMemCacheConfig.Create;
begin
  inherited;
  FMaxSize := 150;
end;

procedure TMainMemCacheConfig.DoReadConfig(AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    SetMaxSize(AConfigData.ReadInteger('MainMemCacheSize', FMaxSize));
  end;
end;

procedure TMainMemCacheConfig.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteInteger('MainMemCacheSize', FMaxSize);
end;

function TMainMemCacheConfig.GetMaxSize: Integer;
begin
  LockRead;
  try
    Result := FMaxSize;
  finally
    UnlockRead;
  end;
end;

procedure TMainMemCacheConfig.SetMaxSize(AValue: Integer);
var
  VMaxSize: Integer;
begin
  VMaxSize := AValue;
  if VMaxSize < 0 then begin
    VMaxSize := 0;
  end;
  LockWrite;
  try
    if FMaxSize <> VMaxSize then begin
      FMaxSize := VMaxSize;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
