unit u_MapMovingConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_MapMovingConfig,
  u_ConfigDataElementBase;

type
  TMapMovingConfig = class(TConfigDataElementBase, IMapMovingConfig)
  private
    FAnimateMove: Boolean;
    FAnimateMoveTime: Cardinal;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetAnimateMove: Boolean;
    procedure SetAnimateMove(AValue: Boolean);

    function GetAnimateMoveTime: Cardinal;
    procedure SetAnimateMoveTime(AValue: Cardinal);
  public
    constructor Create;
  end;

implementation

{ TMapMovingConfig }

constructor TMapMovingConfig.Create;
begin
  inherited;
  FAnimateMove := True;
  FAnimateMoveTime := 200;
end;

procedure TMapMovingConfig.DoReadConfig(AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FAnimateMove := AConfigData.ReadBool('AnimateMove', FAnimateMove);
    FAnimateMoveTime := AConfigData.ReadInteger('AnimateMoveTime', FAnimateMoveTime);
    SetChanged;
  end;
end;

procedure TMapMovingConfig.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteBool('AnimateMove', FAnimateMove);
  AConfigData.WriteInteger('AnimateMoveTime', FAnimateMoveTime);
end;

function TMapMovingConfig.GetAnimateMove: Boolean;
begin
  LockRead;
  try
    Result := FAnimateMove;
  finally
    UnlockRead;
  end;
end;

function TMapMovingConfig.GetAnimateMoveTime: Cardinal;
begin
  LockRead;
  try
    Result := FAnimateMoveTime;
  finally
    UnlockRead;
  end;
end;

procedure TMapMovingConfig.SetAnimateMove(AValue: Boolean);
begin
  LockWrite;
  try
    if FAnimateMove <> AValue then begin
      FAnimateMove := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMapMovingConfig.SetAnimateMoveTime(AValue: Cardinal);
begin
  LockWrite;
  try
    if FAnimateMoveTime <> AValue then begin
      FAnimateMoveTime := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
