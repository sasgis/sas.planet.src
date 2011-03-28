unit u_NavigationToPoint;

interface

uses
  t_GeoTypes,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_NavigationToPoint,
  u_ConfigDataElementBase;

type
  TNavigationToPoint = class(TConfigDataElementBase, INavigationToPoint)
  private
    FIsActive: Boolean;
    FId: Integer;
    FLonLat: TDoublePoint;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetIsActive: Boolean;
    function GetId: Integer;
    function GetLonLat: TDoublePoint;

    procedure StartNavToMark(AId: Integer; APointLonLat: TDoublePoint);
    procedure StartNavLonLat(APointLonLat: TDoublePoint);
    procedure StopNav;
  public
    constructor Create;
  end;
implementation

{ TNavigationToPoint }

constructor TNavigationToPoint.Create;
begin
  inherited;
  FIsActive := False;
  FId := -1;
end;

procedure TNavigationToPoint.DoReadConfig(AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FIsActive := AConfigData.ReadBool('Active', FIsActive);
    FId := AConfigData.ReadInteger('ID', FId);
    FLonLat.X := AConfigData.ReadFloat('X', FLonLat.X);
    FLonLat.Y := AConfigData.ReadFloat('Y', FLonLat.Y);
  end;
end;

procedure TNavigationToPoint.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteBool('Active', FIsActive);
  AConfigData.WriteInteger('ID', FId);
  AConfigData.WriteFloat('X', FLonLat.X);
  AConfigData.WriteFloat('Y', FLonLat.Y);
end;

function TNavigationToPoint.GetId: Integer;
begin
  LockRead;
  try
    Result := FId;
  finally
    UnlockRead;
  end;
end;

function TNavigationToPoint.GetIsActive: Boolean;
begin
  LockRead;
  try
    Result := FIsActive;
  finally
    UnlockRead;
  end;
end;

function TNavigationToPoint.GetLonLat: TDoublePoint;
begin
  LockRead;
  try
    Result := FLonLat;
  finally
    UnlockRead;
  end;
end;

procedure TNavigationToPoint.StartNavLonLat(APointLonLat: TDoublePoint);
begin
  LockWrite;
  try
    FIsActive := True;
    FId := -1;
    FLonLat := APointLonLat;
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

procedure TNavigationToPoint.StartNavToMark(AId: Integer;
  APointLonLat: TDoublePoint);
begin
  LockWrite;
  try
    FIsActive := True;
    FId := AId;
    FLonLat := APointLonLat;
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

procedure TNavigationToPoint.StopNav;
begin
  LockWrite;
  try
    FIsActive := False;
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

end.
