unit u_GPSRecorderStuped;

interface

uses
  SysUtils,
  t_GeoTypes,
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider,
  i_GPS,
  i_IGPSModule,
  i_IGPSRecorder,
  u_ConfigDataElementBase;

type
  TGPSRecorderStuped = class(TConfigDataElementBase, IGPSRecorder)
  private
    FGPSModule: IGPSModule;
    FTrack: TGPSTrackPointArray;
    FPointsCount: Integer;
    FAllocatedPoints: Integer;

    FOdometer1: Double;
    FOdometer2: Double;
    FMaxSpeed: Double;
    FAvgSpeed: Double;
    FDist: Double;
    FLastSpeed: Double;
    FLastAltitude: Double;
    FLastHeading: Double;
    FAvgSpeedTickCount: Double;
    FLastPointIsEmpty: Boolean;

    procedure OnGpsConnect(Sender: TObject);
    procedure OnGpsDataReceive(Sender: TObject);
    procedure OnGpsDisconnect(Sender: TObject);
    procedure AddPoint(APosition: IGPSPosition);
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    procedure ClearTrack;
    function IsEmpty: Boolean;
    function GetLastPoint: TDoublePoint;
    function LastPoints(ACount: Integer): TGPSTrackPointArray;
    function GetAllPoints: TDoublePointArray;
    function GetAllTracPoints: TGPSTrackPointArray;

    function GetOdometer1: Double;
    procedure ResetOdometer1;
    function GetOdometer2: Double;
    procedure ResetOdometer2;
    function GetMaxSpeed: Double;
    procedure ResetMaxSpeed;
    function GetAvgSpeed: Double;
    procedure ResetAvgSpeed;
    function GetDist: Double;
    function GetLastSpeed: Double;
    function GetLastAltitude: Double;
    function GetLastHeading: Double;
  public
    constructor Create(AGPSModule: IGPSModule);
    destructor Destroy; override;
  end;

implementation

{ TGPSRecorderStuped }

constructor TGPSRecorderStuped.Create(AGPSModule: IGPSModule);
begin
  inherited Create;
  FGPSModule := AGPSModule;
  FLastPointIsEmpty := True;
end;

destructor TGPSRecorderStuped.Destroy;
begin
  FTrack := nil;
  inherited;
end;

procedure TGPSRecorderStuped.DoReadConfig(AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FOdometer1 := AConfigData.ReadFloat('Odometer1', FOdometer1);
    FOdometer2 := AConfigData.ReadFloat('Odometer2', FOdometer2);
  end;
end;

procedure TGPSRecorderStuped.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteFloat('Odometer1', FOdometer1);
  AConfigData.WriteFloat('Odometer2', FOdometer2);
end;

procedure TGPSRecorderStuped.AddPoint(APosition: IGPSPosition);
var
  VIsAddPointEmpty: Boolean;
  VIsLastPointEmpty: Boolean;
begin
  VIsAddPointEmpty := APosition.IsFix = 0;
  VIsLastPointEmpty := True;
  LockWrite;
  try
    if FPointsCount > 0 then begin
      VIsLastPointEmpty := (FTrack[FPointsCount - 1].Point.X = 0) and (FTrack[FPointsCount - 1].Point.Y = 0);
    end;
    if (not VIsLastPointEmpty) or (not VIsAddPointEmpty) then begin
      if FPointsCount >= FAllocatedPoints then begin
        if FAllocatedPoints <= 0 then begin
          FAllocatedPoints := 4096;
        end else begin
          Inc(FAllocatedPoints, 4096);
        end;
        SetLength(FTrack, FAllocatedPoints);
      end;
      FTrack[FPointsCount].Point := APosition.Position;
      FTrack[FPointsCount].Speed := APosition.Speed_KMH;
      Inc(FPointsCount);
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TGPSRecorderStuped.ClearTrack;
begin
  LockWrite;
  try
    FPointsCount := 0;
  finally
    UnlockWrite;
  end;
end;

function TGPSRecorderStuped.GetAllPoints: TDoublePointArray;
var
  i: Cardinal;
begin
  LockRead;
  try
    SetLength(Result, FPointsCount);
    for i := 0 to FPointsCount - 1 do begin
      Result[i] := FTrack[i].Point;
    end;
  finally
    UnlockRead;
  end;
end;

function TGPSRecorderStuped.GetAllTracPoints: TGPSTrackPointArray;
begin
  LockRead;
  try
    Result := Copy(FTrack, 0, FPointsCount);
  finally
    UnlockRead;
  end;
end;

function TGPSRecorderStuped.GetAvgSpeed: Double;
begin
  LockRead;
  try
    Result := FAvgSpeed;
  finally
    UnlockRead;
  end;
end;

function TGPSRecorderStuped.GetDist: Double;
begin
  LockRead;
  try
    Result := FDist;
  finally
    UnlockRead;
  end;
end;

function TGPSRecorderStuped.GetLastAltitude: Double;
begin
  LockRead;
  try
    Result := FLastAltitude;
  finally
    UnlockRead;
  end;
end;

function TGPSRecorderStuped.GetLastHeading: Double;
begin
  LockRead;
  try
    Result := FLastHeading;
  finally
    UnlockRead;
  end;
end;

function TGPSRecorderStuped.GetLastPoint: TDoublePoint;
begin
  LockRead;
  try
    if FPointsCount = 0 then begin
      Result.X := 0;
      Result.Y := 0;
    end else begin
      Result := FTrack[FPointsCount - 1].Point;
    end;
  finally
    UnlockRead;
  end;
end;

function TGPSRecorderStuped.GetLastSpeed: Double;
begin
  LockRead;
  try
    Result := FLastSpeed;
  finally
    UnlockRead;
  end;
end;

function TGPSRecorderStuped.GetMaxSpeed: Double;
begin
  LockRead;
  try
    Result := FMaxSpeed;
  finally
    UnlockRead;
  end;
end;

function TGPSRecorderStuped.GetOdometer1: Double;
begin
  LockRead;
  try
    Result := FOdometer1;
  finally
    UnlockRead;
  end;
end;

function TGPSRecorderStuped.GetOdometer2: Double;
begin
  LockRead;
  try
    Result := FOdometer2;
  finally
    UnlockRead;
  end;
end;

function TGPSRecorderStuped.IsEmpty: Boolean;
begin
  LockRead;
  try
    Result := FPointsCount > 0;
  finally
    UnlockRead;
  end;
end;

function TGPSRecorderStuped.LastPoints(ACount: Integer): TGPSTrackPointArray;
var
  VPointsToCopyCount: Integer;
  VStartIndex: Integer;
begin
  LockRead;
  try
    VPointsToCopyCount := ACount;
    if FPointsCount <= VPointsToCopyCount then begin
      VPointsToCopyCount := FPointsCount;
      VStartIndex := 0;
    end else begin
      VStartIndex :=FPointsCount - VPointsToCopyCount;
    end;
    Result := Copy(FTrack, VStartIndex, VPointsToCopyCount);
  finally
    UnlockRead;
  end;
end;

procedure TGPSRecorderStuped.OnGpsConnect(Sender: TObject);
begin
  FMaxSpeed := 0;
  FLastSpeed := 0;
  FLastAltitude := 0;
  FAvgSpeed := 0;
  FAvgSpeedTickCount := 0;
end;

procedure TGPSRecorderStuped.OnGpsDataReceive(Sender: TObject);
begin

end;

procedure TGPSRecorderStuped.OnGpsDisconnect(Sender: TObject);
begin

end;

procedure TGPSRecorderStuped.ResetAvgSpeed;
begin
  LockWrite;
  try
    if FAvgSpeed <> 0 then begin
      FAvgSpeed := 0;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TGPSRecorderStuped.ResetMaxSpeed;
begin
  LockWrite;
  try
    if FMaxSpeed <> 0 then begin
      FMaxSpeed := 0;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TGPSRecorderStuped.ResetOdometer1;
begin
  LockWrite;
  try
    if FOdometer1 <> 0 then begin
      FOdometer1 := 0;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TGPSRecorderStuped.ResetOdometer2;
begin
  LockWrite;
  try
    if FOdometer2 <> 0 then begin
      FOdometer2 := 0;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
