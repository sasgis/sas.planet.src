unit u_GPSRecorderStuped;

interface

uses
  SysUtils,
  t_GeoTypes,
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider,
  i_IDatum,
  i_GPS,
  i_IGPSRecorder,
  u_ConfigDataElementBase;

type
  TGPSRecorderStuped = class(TConfigDataElementBase, IGPSRecorder)
  private
    FDatum: IDatum;
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
    FLastPosition: TDoublePoint;

    FAvgSpeedTickCount: Double;
    FLastPointIsEmpty: Boolean;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    procedure AddPoint(APosition: IGPSPosition);
    procedure ClearTrack;
    function IsEmpty: Boolean;
    function LastPoints(ACount: Integer): TGPSTrackPointArray;
    function GetAllPoints: TDoublePointArray;
    function GetAllTracPoints: TGPSTrackPointArray;

    function GetOdometer1: Double;
    procedure ResetOdometer1;
    function GetOdometer2: Double;
    procedure ResetOdometer2;
    function GetDist: Double;
    procedure ResetDist;
    function GetMaxSpeed: Double;
    procedure ResetMaxSpeed;
    function GetAvgSpeed: Double;
    procedure ResetAvgSpeed;
    function GetLastSpeed: Double;
    function GetLastAltitude: Double;
    function GetLastHeading: Double;
    function GetLastPosition: TDoublePoint;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  u_Datum;

{ TGPSRecorderStuped }

constructor TGPSRecorderStuped.Create;
begin
  inherited Create;
  FDatum := TDatum.Create(3395, 6378137, 6356752);
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
  VAlfa: Double;
  VBeta: Double;
  VDistToPrev: Double;
  VPointPrev: TDoublePoint;
begin
  VIsAddPointEmpty := APosition.IsFix = 0;
  LockWrite;
  try
    if (not FLastPointIsEmpty) or (not VIsAddPointEmpty) then begin
      if FPointsCount >= FAllocatedPoints then begin
        if FAllocatedPoints <= 0 then begin
          FAllocatedPoints := 4096;
        end else begin
          Inc(FAllocatedPoints, 4096);
        end;
        SetLength(FTrack, FAllocatedPoints);
      end;
      if (not VIsAddPointEmpty) then begin
        FLastPosition := APosition.Position;
        FLastAltitude := APosition.Altitude;
        FLastHeading := APosition.Heading;
        FLastSpeed := APosition.Speed_KMH;
        if FLastSpeed > FMaxSpeed then begin
          FMaxSpeed := FLastSpeed;
        end;
        FAvgSpeedTickCount := FAvgSpeedTickCount + 1;
        VAlfa := 1 / FAvgSpeedTickCount;
        VBeta := 1 - VAlfa;
        FAvgSpeed := VAlfa * FLastSpeed + VBeta * FAvgSpeed;

        if not FLastPointIsEmpty then begin
          VPointPrev := FTrack[FPointsCount - 1].Point;
          VDistToPrev := FDatum.CalcDist(VPointPrev, FLastPosition);
          FDist := FDist + VDistToPrev;
          FOdometer1 := FOdometer1 + VDistToPrev;
          FOdometer2 := FOdometer2 + VDistToPrev;
        end;
        FTrack[FPointsCount].Point := FLastPosition;
        FTrack[FPointsCount].Speed := FLastSpeed;
      end else begin
        FTrack[FPointsCount].Point.X := 0;
        FTrack[FPointsCount].Point.Y := 0;
        FTrack[FPointsCount].Speed := 0;
      end;
      Inc(FPointsCount);
      FLastPointIsEmpty := VIsAddPointEmpty;
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
    FLastPointIsEmpty := True;
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

function TGPSRecorderStuped.GetLastPosition: TDoublePoint;
begin
  LockRead;
  try
    Result := FLastPosition;
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

procedure TGPSRecorderStuped.ResetAvgSpeed;
begin
  LockWrite;
  try
    if FAvgSpeed <> 0 then begin
      FAvgSpeed := 0;
      FAvgSpeedTickCount := 0;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TGPSRecorderStuped.ResetDist;
begin
  LockWrite;
  try
    if FDist <> 0 then begin
      FDist := 0;
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
