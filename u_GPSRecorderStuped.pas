unit u_GPSRecorderStuped;

interface

uses
  SysUtils,
  t_GeoTypes,
  i_IGPSRecorder;

type
  TGPSRecorderStuped = class(TInterfacedObject, IGPSRecorder)
  private
    FTrack: TGPSTrackPointArray;
    FPointsCount: Integer;
    FAllocatedPoints: Integer;
    FLock: IReadWriteSync;
  protected
    procedure AddPoint(APoint: TGPSTrackPoint);
    procedure ClearTrack;
    function IsEmpty: Boolean;
    function GetLastPoint: TDoublePoint;
    function GetTwoLastPoints(var APointLast, APointPrev: TDoublePoint): Boolean;
    function LastPoints(ACount: Integer): TGPSTrackPointArray;
    function GetAllPoints: TDoublePointArray;
    function GetAllTracPoints: TGPSTrackPointArray;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TGPSRecorderStuped }

procedure TGPSRecorderStuped.AddPoint(APoint: TGPSTrackPoint);
var
  VIsAddPointEmpty: Boolean;
  VIsLastPointEmpty: Boolean;
begin
  VIsAddPointEmpty := (APoint.Point.X = 0) and (APoint.Point.Y = 0);
  VIsLastPointEmpty := True;
  FLock.BeginWrite;
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
      FTrack[FPointsCount] := APoint;
      Inc(FPointsCount);
    end;
  finally
    FLock.EndWrite;
  end;
end;

procedure TGPSRecorderStuped.ClearTrack;
begin
  FLock.BeginWrite;
  try
    FPointsCount := 0;
  finally
    FLock.EndWrite;
  end;
end;

constructor TGPSRecorderStuped.Create;
begin
  FLock := TMultiReadExclusiveWriteSynchronizer.Create;
end;

destructor TGPSRecorderStuped.Destroy;
begin
  FLock := nil;
  FTrack := nil;
  inherited;
end;

function TGPSRecorderStuped.GetAllPoints: TDoublePointArray;
var
  i: Cardinal;
begin
  FLock.BeginRead;
  try
    SetLength(Result, FPointsCount);
    for i := 0 to FPointsCount - 1 do begin
      Result[i] := FTrack[i].Point;
    end;
  finally
    FLock.EndRead;
  end;
end;

function TGPSRecorderStuped.GetAllTracPoints: TGPSTrackPointArray;
begin
  FLock.BeginRead;
  try
    Result := Copy(FTrack, 0, FPointsCount);
  finally
    FLock.EndRead;
  end;
end;

function TGPSRecorderStuped.GetLastPoint: TDoublePoint;
begin
  FLock.BeginRead;
  try
    if FPointsCount = 0 then begin
      Result.X := 0;
      Result.Y := 0;
    end else begin
      Result := FTrack[FPointsCount - 1].Point;
    end;
  finally
    FLock.EndRead;
  end;
end;

function TGPSRecorderStuped.GetTwoLastPoints(var APointLast,
  APointPrev: TDoublePoint): Boolean;
begin
  Result := False;
  APointLast.X := 0;
  APointLast.Y := 0;
  APointPrev := APointLast;
  FLock.BeginRead;
  try
    if FPointsCount > 0 then begin
      APointLast := FTrack[FPointsCount - 1].Point;
      if FPointsCount > 1 then begin
        APointPrev := FTrack[FPointsCount - 2].Point;
      end else begin
        APointPrev := APointLast;
      end;
    end;
  finally
    FLock.EndRead;
  end;
  if (APointLast.X = 0) and (APointLast.Y = 0) then begin
    APointPrev := APointLast;
  end else if not((APointPrev.X = 0) and (APointPrev.Y = 0)) then begin
    Result := True;
  end;
end;

function TGPSRecorderStuped.IsEmpty: Boolean;
begin
  FLock.BeginRead;
  try
    Result := FPointsCount > 0;
  finally
    FLock.EndRead;
  end;
end;

function TGPSRecorderStuped.LastPoints(ACount: Integer): TGPSTrackPointArray;
var
  VPointsToCopyCount: Integer;
  VStartIndex: Integer;
begin
  FLock.BeginRead;
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
    FLock.EndRead;
  end;
end;

end.
