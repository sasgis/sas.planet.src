{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
{* This program is free software: you can redistribute it and/or modify       *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* This program is distributed in the hope that it will be useful,            *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *}
{*                                                                            *}
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_GPSRecorderStuped;

interface

uses
  SysUtils,
  t_GeoTypes,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_Datum,
  i_GPS,
  i_GPSPositionFactory,
  i_GPSRecorder,
  u_ConfigDataElementBase;

type
  TGPSRecorderStuped = class(TConfigDataElementBase, IGPSRecorder)
  private
    FGPSPositionFactory: IGPSPositionFactory;
    FDatum: IDatum;
    FTrack: TGPSTrackPointArray;
    FPointsCount: Integer;
    FAllocatedPoints: Integer;

    FOdometer1: Double;
    FOdometer2: Double;
    FDist: Double;
    FMaxSpeed: Double;
    FAvgSpeed: Double;
    FAvgSpeedTickCount: Double;

    FLastSpeed: Double;
    FLastAltitude: Double;
    FLastHeading: Double;
    FLastPosition: TDoublePoint;

    FCurrentPosition: IGPSPosition;
    FLastPointIsEmpty: Boolean;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    procedure AddPoint(APosition: IGPSPosition);
    procedure AddEmptyPoint;
    procedure ClearTrack;
    function IsEmpty: Boolean;
    function LastPoints(const AMaxCount: Integer; var APoints: TGPSTrackPointArray): Integer;
    function GetAllPoints: TArrayOfDoublePoint;
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
    function GetCurrentPosition: IGPSPosition;
  public
    constructor Create(ADatum: IDatum; AGPSPositionFactory: IGPSPositionFactory);
    destructor Destroy; override;
  end;

implementation

uses
  Math;

{ TGPSRecorderStuped }

constructor TGPSRecorderStuped.Create(ADatum: IDatum; AGPSPositionFactory: IGPSPositionFactory);
begin
  inherited Create;
  FGPSPositionFactory := AGPSPositionFactory;
  FDatum := ADatum;
  FLastPointIsEmpty := True;
  FCurrentPosition := FGPSPositionFactory.BuildPositionEmpty;
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
    SetChanged;
  end;
end;

procedure TGPSRecorderStuped.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteFloat('Odometer1', FOdometer1);
  AConfigData.WriteFloat('Odometer2', FOdometer2);
end;

procedure TGPSRecorderStuped.AddEmptyPoint;
begin
  LockWrite;
  try
    if (not FLastPointIsEmpty) then begin
      if FPointsCount >= FAllocatedPoints then begin
        if FAllocatedPoints <= 0 then begin
          FAllocatedPoints := 4096;
        end else begin
          Inc(FAllocatedPoints, 4096);
        end;
        SetLength(FTrack, FAllocatedPoints);
      end;
      FTrack[FPointsCount].Point.X := NaN;
      FTrack[FPointsCount].Point.Y := NaN;
      FTrack[FPointsCount].Speed := 0;
      FTrack[FPointsCount].Time := NaN;
      Inc(FPointsCount);
      FLastPointIsEmpty := True;
      SetChanged;
    end;
    FCurrentPosition := FGPSPositionFactory.BuildPositionEmpty;
  finally
    UnlockWrite;
  end;
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
        FTrack[FPointsCount].Point.X := NaN;
        FTrack[FPointsCount].Point.Y := NaN;
        FTrack[FPointsCount].Speed := 0;
      end;
      FTrack[FPointsCount].Time := APosition.UTCDateTime;
      Inc(FPointsCount);
      FLastPointIsEmpty := VIsAddPointEmpty;
      SetChanged;
    end;
    FCurrentPosition := APosition;
  finally
    UnlockWrite;
  end;
end;

procedure TGPSRecorderStuped.ClearTrack;
begin
  LockWrite;
  try
    if FPointsCount <> 0 then begin
      FPointsCount := 0;
      FLastPointIsEmpty := True;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

function TGPSRecorderStuped.GetAllPoints: TArrayOfDoublePoint;
var
  i: Integer;
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

function TGPSRecorderStuped.GetCurrentPosition: IGPSPosition;
begin
  LockRead;
  try
    Result := FCurrentPosition;
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

function TGPSRecorderStuped.LastPoints(
  const AMaxCount: Integer;
  var APoints: TGPSTrackPointArray
): Integer;
var
  VStartIndex: Integer;
begin
  LockRead;
  try
    Result := AMaxCount;
    if FPointsCount <= Result then begin
      Result := FPointsCount;
      VStartIndex := 0;
    end else begin
      VStartIndex :=FPointsCount - Result;
    end;
    if Result > 0 then begin
      if Length(APoints) < Result then begin
        SetLength(APoints, Result);
      end;
      System.Move(FTrack[VStartIndex], APoints[0],
        (Result) * SizeOf(TGPSTrackPoint));
    end;
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
