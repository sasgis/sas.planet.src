{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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
  t_GeoTypes,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_Datum,
  i_VectorItemLonLat,
  i_VectorItmesFactory,
  i_GPS,
  i_GPSPositionFactory,
  i_GPSRecorder,
  vsagps_public_base,
  vsagps_public_position,
  vsagps_public_unit_info,
  u_ConfigDataElementBase;

type
  TGPSRecorderStuped = class(TConfigDataElementBase, IGPSRecorder)
  private
    FGPSPositionFactory: IGPSPositionFactory;
    FVectorItmesFactory: IVectorItmesFactory;
    FDatum: IDatum;
    FTrack: array of TGPSTrackPoint;
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
    FLastPositionOK: Boolean;

    FGPSUnitInfo: String;
  private
    procedure InternalSafeAlloc;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;

    function GenerateGPSUnitInfo(const AUnitIndex: Byte): String;
    procedure ReGenerateGPSUnitInfo;
    procedure DoGPSUnitInfoChanged(Sender: TObject;
                                   const AUnitIndex: Byte;
                                   const AKind: TVSAGPS_UNIT_INFO_Kind);
  protected
    procedure AddPoint(const APosition: IGPSPosition);
    procedure AddEmptyPoint;
    procedure ClearTrack;
    function IsEmpty: Boolean;

    function LastPoints(const AMaxCount: Integer): IEnumGPSTrackPoint;

    function GetAllPoints: ILonLatPath;

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

    procedure ExecuteGPSCommand(Sender: TObject;
                                const AUnitIndex: Byte;
                                const ACommand: LongInt;
                                const APointer: Pointer);

    function GetGPSUnitInfo: String;
  public
    constructor Create(
      const AVectorItmesFactory: IVectorItmesFactory;
      const ADatum: IDatum;
      const AGPSPositionFactory: IGPSPositionFactory
    );
    destructor Destroy; override;
  end;

implementation

uses
  Math,
  i_EnumDoublePoint,
  u_GeoFun;

{ TEnumDoublePointsByArray }

type
  TEnumTrackPointsByArray = class(TInterfacedObject, IEnumDoublePoint, IEnumLonLatPoint, IEnumGPSTrackPoint)
  private
    FPoints: PTrackPointArray;
    FCount: Integer;
    FCopyPoints: Boolean;
    FIndex: Integer;
    function NextEnumDouble(out APoint: TDoublePoint): Boolean;
    function NextEnumTrack(out APoint: TGPSTrackPoint): Boolean;
  private
    function IEnumDoublePoint.Next = NextEnumDouble;
    function IEnumLonLatPoint.Next = NextEnumDouble;
    function IEnumGPSTrackPoint.Next = NextEnumTrack;
  public
    constructor Create(
      ACopyPoints: Boolean;
      APoints: PTrackPointArray;
      ACount: Integer
    );
    destructor Destroy; override;
  end;

constructor TEnumTrackPointsByArray.Create(
  ACopyPoints: Boolean;
  APoints: PTrackPointArray;
  ACount: Integer
);
begin
  FCopyPoints := ACopyPoints;
  FCount := ACount;
  if FCopyPoints then begin
    if FCount > 0 then begin
      FPoints := GetMemory(FCount * SizeOf(TGPSTrackPoint));
      Move(APoints^, FPoints^, FCount * SizeOf(TGPSTrackPoint));
    end;
  end else begin
    FPoints := APoints;
  end;
  FIndex := 0;
end;

destructor TEnumTrackPointsByArray.Destroy;
begin
  if FCopyPoints then begin
    if FCount > 0 then begin
      FreeMemory(FPoints);
    end;
    FPoints := nil;
    FCopyPoints := False;
  end;
  inherited;
end;

function TEnumTrackPointsByArray.NextEnumDouble(out APoint: TDoublePoint): Boolean;
begin
  if FIndex < FCount then begin
    APoint := FPoints[FIndex].Point;
    Inc(FIndex);
    Result := True;
  end else begin
    APoint := CEmptyDoublePoint;
    Result := False;
  end;
end;


function TEnumTrackPointsByArray.NextEnumTrack(
  out APoint: TGPSTrackPoint): Boolean;
begin
  if FIndex < FCount then begin
    APoint := FPoints[FIndex];
    Inc(FIndex);
    Result := True;
  end else begin
    APoint.Point := CEmptyDoublePoint;
    APoint.Speed := NaN;
    APoint.Time := NaN;
    Result := False;
  end;
end;

{ TGPSRecorderStuped }

constructor TGPSRecorderStuped.Create(
  const AVectorItmesFactory: IVectorItmesFactory;
  const ADatum: IDatum;
  const AGPSPositionFactory: IGPSPositionFactory
);
begin
  inherited Create;
  FVectorItmesFactory := AVectorItmesFactory;
  FGPSUnitInfo := '';
  FGPSPositionFactory := AGPSPositionFactory;
  FDatum := ADatum;
  FLastPositionOK := FALSE;
  FCurrentPosition := FGPSPositionFactory.BuildPositionEmpty;
  FGPSPositionFactory.GPSUnitInfoChangedHandler := DoGPSUnitInfoChanged;
end;

destructor TGPSRecorderStuped.Destroy;
begin
  FTrack := nil;
  inherited;
end;

procedure TGPSRecorderStuped.DoGPSUnitInfoChanged(
  Sender: TObject;
  const AUnitIndex: Byte;
  const AKind: TVSAGPS_UNIT_INFO_Kind);
var VNewFullInfo: String;
begin
  if (guik_ClearALL=AKind) then begin
    // clear all info
    VNewFullInfo := '';
  end else begin
    // other parameters
    if not VSAGPS_ChangedFor_GPSUnitInfo(AKind) then
      Exit;
    VNewFullInfo := GenerateGPSUnitInfo(AUnitIndex);
  end;

  LockWrite;
  try
    FGPSUnitInfo := VNewFullInfo;
  finally
    UnlockWrite;
  end;
end;

procedure TGPSRecorderStuped.DoReadConfig(const AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FOdometer1 := AConfigData.ReadFloat('Odometer1', FOdometer1);
    FOdometer2 := AConfigData.ReadFloat('Odometer2', FOdometer2);
    SetChanged;
  end;
end;

procedure TGPSRecorderStuped.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
begin
  inherited;
  AConfigData.WriteFloat('Odometer1', FOdometer1);
  AConfigData.WriteFloat('Odometer2', FOdometer2);
end;

procedure TGPSRecorderStuped.ExecuteGPSCommand(
  Sender: TObject;
  const AUnitIndex: Byte;
  const ACommand: LongInt;
  const APointer: Pointer);
begin
  if (gpsc_Refresh_GPSUnitInfo=ACommand) then begin
    // refresh info
    ReGenerateGPSUnitInfo;
  end else if Assigned(FGPSPositionFactory) then
    FGPSPositionFactory.ExecuteGPSCommand(Sender, AUnitIndex, ACommand, APointer);
end;

procedure TGPSRecorderStuped.AddEmptyPoint;
begin
  LockWrite;
  try
    if (FLastPositionOK) then begin
      // check for alloc
      InternalSafeAlloc;
      // add item to array
      FTrack[FPointsCount].Point := CEmptyDoublePoint;
      FTrack[FPointsCount].Speed := 0;
      FTrack[FPointsCount].Time := NaN;
      // done
      Inc(FPointsCount);
      FLastPositionOK := FALSE;
      SetChanged;
    end;
    FCurrentPosition := FGPSPositionFactory.BuildPositionEmpty;
  finally
    UnlockWrite;
  end;
end;

procedure TGPSRecorderStuped.AddPoint(const APosition: IGPSPosition);
var
  pPos: PSingleGPSData;
  pSatFixAll: PVSAGPS_FIX_ALL;
  VAlfa: Double;
  VBeta: Double;
  VDistToPrev: Double;
  VPointPrev: TDoublePoint;
begin
  if APosition.GetTracksParams(pPos, pSatFixAll) then begin
    LockWrite;
    try
      if FLastPositionOK or pPos^.PositionOK then begin
        // need alloc?
        InternalSafeAlloc;

        // copy to array
        if pPos^.PositionOK then begin
          FTrack[FPointsCount].Point.X := pPos^.PositionLon;
          FTrack[FPointsCount].Point.Y := pPos^.PositionLat;
        end else begin
          FTrack[FPointsCount].Point := CEmptyDoublePoint;
        end;

        if pPos^.UTCDateOK and pPos^.UTCTimeOK then begin
          FTrack[FPointsCount].Time := (pPos^.UTCDate+pPos^.UTCTime);
        end else begin
          FTrack[FPointsCount].Time := NaN;
        end;

        if (not pPos^.PositionOK) or NoData_Float64(pPos^.Altitude) then begin
          FTrack[FPointsCount].Speed := 0;
        end else begin
          FTrack[FPointsCount].Speed := pPos^.Speed_KMH;
        end;

        // check new values
        if pPos^.PositionOK then begin
          FLastPosition.X := pPos^.PositionLon;
          FLastPosition.Y := pPos^.PositionLat;
          FLastAltitude := pPos^.Altitude;
          FLastHeading := pPos^.Heading;
          FLastSpeed := pPos^.Speed_KMH;

          // allow calc max and avg speed even if no stats
          // check AllowCalcStats only for permanent (overall) stats

          // speed may be unavailable
          if (not NoData_Float64(pPos^.Speed_KMH)) then begin
            // max speen
            if (pPos^.Speed_KMH > FMaxSpeed) then
              FMaxSpeed := pPos^.Speed_KMH;
            // avg speed
            FAvgSpeedTickCount := FAvgSpeedTickCount + 1;
            VAlfa := 1 / FAvgSpeedTickCount;
            VBeta := 1 - VAlfa;
            FAvgSpeed := VAlfa * pPos^.Speed_KMH + VBeta * FAvgSpeed;
          end;

          // if prev position available too - calc distance
          // no recalc if AllowCalcStats disabled
          if pPos^.AllowCalcStats then
          if FLastPositionOK then begin
            VPointPrev.X := FTrack[FPointsCount - 1].Point.X; // lon
            VPointPrev.Y := FTrack[FPointsCount - 1].Point.Y; // lat
            VDistToPrev := FDatum.CalcDist(VPointPrev, FLastPosition);
            FDist := FDist + VDistToPrev;
            FOdometer1 := FOdometer1 + VDistToPrev;
            FOdometer2 := FOdometer2 + VDistToPrev;
          end;
        end;

        Inc(FPointsCount);
        FLastPositionOK := pPos^.PositionOK;
        SetChanged;
      end;
      FCurrentPosition := APosition;
    finally
      UnlockWrite;
    end;
  end;
end;

procedure TGPSRecorderStuped.ClearTrack;
begin
  LockWrite;
  try
    if FPointsCount <> 0 then begin
      FPointsCount := 0;
      FLastPositionOK := FALSE;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

function TGPSRecorderStuped.GenerateGPSUnitInfo(const AUnitIndex: Byte): String;
begin
  if Assigned(FGPSPositionFactory) then
    Result:=FGPSPositionFactory.ExecuteGPSCommand(Self, AUnitIndex, gpsc_Refresh_GPSUnitInfo, nil)
  else
    Result:='';
end;

function TGPSRecorderStuped.GetAllPoints: ILonLatPath;
begin
  LockRead;
  try
    Result :=
      FVectorItmesFactory.CreateLonLatPathByEnum(
        TEnumTrackPointsByArray.Create(False, @FTrack[0], FPointsCount)
      );
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

function TGPSRecorderStuped.GetGPSUnitInfo: String;
begin
  LockRead;
  try
    Result:=FGPSUnitInfo;
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

procedure TGPSRecorderStuped.InternalSafeAlloc;
begin
  if FPointsCount >= FAllocatedPoints then begin
    if FAllocatedPoints <= 0 then begin
      FAllocatedPoints := 4096;
    end else begin
      Inc(FAllocatedPoints, 4096);
    end;
    SetLength(FTrack, FAllocatedPoints);
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
  const AMaxCount: Integer
): IEnumGPSTrackPoint;
var
  VStartIndex: Integer;
  VCount: Integer;
begin
  LockRead;
  try
    VCount := AMaxCount;
    if FPointsCount <= VCount then begin
      VCount := FPointsCount;
      VStartIndex := 0;
    end else begin
      VStartIndex :=FPointsCount - VCount;
    end;
    Result :=
      TEnumTrackPointsByArray.Create(
        True,
        @FTrack[VStartIndex],
        VCount
      );
  finally
    UnlockRead;
  end;
end;

procedure TGPSRecorderStuped.ReGenerateGPSUnitInfo;
begin
  LockWrite;
  try
    FGPSUnitInfo := GenerateGPSUnitInfo(cUnitIndex_Reserved);
  finally
    UnlockWrite;
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
