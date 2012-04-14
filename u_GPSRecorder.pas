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

unit u_GPSRecorder;

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
  ITrackPoitnsBlock = interface
  ['{E8161A93-14F0-45BC-B4B7-0A28D92965E6}']
    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetPoints: PTrackPointArray;
    property Points: PTrackPointArray read GetPoints;
  end;

type
  ITrackPointsBlocksListStatic = interface
    ['{B7C17411-5D8A-49D4-BF3B-A1E5A44CC998}']
    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetBlock(AIndex: Integer): ITrackPoitnsBlock;
    property Block[AIndex: Integer]: ITrackPoitnsBlock read GetBlock;
  end;

type
  TGPSRecorder = class(TConfigDataElementBase, IGPSRecorder)
  private
    FGPSPositionFactory: IGPSPositionFactory;
    FVectorItmesFactory: IVectorItmesFactory;
    FDatum: IDatum;

    FTrack: ITrackPointsBlocksListStatic;
    FLastBlock: ITrackPoitnsBlock;
    FPointsInBlockCount: Integer;

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
    function AddPointInternal(const APoint: TGPSTrackPoint): TDoublePoint;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;

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

    function LastPoints(
      const AMaxCount: Integer
    ): IEnumGPSTrackPoint;

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

{ TTrackPoitnsBlock }

type
  TTrackPoitnsBlock = class(TInterfacedObject, ITrackPoitnsBlock)
  private
    FPoints: array of TGPSTrackPoint;
    FCount: Integer;
  private
    function GetCount: Integer;
    function GetPoints: PTrackPointArray;
  public
    constructor Create(
      ACount: Integer
    );
  end;

constructor TTrackPoitnsBlock.Create(ACount: Integer);
begin
  FCount := ACount;
  Assert(FCount > 0);
  SetLength(FPoints, FCount);
end;

function TTrackPoitnsBlock.GetCount: Integer;
begin
  Result := FCount;
end;

function TTrackPoitnsBlock.GetPoints: PTrackPointArray;
begin
  Result := @FPoints[0];
end;

{ TTrackPointsBlocksListStatic }

type
  TTrackPointsBlocksListStatic = class(TInterfacedObject, ITrackPointsBlocksListStatic)
  private
    FCount: Integer;
    FBlocks: array of ITrackPoitnsBlock;
  private
    function GetCount: Integer;
    function GetBlock(AIndex: Integer): ITrackPoitnsBlock;
  public
    constructor Create(
      APrevList: ITrackPointsBlocksListStatic;
      ANewBlock: ITrackPoitnsBlock
    );
    destructor Destroy; override;
  end;

constructor TTrackPointsBlocksListStatic.Create(
  APrevList: ITrackPointsBlocksListStatic;
  ANewBlock: ITrackPoitnsBlock
);
var
  i: Integer;
begin
  if APrevList <> nil then begin
    FCount := APrevList.Count;
  end else begin
    FCount := 0;
  end;
  if ANewBlock <> nil then begin
    Inc(FCount);
  end;
  SetLength(FBlocks, FCount);
  if APrevList <> nil then begin
    for i := 0 to FCount - 1 do begin
      FBlocks[i] := APrevList.Block[i];
    end;
  end;
  if ANewBlock <> nil then begin
    FBlocks[FCount - 1] := ANewBlock;
  end;
end;

destructor TTrackPointsBlocksListStatic.Destroy;
var
  i: Integer;
begin
  for i := 0 to FCount - 1 do begin
    FBlocks[i] := nil;
  end;
  FBlocks := nil;
  inherited;
end;

function TTrackPointsBlocksListStatic.GetBlock(
  AIndex: Integer
): ITrackPoitnsBlock;
begin
  Result := FBlocks[AIndex];
end;

function TTrackPointsBlocksListStatic.GetCount: Integer;
begin
  Result := FCount;
end;

{ TEnumGPSTrackPointByBlocksListBase }

type
  TEnumGPSTrackPointByBlocksListBase = class(TInterfacedObject, IEnumGPSTrackPoint)
  private
    FList: ITrackPointsBlocksListStatic;
    FValidPointsInLastBlock: Integer;

    FBlockIndex: Integer;
    FBlockPoints: PTrackPointArray;
    FBlockPointsCount: Integer;
    FPointInBlockIndex: Integer;
    function PrepareNextBlock: Boolean; virtual; abstract;
  private
    function Next(out APoint: TGPSTrackPoint): Boolean; virtual; abstract;
  public
    constructor Create(AList: ITrackPointsBlocksListStatic; AValidPointsInLastBlock: Integer);
  end;

constructor TEnumGPSTrackPointByBlocksListBase.Create(
  AList: ITrackPointsBlocksListStatic; AValidPointsInLastBlock: Integer);
begin
  FList := AList;
  Assert(FList <> nil);
  FValidPointsInLastBlock := AValidPointsInLastBlock;
  Assert(FList.Count > 0);
  Assert(FList.Block[FList.Count - 1].Count > FValidPointsInLastBlock);
end;

{ TEnumGPSTrackPointByBlocksList }

type
  TEnumGPSTrackPointByBlocksList = class(TEnumGPSTrackPointByBlocksListBase)
  private
    function PrepareNextBlock: Boolean; override;
  private
    function Next(out APoint: TGPSTrackPoint): Boolean; override;
  public
    constructor Create(AList: ITrackPointsBlocksListStatic; AValidPointsInLastBlock: Integer);
  end;

constructor TEnumGPSTrackPointByBlocksList.Create(
  AList: ITrackPointsBlocksListStatic;
  AValidPointsInLastBlock: Integer
);
begin
  inherited;
  FBlockIndex := -1;
  FBlockPointsCount := 0;
  FPointInBlockIndex := 0;
end;

function TEnumGPSTrackPointByBlocksList.Next(
  out APoint: TGPSTrackPoint): Boolean;
begin
  Result := True;
  if FPointInBlockIndex >= FBlockPointsCount then begin
    Result := PrepareNextBlock;
  end;

  if Result then begin
    APoint := FBlockPoints[FPointInBlockIndex];
    Inc(FPointInBlockIndex);
  end;
end;

function TEnumGPSTrackPointByBlocksList.PrepareNextBlock: Boolean;
var
  VBlock: ITrackPoitnsBlock;
begin
  if FBlockIndex  >= FList.Count - 1 then begin
    Result := False;
  end else begin
    Inc(FBlockIndex);
    VBlock := FList.Block[FBlockIndex];
    FBlockPoints := VBlock.Points;
    FBlockPointsCount := VBlock.Count;
    FPointInBlockIndex := 0;
    if FBlockIndex = FList.Count - 1 then begin
      FBlockPointsCount := FValidPointsInLastBlock;
      Result := FBlockPointsCount > 0;
    end else begin
      Result := True;
    end;
  end;
end;

{ TEnumGPSTrackPointByBlocksListBackward }

type
  TEnumGPSTrackPointByBlocksListBackward = class(TEnumGPSTrackPointByBlocksListBase)
  private
    function PrepareNextBlock: Boolean; override;
  private
    function Next(out APoint: TGPSTrackPoint): Boolean; override;
  public
    constructor Create(AList: ITrackPointsBlocksListStatic; AValidPointsInLastBlock: Integer);
  end;

constructor TEnumGPSTrackPointByBlocksListBackward.Create(
  AList: ITrackPointsBlocksListStatic;
  AValidPointsInLastBlock: Integer
);
begin
  inherited;
  FBlockIndex := FList.Count - 1;
  FPointInBlockIndex := FValidPointsInLastBlock - 1;
end;

function TEnumGPSTrackPointByBlocksListBackward.Next(
  out APoint: TGPSTrackPoint): Boolean;
begin
  Result := True;
  if FPointInBlockIndex < 0 then begin
    Result := PrepareNextBlock;
  end;

  if Result then begin
    APoint := FBlockPoints[FPointInBlockIndex];
    Dec(FPointInBlockIndex);
  end;
end;

function TEnumGPSTrackPointByBlocksListBackward.PrepareNextBlock: Boolean;
var
  VBlock: ITrackPoitnsBlock;
begin
  if FBlockIndex  < 0 then begin
    Result := False;
  end else begin
    Inc(FBlockIndex);
    Result := FBlockIndex  >= 0;
    if Result then begin
      VBlock := FList.Block[FBlockIndex];
      FBlockPoints := VBlock.Points;
      FBlockPointsCount := VBlock.Count;
      FPointInBlockIndex := FBlockPointsCount - 1;
    end;
  end;
end;

{ TEnumDoublePointsByEnumGPSTrackPoint }

type
  TEnumTrackPointsByEnumGPSTrackPoint = class(TInterfacedObject, IEnumDoublePoint, IEnumLonLatPoint)
  private
    FSource: IEnumGPSTrackPoint;
  private
    function Next(out APoint: TDoublePoint): Boolean;
  public
    constructor Create(
      ASource: IEnumGPSTrackPoint
    );
  end;

constructor TEnumTrackPointsByEnumGPSTrackPoint.Create(
  ASource: IEnumGPSTrackPoint
);
begin
  FSource := ASource;
end;

function TEnumTrackPointsByEnumGPSTrackPoint.Next(out APoint: TDoublePoint): Boolean;
var
  VPoint: TGPSTrackPoint;
begin
  Result := FSource.Next(VPoint);
  if Result then begin
    APoint := VPoint.Point;
  end;
end;


{ TGPSRecorderStuped }

constructor TGPSRecorder.Create(
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

destructor TGPSRecorder.Destroy;
begin
  FTrack := nil;
  inherited;
end;

procedure TGPSRecorder.DoGPSUnitInfoChanged(
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

procedure TGPSRecorder.DoReadConfig(AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FOdometer1 := AConfigData.ReadFloat('Odometer1', FOdometer1);
    FOdometer2 := AConfigData.ReadFloat('Odometer2', FOdometer2);
    SetChanged;
  end;
end;

procedure TGPSRecorder.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteFloat('Odometer1', FOdometer1);
  AConfigData.WriteFloat('Odometer2', FOdometer2);
end;

procedure TGPSRecorder.ExecuteGPSCommand(
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

function TGPSRecorder.AddPointInternal(const APoint: TGPSTrackPoint): TDoublePoint;
begin
  if FLastBlock = nil then begin
    FLastBlock := TTrackPoitnsBlock.Create(2048);
    FPointsInBlockCount := 0;
    Result := CEmptyDoublePoint;
  end else begin
    Result := FLastBlock.Points[FPointsInBlockCount - 1].Point;
    if FPointsInBlockCount >= FLastBlock.Count then begin
      FTrack := TTrackPointsBlocksListStatic.Create(FTrack, FLastBlock);
      FLastBlock := TTrackPoitnsBlock.Create(2048);
      FPointsInBlockCount := 0;
    end;
  end;
  FLastBlock.Points[FPointsInBlockCount] := APoint;
  Inc(FPointsInBlockCount);
end;

procedure TGPSRecorder.AddEmptyPoint;
var
  VPoint: TGPSTrackPoint;
begin
  VPoint.Point := CEmptyDoublePoint;
  VPoint.Speed := 0;
  VPoint.Time := NaN;
  LockWrite;
  try
    if (FLastPositionOK) then begin
      AddPointInternal(VPoint);
      FLastPositionOK := False;
      SetChanged;
    end;
    FCurrentPosition := FGPSPositionFactory.BuildPositionEmpty;
  finally
    UnlockWrite;
  end;
end;

procedure TGPSRecorder.AddPoint(const APosition: IGPSPosition);
var
  pPos: PSingleGPSData;
  pSatFixAll: PVSAGPS_FIX_ALL;
  VAlfa: Double;
  VBeta: Double;
  VDistToPrev: Double;
  VPointPrev: TDoublePoint;
  VPoint: TGPSTrackPoint;
begin
  if APosition.GetTracksParams(pPos, pSatFixAll) then begin
    if pPos^.PositionOK then begin
      VPoint.Point.X := pPos^.PositionLon;
      VPoint.Point.Y := pPos^.PositionLat;
    end else begin
      VPoint.Point := CEmptyDoublePoint;
    end;

    if pPos^.UTCDateOK and pPos^.UTCTimeOK then begin
      VPoint.Time := (pPos^.UTCDate+pPos^.UTCTime);
    end else begin
      VPoint.Time := NaN;
    end;

    if (not pPos^.PositionOK) or NoData_Float64(pPos^.Speed_KMH) then begin
      VPoint.Speed := 0;
    end else begin
      VPoint.Speed := pPos^.Speed_KMH;
    end;

    LockWrite;
    try
      if FLastPositionOK or pPos^.PositionOK then begin
        VPointPrev := AddPointInternal(VPoint);

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
          if not PointIsEmpty(VPointPrev) then begin
            VDistToPrev := FDatum.CalcDist(VPointPrev, FLastPosition);
            FDist := FDist + VDistToPrev;
            FOdometer1 := FOdometer1 + VDistToPrev;
            FOdometer2 := FOdometer2 + VDistToPrev;
          end;
        end;

        FLastPositionOK := pPos^.PositionOK;
        SetChanged;
      end;
      FCurrentPosition := APosition;
    finally
      UnlockWrite;
    end;
  end;
end;

procedure TGPSRecorder.ClearTrack;
begin
  LockWrite;
  try
    if FTrack <> nil then begin
      FTrack := nil;
      FPointsInBlockCount := 0;
      FLastPositionOK := FALSE;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

function TGPSRecorder.GenerateGPSUnitInfo(const AUnitIndex: Byte): String;
begin
  if Assigned(FGPSPositionFactory) then
    Result:=FGPSPositionFactory.ExecuteGPSCommand(Self, AUnitIndex, gpsc_Refresh_GPSUnitInfo, nil)
  else
    Result:='';
end;

function TGPSRecorder.GetAllPoints: ILonLatPath;
begin
  LockRead;
  try
    Result :=
      FVectorItmesFactory.CreateLonLatPathByEnum(
        TEnumTrackPointsByEnumGPSTrackPoint.Create(
          TEnumGPSTrackPointByBlocksList.Create(
            FTrack,
            FPointsInBlockCount
          )
        )
      );
  finally
    UnlockRead;
  end;
end;

function TGPSRecorder.GetAvgSpeed: Double;
begin
  LockRead;
  try
    Result := FAvgSpeed;
  finally
    UnlockRead;
  end;
end;

function TGPSRecorder.GetCurrentPosition: IGPSPosition;
begin
  LockRead;
  try
    Result := FCurrentPosition;
  finally
    UnlockRead;
  end;
end;

function TGPSRecorder.GetDist: Double;
begin
  LockRead;
  try
    Result := FDist;
  finally
    UnlockRead;
  end;
end;

function TGPSRecorder.GetGPSUnitInfo: String;
begin
  LockRead;
  try
    Result:=FGPSUnitInfo;
  finally
    UnlockRead;
  end;
end;

function TGPSRecorder.GetLastAltitude: Double;
begin
  LockRead;
  try
    Result := FLastAltitude;
  finally
    UnlockRead;
  end;
end;

function TGPSRecorder.GetLastHeading: Double;
begin
  LockRead;
  try
    Result := FLastHeading;
  finally
    UnlockRead;
  end;
end;

function TGPSRecorder.GetLastPosition: TDoublePoint;
begin
  LockRead;
  try
    Result := FLastPosition;
  finally
    UnlockRead;
  end;
end;

function TGPSRecorder.GetLastSpeed: Double;
begin
  LockRead;
  try
    Result := FLastSpeed;
  finally
    UnlockRead;
  end;
end;

function TGPSRecorder.GetMaxSpeed: Double;
begin
  LockRead;
  try
    Result := FMaxSpeed;
  finally
    UnlockRead;
  end;
end;

function TGPSRecorder.GetOdometer1: Double;
begin
  LockRead;
  try
    Result := FOdometer1;
  finally
    UnlockRead;
  end;
end;

function TGPSRecorder.GetOdometer2: Double;
begin
  LockRead;
  try
    Result := FOdometer2;
  finally
    UnlockRead;
  end;
end;

function TGPSRecorder.IsEmpty: Boolean;
begin
  LockRead;
  try
    Result := FTrack <> nil;
  finally
    UnlockRead;
  end;
end;

function TGPSRecorder.LastPoints(
  const AMaxCount: Integer
): IEnumGPSTrackPoint;
begin
  LockRead;
  try
    Result :=
      TEnumGPSTrackPointByBlocksListBackward.Create(
        FTrack,
        FPointsInBlockCount
      );
  finally
    UnlockRead;
  end;
end;

procedure TGPSRecorder.ReGenerateGPSUnitInfo;
begin
  LockWrite;
  try
    FGPSUnitInfo := GenerateGPSUnitInfo(cUnitIndex_Reserved);
  finally
    UnlockWrite;
  end;
end;

procedure TGPSRecorder.ResetAvgSpeed;
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

procedure TGPSRecorder.ResetDist;
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

procedure TGPSRecorder.ResetMaxSpeed;
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

procedure TGPSRecorder.ResetOdometer1;
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

procedure TGPSRecorder.ResetOdometer2;
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
