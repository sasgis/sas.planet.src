{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
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
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

unit u_GpsTrackRecorder;

interface

uses
  Windows,
  t_GeoTypes,
  i_GeometryLonLat,
  i_GeometryLonLatFactory,
  i_PathConfig,
  i_GPS,
  i_GPSRecorder,
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
  TGpsTrackRecorder = class(TConfigDataElementBaseEmptySaveLoad, IGpsTrackRecorder, IGpsTrackRecorderInternal)
  private
    FDataFile: IPathConfig;
    FVectorGeometryLonLatFactory: IGeometryLonLatFactory;

    FTrack: ITrackPointsBlocksListStatic;
    FLastBlock: ITrackPoitnsBlock;
    FPointsInBlockCount: Integer;
    FLastPositionOK: Boolean;

    procedure _AddPointInternal(const APoint: TGPSTrackPoint);
  private
    procedure Load;
    procedure Save;
  private
    procedure AddPoint(const APosition: IGPSPosition);
    procedure AddEmptyPoint;
    procedure ClearTrack;
    function IsEmpty: Boolean;

    function LastPoints(
      const AMaxCount: Integer
    ): IEnumGPSTrackPoint;

    function GetAllPoints: IGeometryLonLatLine;
  public
    constructor Create(
      const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
      const ADataFile: IPathConfig
    );
  end;

implementation

uses
  Math,
  SysUtils,
  Classes,
  i_EnumDoublePoint,
  u_GeoFunc,
  u_BaseInterfacedObject;

{ TTrackPoitnsBlock }

type
  TTrackPoitnsBlock = class(TBaseInterfacedObject, ITrackPoitnsBlock)
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
  inherited Create;
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
  TTrackPointsBlocksListStatic = class(TBaseInterfacedObject, ITrackPointsBlocksListStatic)
  private
    FCount: Integer;
    FBlocks: array of ITrackPoitnsBlock;
  private
    function GetCount: Integer;
    function GetBlock(AIndex: Integer): ITrackPoitnsBlock;
  public
    constructor Create;
    constructor CreateWithAppend(
      const APrevList: ITrackPointsBlocksListStatic;
      const ANewBlock: ITrackPoitnsBlock
    );
    constructor CreateLastBlocks(
      const ASourceList: ITrackPointsBlocksListStatic;
      const ALastBlockCount: Integer
    );
    destructor Destroy; override;
  end;

constructor TTrackPointsBlocksListStatic.Create;
begin
  inherited Create;
  FCount := 0;
end;

constructor TTrackPointsBlocksListStatic.CreateLastBlocks(
  const ASourceList: ITrackPointsBlocksListStatic;
  const ALastBlockCount: Integer
);
var
  i: Integer;
  VIndex: Integer;
begin
  inherited Create;
  Assert(ASourceList <> nil);
  Assert(ALastBlockCount > 0);
  Assert(ASourceList.Count > ALastBlockCount);
  if (ASourceList <> nil) and (ALastBlockCount > 0) then begin
    FCount := ASourceList.Count;
    if FCount > ALastBlockCount then begin
      FCount := ALastBlockCount;
    end;
  end else begin
    FCount := 0;
  end;
  SetLength(FBlocks, FCount);
  VIndex := ASourceList.Count - FCount;
  for i := 0 to FCount - 1 do begin
    FBlocks[i] := ASourceList.Block[VIndex];
    Inc(VIndex);
  end;
end;

constructor TTrackPointsBlocksListStatic.CreateWithAppend(
  const APrevList: ITrackPointsBlocksListStatic;
  const ANewBlock: ITrackPoitnsBlock
);
var
  i: Integer;
begin
  inherited Create;
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
    for i := 0 to APrevList.Count - 1 do begin
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

{ TEnumGPSTrackPointEmpty }

type
  TEnumGPSTrackPointEmpty = class(TBaseInterfacedObject, IEnumGPSTrackPoint)
  private
    function Next(out APoint: TGPSTrackPoint): Boolean;
  end;

function TEnumGPSTrackPointEmpty.Next(out APoint: TGPSTrackPoint): Boolean;
begin
  APoint.Point := CEmptyDoublePoint;
  APoint.Speed := NaN;
  APoint.Time := NaN;
  Result := False;
end;

{ TEnumGPSTrackPointByBlocksListBase }

type
  TEnumGPSTrackPointByBlocksListBase = class(TBaseInterfacedObject, IEnumGPSTrackPoint)
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
    constructor Create(
      const AList: ITrackPointsBlocksListStatic;
      AValidPointsInLastBlock: Integer
    );
  end;

constructor TEnumGPSTrackPointByBlocksListBase.Create(
  const AList: ITrackPointsBlocksListStatic;
  AValidPointsInLastBlock: Integer
);
begin
  inherited Create;
  FList := AList;
  Assert(FList <> nil);
  FValidPointsInLastBlock := AValidPointsInLastBlock;
  Assert((FList.Count = 0) or (FList.Block[FList.Count - 1].Count >= FValidPointsInLastBlock));
end;

{ TEnumGPSTrackPointByBlocksList }

type
  TEnumGPSTrackPointByBlocksList = class(TEnumGPSTrackPointByBlocksListBase)
  private
    function PrepareNextBlock: Boolean; override;
  private
    function Next(out APoint: TGPSTrackPoint): Boolean; override;
  public
    constructor Create(
      const AList: ITrackPointsBlocksListStatic;
      AValidPointsInLastBlock: Integer
    );
  end;

constructor TEnumGPSTrackPointByBlocksList.Create(
  const AList: ITrackPointsBlocksListStatic;
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
  if FBlockIndex >= FList.Count - 1 then begin
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
    constructor Create(
      const AList: ITrackPointsBlocksListStatic;
      AValidPointsInLastBlock: Integer
    );
  end;

constructor TEnumGPSTrackPointByBlocksListBackward.Create(
  const AList: ITrackPointsBlocksListStatic;
  AValidPointsInLastBlock: Integer
);
begin
  inherited;
  FBlockIndex := FList.Count - 1;
  FPointInBlockIndex := FValidPointsInLastBlock - 1;
  if FBlockIndex >= 0 then begin
    FBlockPoints := FList.Block[FBlockIndex].Points;
  end;
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
  if FBlockIndex < 0 then begin
    Result := False;
  end else begin
    Dec(FBlockIndex);
    Result := FBlockIndex >= 0;
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
  TEnumTrackPointsByEnumGPSTrackPoint = class(TBaseInterfacedObject, IEnumDoublePoint, IEnumLonLatPoint)
  private
    FSource: IEnumGPSTrackPoint;
  private
    function Next(out APoint: TDoublePoint): Boolean;
  public
    constructor Create(
      const ASource: IEnumGPSTrackPoint
    );
  end;

constructor TEnumTrackPointsByEnumGPSTrackPoint.Create(
  const ASource: IEnumGPSTrackPoint
);
begin
  inherited Create;
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

const
  CVersionMagicID = 20130127;

{ TGpsTrackRecorder }

constructor TGpsTrackRecorder.Create(
  const AVectorGeometryLonLatFactory: IGeometryLonLatFactory; const ADataFile: IPathConfig);
begin
  inherited Create;
  FVectorGeometryLonLatFactory := AVectorGeometryLonLatFactory;
  FDataFile := ADataFile;
  FLastPositionOK := False;
  FTrack := TTrackPointsBlocksListStatic.Create;
end;

procedure TGpsTrackRecorder.AddEmptyPoint;
var
  VPoint: TGPSTrackPoint;
begin
  VPoint.Point := CEmptyDoublePoint;
  VPoint.Speed := 0;
  VPoint.Time := NaN;
  LockWrite;
  try
    if (FLastPositionOK) then begin
      _AddPointInternal(VPoint);
      FLastPositionOK := False;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TGpsTrackRecorder.AddPoint(const APosition: IGPSPosition);
var
  VPoint: TGPSTrackPoint;
begin
  if APosition.PositionOK then begin
    VPoint.Point := APosition.LonLat;
  end else begin
    VPoint.Point := CEmptyDoublePoint;
  end;

  if APosition.UTCTimeOK then begin
    VPoint.Time := APosition.UTCTime;
  end else begin
    VPoint.Time := NaN;
  end;

  if (not APosition.PositionOK) or (not APosition.SpeedOK) then begin
    VPoint.Speed := 0;
  end else begin
    VPoint.Speed := APosition.Speed_KMH;
  end;
  LockWrite;
  try
    if FLastPositionOK or APosition.PositionOK then begin
      _AddPointInternal(VPoint);
      FLastPositionOK := APosition.PositionOK;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TGpsTrackRecorder._AddPointInternal(
  const APoint: TGPSTrackPoint
);
begin
  if FLastBlock = nil then begin
    FLastBlock := TTrackPoitnsBlock.Create(2048);
    FTrack := TTrackPointsBlocksListStatic.CreateWithAppend(FTrack, FLastBlock);
    FPointsInBlockCount := 0;
  end else begin
    if FPointsInBlockCount >= FLastBlock.Count then begin
      FLastBlock := TTrackPoitnsBlock.Create(2048);
      FTrack := TTrackPointsBlocksListStatic.CreateWithAppend(FTrack, FLastBlock);
      FPointsInBlockCount := 0;
    end;
  end;
  FLastBlock.Points[FPointsInBlockCount] := APoint;
  Inc(FPointsInBlockCount);
end;

procedure TGpsTrackRecorder.ClearTrack;
begin
  LockWrite;
  try
    if FTrack <> nil then begin
      FTrack := TTrackPointsBlocksListStatic.Create;
      FLastBlock := nil;
      FPointsInBlockCount := 0;
      FLastPositionOK := False;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

function TGpsTrackRecorder.GetAllPoints: IGeometryLonLatLine;
var
  VTrackPointsEnum: IEnumGPSTrackPoint;
  VPointsEnum: IEnumLonLatPoint;
begin
  LockRead;
  try
    VTrackPointsEnum :=
      TEnumGPSTrackPointByBlocksList.Create(
        FTrack,
        FPointsInBlockCount
      );
    VPointsEnum :=
      TEnumTrackPointsByEnumGPSTrackPoint.Create(VTrackPointsEnum);
    Result := FVectorGeometryLonLatFactory.CreateLonLatMultiLineByEnum(VPointsEnum);
  finally
    UnlockRead;
  end;
end;

function TGpsTrackRecorder.IsEmpty: Boolean;
begin
  LockRead;
  try
    Result := FLastBlock <> nil;
  finally
    UnlockRead;
  end;
end;

function TGpsTrackRecorder.LastPoints(
  const AMaxCount: Integer
): IEnumGPSTrackPoint;
var
  VBlockCount: Integer;
  VPointsLeft: Integer;
  VBlockIndex: Integer;
begin
  Assert(AMaxCount > 0);
  VPointsLeft := AMaxCount;
  if VPointsLeft < 0 then begin
    VPointsLeft := 0;
  end;
  LockRead;
  try
    if (VPointsLeft <= 0) or ((FTrack.Count = 1) and (FPointsInBlockCount = 0)) then begin
      Result := TEnumGPSTrackPointEmpty.Create;
    end else begin
      VBlockCount := 1;
      VPointsLeft := VPointsLeft - FPointsInBlockCount;
      VBlockIndex := FTrack.Count - 2;
      while (VPointsLeft > 0) and (VBlockIndex >= 0) do begin
        VPointsLeft := VPointsLeft - FTrack.Block[VBlockIndex].Count;
        Inc(VBlockCount);
        Dec(VBlockIndex);
      end;
      if VBlockCount < FTrack.Count then begin
        Result :=
          TEnumGPSTrackPointByBlocksListBackward.Create(
            TTrackPointsBlocksListStatic.CreateLastBlocks(FTrack, VBlockCount),
            FPointsInBlockCount
          );
      end else begin
        Result :=
          TEnumGPSTrackPointByBlocksListBackward.Create(
            FTrack,
            FPointsInBlockCount
          );
      end;
    end;
  finally
    UnlockRead;
  end;
end;

procedure TGpsTrackRecorder.Load;
var
  VFileName: string;
  VStream: TStream;
  VPoint: TGPSTrackPoint;
  VVersion: Integer;
begin
  inherited;
  VFileName := FDataFile.FullPath;
  if FileExists(VFileName) then begin
    try
      VStream := TFileStream.Create(VFileName, fmOpenRead);
      try
        if VStream.Read(VVersion, SizeOf(VVersion)) = SizeOf(VVersion) then begin
          if VVersion = CVersionMagicID then begin
            while True do begin
              if VStream.Read(VPoint.Point.X, SizeOf(VPoint.Point.X)) <> SizeOf(VPoint.Point.X) then begin
                AddEmptyPoint;
                Break;
              end;
              if VStream.Read(VPoint.Point.Y, SizeOf(VPoint.Point.Y)) <> SizeOf(VPoint.Point.Y) then begin
                AddEmptyPoint;
                Break;
              end;
              if VStream.Read(VPoint.Speed, SizeOf(VPoint.Speed)) <> SizeOf(VPoint.Speed) then begin
                AddEmptyPoint;
                Break;
              end;
              if VStream.Read(VPoint.Time, SizeOf(VPoint.Time)) <> SizeOf(VPoint.Time) then begin
                AddEmptyPoint;
                Break;
              end;
              if PointIsEmpty(VPoint.Point) then begin
                AddEmptyPoint;
              end else begin
                _AddPointInternal(VPoint);
                FLastPositionOK := True;
                SetChanged;
              end;
            end;
          end;
        end;
      finally
        VStream.Free;
      end;
    except
      Assert(False, 'Exception on GPSRecorder read');
    end;
  end;
end;

procedure TGpsTrackRecorder.Save;
var
  VFileName: string;
  VEnum: IEnumGPSTrackPoint;
  VStream: TStream;
  VPoint: TGPSTrackPoint;
  VVersion: Integer;
begin
  inherited;
  VFileName := FDataFile.FullPath;
  try
    VStream := nil;
    try
      VEnum := LastPoints(10000);
      while VEnum.Next(VPoint) do begin
        if not Assigned(VStream) then begin
          VStream := TFileStream.Create(VFileName, fmCreate);
          VVersion := CVersionMagicID;
          VStream.WriteBuffer(VVersion, SizeOf(VVersion));
        end;
        VStream.WriteBuffer(VPoint.Point.X, SizeOf(VPoint.Point.X));
        VStream.WriteBuffer(VPoint.Point.Y, SizeOf(VPoint.Point.Y));
        VStream.WriteBuffer(VPoint.Speed, SizeOf(VPoint.Speed));
        VStream.WriteBuffer(VPoint.Time, SizeOf(VPoint.Time));
      end;
      if not Assigned(VStream) then begin
        DeleteFile(VFileName);
      end;
    finally
      VStream.Free;
    end;
  except
    Assert(False, 'Exception on GPSRecorder write');
  end;
end;

end.
