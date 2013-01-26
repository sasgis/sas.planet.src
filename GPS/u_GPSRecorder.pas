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
  i_VectorItemsFactory,
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
  TGPSRecorder = class(TConfigDataElementBase, IGPSRecorder)
  private
    FDataFile: IPathConfig;
    FVectorItemsFactory: IVectorItemsFactory;
    FDatum: IDatum;
    FEmptyPosition: IGPSPosition;

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

    function AddPointInternal(const APoint: TGPSTrackPoint): TDoublePoint;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
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
  public
    constructor Create(
      const AVectorItemsFactory: IVectorItemsFactory;
      const ADatum: IDatum;
      const ADataFile: IPathConfig;
      const AEmptyPosition: IGPSPosition
    );
    destructor Destroy; override;
  end;

implementation

uses
  Math,
  SysUtils,
  Classes,
  IniFiles,
  vsagps_public_base,
  i_EnumDoublePoint,
  i_BinaryData,
  u_GeoFun,
  u_BinaryDataByMemStream,
  u_StreamReadOnlyByBinaryData,
  u_ConfigDataProviderByIniFile,
  u_ConfigDataWriteProviderByIniFile,
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
  CVersionMagicID = 20130123;

{ TGPSRecorderStuped }

constructor TGPSRecorder.Create(
  const AVectorItemsFactory: IVectorItemsFactory;
  const ADatum: IDatum;
  const ADataFile: IPathConfig;
  const AEmptyPosition: IGPSPosition
);
begin
  inherited Create;
  FVectorItemsFactory := AVectorItemsFactory;
  FDataFile := ADataFile;
  FEmptyPosition := AEmptyPosition;
  FDatum := ADatum;
  FLastPositionOK := FALSE;
  FCurrentPosition := FEmptyPosition;
  FTrack := TTrackPointsBlocksListStatic.Create;
end;

destructor TGPSRecorder.Destroy;
begin
  FTrack := nil;
  FLastBlock := nil;
  inherited;
end;

procedure TGPSRecorder.DoReadConfig(const AConfigData: IConfigDataProvider);
var
  VFileName: string;
  VIniFile: TMemIniFile;
  VData: IConfigDataProvider;
  VSensorsData: IConfigDataProvider;
  VTrackData: IConfigDataProvider;
  VTrackBinData: IBinaryData;
  VStream: TCustomMemoryStream;
  VPoint: TGPSTrackPoint;
begin
  inherited;
  VFileName := FDataFile.FullPath;
  if FileExists(VFileName) then begin
    try
      VIniFile := TMemIniFile.Create(VFileName);
      try
        VData := TConfigDataProviderByIniFile.Create(VIniFile);
        VIniFile := nil;
        VSensorsData := VData.GetSubItem('GPS');
        if VSensorsData <> nil then begin
          FOdometer1 := VSensorsData.ReadFloat('Odometer1', FOdometer1);
          FOdometer2 := VSensorsData.ReadFloat('Odometer2', FOdometer2);
          SetChanged;
        end;

        VTrackData := VData.GetSubItem('Track');
        if VTrackData <> nil then begin
          if VTrackData.ReadInteger('Version', 0) = CVersionMagicID then begin
            VTrackBinData := VTrackData.ReadBinary('Data');
            if VTrackBinData <> nil then begin
              VStream := TStreamReadOnlyByBinaryData.Create(VTrackBinData);
              try
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
                    AddPointInternal(VPoint);
                    FLastPositionOK := True;
                  end;
                end;
              finally
                VStream.Free;
              end;
            end;
          end;
        end;
      finally
        VIniFile.Free;
      end;
    except
      Assert(False, 'Exception on GPSRecorder read');
    end;
  end;
end;

procedure TGPSRecorder.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
var
  VFileName: string;
  VIniFile: TMemIniFile;
  VData: IConfigDataWriteProvider;
  VSensorsData: IConfigDataWriteProvider;
  VTrackData: IConfigDataWriteProvider;
  VEnum: IEnumGPSTrackPoint;
  VMemStream: TMemoryStream;
  VPoint: TGPSTrackPoint;
  VBinaryData: IBinaryData;
begin
  inherited;
  VFileName := FDataFile.FullPath;
  try
    VIniFile := TMemIniFile.Create(VFileName);
    try
      VData := TConfigDataWriteProviderByIniFile.Create(VIniFile);
      VIniFile := nil;
      VSensorsData := VData.GetOrCreateSubItem('GPS');
      VSensorsData.WriteFloat('Odometer1', FOdometer1);
      VSensorsData.WriteFloat('Odometer2', FOdometer2);

      VTrackData := VData.GetOrCreateSubItem('Track');
      VTrackData.WriteInteger('Version', CVersionMagicID);
      VEnum := LastPoints(10000);
      VBinaryData := nil;
      VMemStream := TMemoryStream.Create;
      try
        while VEnum.Next(VPoint) do begin
          VMemStream.WriteBuffer(VPoint.Point.X, SizeOf(VPoint.Point.X));
          VMemStream.WriteBuffer(VPoint.Point.Y, SizeOf(VPoint.Point.Y));
          VMemStream.WriteBuffer(VPoint.Speed, SizeOf(VPoint.Speed));
          VMemStream.WriteBuffer(VPoint.Time, SizeOf(VPoint.Time));
        end;
        if VMemStream.Size > 0 then begin
          VBinaryData := TBinaryDataByMemStream.CreateWithOwn(VMemStream);
          VMemStream := nil;
        end;
      finally
        VMemStream.Free;
      end;
      if VBinaryData <> nil then begin
        VTrackData.WriteBinary('Data', VBinaryData);
      end else begin
        VTrackData.DeleteValue('Data');
      end;
    finally
      VIniFile.Free;
    end;
  except
    Assert(False, 'Exception on GPSRecorder write');
  end;
end;

function TGPSRecorder.AddPointInternal(const APoint: TGPSTrackPoint): TDoublePoint;
begin
  if FLastBlock = nil then begin
    FLastBlock := TTrackPoitnsBlock.Create(2048);
    FTrack := TTrackPointsBlocksListStatic.CreateWithAppend(FTrack, FLastBlock);
    FPointsInBlockCount := 0;
    Result := CEmptyDoublePoint;
  end else begin
    Result := FLastBlock.Points[FPointsInBlockCount - 1].Point;
    if FPointsInBlockCount >= FLastBlock.Count then begin
      FLastBlock := TTrackPoitnsBlock.Create(2048);
      FTrack := TTrackPointsBlocksListStatic.CreateWithAppend(FTrack, FLastBlock);
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
    FCurrentPosition := FEmptyPosition;
  finally
    UnlockWrite;
  end;
end;

procedure TGPSRecorder.AddPoint(const APosition: IGPSPosition);
var
  VAlfa: Double;
  VBeta: Double;
  VDistToPrev: Double;
  VPointPrev: TDoublePoint;
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
        VPointPrev := AddPointInternal(VPoint);

        // check new values
        if APosition.PositionOK then begin
          FLastPosition := APosition.LonLat;
          FLastAltitude := APosition.Altitude;
          FLastHeading := APosition.Heading;
          FLastSpeed := APosition.Speed_KMH;

          // allow calc max and avg speed even if no stats
          // check AllowCalcStats only for permanent (overall) stats

          // speed may be unavailable
          if APosition.SpeedOK then begin
            // max speen
            if (APosition.Speed_KMH > FMaxSpeed) then begin
              FMaxSpeed := APosition.Speed_KMH;
            end;
            // avg speed
            FAvgSpeedTickCount := FAvgSpeedTickCount + 1;
            VAlfa := 1 / FAvgSpeedTickCount;
            VBeta := 1 - VAlfa;
            FAvgSpeed := VAlfa * APosition.Speed_KMH + VBeta * FAvgSpeed;
          end;

          // if prev position available too - calc distance
          // no recalc if AllowCalcStats disabled
          if not PointIsEmpty(VPointPrev) then begin
            VDistToPrev := FDatum.CalcDist(VPointPrev, FLastPosition);
            FDist := FDist + VDistToPrev;
            FOdometer1 := FOdometer1 + VDistToPrev;
            FOdometer2 := FOdometer2 + VDistToPrev;
          end;
        end;

        FLastPositionOK := APosition.PositionOK;
        SetChanged;
      end;
      FCurrentPosition := APosition;
    finally
      UnlockWrite;
    end;
end;

procedure TGPSRecorder.ClearTrack;
begin
  LockWrite;
  try
    if FTrack <> nil then begin
      FTrack := TTrackPointsBlocksListStatic.Create;
      FLastBlock := nil;
      FPointsInBlockCount := 0;
      FLastPositionOK := FALSE;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

function TGPSRecorder.GetAllPoints: ILonLatPath;
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
    Result := FVectorItemsFactory.CreateLonLatPathByEnum(VPointsEnum);
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
    Result := FLastBlock <> nil;
  finally
    UnlockRead;
  end;
end;

function TGPSRecorder.LastPoints(
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
