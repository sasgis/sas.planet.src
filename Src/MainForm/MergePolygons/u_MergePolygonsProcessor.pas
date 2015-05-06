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

unit u_MergePolygonsProcessor;

interface

uses
  clipper,
  t_MergePolygonsProcessor,
  i_MergePolygonsProgress,
  i_Timer,
  i_BackgroundTask,
  i_VectorDataFactory,
  i_VectorDataItemSimple,
  i_GeometryLonLat,
  i_GeometryLonLatFactory,
  i_NotifierOperation;

type
  TMergePolygonsProcessor = class
  private
    FItems: TMergePolygonsItemArray;
    FOperation: TMergeOperation;
    FBackgroundTask: IBackgroundTask;
    FAppClosingNotifier: INotifierOneOperation;
    FVectorDataFactory: IVectorDataFactory;
    FVectorGeometryLonLatFactory: IGeometryLonLatFactory;
    FPolyCount: Integer;
    FHolesCount: Integer;
    FTimer: ITimer;
    FMergePolygonsProgress: IMergePolygonsProgress;
    FIntToDoubleCoeff: Int64;
  private
    procedure OnExecute(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation
    );
    function ProcessGroupOperation(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation
    ): IGeometryLonLatPolygon;
    function ProcessLogicOperation(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation
    ): IGeometryLonLatPolygon;
  private
    // clipper lib helpers
    function GetPaths(const AIndex: Integer): TPaths;
    function GetSubjPoly: TPaths;
    function GetClipPoly: TPaths;
    procedure ProcessClipperNode(
      const ANode: TPolyNode;
      const AMultiPolygonBuilder: IGeometryLonLatMultiPolygonBuilder
    );
    function MultiPolygonToClipperPaths(
      const APolygon: IGeometryLonLatMultiPolygon
    ): TPaths;
    function SinglePolygonToClipperPaths(
      const APolygon: IGeometryLonLatSinglePolygon
    ): TPaths;
    function ClipperPathToSinglePolygon(
      const APath: TPath
    ): IGeometryLonLatSinglePolygon;
    function GetClipType(
      const AMergeOperation: TMergeOperation
    ): TClipType;
  private
    function GetCurTime: Int64; inline;
    function GetCurTimeDiff(const ATime: Int64): Double; inline;
  public
    procedure MergeAsync(
      const AItems: TMergePolygonsItemArray;
      const AOperation: TMergeOperation
    );
    procedure AbortOperation;
  public
    constructor Create(
      const AMergePolygonsProgress: IMergePolygonsProgress;
      const AAppClosingNotifier: INotifierOneOperation;
      const AVectorDataFactory: IVectorDataFactory;
      const AVectorGeometryLonLatFactory: IGeometryLonLatFactory
    );
    destructor Destroy; override;
  end;

implementation

uses
  Classes,
  SysUtils,
  t_GeoTypes,
  i_ThreadConfig,
  u_TimerByQueryPerformanceCounter,
  u_ThreadConfig,
  u_BackgroundTask;

type
  EMergePolygonsProcessorError = class(Exception);

{.$DEFINE MAX_PRECISION}

function MakePathsUnion(AClipper: TClipper; ASubj, AClip: TPaths): TPaths; inline;
begin
  if not AClipper.AddPaths(ASubj, ptSubject, True) then begin
    raise EMergePolygonsProcessorError.Create(
      'MakePathsUnion: Add subject FAIL!'
    );
  end;
  if not AClipper.AddPaths(AClip, ptClip, True) then begin
    raise EMergePolygonsProcessorError.Create(
      'MakePathsUnion: Add clip FAIL!'
    );
  end;
  if not AClipper.Execute(ctUnion, Result) then begin
    raise EMergePolygonsProcessorError.Create(
      'MakePathsUnion: Clipper Exec FAIL!'
    );
  end;
end;

{ TMergePolygonsProcessor }

constructor TMergePolygonsProcessor.Create(
  const AMergePolygonsProgress: IMergePolygonsProgress;
  const AAppClosingNotifier: INotifierOneOperation;
  const AVectorDataFactory: IVectorDataFactory;
  const AVectorGeometryLonLatFactory: IGeometryLonLatFactory
);
begin
  inherited Create;
  
  FMergePolygonsProgress := AMergePolygonsProgress;
  FAppClosingNotifier := AAppClosingNotifier;
  FVectorDataFactory := AVectorDataFactory;
  FVectorGeometryLonLatFactory := AVectorGeometryLonLatFactory;

  FTimer := MakeTimerByQueryPerformanceCounter;
  FBackgroundTask := nil;

  Assert(SizeOf(clipper.cInt) = SizeOf(Int64));

  // Coeff for Double <-> Int64 coversions
  {$IFNDEF MAX_PRECISION}
  FIntToDoubleCoeff := clipper.LoRange div 180; // fastes
  {$ELSE}
  FIntToDoubleCoeff := clipper.HiRange div 180; // big integer math
  {$ENDIF}
end;

destructor TMergePolygonsProcessor.Destroy;
begin
  AbortOperation;
  inherited Destroy;
end;

procedure TMergePolygonsProcessor.MergeAsync(
  const AItems: TMergePolygonsItemArray;
  const AOperation: TMergeOperation
);
var
  VThreadConfig: IThreadConfig;
begin
  Assert(Length(AItems) >= 2);
  
  FItems := AItems;
  FOperation := AOperation;

  if not Assigned(FBackgroundTask) then begin
    VThreadConfig := TThreadConfig.Create(tpNormal);

    FBackgroundTask :=
      TBackgroundTask.Create(
        FAppClosingNotifier,
        Self.OnExecute,
        VThreadConfig,
        Self.ClassName
      );

    FBackgroundTask.Start;
  end else begin
    FBackgroundTask.StopExecute;
  end;

  FBackgroundTask.StartExecute;
end;

procedure TMergePolygonsProcessor.AbortOperation;
begin
  FMergePolygonsProgress.IsAborted := True;
  if Assigned(FBackgroundTask) then begin
    FBackgroundTask.StopExecute;
    FBackgroundTask.Terminate;
    FBackgroundTask := nil;
  end;
end;

procedure TMergePolygonsProcessor.OnExecute(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation
);
var
  VTime: Int64;
  VTimeDiff: Double;
  VVectorItem: IVectorDataItem;
  VResultPolygon: IGeometryLonLatPolygon;
begin
  FMergePolygonsProgress.ResetProgress;
  try
    FPolyCount := 0;
    FHolesCount := 0;
    VVectorItem := nil;

    if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
      FMergePolygonsProgress.IsAborted := True;
      Exit;
    end;

    VTime := GetCurTime;

    if FOperation = moGroup then begin
      VResultPolygon := ProcessGroupOperation(AOperationID, ACancelNotifier);
    end else begin
      VResultPolygon := ProcessLogicOperation(AOperationID, ACancelNotifier)
    end;

    VTimeDiff := GetCurTimeDiff(VTime);

    if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
      FMergePolygonsProgress.IsAborted := True;
      Exit;
    end;

    if Assigned(VResultPolygon) then begin
      VVectorItem :=
        FVectorDataFactory.BuildItem(
          FItems[0].VectorData.MainInfo,
          FItems[0].VectorData.Appearance,
          VResultPolygon
        );
    end;

    FMergePolygonsProgress.SetProgress(FPolyCount, FHolesCount, VTimeDiff, VVectorItem);
  finally
    FMergePolygonsProgress.IsFinished := True;
  end;
end;

function TMergePolygonsProcessor.ProcessGroupOperation(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation
): IGeometryLonLatPolygon;
var
  I, J: Integer;
  VMultiPolygonBuilder: IGeometryLonLatMultiPolygonBuilder;
begin
  Result := nil;
  VMultiPolygonBuilder := FVectorGeometryLonLatFactory.MakeGeometryLonLatMultiPolygonBuilder;

  for I := 0 to Length(FItems) - 1 do begin
    if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
      Exit;
    end;
    if Assigned(FItems[I].SinglePolygon) then begin
      VMultiPolygonBuilder.Add(FItems[I].SinglePolygon);
      Inc(FPolyCount);
    end else begin
      for J := 0 to FItems[I].MultiPolygon.Count - 1 do begin
        VMultiPolygonBuilder.Add(FItems[I].MultiPolygon.Item[J]);
        Inc(FPolyCount);
        if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
          Exit;
        end;
      end;
    end;
  end;
  Result := VMultiPolygonBuilder.MakeStaticAndClear;
end;

function TMergePolygonsProcessor.ProcessLogicOperation(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation
): IGeometryLonLatPolygon;
var
  I: Integer;
  VClipper: TClipper;
  VPolyTree: TPolyTree;
  VMultiPolygonBuilder: IGeometryLonLatMultiPolygonBuilder;
begin
  Result := nil;

  VClipper := TClipper.Create;
  try
    if not VClipper.AddPaths(GetSubjPoly, ptSubject, True) then begin
      raise EMergePolygonsProcessorError.Create(
        'ProcessLogicOperation: Add subject FAIL!'
      );
    end;

    if not VClipper.AddPaths(GetClipPoly, ptClip, True) then begin
      raise EMergePolygonsProcessorError.Create(
        'ProcessLogicOperation: Add clip FAIL!'
      );
    end;

    if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
      FMergePolygonsProgress.IsAborted := True;
      Exit;
    end;

    VPolyTree := TPolyTree.Create;
    try
      if VClipper.Execute(GetClipType(FOperation), VPolyTree) then begin
        if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
          FMergePolygonsProgress.IsAborted := True;
          Exit;
        end;

        VMultiPolygonBuilder := FVectorGeometryLonLatFactory.MakeGeometryLonLatMultiPolygonBuilder;
        for I := 0 to VPolyTree.ChildCount - 1 do begin
          ProcessClipperNode(VPolyTree.Childs[I], VMultiPolygonBuilder);
        end;

        Result := VMultiPolygonBuilder.MakeStaticAndClear;
      end else begin
        raise EMergePolygonsProcessorError.Create(
          'ProcessLogicOperation: Clipper Exec FAIL!'
        );
      end;
    finally
      VPolyTree.Free;
    end;
  finally
    VClipper.Free;
  end;
end;

function TMergePolygonsProcessor.GetPaths(const AIndex: Integer): TPaths;
begin
  if Assigned(FItems[AIndex].SinglePolygon) then begin
    Result := SinglePolygonToClipperPaths(FItems[AIndex].SinglePolygon);
  end else begin
    Result := MultiPolygonToClipperPaths(FItems[AIndex].MultiPolygon);
  end;
end;

function TMergePolygonsProcessor.GetSubjPoly: TPaths;
begin
  Result := GetPaths(0);
end;

function TMergePolygonsProcessor.GetClipPoly: TPaths;
var
  I: Integer;
  VPaths: TPaths;
  VClipper: TClipper;
begin
  // Make Union of all clip's: (((clip1 OR clip2) OR clip3) OR clip4)...
  VClipper := TClipper.Create;
  try
    SetLength(Result, 0);
    for I := 1 to Length(FItems) - 1 do begin
      VPaths := GetPaths(I);
      if Length(Result) > 0 then begin
        VClipper.Clear;
        Result := MakePathsUnion(VClipper, Result, VPaths);
      end else begin
        Result := VPaths;
      end;
    end;
    if Length(Result) = 0 then begin
      raise EMergePolygonsProcessorError.Create(
        'GetClipPoly: Empty result!'
      );
    end;
  finally
    VClipper.Free;
  end;
end;

procedure TMergePolygonsProcessor.ProcessClipperNode(
  const ANode: TPolyNode;
  const AMultiPolygonBuilder: IGeometryLonLatMultiPolygonBuilder
);
var
  I: Integer;
  VSinglePoly: IGeometryLonLatSinglePolygon;
begin
  if ANode.IsHole then begin
    //ToDo
    Inc(FHolesCount);
  end else begin
    VSinglePoly := ClipperPathToSinglePolygon(ANode.Contour);
    if Assigned(VSinglePoly) then begin
      AMultiPolygonBuilder.Add(VSinglePoly);
      Inc(FPolyCount);
    end;
  end;
  for I := 0 to ANode.ChildCount - 1 do begin
    ProcessClipperNode(ANode.Childs[I], AMultiPolygonBuilder);
  end;
end;

function TMergePolygonsProcessor.GetClipType(
  const AMergeOperation: TMergeOperation
): TClipType;
begin
  case AMergeOperation of
    moAND: Result := ctIntersection;
    moOR:  Result := ctUnion;
    moNOT: Result := ctDifference;
    moXOR: Result := ctXor;
  else
    raise EMergePolygonsProcessorError.CreateFmt(
      'Unknown merge operation: %d', [Integer(AMergeOperation)]
    );
  end;
end;

function TMergePolygonsProcessor.MultiPolygonToClipperPaths(
  const APolygon: IGeometryLonLatMultiPolygon
): TPaths;
var
  I: Integer;
  VPaths: TPaths;
  VClipper: TClipper;
begin
  // Make Union of all polygons in multipoligon
  SetLength(Result, 0);
  VClipper := TClipper.Create;
  try
    for I := 0 to APolygon.Count - 1 do begin
      VPaths := SinglePolygonToClipperPaths(APolygon.Item[I]);
      if Length(Result) > 0 then begin
        VClipper.Clear;
        Result := MakePathsUnion(VClipper, Result, VPaths);
      end else begin
        Result := VPaths;
      end;
    end;
    if Length(Result) = 0 then begin
      raise EMergePolygonsProcessorError.Create(
        'MultiPolygonToClipperPaths: Empty result!'
      );
    end;
  finally
    VClipper.Free;
  end;
end;

function TMergePolygonsProcessor.SinglePolygonToClipperPaths(
  const APolygon: IGeometryLonLatSinglePolygon
): TPaths;
var
  I: Integer;
  VPath: TPath;
  VPoints: PDoublePointArray;
begin
  SetLength(VPath, APolygon.Count);
  VPoints := APolygon.Points;
  for I := 0 to APolygon.Count - 1 do begin
    VPath[I].X := Round(VPoints[I].X * FIntToDoubleCoeff);
    VPath[I].Y := Round(VPoints[I].Y * FIntToDoubleCoeff);
  end;

  // Convert a self-intersecting polygon into a simple polygon
  Result := SimplifyPolygon(VPath);

  if Length(Result) = 0 then begin
    raise EMergePolygonsProcessorError.Create(
      'SinglePolygonToClipperPaths: Empty result!'
    );
  end;
end;

function TMergePolygonsProcessor.ClipperPathToSinglePolygon(
  const APath: TPath
): IGeometryLonLatSinglePolygon;
var
  I: Integer;
  VCount: Integer;
  VPoints: array of TDoublePoint;
begin
  VCount := Length(APath);
  SetLength(VPoints, VCount);
  for I := 0 to VCount - 1 do begin
    VPoints[I].X := APath[I].X / FIntToDoubleCoeff;
    VPoints[I].Y := APath[I].Y / FIntToDoubleCoeff;
  end;
  Result := FVectorGeometryLonLatFactory.CreateLonLatPolygon(@VPoints[0], VCount);
end;

function TMergePolygonsProcessor.GetCurTime: Int64;
begin
  if Assigned(FTimer) then begin
    Result := FTimer.CurrentTime;
  end else begin
    Result := 0;
  end;
end;

function TMergePolygonsProcessor.GetCurTimeDiff(const ATime: Int64): Double;
begin
  if Assigned(FTimer) then begin
    Result := (FTimer.CurrentTime - ATime) / FTimer.Freq;
  end else begin
    Result := 0;
  end;
end;

end.
