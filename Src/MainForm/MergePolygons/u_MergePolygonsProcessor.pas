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
  i_BackgroundTask,
  i_VectorDataFactory,
  i_GeometryLonLat,
  i_GeometryLonLatFactory,
  i_NotifierOperation;

type
  TMergePolygonsProcessor = class
  private
    FItems: TMergePolygonsItemArray;
    FOperation: TMergeOperation;
    FOnMergeFinished: TOnMergeFinished;
    FBackgroundTask: IBackgroundTask;
    FAppClosingNotifier: INotifierOneOperation;
    FVectorDataFactory: IVectorDataFactory;
    FVectorGeometryLonLatFactory: IGeometryLonLatFactory;
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
    procedure ProcessClipperNode(
      const ANode: TPolyNode;
      const AMultiPolygonBuilder: IGeometryLonLatMultiPolygonBuilder
    );
    function MultiPolygonToClipperPaths(
      const APolygon: IGeometryLonLatMultiPolygon;
      out APaths: TPaths
    ): Boolean;
    function SinglePolygonToClipperPath(
      const APolygon: IGeometryLonLatSinglePolygon;
      out APath: TPath
    ): Boolean;
    function ClipperPathToSinglePolygon(
      const APath: TPath
    ): IGeometryLonLatSinglePolygon;
    function GetClipType(
      const AMergeOperation: TMergeOperation
    ): TClipType;
  public
    procedure MergeAsync(
      const AItems: TMergePolygonsItemArray;
      const AOperation: TMergeOperation;
      const AOnMergeFinished: TOnMergeFinished
    );
    procedure AbortOperation;
  public
    constructor Create(
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
  i_VectorDataItemSimple,
  u_ThreadConfig,
  u_BackgroundTask;

const
  cClipperCoeff = 1000000; // for Double <-> Int64 coversions

{ TMergePolygonsProcessor }

constructor TMergePolygonsProcessor.Create(
  const AAppClosingNotifier: INotifierOneOperation;
  const AVectorDataFactory: IVectorDataFactory;
  const AVectorGeometryLonLatFactory: IGeometryLonLatFactory
);
begin
  inherited Create;

  FAppClosingNotifier := AAppClosingNotifier;
  FVectorDataFactory := AVectorDataFactory;
  FVectorGeometryLonLatFactory := AVectorGeometryLonLatFactory;

  FBackgroundTask := nil;
end;

destructor TMergePolygonsProcessor.Destroy;
begin
  if Assigned(FBackgroundTask) then begin
    FBackgroundTask.StopExecute;
    FBackgroundTask.Terminate;
    FBackgroundTask := nil;
  end;
  inherited Destroy;
end;

procedure TMergePolygonsProcessor.MergeAsync(
  const AItems: TMergePolygonsItemArray;
  const AOperation: TMergeOperation;
  const AOnMergeFinished: TOnMergeFinished
);
var
  VThreadConfig: IThreadConfig;
begin
  Assert(Length(AItems) > 0);
  Assert(Assigned(AOnMergeFinished));
  
  FItems := AItems;
  FOperation := AOperation;
  FOnMergeFinished := AOnMergeFinished;

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
  FBackgroundTask.StopExecute;
end;

procedure TMergePolygonsProcessor.OnExecute(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation
);
var
  VVectorItem: IVectorDataItem;
  VResultPolygon: IGeometryLonLatPolygon;
begin
  VVectorItem := nil;
  try
    if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
      Exit;
    end;

    if FOperation = moGroup then begin
      VResultPolygon := ProcessGroupOperation(AOperationID, ACancelNotifier);
    end else begin
      VResultPolygon := ProcessLogicOperation(AOperationID, ACancelNotifier)
    end;

    if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
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
  finally
    FOnMergeFinished(VVectorItem);
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
    end else begin
      for J := 0 to FItems[I].MultiPolygon.Count - 1 do begin
        VMultiPolygonBuilder.Add(FItems[I].MultiPolygon.Item[J]);
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
{$DEFINE CLIPPER_POLY_TREE}
var
  I: Integer;
  VClipper: TClipper;
  VClipType: TClipType;
  VPolyType: TPolyType;
  VPath: TPath;
  VPaths: TPaths;
  {$IFDEF CLIPPER_POLY_TREE}
  VPolyTree: TPolyTree;
  {$ELSE}
  //VIsHole: Boolean;
  VSinglePoly: IGeometryLonLatSinglePolygon;
  {$ENDIF}
  VMultiPolygonBuilder: IGeometryLonLatMultiPolygonBuilder;
begin
  Result := nil;
  
  VClipType := GetClipType(FOperation);

  VClipper := TClipper.Create;
  try
    // init clipper by polygons
    for I := 0 to Length(FItems) - 1 do begin
      if I > 0 then begin
        VPolyType := ptClip;
      end else begin
        VPolyType := ptSubject;
      end;
      if Assigned(FItems[I].SinglePolygon) then begin
        if SinglePolygonToClipperPath(FItems[I].SinglePolygon, VPath) then begin
          if not VClipper.AddPath(VPath, VPolyType, True) then begin
            Assert(False);
          end;
        end else begin
          Assert(False);
        end;
      end else begin
        if MultiPolygonToClipperPaths(FItems[I].MultiPolygon, VPaths) then begin
          if not VClipper.AddPaths(VPaths, VPolyType, True) then begin
            Assert(False);
          end;
        end else begin
          Assert(False);
        end;
      end;
    end;

    //ToDo: compare usability and performanse between TPolyTree and TPaths

    {$IFDEF CLIPPER_POLY_TREE}
    VPolyTree := TPolyTree.Create;
    try
      if VClipper.Execute(VClipType, VPolyTree) then begin
        VMultiPolygonBuilder := FVectorGeometryLonLatFactory.MakeGeometryLonLatMultiPolygonBuilder;
        for I := 0 to VPolyTree.ChildCount - 1 do begin
          ProcessClipperNode(VPolyTree.Childs[I], VMultiPolygonBuilder);
        end;
        Result := VMultiPolygonBuilder.MakeStaticAndClear;
      end;
    finally
      VPolyTree.Free;
    end;
    {$ELSE}
    if VClipper.Execute(VClipType, VPaths) then begin
      VMultiPolygonBuilder := FVectorGeometryLonLatFactory.MakeGeometryLonLatMultiPolygonBuilder;
      for I := 0 to Length(VPaths) - 1 do begin
        //VIsHole := not Orientation(VPaths[I]); // slow?
        VSinglePoly := ClipperPathToSinglePolygon(VPaths[I]);
        if Assigned(VSinglePoly) then begin
          VMultiPolygonBuilder.Add(VSinglePoly);
        end;
      end;
      Result := VMultiPolygonBuilder.MakeStaticAndClear;
    end;
    {$ENDIF}
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
  VSinglePoly := ClipperPathToSinglePolygon(ANode.Contour);
  if Assigned(VSinglePoly) then begin
    AMultiPolygonBuilder.Add(VSinglePoly);
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
    raise Exception.CreateFmt('Unknown merge operation: %d', [Integer(AMergeOperation)]);
  end;
end;

function TMergePolygonsProcessor.MultiPolygonToClipperPaths(
  const APolygon: IGeometryLonLatMultiPolygon;
  out APaths: TPaths
): Boolean;
var
  I: Integer;
begin
  Result := False;
  SetLength(APaths, APolygon.Count);
  for I := 0 to APolygon.Count - 1 do begin
    Result := SinglePolygonToClipperPath(APolygon.Item[I], APaths[I]);
    if not Result then begin
      Break;
    end;
  end;
end;

function TMergePolygonsProcessor.SinglePolygonToClipperPath(
  const APolygon: IGeometryLonLatSinglePolygon;
  out APath: TPath
): Boolean;
var
  I: Integer;
  VPoints: PDoublePointArray;
begin
  SetLength(APath, APolygon.Count);
  VPoints := APolygon.Points;
  for I := 0 to APolygon.Count - 1 do begin
    APath[I].X := Round(VPoints[I].X * cClipperCoeff);
    APath[I].Y := Round(VPoints[I].Y * cClipperCoeff);
  end;
  Result := Length(APath) > 0;
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
    VPoints[I].X := APath[I].X / cClipperCoeff;
    VPoints[I].Y := APath[I].Y / cClipperCoeff;
  end;
  Result := FVectorGeometryLonLatFactory.CreateLonLatPolygon(@VPoints[0], VCount);
end;

end.
