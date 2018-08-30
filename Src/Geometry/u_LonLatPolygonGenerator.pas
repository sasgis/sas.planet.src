{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2018, SAS.Planet development team.                      *}
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

unit u_LonLatPolygonGenerator;

interface

{$IFDEF DEBUG}
  {.$DEFINE WITH_PERF_COUNTER}
{$ENDIF}

uses
  SysUtils,
  clipper,
  t_GeoTypes,
  i_Datum,
  i_GeometryLonLat,
  i_GeometryLonLatFactory;

type
  TLonLatPolygonGenerator = class
  private
    FDatum: IDatum;
    FRadius: Double;

    FIntToDoubleCoeff: Int64;

    FPattern: TPath;
    FPatternPoint: TIntPoint;
    FMaxPatternDiff: Int64;

    FMaxPointDiff: TIntPoint;

    procedure GeneratePolygonBySingleLine(
      const ALine: IGeometryLonLatSingleLine;
      const ABuilder: IGeometryLonLatPolygonBuilder
    );
    procedure MakePattern(const APoint: TIntPoint); inline;
    function Minkowski(const APath: TPath; const AIsClosed: Boolean): TPaths;
  public
    function Generate(
      const ABuilder: IGeometryLonLatPolygonBuilder;
      const ADatum: IDatum;
      const ALine: IGeometryLonLatLine;
      const ARadius: Double
    ): IGeometryLonLatPolygon;
  end;

implementation

uses
  {$IFDEF WITH_PERF_COUNTER}
  Windows,
  i_Timer,
  u_TimerByQueryPerformanceCounter,
  {$ENDIF}
  i_DoublePoints,
  u_DoublePoints;

const
  cPatternLen = 6;
  cPatternStep = 360 / cPatternLen;

{ TLonLatPolygonGenerator }

function TLonLatPolygonGenerator.Generate(
  const ABuilder: IGeometryLonLatPolygonBuilder;
  const ADatum: IDatum;
  const ALine: IGeometryLonLatLine;
  const ARadius: Double
): IGeometryLonLatPolygon;
var
  I: Integer;
  VLineSingle: IGeometryLonLatSingleLine;
  VLineMulti: IGeometryLonLatMultiLine;
  {$IFDEF WITH_PERF_COUNTER}
  VTimer: ITimer;
  VStartTime: Int64;
  VTime: Double;
  VMsg: string;
  {$ENDIF}
begin
  FDatum := ADatum;
  FRadius := ARadius;

  FIntToDoubleCoeff := clipper.LoRange div (180 * 4);

  SetLength(FPattern, cPatternLen);
  FMaxPatternDiff := Round(0.5 * FIntToDoubleCoeff); // 0.5 degree

  {$IFDEF WITH_PERF_COUNTER}
  VTimer := MakeTimerByQueryPerformanceCounter;
  if VTimer <> nil then begin
    VStartTime := VTimer.CurrentTime;
  end else begin
    VStartTime := 0;
  end;
  {$ENDIF}

  if Supports(ALine, IGeometryLonLatSingleLine, VLineSingle) then begin
    GeneratePolygonBySingleLine(VLineSingle, ABuilder);
  end else if Supports(ALine, IGeometryLonLatMultiLine, VLineMulti) then begin
    for I := 0 to VLineMulti.Count - 1 do begin
      VLineSingle := VLineMulti.Item[I];
      GeneratePolygonBySingleLine(VLineSingle, ABuilder);
    end;
  end;
  Result := ABuilder.MakeStaticAndClear;

  {$IFDEF WITH_PERF_COUNTER}
  if VTimer <> nil then begin
    VTime := (VTimer.CurrentTime - VStartTime) / VTimer.Freq;
    VMsg := Format('Polygon by Minkowski Sum at %.8f sec.', [VTime]);
    OutputDebugString(PChar(VMsg));
  end
  {$ENDIF}
end;

procedure TLonLatPolygonGenerator.MakePattern(
  const APoint: TIntPoint
);
var
  I: Integer;
  VTmp: Integer;
  VPoint, VPatternPoint: t_GeoTypes.TDoublePoint;
begin
  FPatternPoint := APoint;

  VPatternPoint.X := APoint.X / FIntToDoubleCoeff;
  VPatternPoint.Y := APoint.Y / FIntToDoubleCoeff;

  for I := 0 to cPatternLen - 1 do begin
    VPoint :=
      FDatum.CalcFinishPosition(
        VPatternPoint,
        I * cPatternStep,
        FRadius
      );

    FPattern[I].X := Round(VPoint.X * FIntToDoubleCoeff) - FPatternPoint.X;
    FPattern[I].Y := Round(VPoint.Y * FIntToDoubleCoeff) - FPatternPoint.Y;

    if I = 0 then begin
      FMaxPointDiff.X := Abs(FPattern[I].X);
      FMaxPointDiff.Y := Abs(FPattern[I].Y);
    end else begin
      VTmp := Abs(FPattern[I].X);
      if VTmp > FMaxPointDiff.X then begin
        FMaxPointDiff.X := VTmp;
      end;
      VTmp := Abs(FPattern[I].Y);
      if VTmp > FMaxPointDiff.Y then begin
        FMaxPointDiff.Y := VTmp;
      end;
    end;
  end;
end;

function TLonLatPolygonGenerator.Minkowski(
  const APath: TPath;
  const AIsClosed: Boolean
): TPaths;
var
  I, J, K: Integer;
  VPrevPoint: PIntPoint;
  VDelta, VPathLen: Integer;
  VQuad: TPath;
  VTmp: TPaths;
begin
  MakePattern(APath[0]);

  if AIsClosed then begin
    VDelta := 1;
  end else begin
    VDelta := 0;
  end;

  VPathLen := Length(APath);
  SetLength(VTmp, VPathLen);

  K := 0;
  VPrevPoint := nil;
  for I := 0 to VPathLen - 1 do begin
    if Abs(FPatternPoint.Y - APath[I].Y) > FMaxPatternDiff then begin
      MakePattern(APath[I]);
    end;

    if (I > 0) and (I < VPathLen - 1) and
      (Abs(APath[I].X - VPrevPoint.X) < FMaxPointDiff.X) and
      (Abs(APath[I].Y - VPrevPoint.Y) < FMaxPointDiff.Y) then
    begin
      Continue; // skip nearest points
    end;

    SetLength(VTmp[K], cPatternLen);
    for J := 0 to cPatternLen - 1 do begin
      VTmp[K][J].X := APath[I].X + FPattern[J].X;
      VTmp[K][J].Y := APath[I].Y + FPattern[J].Y;
    end;
    Inc(K);
    VPrevPoint := @APath[I];
  end;

  SetLength(VQuad, 4);
  SetLength(Result, (K + VDelta) * (cPatternLen + 1));

  for I := 0 to K - 2 + VDelta do begin
    for J := 0 to cPatternLen - 1 do begin
      VQuad[0] := VTmp[I mod K][J mod cPatternLen];
      VQuad[1] := VTmp[(I+1) mod K][J mod cPatternLen];
      VQuad[2] := VTmp[(I+1) mod K][(J+1) mod cPatternLen];
      VQuad[3] := VTmp[I mod K][(J+1) mod cPatternLen];
      if not Orientation(VQuad) then begin
        VQuad := ReversePath(VQuad);
      end;
      Result[I*cPatternLen + J] := Copy(VQuad, 0, 4);
    end;
  end;
end;

procedure TLonLatPolygonGenerator.GeneratePolygonBySingleLine(
  const ALine: IGeometryLonLatSingleLine;
  const ABuilder: IGeometryLonLatPolygonBuilder
);

  function _PathToSinglePolygon(const APath: TPath): IDoublePoints;
  var
    I: Integer;
    VCount: Integer;
    VPointsArray: PDoublePointArray;
  begin
    VCount := Length(APath);
    GetMem(VPointsArray, VCount * SizeOf(TDoublePoint));
    for I := 0 to VCount - 1 do begin
      VPointsArray[I].X := APath[I].X / FIntToDoubleCoeff;
      VPointsArray[I].Y := APath[I].Y / FIntToDoubleCoeff;
    end;
    Result := TDoublePoints.CreateWithOwn(VPointsArray, VCount);
  end;

  procedure _ProcessNode(const ANode: TPolyNode);
  var
    I: Integer;
    VSinglePoly: IDoublePoints;
  begin
    VSinglePoly := _PathToSinglePolygon(ANode.Contour);
    if Assigned(VSinglePoly) then begin
      if ANode.IsHole then begin
        ABuilder.AddHole(VSinglePoly);
      end else begin
        ABuilder.AddOuter(VSinglePoly);
      end;
    end;
    for I := 0 to ANode.ChildCount - 1 do begin
      _ProcessNode(ANode.Childs[I]);
    end;
  end;

var
  I: Integer;
  VPath: TPath;
  VPaths: TPaths;
  VClipper: TClipper;
  VPolyTree: TPolyTree;
begin
  if (ALine = nil) or (ALine.Count <= 1) then begin
    Exit;
  end;

  SetLength(VPath, ALine.Count);
  for I := 0 to ALine.Count - 1 do begin
    VPath[I].X := Round(ALine.Points[I].X * FIntToDoubleCoeff);
    VPath[I].Y := Round(ALine.Points[I].Y * FIntToDoubleCoeff);
  end;

  VPaths := Minkowski(VPath, False);

  VClipper := TClipper.Create;
  try
    if VClipper.AddPaths(VPaths, ptSubject, True) then begin
      VPolyTree := TPolyTree.Create;
      try
        if VClipper.Execute(ctUnion, VPolyTree, pftNonZero) then begin
          for I := 0 to VPolyTree.ChildCount - 1 do begin
            _ProcessNode(VPolyTree.Childs[I]);
          end;
        end;
      finally
        VPolyTree.Free;
      end;
    end;
  finally
    VClipper.Free;
  end;
end;

end.
