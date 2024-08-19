{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_LonLatPolygonGenerator;

interface

{$IFDEF DEBUG}
  {.$DEFINE RETURN_MINKOWSKI_SUM}
{$ENDIF}

uses
  SysUtils,
  Clipper,
  Clipper.Core,
  Clipper.Engine,
  t_GeoTypes,
  i_Datum,
  i_GeometryLonLat,
  i_GeometryLonLatFactory;

type
  TClipperPoint = Clipper.TPoint64;
  PClipperPoint = ^TClipperPoint;

  TClipperPath = Clipper.TPath64;
  TClipperPaths = Clipper.TPaths64;

  TClipperPolyTree = Clipper.Engine.TPolyTree64;
  TClipperPolyNode = Clipper.Engine.TPolyPath64;

  TLonLatPolygonGenerator = class
  private const
    CIntToDoubleCoeff = Clipper.Core.MaxCoord div (180 * 32);
  private
    FDatum: IDatum;
    FRadius: Double;

    FPattern: TClipperPath;
    FPatternPoint: TClipperPoint;
    FMaxPatternDiff: Int64;

    FMaxPointDiff: TClipperPoint;

    procedure GeneratePolygonBySingleLine(
      const ALine: IGeometryLonLatSingleLine;
      const ABuilder: IGeometryLonLatPolygonBuilder
    );
    procedure MakePattern(const APoint: TClipperPoint); inline;
    function Minkowski(const APath: TClipperPath; const AIsClosed: Boolean): TClipperPaths;
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
  i_DoublePoints,
  u_DoublePoints;

const
  cPatternLen = 12; // ToDo: read this value from config
  cPatternStep = 360 / cPatternLen;

const
  cNearestPointRadiusCoeff = 0.25; // ToDo: read this value from config

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
begin
  FDatum := ADatum;
  FRadius := ARadius;

  SetLength(FPattern, cPatternLen);
  FMaxPatternDiff := Round(0.5 * CIntToDoubleCoeff); // 0.5 degree

  if Supports(ALine, IGeometryLonLatSingleLine, VLineSingle) then begin
    GeneratePolygonBySingleLine(VLineSingle, ABuilder);
  end else if Supports(ALine, IGeometryLonLatMultiLine, VLineMulti) then begin
    for I := 0 to VLineMulti.Count - 1 do begin
      VLineSingle := VLineMulti.Item[I];
      GeneratePolygonBySingleLine(VLineSingle, ABuilder);
    end;
  end;
  Result := ABuilder.MakeStaticAndClear;
end;

procedure TLonLatPolygonGenerator.MakePattern(
  const APoint: TClipperPoint
);
var
  I: Integer;
  VPoint, VPatternPoint: t_GeoTypes.TDoublePoint;
begin
  FPatternPoint := APoint;

  VPatternPoint.X := APoint.X / CIntToDoubleCoeff;
  VPatternPoint.Y := APoint.Y / CIntToDoubleCoeff;

  for I := 0 to cPatternLen - 1 do begin
    VPoint :=
      FDatum.CalcFinishPosition(
        VPatternPoint,
        I * cPatternStep,
        FRadius
      );

    FPattern[I].X := Round(VPoint.X * CIntToDoubleCoeff) - FPatternPoint.X;
    FPattern[I].Y := Round(VPoint.Y * CIntToDoubleCoeff) - FPatternPoint.Y;
  end;

  VPoint := FDatum.CalcFinishPosition(VPatternPoint, 90, FRadius * cNearestPointRadiusCoeff);
  FMaxPointDiff.X := Abs(Round(VPoint.X * CIntToDoubleCoeff) - FPatternPoint.X);

  VPoint := FDatum.CalcFinishPosition(VPatternPoint, 0, FRadius * cNearestPointRadiusCoeff);
  FMaxPointDiff.Y := Abs(Round(VPoint.Y * CIntToDoubleCoeff) - FPatternPoint.Y);
end;

function TLonLatPolygonGenerator.Minkowski(
  const APath: TClipperPath;
  const AIsClosed: Boolean
): TClipperPaths;
var
  I, J, K, G, H: Integer;
  VPrevPoint: PClipperPoint;
  VDelta, VPathLen: Integer;
  VQuad: TClipperPath;
  VTmp: TClipperPaths;
begin
  MakePattern(APath[0]);

  if AIsClosed then begin
    VDelta := 0;
  end else begin
    VDelta := 1;
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
  VPathLen := K;
  SetLength(VTmp, K);

  SetLength(VQuad, 4);
  SetLength(Result, (VPathLen - VDelta) * cPatternLen);

  if AIsClosed then begin
    G := VPathLen - 1;
  end else begin
    G := 0;
  end;

  for I := VDelta to VPathLen - 1 do begin
    H := cPatternLen - 1;
    K := (I - VDelta) * cPatternLen;
    for J := 0 to cPatternLen - 1 do begin
      VQuad[0] := VTmp[G][H];
      VQuad[1] := VTmp[I][H];
      VQuad[2] := VTmp[I][J];
      VQuad[3] := VTmp[G][J];
      if not IsPositive(VQuad) then begin
        Result[K + J] := ReversePath(VQuad);
      end else begin
        Result[K + J] := Copy(VQuad, 0, 4);
      end;
      H := J;
    end;
    G := I;
  end;
end;

procedure TLonLatPolygonGenerator.GeneratePolygonBySingleLine(
  const ALine: IGeometryLonLatSingleLine;
  const ABuilder: IGeometryLonLatPolygonBuilder
);

  function _PathToSinglePolygon(const APath: TClipperPath): IDoublePoints;
  var
    I: Integer;
    VCount: Integer;
    VPointsArray: PDoublePointArray;
  begin
    VCount := Length(APath);
    if VCount = 0 then begin
      Result := nil;
      Exit;
    end;
    GetMem(VPointsArray, VCount * SizeOf(TDoublePoint));
    for I := 0 to VCount - 1 do begin
      VPointsArray[I].X := APath[I].X / CIntToDoubleCoeff;
      VPointsArray[I].Y := APath[I].Y / CIntToDoubleCoeff;
    end;
    Result := TDoublePoints.CreateWithOwn(VPointsArray, nil, VCount);
  end;

  procedure _ProcessNode(const ANode: TClipperPolyNode);
  var
    I: Integer;
    VSinglePoly: IDoublePoints;
  begin
    VSinglePoly := _PathToSinglePolygon(ANode.Polygon);
    if VSinglePoly <> nil then begin
      if ANode.IsHole then begin
        ABuilder.AddHole(VSinglePoly);
      end else begin
        ABuilder.AddOuter(VSinglePoly);
      end;
    end;
    for I := 0 to ANode.Count - 1 do begin
      _ProcessNode(ANode.Child[I]);
    end;
  end;

var
  I: Integer;
  VPath: TClipperPath;
  VPaths: TClipperPaths;
  VOpenPaths: TClipperPaths;
  VClipper: TClipper;
  VPolyTree: TClipperPolyTree;
begin
  if (ALine = nil) or (ALine.Count < 2) then begin
    Exit;
  end;

  SetLength(VPath, ALine.Count);
  for I := 0 to ALine.Count - 1 do begin
    VPath[I].X := Round(ALine.Points[I].X * CIntToDoubleCoeff);
    VPath[I].Y := Round(ALine.Points[I].Y * CIntToDoubleCoeff);
  end;

  VPaths := Minkowski(VPath, False);

  {$IFDEF RETURN_MINKOWSKI_SUM}
  for I := 0 to Length(VPaths) - 1 do begin
    var VSinglePoly := _PathToSinglePolygon(VPaths[I]);
    if VSinglePoly <> nil then begin
      ABuilder.AddOuter(VSinglePoly);
    end;
  end;
  Exit;
  {$ENDIF}

  VClipper := TClipper.Create;
  try
    VClipper.AddSubject(VPaths);
    VPolyTree := TClipperPolyTree.Create;
    try
      if VClipper.Execute(ctUnion, frNonZero, VPolyTree, VOpenPaths) then begin
        for I := 0 to VPolyTree.Count - 1 do begin
          _ProcessNode(VPolyTree.Child[I]);
        end;
      end;
    finally
      VPolyTree.Free;
    end;
  finally
    VClipper.Free;
  end;
end;

end.
