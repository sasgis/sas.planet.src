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

unit u_LineOnMapEdit;

interface

uses
  t_GeoTypes,
  i_GeometryLonLat,
  i_GeometryLonLatFactory,
  i_LineOnMapEdit,
  i_DoublePointsAggregator,
  u_ChangeableBase;

type
  TLineOnMapEdit = class(TChangeableWithSimpleLockBase, ILineOnMapEdit)
  private
    FVectorGeometryLonLatFactory: IGeometryLonLatFactory;
    FPoints: IDoublePointsAggregator;
    FSelectedPointIndex: Integer;
    procedure _UpdateLineObject; virtual; abstract;
    procedure _UpdateLineWithSelected; virtual; abstract;
  private
    function SetSelectedNextPoint: TDoublePoint;
    function SetSelectedPrevPoint: TDoublePoint;
    function SelectPointInLonLatRect(const ARect: TDoubleRect): Boolean;

    function IsEmpty: Boolean; virtual; abstract;
    function IsReady: Boolean; virtual; abstract;
    function IsNearSplit: Boolean;
    procedure Clear;
    procedure DeleteActivePoint;
    procedure InsertPoint(const APoint: TDoublePoint);
    procedure TogleSplit;
    procedure MoveActivePoint(const APoint: TDoublePoint);
  public
    constructor Create(
      const AVectorGeometryLonLatFactory: IGeometryLonLatFactory
    );
  end;

  TPathOnMapEdit = class(TLineOnMapEdit, IPathOnMapEdit)
  private
    FBuilder: IGeometryLonLatLineBuilder;
    FLine: IGeometryLonLatLine;
    FLineWithSelected: ILonLatPathWithSelected;
    function _MakeLine(
      const APoints: PDoublePointArray;
      ACount: Integer
    ): IGeometryLonLatLine;
    procedure _UpdateLineObject; override;
    procedure _UpdateLineWithSelected; override;
    procedure _SetPath(const AValue: IGeometryLonLatLine);
  private
    function IsEmpty: Boolean; override;
    function IsReady: Boolean; override;
    function GetPath: ILonLatPathWithSelected;
    procedure SetPath(const AValue: ILonLatPathWithSelected); overload;
    procedure SetPath(const AValue: IGeometryLonLatLine); overload;
  public
    constructor Create(
      const AVectorGeometryLonLatFactory: IGeometryLonLatFactory
    );
  end;

  TPolygonOnMapEdit = class(TLineOnMapEdit, IPolygonOnMapEdit)
  private
    FBuilder: IGeometryLonLatPolygonBuilder;
    FLine: IGeometryLonLatPolygon;
    FLineWithSelected: ILonLatPolygonWithSelected;
    function _MakePolygon(
      const APoints: PDoublePointArray;
      ACount: Integer
    ): IGeometryLonLatPolygon;
    procedure _UpdateLineObject; override;
    procedure _UpdateLineWithSelected; override;
    procedure _SetPolygon(const AValue: IGeometryLonLatPolygon);
    procedure _AddContour(const AValue: IGeometryLonLatContour);
    procedure _AddSinglePolygon(const AValue: IGeometryLonLatSinglePolygon);
  private
    function IsEmpty: Boolean; override;
    function IsReady: Boolean; override;
    function GetPolygon: ILonLatPolygonWithSelected;
    procedure SetPolygon(const AValue: ILonLatPolygonWithSelected); overload;
    procedure SetPolygon(const AValue: IGeometryLonLatPolygon); overload;
  public
    constructor Create(
      const AVectorGeometryLonLatFactory: IGeometryLonLatFactory
    );
  end;

implementation

uses
  SysUtils,
  Math,
  i_DoublePoints,
  u_DoublePoints,
  u_DoublePointsAggregator,
  u_GeoFunc,
  u_GeometryFunc,
  u_BaseInterfacedObject;

type
  TLonLatLineWithSelectedBase = class(TBaseInterfacedObject)
  private
    FSelectedPointIndex: Integer;
  private
    function GetSelectedPointIndex: Integer;
  public
    constructor Create(
      ASelectedPointIndex: Integer
    );
  end;

  TLonLatPathWithSelected = class(TLonLatLineWithSelectedBase, ILonLatPathWithSelected)
  private
    FLine: IGeometryLonLatLine;
  private
    function GetGeometry: IGeometryLonLatLine;
  public
    constructor Create(
      const ALine: IGeometryLonLatLine;
      ASelectedPointIndex: Integer
    );
  end;

  TLonLatPolygonWithSelected = class(TLonLatLineWithSelectedBase, ILonLatPolygonWithSelected)
  private
    FLine: IGeometryLonLatPolygon;
  private
    function GetGeometry: IGeometryLonLatPolygon;
  public
    constructor Create(
      const ALine: IGeometryLonLatPolygon;
      ASelectedPointIndex: Integer
    );
  end;

{ TLineOnMapEdit }

constructor TLineOnMapEdit.Create(const AVectorGeometryLonLatFactory: IGeometryLonLatFactory);
begin
  inherited Create;
  FVectorGeometryLonLatFactory := AVectorGeometryLonLatFactory;
  FPoints := TDoublePointsAggregator.Create;
  FSelectedPointIndex := 0;
end;

procedure TLineOnMapEdit.Clear;
begin
  CS.BeginWrite;
  try
    if FPoints.Count > 0 then begin
      FPoints.Clear;
      FSelectedPointIndex := 0;
      _UpdateLineObject;
    end;
  finally
    CS.EndWrite;
  end;
  DoChangeNotify;
end;

procedure TLineOnMapEdit.DeleteActivePoint;
var
  VCurrPoint: TDoublePoint;
  VPrevPoint: TDoublePoint;
begin
  CS.BeginWrite;
  try
    if FPoints.Count > 0 then begin
      Assert(FSelectedPointIndex >= 0);
      Assert(FSelectedPointIndex < FPoints.Count);
      if FSelectedPointIndex < FPoints.Count then begin
        if FSelectedPointIndex = FPoints.Count - 1 then begin
          FPoints.Delete(FSelectedPointIndex);
          Dec(FSelectedPointIndex);
        end else if FSelectedPointIndex = 0 then begin
          FPoints.Delete(FSelectedPointIndex);
        end else begin
          VCurrPoint := FPoints.Points[FSelectedPointIndex];
          if PointIsEmpty(VCurrPoint) then begin
            FPoints.Delete(FSelectedPointIndex);
            Dec(FSelectedPointIndex);
          end else begin
            VPrevPoint := FPoints.Points[FSelectedPointIndex - 1];
            if PointIsEmpty(VPrevPoint) then begin
              if FSelectedPointIndex = 1 then begin
                FPoints.DeletePoints(0, 2);
                FSelectedPointIndex := 0;
              end else begin
                FPoints.DeletePoints(FSelectedPointIndex - 1, 2);
                Dec(FSelectedPointIndex, 2);
              end;
            end else begin
              FPoints.Delete(FSelectedPointIndex);
              Dec(FSelectedPointIndex);
            end;
          end;
        end;
        _UpdateLineObject;
      end;
    end;
  finally
    CS.EndWrite;
  end;
  DoChangeNotify;
end;

procedure TLineOnMapEdit.InsertPoint(const APoint: TDoublePoint);
var
  VCurrPoint: TDoublePoint;
begin
  Assert(not PointIsEmpty(APoint));
  CS.BeginWrite;
  try
    Assert(FSelectedPointIndex >= 0);
    Assert((FPoints.Count = 0) or (FSelectedPointIndex < FPoints.Count));
    if (FPoints.Count = 0) then begin
      FPoints.Add(APoint);
      FSelectedPointIndex := 0;
    end else begin
      if FSelectedPointIndex = FPoints.Count - 1 then begin
        FPoints.Add(APoint);
        Inc(FSelectedPointIndex);
      end else begin
        VCurrPoint := FPoints.Points[FSelectedPointIndex];
        if PointIsEmpty(VCurrPoint) then begin
          if FSelectedPointIndex = 0 then begin
            FPoints.Insert(FSelectedPointIndex, APoint);
          end else begin
            FPoints.Insert(FSelectedPointIndex + 1, APoint);
            FPoints.Insert(FSelectedPointIndex + 2, CEmptyDoublePoint);
            Inc(FSelectedPointIndex);
          end;
        end else begin
          FPoints.Insert(FSelectedPointIndex + 1, APoint);
          Inc(FSelectedPointIndex);
        end;
      end;
    end;
    _UpdateLineObject;
  finally
    CS.EndWrite;
  end;
  DoChangeNotify;
end;

function TLineOnMapEdit.IsNearSplit: Boolean;
var
  VPoint: TDoublePoint;
begin
  Result := False;
  CS.BeginRead;
  try
    if FPoints.Count > 0 then begin
      if (FSelectedPointIndex >= 0) and (FSelectedPointIndex < FPoints.Count) then begin
        VPoint := FPoints.Points[FSelectedPointIndex];
        if PointIsEmpty(VPoint) then begin
          Result := True;
        end else begin
          if FSelectedPointIndex < FPoints.Count - 1 then begin
            VPoint := FPoints.Points[FSelectedPointIndex + 1];
            if PointIsEmpty(VPoint) then begin
              Result := True;
            end;
          end;
          if FSelectedPointIndex > 0 then begin
            VPoint := FPoints.Points[FSelectedPointIndex - 1];
            if PointIsEmpty(VPoint) then begin
              Result := True;
            end;
          end;
        end;
      end;
    end;
  finally
    CS.EndRead;
  end;
end;

procedure TLineOnMapEdit.MoveActivePoint(const APoint: TDoublePoint);
var
  VCurrPoint: TDoublePoint;
begin
  Assert(not PointIsEmpty(APoint));
  if not PointIsEmpty(APoint) then begin
    CS.BeginWrite;
    try
      Assert(FPoints.Count > 0);
      Assert(FSelectedPointIndex >= 0);
      Assert(FSelectedPointIndex < FPoints.Count);
      if FPoints.Count > 0 then begin
        if FSelectedPointIndex < FPoints.Count then begin
          VCurrPoint := FPoints.Points[FSelectedPointIndex];
          Assert(not PointIsEmpty(VCurrPoint));
          if not PointIsEmpty(VCurrPoint) then begin
            if not DoublePointsEqual(APoint, VCurrPoint) then begin
              FPoints.Points[FSelectedPointIndex] := APoint;
              _UpdateLineObject;
            end;
          end;
        end;
      end;
    finally
      CS.EndWrite;
    end;
    DoChangeNotify;
  end;
end;

function TLineOnMapEdit.SelectPointInLonLatRect(const ARect: TDoubleRect): Boolean;
var
  VIndex: Integer;
  VPoint: TDoublePoint;
  VPoints: PDoublePointArray;
  i: Integer;
begin
  Result := False;
  CS.BeginWrite;
  try
    VIndex := -1;
    VPoints := FPoints.Points;
    if FPoints.Count > 0 then begin
      if FSelectedPointIndex < FPoints.Count then begin
        VPoint := VPoints[FSelectedPointIndex];
        if not PointIsEmpty(VPoint) then begin
          if LonLatPointInRect(VPoint, ARect) then begin
            VIndex := FSelectedPointIndex;
          end;
        end;
      end;
    end;

    if VIndex < 0 then begin
      for i := FPoints.Count - 1 downto 0 do begin
        VPoint := VPoints[i];
        if not PointIsEmpty(VPoint) then begin
          if LonLatPointInRect(VPoint, ARect) then begin
            VIndex := i;
            Break;
          end;
        end;
      end;
    end;

    if VIndex >= 0 then begin
      if FSelectedPointIndex <> VIndex then begin
        FSelectedPointIndex := VIndex;
        _UpdateLineWithSelected;
      end;
      Result := True;
    end;
  finally
    CS.EndWrite;
  end;
  DoChangeNotify;
end;

function TLineOnMapEdit.SetSelectedNextPoint: TDoublePoint;
var
  VPoint: TDoublePoint;
begin
  CS.BeginWrite;
  try
    Result := CEmptyDoublePoint;
    if FPoints.Count > 0 then begin
      if FSelectedPointIndex < FPoints.Count - 2 then begin
        VPoint := FPoints.Points[FSelectedPointIndex + 1];
        if PointIsEmpty(VPoint) then begin
          Inc(FSelectedPointIndex, 2);
        end else begin
          Inc(FSelectedPointIndex);
        end;
        _UpdateLineWithSelected;
        if FSelectedPointIndex < FPoints.Count then begin
          Result := FPoints.Points[FSelectedPointIndex];
        end;
      end else if FSelectedPointIndex = FPoints.Count - 2 then begin
        Inc(FSelectedPointIndex);
        _UpdateLineWithSelected;
        if FSelectedPointIndex < FPoints.Count then begin
          Result := FPoints.Points[FSelectedPointIndex];
        end;
      end;
    end;
  finally
    CS.EndWrite;
  end;
  DoChangeNotify;
end;

function TLineOnMapEdit.SetSelectedPrevPoint: TDoublePoint;
var
  VPoint: TDoublePoint;
begin
  CS.BeginWrite;
  try
    Result := CEmptyDoublePoint;
    if FPoints.Count > 0 then begin
      if FSelectedPointIndex > 1 then begin
        VPoint := FPoints.Points[FSelectedPointIndex - 1];
        if PointIsEmpty(VPoint) then begin
          Dec(FSelectedPointIndex, 2);
        end else begin
          Dec(FSelectedPointIndex);
        end;
        _UpdateLineWithSelected;
        Result := FPoints.Points[FSelectedPointIndex];
      end else if FSelectedPointIndex = 1 then begin
        Dec(FSelectedPointIndex);
        _UpdateLineWithSelected;
        Result := FPoints.Points[FSelectedPointIndex];
      end;
    end;
  finally
    CS.EndWrite;
  end;
  DoChangeNotify;
end;

procedure TLineOnMapEdit.TogleSplit;
var
  VPoint: TDoublePoint;
begin
  CS.BeginWrite;
  try
    if FPoints.Count > 0 then begin
      if (FSelectedPointIndex >= 0) and (FSelectedPointIndex < FPoints.Count) then begin
        VPoint := FPoints.Points[FSelectedPointIndex];
        if PointIsEmpty(VPoint) then begin
          if (FSelectedPointIndex > 0) and (FSelectedPointIndex < FPoints.Count - 1) and
            DoublePointsEqual(FPoints.Points[FSelectedPointIndex - 1], FPoints.Points[FSelectedPointIndex + 1])
          then begin
            FPoints.DeletePoints(FSelectedPointIndex, 2);
            Dec(FSelectedPointIndex, 2);
          end else begin
            FPoints.Delete(FSelectedPointIndex);
            Dec(FSelectedPointIndex);
          end;
        end else begin
          if FSelectedPointIndex = FPoints.Count - 1 then begin
            VPoint := FPoints.Points[FSelectedPointIndex - 1];
            if PointIsEmpty(VPoint) then begin
              FPoints.Delete(FSelectedPointIndex - 1);
              Dec(FSelectedPointIndex);
            end else begin
              FPoints.Add(CEmptyDoublePoint);
              Inc(FSelectedPointIndex);
            end;
          end else if FSelectedPointIndex = 0 then begin
            VPoint := FPoints.Points[FSelectedPointIndex + 1];
            if PointIsEmpty(VPoint) then begin
              FPoints.Delete(FSelectedPointIndex + 1);
            end else begin
              FPoints.Insert(FSelectedPointIndex, CEmptyDoublePoint);
            end;
          end else begin
            if PointIsEmpty(FPoints.Points[FSelectedPointIndex - 1]) then begin
              if (FSelectedPointIndex > 1) and
                DoublePointsEqual(FPoints.Points[FSelectedPointIndex], FPoints.Points[FSelectedPointIndex - 2])
              then begin
                FPoints.DeletePoints(FSelectedPointIndex - 1, 2);
                Dec(FSelectedPointIndex, 2);
              end else begin
                FPoints.Delete(FSelectedPointIndex - 1);
                Dec(FSelectedPointIndex);
              end;
            end else if PointIsEmpty(FPoints.Points[FSelectedPointIndex + 1]) then begin
              if (FSelectedPointIndex < FPoints.Count - 2) and
                DoublePointsEqual(FPoints.Points[FSelectedPointIndex], FPoints.Points[FSelectedPointIndex + 2])
              then begin
                FPoints.DeletePoints(FSelectedPointIndex + 1, 2);
              end else begin
                FPoints.Delete(FSelectedPointIndex + 1);
              end;
            end else begin
              FPoints.Insert(FSelectedPointIndex + 1, VPoint);
              FPoints.Insert(FSelectedPointIndex + 1, CEmptyDoublePoint);
            end;
          end;
        end;
      end;
      _UpdateLineObject;
    end;
  finally
    CS.EndWrite;
  end;
  DoChangeNotify;
end;

{ TPathOnMapEdit }

constructor TPathOnMapEdit.Create(
  const AVectorGeometryLonLatFactory: IGeometryLonLatFactory
);
begin
  inherited Create(AVectorGeometryLonLatFactory);
  FBuilder := FVectorGeometryLonLatFactory.MakeLineBuilder;
end;

function TPathOnMapEdit.GetPath: ILonLatPathWithSelected;
begin
  CS.BeginRead;
  try
    Result := FLineWithSelected;
  finally
    CS.EndRead;
  end;
end;

procedure TPathOnMapEdit.SetPath(const AValue: ILonLatPathWithSelected);
begin
  CS.BeginWrite;
  try
    _SetPath(AValue.Geometry);
    FSelectedPointIndex := AValue.GetSelectedPointIndex;
    if FSelectedPointIndex < 0 then begin
      if FPoints.Count > 0 then begin
        FSelectedPointIndex := FPoints.Count - 1;
      end else begin
        FSelectedPointIndex := 0;
      end;
    end else if FSelectedPointIndex >= FPoints.Count then begin
      FSelectedPointIndex := 0;
    end;
    _UpdateLineObject;
  finally
    CS.EndWrite;
  end;
  DoChangeNotify;
end;

function TPathOnMapEdit.IsEmpty: Boolean;
begin
  CS.BeginRead;
  try
    Result := not Assigned(FLine);
  finally
    CS.EndRead;
  end;
end;

function TPathOnMapEdit.IsReady: Boolean;
begin
  CS.BeginRead;
  try
    Result := IsValidLonLatLine(FLine);
  finally
    CS.EndRead;
  end;
end;

procedure TPathOnMapEdit.SetPath(const AValue: IGeometryLonLatLine);
begin
  CS.BeginWrite;
  try
    _SetPath(AValue);
    _UpdateLineObject;
  finally
    CS.EndWrite;
  end;
  DoChangeNotify;
end;

procedure TPathOnMapEdit._SetPath(const AValue: IGeometryLonLatLine);
var
  i: Integer;
  VLine: IGeometryLonLatSingleLine;
  VMultiLine: IGeometryLonLatMultiLine;
begin
  FPoints.Clear;
  if Supports(AValue, IGeometryLonLatSingleLine, VLine) then begin
    FPoints.AddPoints(VLine.Points, VLine.Count);
    FPoints.Add(CEmptyDoublePoint);
  end else if Supports(AValue, IGeometryLonLatMultiLine, VMultiLine) then begin
    for i := 0 to VMultiLine.Count - 1 do begin
      VLine := VMultiLine.Item[i];
      FPoints.AddPoints(VLine.Points, VLine.Count);
      FPoints.Add(CEmptyDoublePoint);
    end;
  end;
  if FPoints.Count > 0 then begin
    FPoints.Delete(FPoints.Count - 1);
  end;

  if FPoints.Count > 0 then begin
    FSelectedPointIndex := FPoints.Count - 1;
  end else begin
    FSelectedPointIndex := 0;
  end;
end;

function TPathOnMapEdit._MakeLine(
  const APoints: PDoublePointArray;
  ACount: Integer
): IGeometryLonLatLine;
var
  i: Integer;
  VStart: PDoublePointArray;
  VLineLen: Integer;
  VPoint: TDoublePoint;
  VPoints: IDoublePoints;
  VLineBounds: TDoubleRect;
begin
  VStart := APoints;
  VLineLen := 0;
  for i := 0 to ACount - 1 do begin
    VPoint := APoints[i];
    if PointIsEmpty(VPoint) then begin
      if VLineLen > 0 then begin
        VPoints := TDoublePoints.Create(VStart, VLineLen);
        FBuilder.AddLine(VLineBounds, VPoints);
        VLineLen := 0;
      end;
    end else begin
      if VLineLen = 0 then begin
        VStart := @APoints[i];
        VLineBounds.TopLeft := VPoint;
        VLineBounds.BottomRight := VPoint;
      end else begin
        UpdateLonLatMBRByPoint(VLineBounds, VPoint);
      end;
      Inc(VLineLen);
    end;
  end;
  if VLineLen > 0 then begin
    VPoints := TDoublePoints.Create(VStart, VLineLen);
    FBuilder.AddLine(VLineBounds, VPoints);
  end;
  Result := FBuilder.MakeStaticAndClear;
end;

procedure TPathOnMapEdit._UpdateLineObject;
begin
  FLine := _MakeLine(FPoints.Points, FPoints.Count);
  _UpdateLineWithSelected;
end;

procedure TPathOnMapEdit._UpdateLineWithSelected;
begin
  if Assigned(FLine) then begin
    FLineWithSelected := TLonLatPathWithSelected.Create(FLine, FSelectedPointIndex);
  end else begin
    FLineWithSelected := nil;
  end;
end;

{ TPolygonOnMapEdit }

constructor TPolygonOnMapEdit.Create(
  const AVectorGeometryLonLatFactory: IGeometryLonLatFactory
);
begin
  inherited Create(AVectorGeometryLonLatFactory);
  FBuilder := FVectorGeometryLonLatFactory.MakePolygonBuilder;
end;

function TPolygonOnMapEdit.GetPolygon: ILonLatPolygonWithSelected;
begin
  CS.BeginRead;
  try
    Result := FLineWithSelected;
  finally
    CS.EndRead;
  end;
end;

procedure TPolygonOnMapEdit.SetPolygon(const AValue: ILonLatPolygonWithSelected);
begin
  CS.BeginWrite;
  try
    _SetPolygon(AValue.Geometry);
    FSelectedPointIndex := AValue.GetSelectedPointIndex;
    if FSelectedPointIndex < 0 then begin
      if FPoints.Count > 0 then begin
        FSelectedPointIndex := FPoints.Count - 1;
      end else begin
        FSelectedPointIndex := 0;
      end;
    end;
    _UpdateLineObject;
  finally
    CS.EndWrite;
  end;
  DoChangeNotify;
end;

function TPolygonOnMapEdit.IsEmpty: Boolean;
begin
  CS.BeginRead;
  try
    Result := not Assigned(FLine);
  finally
    CS.EndRead;
  end;
end;

function TPolygonOnMapEdit.IsReady: Boolean;
begin
  CS.BeginRead;
  try
    Result := IsValidLonLatPolygon(FLine);
  finally
    CS.EndRead;
  end;
end;

procedure TPolygonOnMapEdit.SetPolygon(const AValue: IGeometryLonLatPolygon);
begin
  CS.BeginWrite;
  try
    _SetPolygon(AValue);
    _UpdateLineObject;
  finally
    CS.EndWrite;
  end;
  DoChangeNotify;
end;

procedure TPolygonOnMapEdit._AddContour(const AValue: IGeometryLonLatContour);
begin
  FPoints.AddPoints(AValue.Points, AValue.Count);
  FPoints.Add(CEmptyDoublePoint);
end;

procedure TPolygonOnMapEdit._AddSinglePolygon(
  const AValue: IGeometryLonLatSinglePolygon
);
var
  i: Integer;
begin
  _AddContour(AValue.OuterBorder);
  for i := 0 to AValue.HoleCount - 1 do begin
    FPoints.Points[FPoints.Count - 1].Y := -1;
    _AddContour(AValue.HoleBorder[i]);
  end;
end;

procedure TPolygonOnMapEdit._SetPolygon(const AValue: IGeometryLonLatPolygon);
var
  i: Integer;
  VLine: IGeometryLonLatSinglePolygon;
  VMultiLine: IGeometryLonLatMultiPolygon;
begin
  if Supports(AValue, IGeometryLonLatSinglePolygon, VLine) then begin
    _AddSinglePolygon(VLine);
  end else if Supports(AValue, IGeometryLonLatMultiPolygon, VMultiLine) then begin
    for i := 0 to VMultiLine.Count - 1 do begin
      VLine := VMultiLine.Item[i];
      _AddSinglePolygon(VLine);
    end;
  end;
  if FPoints.Count > 0 then begin
    FPoints.Delete(FPoints.Count - 1);
  end;
  if FPoints.Count > 0 then begin
    FSelectedPointIndex := FPoints.Count - 1;
  end else begin
    FSelectedPointIndex := 0;
  end;
end;


function TPolygonOnMapEdit._MakePolygon(
  const APoints: PDoublePointArray;
  ACount: Integer
): IGeometryLonLatPolygon;
var
  i: Integer;
  VStart: PDoublePointArray;
  VLineLen: Integer;
  VPoint: TDoublePoint;
  VPoints: IDoublePoints;
  VLineBounds: TDoubleRect;
  VIsValidX: Boolean;
  VIsValidY: Boolean;
  VIsOuter: Boolean;
begin
  VIsOuter := True;
  VStart := APoints;
  VLineLen := 0;
  for i := 0 to ACount - 1 do begin
    VPoint := APoints[i];
    VIsValidX := not IsNan(VPoint.X);
    VIsValidY := not IsNan(VPoint.Y);
    if VIsValidX and VIsValidY then begin
      if VLineLen = 0 then begin
        VStart := @APoints[i];
        VLineBounds.TopLeft := VPoint;
        VLineBounds.BottomRight := VPoint;
      end else begin
        UpdateLonLatMBRByPoint(VLineBounds, VPoint);
      end;
      Inc(VLineLen);
    end else begin
      if VLineLen > 0 then begin
        VPoints := TDoublePoints.Create(VStart, VLineLen);
        if VIsOuter then begin
          FBuilder.AddOuter(VLineBounds, VPoints);
        end else begin
          FBuilder.AddHole(VLineBounds, VPoints);
        end;
        VIsOuter := not VIsValidY;
        VLineLen := 0;
      end;
    end;
  end;
  if VLineLen > 0 then begin
    VPoints := TDoublePoints.Create(VStart, VLineLen);
    if VIsOuter then begin
      FBuilder.AddOuter(VLineBounds, VPoints);
    end else begin
      FBuilder.AddHole(VLineBounds, VPoints);
    end;
  end;
  Result := FBuilder.MakeStaticAndClear;
end;

procedure TPolygonOnMapEdit._UpdateLineObject;
begin
  FLine := _MakePolygon(FPoints.Points, FPoints.Count);
  _UpdateLineWithSelected;
end;

procedure TPolygonOnMapEdit._UpdateLineWithSelected;
begin
  if Assigned(FLine) then begin
    FLineWithSelected := TLonLatPolygonWithSelected.Create(FLine, FSelectedPointIndex);
  end else begin
    FLineWithSelected := nil;
  end;
end;

{ TLonLatLineWithSelectedBase }

constructor TLonLatLineWithSelectedBase.Create(
  ASelectedPointIndex: Integer
);
begin
  inherited Create;
  FSelectedPointIndex := ASelectedPointIndex;
end;

function TLonLatLineWithSelectedBase.GetSelectedPointIndex: Integer;
begin
  Result := FSelectedPointIndex;
end;

{ TLonLatPathWithSelected }

constructor TLonLatPathWithSelected.Create(
  const ALine: IGeometryLonLatLine;
  ASelectedPointIndex: Integer
);
begin
  Assert(Assigned(ALine));
  inherited Create(ASelectedPointIndex);
  FLine := ALine;
end;

function TLonLatPathWithSelected.GetGeometry: IGeometryLonLatLine;
begin
  Result := FLine;
end;

{ TLonLatPolygonWithSelected }

constructor TLonLatPolygonWithSelected.Create(
  const ALine: IGeometryLonLatPolygon;
  ASelectedPointIndex: Integer
);
begin
  Assert(Assigned(ALine));
  inherited Create(ASelectedPointIndex);
  FLine := ALine;
end;

function TLonLatPolygonWithSelected.GetGeometry: IGeometryLonLatPolygon;
begin
  Result := FLine;
end;

end.
