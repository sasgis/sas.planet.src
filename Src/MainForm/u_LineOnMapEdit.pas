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
  u_ChangeableBase;

type
  TLineOnMapEdit = class(TChangeableWithSimpleLockBase, ILineOnMapEdit)
  private
    FVectorGeometryLonLatFactory: IGeometryLonLatFactory;
    FPoints: array of TDoublePoint;
    FPointsCount: Integer;
    FSelectedPointIndex: Integer;
    procedure _GrowPoints(AAddCount: Integer);
    procedure _UpdateLineObject; virtual; abstract;
    procedure _UpdateLineWithSelected; virtual; abstract;
  private
    procedure SetSelectedPoint(
      ASegmentIndex: Integer;
      APointIndex: Integer
    );
    function SetSelectedNextPoint: TDoublePoint;
    function SetSelectedPrevPoint: TDoublePoint;
    function SelectPointInLonLatRect(const ARect: TDoubleRect): Boolean;

    function IsEmpty: Boolean; virtual; abstract;
    function IsReady: Boolean; virtual; abstract;
    procedure Clear;
    procedure DeleteActivePoint;
    procedure InsertPoint(const APoint: TDoublePoint);
    procedure MoveActivePoint(const APoint: TDoublePoint);
  public
    constructor Create(
      const AVectorGeometryLonLatFactory: IGeometryLonLatFactory
    );
    procedure AfterConstruction; override;
  end;

  TPathOnMapEdit = class(TLineOnMapEdit, IPathOnMapEdit)
  private
    FLine: IGeometryLonLatLine;
    FLineWithSelected: ILonLatPathWithSelected;
    procedure _UpdateLineObject; override;
    procedure _UpdateLineWithSelected; override;
    procedure _SetPath(const AValue: IGeometryLonLatLine);
  private
    function IsEmpty: Boolean; override;
    function IsReady: Boolean; override;
    function GetPath: ILonLatPathWithSelected;
    procedure SetPath(const AValue: ILonLatPathWithSelected); overload;
    procedure SetPath(const AValue: IGeometryLonLatLine); overload;
  end;

  TPolygonOnMapEdit = class(TLineOnMapEdit, IPolygonOnMapEdit)
  private
    FLine: IGeometryLonLatPolygon;
    FLineWithSelected: ILonLatPolygonWithSelected;
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
  end;

implementation

uses
  SysUtils,
  Math,
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
  FPointsCount := 0;
  FSelectedPointIndex := 0;
  SetLength(FPoints, 0);
end;

procedure TLineOnMapEdit.AfterConstruction;
begin
  inherited;
  _UpdateLineObject;
end;

procedure TLineOnMapEdit.Clear;
begin
  CS.BeginWrite;
  try
    if FPointsCount > 0 then begin
      FPointsCount := 0;
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
  VPrevPoint: TDoublePoint;
  VNextPoint: TDoublePoint;
  VDelCount: Integer;
begin
  CS.BeginWrite;
  try
    if FPointsCount > 0 then begin
      if FSelectedPointIndex < FPointsCount then begin
        if FSelectedPointIndex = FPointsCount - 1 then begin
          Dec(FPointsCount);
          if FPointsCount > 0 then begin
            Dec(FSelectedPointIndex);
          end;
        end else begin
          VNextPoint := FPoints[FSelectedPointIndex + 1];
          if FSelectedPointIndex = 0 then begin
            VPrevPoint := CEmptyDoublePoint;
          end else begin
            VPrevPoint := FPoints[FSelectedPointIndex - 1];
          end;
          if PointIsEmpty(VPrevPoint) and PointIsEmpty(VNextPoint) then begin
            VDelCount := 2;
          end else begin
            VDelCount := 1;
          end;
          Move(FPoints[FSelectedPointIndex + VDelCount], FPoints[FSelectedPointIndex], (FPointsCount - FSelectedPointIndex - VDelCount) * SizeOf(TDoublePoint));
          Dec(FPointsCount, VDelCount);
          if FSelectedPointIndex > 0 then begin
            Dec(FSelectedPointIndex);
          end;
        end;
        _UpdateLineObject;
      end else begin
        if FPointsCount > 0 then begin
          FSelectedPointIndex := FPointsCount - 1;
          _UpdateLineWithSelected;
        end;
      end;
    end;
  finally
    CS.EndWrite;
  end;
  DoChangeNotify;
end;

procedure TLineOnMapEdit.InsertPoint(const APoint: TDoublePoint);
var
  VInsertCount: Integer;
  VCurrPoint: TDoublePoint;
  VNextPoint: TDoublePoint;
begin
  CS.BeginWrite;
  try
    if (FPointsCount <= 0) or (FSelectedPointIndex >= FPointsCount) then begin
      VCurrPoint := CEmptyDoublePoint;
      VNextPoint := CEmptyDoublePoint;
    end else begin
      if FSelectedPointIndex = FPointsCount - 1 then begin
        VCurrPoint := FPoints[FSelectedPointIndex];
        VNextPoint := CEmptyDoublePoint;
      end else begin
        VCurrPoint := FPoints[FSelectedPointIndex];
        VNextPoint := FPoints[FSelectedPointIndex + 1];
      end;
    end;

    if PointIsEmpty(APoint) then begin
      if not PointIsEmpty(VCurrPoint) then begin
        if PointIsEmpty(VNextPoint) then begin
          Inc(FSelectedPointIndex);
          _UpdateLineWithSelected;
        end else begin
          VInsertCount := 1;
          _GrowPoints(VInsertCount);
          Move(FPoints[FSelectedPointIndex + 1], FPoints[FSelectedPointIndex + VInsertCount + 1], (FPointsCount - FSelectedPointIndex - 1) * SizeOf(TDoublePoint));
          FPoints[FSelectedPointIndex] := CEmptyDoublePoint;
          Inc(FPointsCount, VInsertCount);
          Inc(FSelectedPointIndex, 2);
          _UpdateLineObject;
        end;
      end;
    end else begin
      if PointIsEmpty(VCurrPoint) then begin
        if FSelectedPointIndex < FPointsCount then begin
          VInsertCount := 2;
        end else begin
          VInsertCount := 1;
        end;
        _GrowPoints(VInsertCount);
        if FSelectedPointIndex < FPointsCount then begin
          Move(FPoints[FSelectedPointIndex], FPoints[FSelectedPointIndex + VInsertCount], (FPointsCount - FSelectedPointIndex) * SizeOf(TDoublePoint));
          FPoints[FSelectedPointIndex + 1] := APoint;
          FPoints[FSelectedPointIndex + 2] := CEmptyDoublePoint;
          Inc(FSelectedPointIndex);
        end else begin
          FPoints[FSelectedPointIndex] := APoint;
        end;
        Inc(FPointsCount, VInsertCount);
        _UpdateLineObject;
      end else begin
        VInsertCount := 1;
        _GrowPoints(VInsertCount);
        if FSelectedPointIndex < FPointsCount - 1 then begin
          Move(FPoints[FSelectedPointIndex + 1], FPoints[FSelectedPointIndex + VInsertCount + 1], (FPointsCount - FSelectedPointIndex - 1) * SizeOf(TDoublePoint));
        end;
        FPoints[FSelectedPointIndex + 1] := APoint;
        Inc(FPointsCount, VInsertCount);
        Inc(FSelectedPointIndex);
        _UpdateLineObject;
      end;
    end;
  finally
    CS.EndWrite;
  end;
  DoChangeNotify;
end;

procedure TLineOnMapEdit.MoveActivePoint(const APoint: TDoublePoint);
var
  VCurrPoint: TDoublePoint;
begin
  if not PointIsEmpty(APoint) then begin
    CS.BeginWrite;
    try
      if FPointsCount > 0 then begin
        if FSelectedPointIndex < FPointsCount then begin
          VCurrPoint := FPoints[FSelectedPointIndex];
          if not PointIsEmpty(VCurrPoint) then begin
            if not DoublePointsEqual(APoint, VCurrPoint) then begin
              FPoints[FSelectedPointIndex] := APoint;
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
  i: Integer;
begin
  Result := False;
  CS.BeginWrite;
  try
    VIndex := -1;
    if FPointsCount > 0 then begin
      if FSelectedPointIndex < FPointsCount then begin
        VPoint := FPoints[FSelectedPointIndex];
        if not PointIsEmpty(VPoint) then begin
          if LonLatPointInRect(VPoint, ARect) then begin
            VIndex := FSelectedPointIndex;
          end;
        end;
      end;
    end;

    if VIndex < 0 then begin
      for i := FPointsCount - 1 downto 0 do begin
        VPoint := FPoints[i];
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
begin
  CS.BeginWrite;
  try
    Result := CEmptyDoublePoint;
    if FPointsCount > 0 then begin
      if FSelectedPointIndex < FPointsCount then begin
        Inc(FSelectedPointIndex);
        _UpdateLineWithSelected;
        if FSelectedPointIndex < FPointsCount then begin
          Result := FPoints[FSelectedPointIndex];
        end;
      end;
    end;
  finally
    CS.EndWrite;
  end;
  DoChangeNotify;
end;

procedure TLineOnMapEdit.SetSelectedPoint(ASegmentIndex,
  APointIndex: Integer);
var
  i: Integer;
  VSegmentIndex: Integer;
  VPointIndex: Integer;
  VResult: Integer;
begin
  CS.BeginWrite;
  try
    VResult := -1;
    VSegmentIndex := 0;
    VPointIndex := 0;
    for i := 0 to FPointsCount - 1 do begin
      if VSegmentIndex = ASegmentIndex then begin
        if VPointIndex = APointIndex then begin
          VResult := i;
          Break;
        end;
      end;
      if PointIsEmpty(FPoints[i]) then begin
        Inc(VSegmentIndex);
        VPointIndex := 0;
      end else begin
        Inc(VPointIndex);
      end;
    end;
    if VResult >= 0 then begin
      if FSelectedPointIndex <> VResult then begin
        FSelectedPointIndex := VResult;
        _UpdateLineWithSelected;
      end;
    end;
  finally
    CS.EndWrite;
  end;
  DoChangeNotify;
end;

function TLineOnMapEdit.SetSelectedPrevPoint: TDoublePoint;
begin
  CS.BeginWrite;
  try
    Result := CEmptyDoublePoint;
    if FPointsCount > 0 then begin
      if FSelectedPointIndex > 0 then begin
        Dec(FSelectedPointIndex);
        _UpdateLineWithSelected;
        Result := FPoints[FSelectedPointIndex];
      end;
    end;
  finally
    CS.EndWrite;
  end;
  DoChangeNotify;
end;

procedure TLineOnMapEdit._GrowPoints(AAddCount: Integer);
var
  VSize: Integer;
begin
  VSize := Length(FPoints);
  if FPointsCount + AAddCount > VSize then begin
    while FPointsCount + AAddCount > VSize do begin
      if VSize < 64 then begin
        VSize := 64;
      end else if VSize < 1024 then begin
        VSize := VSize * 2;
      end else begin
        VSize := VSize + 1024;
      end;
    end;
    SetLength(FPoints, VSize);
  end;
end;

{ TPathOnMapEdit }

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
      if FPointsCount > 0 then begin
        FSelectedPointIndex := FPointsCount - 1;
      end else begin
        FSelectedPointIndex := 0;
      end;
    end else if FSelectedPointIndex >= FPointsCount then begin
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
    Result := FLine.IsEmpty;
  finally
    CS.EndRead;
  end;
end;

function TPathOnMapEdit.IsReady: Boolean;
begin
  CS.BeginRead;
  try
    Result := False;
    if not FLine.IsEmpty then begin
      Result := IsValidLonLatLine(FLine);
    end;
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
  FPointsCount := 0;
  if Supports(AValue, IGeometryLonLatSingleLine, VLine) then begin
    _GrowPoints(VLine.Count + 1);
    Move(VLine.Points[0], FPoints[FPointsCount], VLine.Count * SizeOf(TDoublePoint));
    Inc(FPointsCount, VLine.Count);
    FPoints[FPointsCount] := CEmptyDoublePoint;
    Inc(FPointsCount);
  end else if Supports(AValue, IGeometryLonLatMultiLine, VMultiLine) then begin
    for i := 0 to VMultiLine.Count - 1 do begin
      VLine := VMultiLine.Item[i];
      _GrowPoints(VLine.Count + 1);
      Move(VLine.Points[0], FPoints[FPointsCount], VLine.Count * SizeOf(TDoublePoint));
      Inc(FPointsCount, VLine.Count);
      FPoints[FPointsCount] := CEmptyDoublePoint;
      Inc(FPointsCount);
    end;
  end;
  if FPointsCount > 0 then begin
    Dec(FPointsCount);
  end;

  if FPointsCount > 0 then begin
    FSelectedPointIndex := FPointsCount - 1;
  end else begin
    FSelectedPointIndex := 0;
  end;
end;

procedure TPathOnMapEdit._UpdateLineObject;
begin
  FLine := FVectorGeometryLonLatFactory.CreateLonLatLine(@FPoints[0], FPointsCount);
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
      if FPointsCount > 0 then begin
        FSelectedPointIndex := FPointsCount - 1;
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
    Result := not FLine.IsEmpty;
  finally
    CS.EndRead;
  end;
end;

function TPolygonOnMapEdit.IsReady: Boolean;
begin
  CS.BeginRead;
  try
    Result := False;
    if not FLine.IsEmpty then begin
      Result := IsValidLonLatPolygon(FLine);
    end;
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
  _GrowPoints(AValue.Count + 1);
  Move(AValue.Points[0], FPoints[FPointsCount], AValue.Count * SizeOf(TDoublePoint));
  Inc(FPointsCount, AValue.Count);
  FPoints[FPointsCount] := CEmptyDoublePoint;
  Inc(FPointsCount);
end;

procedure TPolygonOnMapEdit._AddSinglePolygon(
  const AValue: IGeometryLonLatSinglePolygon
);
var
  i: Integer;
begin
  _AddContour(AValue.OuterBorder);
  for i := 0 to AValue.HoleCount - 1 do begin
    FPoints[FPointsCount - 1].Y := -1;
    _AddContour(AValue.HoleBorder[i]);
  end;
end;

procedure TPolygonOnMapEdit._SetPolygon(const AValue: IGeometryLonLatPolygon);
var
  i: Integer;
  VLine: IGeometryLonLatSinglePolygon;
  VMultiLine: IGeometryLonLatMultiPolygon;
begin
  FPointsCount := 0;
  if Supports(AValue, IGeometryLonLatSinglePolygon, VLine) then begin
    _AddSinglePolygon(VLine);
  end else if Supports(AValue, IGeometryLonLatMultiPolygon, VMultiLine) then begin
    for i := 0 to VMultiLine.Count - 1 do begin
      VLine := VMultiLine.Item[i];
      _AddSinglePolygon(VLine);
    end;
  end;
  if FPointsCount > 0 then begin
    Dec(FPointsCount);
  end;
  if FPointsCount > 0 then begin
    FSelectedPointIndex := FPointsCount - 1;
  end else begin
    FSelectedPointIndex := 0;
  end;
end;

procedure TPolygonOnMapEdit._UpdateLineObject;
begin
  FLine := FVectorGeometryLonLatFactory.CreateLonLatPolygon(@FPoints[0], FPointsCount);
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
