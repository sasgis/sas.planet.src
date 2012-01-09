{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
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

unit u_LineOnMapEdit;

interface

uses
  Classes,
  t_GeoTypes,
  i_VectorItemLonLat,
  i_VectorItmesFactory,
  i_EnumDoublePoint,
  u_ConfigDataElementBase,
  i_LineOnMapEdit;

type
  TLineOnMapEdit = class(TConfigDataElementBaseEmptySaveLoad, ILineOnMapEdit)
  private
    FPoints: TArrayOfDoublePoint;
    FCount: Integer;
    FActiveIndex: Integer;
  protected
    function GetCount: Integer;
    function GetActiveIndex: Integer;
    function GetPoints: TArrayOfDoublePoint;
    function GetPointIndexInLonLatRect(ARect: TDoubleRect): Integer;
    procedure Empty;
    procedure SetActiveIndex(AValue: Integer);
    procedure DeleteActivePoint;
    procedure InsertPoint(APoint: TDoublePoint);
    procedure MoveActivePoint(APoint: TDoublePoint);
    procedure SetPoints(AValue: TArrayOfDoublePoint);
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TLonLatLineWithSelectedBase = class(TInterfacedObject)
  private
    FSelectedPoint: TDoublePoint;
    FSelectedSegmentIndex: Integer;
    FSelectedPointIndex: Integer;
  private
    function GetSelectedPoint: TDoublePoint;
    function GetSelectedSegmentIndex: Integer;
    function GetSelectedPointIndex: Integer;
  public
    constructor Create(
      ASelectedPoint: TDoublePoint;
      ASelectedSegmentIndex: Integer;
      ASelectedPointIndex: Integer
    );
  end;

  TLonLatPathWithSelected = class(TLonLatLineWithSelectedBase, ILonLatPathWithSelected)
  private
    FLine: ILonLatPath;
  private
    function GetEnum: IEnumLonLatPoint;
    function GetCount: Integer;
    function GetItem(AIndex: Integer): ILonLatPathLine;
  public
    constructor Create(
      ALine: ILonLatPath;
      ASelectedSegmentIndex: Integer;
      ASelectedPointIndex: Integer
    );
  end;

  TLonLatPolygonWithSelected = class(TLonLatLineWithSelectedBase, ILonLatPolygonWithSelected)
  private
    FLine: ILonLatPolygon;
  private
    function GetEnum: IEnumLonLatPoint;
    function GetCount: Integer;
    function GetItem(AIndex: Integer): ILonLatPolygonLine;
  public
    constructor Create(
      ALine: ILonLatPolygon;
      ASelectedSegmentIndex: Integer;
      ASelectedPointIndex: Integer
    );
  end;




  TLineOnMapEditNew = class(TConfigDataElementBaseEmptySaveLoad, ILineOnMapEditNew)
  private
    FFactory: IVectorItmesFactory;
    FPoints: TArrayOfDoublePoint;
    FSegmentStartInexes: array of Integer;
    FSegmentsCount: Integer;
    FPointsCount: Integer;
    FSelectedSegmentIndex: Integer;
    FSelectedPointIndex: Integer;
    procedure _UpdateLineObject; virtual; abstract;
    procedure _UpdateLineWithSelected; virtual; abstract;
  private
    procedure SetSelectedPoint(ASegmentIndex: Integer; APointIndex: Integer);
    procedure SetSelectedNextPoint;
    procedure SetSelectedPrevPoint;
    function SelectPointInLonLatRect(ARect: TDoubleRect): Boolean;

    procedure Clear;
    procedure DeleteActivePoint;
    procedure InsertPoint(APoint: TDoublePoint);
    procedure MoveActivePoint(APoint: TDoublePoint);
  public
    constructor Create(AFactory: IVectorItmesFactory);
    destructor Destroy; override;
  end;

  TPathOnMapEdit = class(TLineOnMapEditNew, IPathOnMapEdit)
  private
  private
    function GetPath: ILonLatPathWithSelected;
    procedure SetPath(AValue: ILonLatPathWithSelected);
  end;

implementation

uses
  u_GeoFun;

{ TLineOnMapEdit }

constructor TLineOnMapEdit.Create;
begin
  inherited;
  SetLength(FPoints, 64);
  FCount := 0;
  FActiveIndex := -1;
end;

destructor TLineOnMapEdit.Destroy;
begin
  FPoints := nil;
  inherited;
end;

procedure TLineOnMapEdit.DeleteActivePoint;
begin
  LockWrite;
  try
    if FCount > 0 then begin
      if FActiveIndex >= FCount - 1 then begin
        Dec(FCount);
        FActiveIndex := FCount - 1;
      end else begin
        if FActiveIndex < 0 then begin
          FActiveIndex := 0;
        end;
        Move(FPoints[FActiveIndex + 1], FPoints[FActiveIndex], (FCount - FActiveIndex - 1) * SizeOf(FPoints[0]));
        Dec(FCount);
        Dec(FActiveIndex);
        if FActiveIndex < 0 then begin
          FActiveIndex := 0;
        end;
      end;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TLineOnMapEdit.Empty;
begin
  LockWrite;
  try
    if FCount > 0 then begin
      FCount := 0;
      FActiveIndex := -1;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

function TLineOnMapEdit.GetActiveIndex: Integer;
begin
  LockRead;
  try
    Result := FActiveIndex;
  finally
    UnlockRead;
  end;
end;

function TLineOnMapEdit.GetCount: Integer;
begin
  LockRead;
  try
    Result := FCount;
  finally
    UnlockRead;
  end;
end;

function TLineOnMapEdit.GetPointIndexInLonLatRect(ARect: TDoubleRect): Integer;
var
  i: Integer;
begin
  Result := -1;
  LockRead;
  try
    for i := FCount - 1 downto 0 do begin
      if LonLatPointInRect(FPoints[i], ARect) then begin
        Result := i;
        Break;
      end;
    end;
  finally
    UnlockRead;
  end;
end;

function TLineOnMapEdit.GetPoints: TArrayOfDoublePoint;
begin
  LockRead;
  try
    Result := Copy(FPoints, 0, FCount);
  finally
    UnlockRead;
  end;
end;

procedure TLineOnMapEdit.InsertPoint(APoint: TDoublePoint);
begin
  LockWrite;
  try
    if Length(FPoints) <= FCount then begin
      SetLength(FPoints, FCount * 2);
    end;
    if FActiveIndex >= FCount - 1 then begin
      FPoints[FCount] := APoint;
      FActiveIndex := FCount;
      Inc(FCount);
    end else begin
      if FActiveIndex < 0 then begin
        FActiveIndex := 0;
      end;
      Move(FPoints[FActiveIndex + 1], FPoints[FActiveIndex + 2], (FCount - FActiveIndex - 1) * SizeOf(FPoints[0]));
      FPoints[FActiveIndex + 1] := APoint;
      Inc(FActiveIndex);
      Inc(FCount);
    end;
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

procedure TLineOnMapEdit.MoveActivePoint(APoint: TDoublePoint);
begin
  LockWrite;
  try
    if (FCount > 0) then begin
      if FActiveIndex < 0 then begin
        FActiveIndex := 0;
      end;
      if FActiveIndex >= FCount then begin
        FActiveIndex := FCount - 1;
      end;
      if not DoublePointsEqual(APoint, FPoints[FActiveIndex]) then begin
        FPoints[FActiveIndex] := APoint;
        SetChanged;
      end;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TLineOnMapEdit.SetActiveIndex(AValue: Integer);
begin
  LockWrite;
  try
    if (AValue >= 0) and (AValue < FCount) then begin
      if FActiveIndex <> AValue then begin
        FActiveIndex := AValue;
        SetChanged;
      end;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TLineOnMapEdit.SetPoints(AValue: TArrayOfDoublePoint);
var
  VNewCount: Integer;
begin
  VNewCount := Length(AValue);
  if VNewCount > 1 then begin
    if DoublePointsEqual(AValue[0], AValue[VNewCount - 1]) then begin
      Dec(VNewCount);
    end;
  end;
  LockWrite;
  try
    if VNewCount > 0 then begin
    if Length(FPoints) < VNewCount then begin
        SetLength(FPoints, VNewCount);
      end;
      FCount := VNewCount;
      FActiveIndex := FCount - 1;
      Move(AValue[0], FPoints[0], FCount * SizeOf(FPoints[0]));
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

{ TLineOnMapEditNew }

constructor TLineOnMapEditNew.Create(AFactory: IVectorItmesFactory);
begin
  inherited Create;
  FFactory := AFactory;
  FSegmentsCount := 0;
  FPointsCount := 0;
  FSelectedSegmentIndex := 0;
  FSelectedPointIndex := 0;
  SetLength(FSegmentStartInexes, FSegmentsCount + 1);
  SetLength(FPoints, 0);
end;

destructor TLineOnMapEditNew.Destroy;
begin

  inherited;
end;

procedure TLineOnMapEditNew.Clear;
begin
  LockWrite;
  try
    if FSegmentsCount > 0 then begin
      FSegmentsCount := 0;
      FPointsCount := 0;
      FSelectedSegmentIndex := 0;
      FSelectedPointIndex := 0;
      SetLength(FSegmentStartInexes, 1);
      SetLength(FPoints, 0);
      _UpdateLineObject;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TLineOnMapEditNew.DeleteActivePoint;
begin
  LockWrite;
  try
    if FSegmentsCount > 0 then begin
      if FSelectedSegmentIndex < FSegmentsCount then begin

      end else begin
        FSelectedSegmentIndex := FSegmentsCount - 1;
      end;

      _UpdateLineObject;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TLineOnMapEditNew.InsertPoint(APoint: TDoublePoint);
begin
  LockWrite;
  try

    _UpdateLineObject;
  finally
    UnlockWrite;
  end;
end;

procedure TLineOnMapEditNew.MoveActivePoint(APoint: TDoublePoint);
begin
  LockWrite;
  try

    _UpdateLineObject;
  finally
    UnlockWrite;
  end;
end;

function TLineOnMapEditNew.SelectPointInLonLatRect(ARect: TDoubleRect): Boolean;
begin
  LockWrite;
  try

    _UpdateLineWithSelected;
  finally
    UnlockWrite;
  end;
end;

procedure TLineOnMapEditNew.SetSelectedNextPoint;
begin
  LockWrite;
  try

    _UpdateLineWithSelected;
  finally
    UnlockWrite;
  end;
end;

procedure TLineOnMapEditNew.SetSelectedPoint(ASegmentIndex,
  APointIndex: Integer);
begin
  LockWrite;
  try

    _UpdateLineWithSelected;
  finally
    UnlockWrite;
  end;
end;

procedure TLineOnMapEditNew.SetSelectedPrevPoint;
begin
  LockWrite;
  try

    _UpdateLineWithSelected;
  finally
    UnlockWrite;
  end;
end;

{ TPathOnMapEdit }

function TPathOnMapEdit.GetPath: ILonLatPathWithSelected;
begin

end;

procedure TPathOnMapEdit.SetPath(AValue: ILonLatPathWithSelected);
begin

end;

{ TLonLatLineWithSelectedBase }

constructor TLonLatLineWithSelectedBase.Create(ASelectedPoint: TDoublePoint;
  ASelectedSegmentIndex, ASelectedPointIndex: Integer);
begin
  FSelectedPoint := ASelectedPoint;
  FSelectedSegmentIndex := ASelectedSegmentIndex;
  FSelectedPointIndex := ASelectedPointIndex;
end;

function TLonLatLineWithSelectedBase.GetSelectedPoint: TDoublePoint;
begin
  Result := FSelectedPoint;
end;

function TLonLatLineWithSelectedBase.GetSelectedPointIndex: Integer;
begin
  Result := FSelectedPointIndex;
end;

function TLonLatLineWithSelectedBase.GetSelectedSegmentIndex: Integer;
begin
  Result := FSelectedSegmentIndex;
end;

{ TLonLatPathWithSelected }

constructor TLonLatPathWithSelected.Create(ALine: ILonLatPath;
  ASelectedSegmentIndex, ASelectedPointIndex: Integer);
var
  VLine: ILonLatPathLine;
  VPoint: TDoublePoint;
begin
  Assert(ASelectedSegmentIndex >= 0);
  Assert(ASelectedSegmentIndex < ALine.Count);
  VLine := ALine.Item[ASelectedSegmentIndex];
  Assert(ASelectedPointIndex >= 0);
  Assert(ASelectedPointIndex <= VLine.Count);
  if ASelectedPointIndex < VLine.Count then begin
    VPoint := VLine.Points[ASelectedPointIndex];
  end else begin
    VPoint := CEmptyDoublePoint;
  end;
  inherited Create(VPoint, ASelectedSegmentIndex, ASelectedPointIndex);
  FLine := ALine;
end;

function TLonLatPathWithSelected.GetCount: Integer;
begin
  Result := FLine.Count;
end;

function TLonLatPathWithSelected.GetEnum: IEnumLonLatPoint;
begin
  Result := FLine.GetEnum;
end;

function TLonLatPathWithSelected.GetItem(AIndex: Integer): ILonLatPathLine;
begin
  Result := FLine.Item[AIndex];
end;

{ TLonLatPolygonWithSelected }

constructor TLonLatPolygonWithSelected.Create(ALine: ILonLatPolygon;
  ASelectedSegmentIndex, ASelectedPointIndex: Integer);
var
  VLine: ILonLatPolygonLine;
  VPoint: TDoublePoint;
begin
  Assert(ASelectedSegmentIndex >= 0);
  Assert(ASelectedSegmentIndex < ALine.Count);
  VLine := ALine.Item[ASelectedSegmentIndex];
  Assert(ASelectedPointIndex >= 0);
  Assert(ASelectedPointIndex < VLine.Count);
  if ASelectedPointIndex < VLine.Count then begin
    VPoint := VLine.Points[ASelectedPointIndex];
  end else begin
    VPoint := CEmptyDoublePoint;
  end;
  inherited Create(VPoint, ASelectedSegmentIndex, ASelectedPointIndex);
  FLine := ALine;
end;

function TLonLatPolygonWithSelected.GetCount: Integer;
begin
  Result := FLine.Count;
end;

function TLonLatPolygonWithSelected.GetEnum: IEnumLonLatPoint;
begin
  Result := FLine.GetEnum;
end;

function TLonLatPolygonWithSelected.GetItem(
  AIndex: Integer): ILonLatPolygonLine;
begin
  Result := FLine.Item[AIndex];
end;

end.
