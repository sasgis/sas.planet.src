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

unit u_DoublePointsAggregator;

interface

uses
  t_GeoTypes,
  i_DoublePoints,
  i_DoublePointsAggregator,
  u_BaseInterfacedObject;

type
  TDoublePointsAggregator = class(TBaseInterfacedObject, IDoublePointsAggregator)
  private
    FPoints: PDoublePointArray;
    FCount: Integer;
    FCapacity: Integer;
    procedure Grow(const AAddCount: Integer);
  private
    procedure Add(const APoint: TDoublePoint);
    procedure AddPoints(
      const APoints: PDoublePointArray;
      const ACount: Integer
    );
    procedure Insert(
      const AIndex: Integer;
      const APoint: TDoublePoint
    );
    procedure InsertPoints(
      const AIndex: Integer;
      const APoints: PDoublePointArray;
      const ACount: Integer
    );
    procedure Delete(const AIndex: Integer);
    procedure DeletePoints(
      const AIndex: Integer;
      const ACount: Integer
    );
    procedure Clear;

    function GetCount: Integer;
    function GetPoints: PDoublePointArray;
    function MakeStaticAndClear: IDoublePoints;
    function MakeStaticCopy: IDoublePoints;
  public
    constructor Create(const ACapacity: Integer = 0);
    destructor Destroy; override;
  end;

implementation

uses
  u_DoublePoints;

{ TDoublePointsAggregator }

constructor TDoublePointsAggregator.Create(const ACapacity: Integer = 0);
begin
  Assert(ACapacity >= 0);
  inherited Create;
  FCount := 0;
  FCapacity := ACapacity;
  GetMem(FPoints, FCapacity * SizeOf(TDoublePoint));
end;

destructor TDoublePointsAggregator.Destroy;
begin
  FreeMem(FPoints);
  FPoints := nil;
  FCount := 0;
  FCapacity := 0;
  inherited;
end;

procedure TDoublePointsAggregator.Grow(const AAddCount: Integer);
var
  VNewCount: Integer;
  VNewCapacity: Integer;
  VPoints: PDoublePointArray;
begin
  Assert(AAddCount >= 0);
  VNewCount := FCount + AAddCount;
  if VNewCount > FCapacity then begin
    VNewCapacity := FCapacity;
    while VNewCount > VNewCapacity do begin
      if VNewCapacity < 256 then begin
        VNewCapacity := 256;
      end else if VNewCapacity < 4 * 1024 then begin
        VNewCapacity := VNewCapacity * 2;
      end else begin
        VNewCapacity := VNewCapacity + 4 * 1024;
      end;
    end;
    if FCount > 0 then begin
      GetMem(VPoints, VNewCapacity * SizeOf(TDoublePoint));
      Move(FPoints[0], VPoints[0], FCount * SizeOf(TDoublePoint));
      FreeMem(FPoints);
      FPoints := VPoints;
    end else begin
      GetMem(FPoints, VNewCapacity * SizeOf(TDoublePoint));
    end;
    FCapacity := VNewCapacity;
  end;
end;

procedure TDoublePointsAggregator.Add(const APoint: TDoublePoint);
begin
  Grow(1);
  FPoints[FCount] := APoint;
  Inc(FCount);
end;

procedure TDoublePointsAggregator.AddPoints(
  const APoints: PDoublePointArray;
  const ACount: Integer
);
begin
  if ACount > 0 then begin
    Grow(ACount);
    Move(APoints[0], FPoints[FCount], ACount * SizeOf(TDoublePoint));
    FCount := FCount + ACount;
  end;
end;

procedure TDoublePointsAggregator.Insert(
  const AIndex: Integer;
  const APoint: TDoublePoint
);
begin
  Assert((AIndex >= 0) or (AIndex <= FCount));
  if (AIndex < 0) or (AIndex > FCount) then begin
    Assert(False);
  end else if AIndex = FCount then begin
    Add(APoint);
  end else begin
    Grow(1);
    Move(FPoints[AIndex], FPoints[AIndex + 1], (FCount - AIndex) * SizeOf(TDoublePoint));
    FPoints[AIndex] := APoint;
    Inc(FCount);
  end;
end;

procedure TDoublePointsAggregator.InsertPoints(
  const AIndex: Integer;
  const APoints: PDoublePointArray;
  const ACount: Integer
);
begin
  Assert((AIndex >= 0) or (AIndex <= FCount));
  if (AIndex < 0) or (AIndex > FCount) then begin
    Assert(False);
  end else if AIndex = FCount then begin
    AddPoints(APoints, ACount);
  end else begin
    if ACount > 0 then begin
      Grow(ACount);
      Move(FPoints[AIndex], FPoints[AIndex + ACount], (FCount - AIndex) * SizeOf(TDoublePoint));
      Move(APoints[0], FPoints[AIndex], ACount * SizeOf(TDoublePoint));
      FCount := FCount + ACount;
    end;
  end;
end;

procedure TDoublePointsAggregator.Delete(const AIndex: Integer);
begin
  Assert((AIndex >= 0) or (AIndex < FCount));
  if (AIndex < 0) or (AIndex >= FCount) then begin
    Assert(False);
  end else if AIndex = FCount - 1 then begin
    Dec(FCount);
  end else begin
    Move(FPoints[AIndex + 1], FPoints[AIndex], (FCount - AIndex - 1) * SizeOf(TDoublePoint));
    Dec(FCount);
  end;
end;

procedure TDoublePointsAggregator.DeletePoints(const AIndex, ACount: Integer);
begin
  Assert((AIndex >= 0) or (AIndex + ACount < FCount));
  if (AIndex < 0) or (AIndex + ACount >= FCount) then begin
    Assert(False);
  end else if AIndex + ACount = FCount - 1 then begin
    Dec(FCount, ACount);
  end else begin
    Move(FPoints[AIndex + ACount], FPoints[AIndex], (FCount - AIndex - ACount) * SizeOf(TDoublePoint));
    Dec(FCount, ACount);
  end;
end;

procedure TDoublePointsAggregator.Clear;
begin
  FCount := 0;
end;

function TDoublePointsAggregator.GetCount: Integer;
begin
  Result := FCount;
end;

function TDoublePointsAggregator.GetPoints: PDoublePointArray;
begin
  Result := FPoints;
end;

function TDoublePointsAggregator.MakeStaticAndClear: IDoublePoints;
begin
  Result := nil;
  if FCount > 0 then begin
    Result := TDoublePoints.Create(FPoints, FCount);
    FCount := 0;
  end;
end;

function TDoublePointsAggregator.MakeStaticCopy: IDoublePoints;
begin
  Result := nil;
  if FCount > 0 then begin
    Result := TDoublePoints.Create(FPoints, FCount);
  end;
end;

end.
