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

unit u_DoublePointsAggregator;

interface

uses
  t_GeoTypes,
  i_DoublePoints,
  i_DoublePointsAggregator,
  u_DoublePointsMetaAggregator,
  u_BaseInterfacedObject;

type
  TDoublePointsAggregator = class(TBaseInterfacedObject, IDoublePointsAggregator)
  private
    FPoints: PDoublePointArray;
    FMetaAggregator: TDoublePointsMetaAggregator;
    FCount: Integer;
    FCapacity: Integer;
    procedure Grow(const AAddCount: Integer);
  private
    { IDoublePointsAggregator }
    procedure Add(
      const APoint: TDoublePoint;
      const AMetaItem: PDoublePointsMetaItem
    );
    procedure AddPoints(
      const APoints: PDoublePointArray;
      const AMeta: PDoublePointsMeta;
      const ACount: Integer
    );

    procedure Insert(
      const AIndex: Integer;
      const APoint: TDoublePoint;
      const AMetaItem: PDoublePointsMetaItem
    );
    procedure InsertPoints(
      const AIndex: Integer;
      const APoints: PDoublePointArray;
      const AMeta: PDoublePointsMeta;
      const ACount: Integer
    );

    procedure Delete(const AIndex: Integer);
    procedure DeletePoints(
      const AIndex: Integer;
      const ACount: Integer
    );

    procedure Update(
      const AIndex: Integer;
      const APoint: TDoublePoint;
      const AMetaItem: PDoublePointsMetaItem
    );

    procedure Clear;

    function GetCount: Integer;
    function GetPoints: PDoublePointArray;
    function GetMeta: PDoublePointsMeta;

    function MakeStaticAndClear: IDoublePoints;
    function MakeStaticCopy: IDoublePoints;
  public
    constructor Create(const ACapacity: Integer = 0);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  u_DoublePoints;

{ TDoublePointsAggregator }

constructor TDoublePointsAggregator.Create(const ACapacity: Integer);
begin
  Assert(ACapacity >= 0);
  inherited Create;
  FCount := 0;
  FCapacity := ACapacity;
  GetMem(FPoints, FCapacity * SizeOf(TDoublePoint));
  FMetaAggregator := TDoublePointsMetaAggregator.Create(@FCount, @FCapacity);
end;

destructor TDoublePointsAggregator.Destroy;
begin
  FreeMem(FPoints);
  FPoints := nil;
  FCount := 0;
  FCapacity := 0;
  FreeAndNil(FMetaAggregator);
  inherited Destroy;
end;

procedure TDoublePointsAggregator.Grow(const AAddCount: Integer);
var
  VNewCount: Integer;
  VNewCapacity: Integer;
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
    ReallocMem(FPoints, VNewCapacity * SizeOf(TDoublePoint));
    FCapacity := VNewCapacity;
  end;
end;

procedure TDoublePointsAggregator.Add(
  const APoint: TDoublePoint;
  const AMetaItem: PDoublePointsMetaItem
);
begin
  Grow(1);
  FPoints[FCount] := APoint;
  FMetaAggregator.AddItem(AMetaItem);
  Inc(FCount);
end;

procedure TDoublePointsAggregator.AddPoints(
  const APoints: PDoublePointArray;
  const AMeta: PDoublePointsMeta;
  const ACount: Integer
);
begin
  if ACount > 0 then begin
    Grow(ACount);
    Move(APoints[0], FPoints[FCount], ACount * SizeOf(TDoublePoint));
    FMetaAggregator.AddItems(AMeta, ACount);
    Inc(FCount, ACount);
  end;
end;

procedure TDoublePointsAggregator.Insert(
  const AIndex: Integer;
  const APoint: TDoublePoint;
  const AMetaItem: PDoublePointsMetaItem
);
begin
  Assert((AIndex >= 0) or (AIndex <= FCount));
  if (AIndex < 0) or (AIndex > FCount) then begin
    Assert(False);
  end else if AIndex = FCount then begin
    Add(APoint, AMetaItem);
  end else begin
    Grow(1);
    Move(FPoints[AIndex], FPoints[AIndex + 1], (FCount - AIndex) * SizeOf(TDoublePoint));
    FPoints[AIndex] := APoint;
    FMetaAggregator.InsertItem(AIndex, AMetaItem);
    Inc(FCount);
  end;
end;

procedure TDoublePointsAggregator.InsertPoints(
  const AIndex: Integer;
  const APoints: PDoublePointArray;
  const AMeta: PDoublePointsMeta;
  const ACount: Integer
);
begin
  Assert((AIndex >= 0) or (AIndex <= FCount));
  if (AIndex < 0) or (AIndex > FCount) then begin
    Assert(False);
  end else if AIndex = FCount then begin
    AddPoints(APoints, AMeta, ACount);
  end else begin
    if ACount > 0 then begin
      Grow(ACount);
      Move(FPoints[AIndex], FPoints[AIndex + ACount], (FCount - AIndex) * SizeOf(TDoublePoint));
      Move(APoints[0], FPoints[AIndex], ACount * SizeOf(TDoublePoint));
      FMetaAggregator.InsertItems(AIndex, AMeta, ACount);
      Inc(FCount, ACount);
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
    FMetaAggregator.DeleteItems(AIndex, 1);
    Dec(FCount);
  end;
end;

procedure TDoublePointsAggregator.DeletePoints(const AIndex, ACount: Integer);
begin
  Assert((AIndex >= 0) or (AIndex + ACount < FCount));
  if (AIndex < 0) or (AIndex + ACount >= FCount) then begin
    Assert(False);
  end else if AIndex + ACount = FCount then begin
    Dec(FCount, ACount);
  end else begin
    Move(FPoints[AIndex + ACount], FPoints[AIndex], (FCount - AIndex - ACount) * SizeOf(TDoublePoint));
    FMetaAggregator.DeleteItems(AIndex, ACount);
    Dec(FCount, ACount);
  end;
end;

procedure TDoublePointsAggregator.Update(
  const AIndex: Integer;
  const APoint: TDoublePoint;
  const AMetaItem: PDoublePointsMetaItem
);
begin
  Assert((AIndex >= 0) and (AIndex < FCount));
  FPoints[AIndex] := APoint;
  FMetaAggregator.UpdateItem(AIndex, AMetaItem);
end;

procedure TDoublePointsAggregator.Clear;
begin
  FCount := 0;
end;

function TDoublePointsAggregator.GetCount: Integer;
begin
  Result := FCount;
end;

function TDoublePointsAggregator.GetMeta: PDoublePointsMeta;
begin
  Result := FMetaAggregator.Meta;
end;

function TDoublePointsAggregator.GetPoints: PDoublePointArray;
begin
  Result := FPoints;
end;

function TDoublePointsAggregator.MakeStaticAndClear: IDoublePoints;
begin
  Result := nil;
  if FCount > 0 then begin
    Result := TDoublePoints.Create(FPoints, FMetaAggregator.Meta, FCount);
    FCount := 0;
  end;
end;

function TDoublePointsAggregator.MakeStaticCopy: IDoublePoints;
begin
  Result := nil;
  if FCount > 0 then begin
    Result := TDoublePoints.Create(FPoints, FMetaAggregator.Meta, FCount);
  end;
end;

end.
