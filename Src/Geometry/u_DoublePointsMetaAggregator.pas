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

unit u_DoublePointsMetaAggregator;

interface

uses
  t_GeoTypes;

type
  /// Attention: This class is designed to be used only in conjunction
  /// with TDoublePointsAggregator!
  TDoublePointsMetaAggregator = class
  private
    FMeta: TDoublePointsMeta;

    FElevation: array of Double;
    FTimeStamp: array of TDateTime;

    FPointsCountPtr: PInteger;
    FPointsCapacityPtr: PInteger;

    procedure Grow(
      const AIsElevationOk: Boolean;
      const AIsTimeStampOk: Boolean
    );

    procedure MoveRight(
      const AIndex: Integer;
      const ACount: Integer;
      const AIsElevationOk: Boolean;
      const AIsTimeStampOk: Boolean
    );
  public
    procedure AddItem(
      const AItem: PDoublePointsMetaItem
    );
    procedure AddItems(
      const AItems: PDoublePointsMeta;
      const ACount: Integer
    );

    procedure InsertItem(
      const AIndex: Integer;
      const AItem: PDoublePointsMetaItem
    );
    procedure InsertItems(
      const AIndex: Integer;
      const AItems: PDoublePointsMeta;
      const ACount: Integer
    );

    procedure DeleteItems(
      const AIndex: Integer;
      const ACount: Integer
    );

    procedure UpdateItem(
      const AIndex: Integer;
      const AItem: PDoublePointsMetaItem
    );

    function GetMeta: PDoublePointsMeta;
    property Meta: PDoublePointsMeta read GetMeta;

    constructor Create(
      const ACount: PInteger;
      const ACapacity: PInteger
    );
  end;

implementation

{ TDoublePointsMetaAggregator }

constructor TDoublePointsMetaAggregator.Create(
  const ACount: PInteger;
  const ACapacity: PInteger
);
begin
  Assert(ACount <> nil);
  Assert(ACapacity <> nil);

  inherited Create;

  FPointsCountPtr := ACount;
  FPointsCapacityPtr := ACapacity;
end;

function TDoublePointsMetaAggregator.GetMeta: PDoublePointsMeta;
begin
  FMeta.Elevation := @FElevation[0];
  FMeta.TimeStamp := @FTimeStamp[0];

  Result := @FMeta;
end;

procedure TDoublePointsMetaAggregator.MoveRight(
  const AIndex, ACount: Integer;
  const AIsElevationOk, AIsTimeStampOk: Boolean
);

  procedure MoveRightArrayOfDouble(A: PArrayOfDouble; const AIsOk: Boolean);
  begin
    Move(A[AIndex], A[AIndex + ACount], (FPointsCountPtr^ - AIndex) * SizeOf(A[0]));
    if not AIsOk then begin
      FillChar(A[AIndex], ACount * SizeOf(Double), 0);
    end;
  end;

begin
  if AIsElevationOk or (Length(FElevation) > 0) then begin
    MoveRightArrayOfDouble(@FElevation[0], AIsElevationOk);
  end;

  if AIsTimeStampOk or (Length(FTimeStamp) > 0) then begin
    MoveRightArrayOfDouble(@FTimeStamp[0], AIsTimeStampOk);
  end;
end;

procedure TDoublePointsMetaAggregator.Grow(
  const AIsElevationOk: Boolean;
  const AIsTimeStampOk: Boolean
);
var
  VNewCapacity: Integer;
begin
  VNewCapacity := FPointsCapacityPtr^;

  if AIsElevationOk or (Length(FElevation) > 0) then begin
    if Length(FElevation) < VNewCapacity then begin
      SetLength(FElevation, VNewCapacity);
    end;
  end;

  if AIsTimeStampOk or (Length(FTimeStamp) > 0) then begin
    if Length(FTimeStamp) < VNewCapacity then begin
      SetLength(FTimeStamp, VNewCapacity);
    end;
  end;
end;

procedure TDoublePointsMetaAggregator.AddItem(
  const AItem: PDoublePointsMetaItem
);
var
  VIsElevationOk: Boolean;
  VIsTimeStampOk: Boolean;
begin
  VIsElevationOk := (AItem <> nil) and AItem.IsElevationOk;
  VIsTimeStampOk := (AItem <> nil) and AItem.IsTimeStampOk;

  Grow(VIsElevationOk, VIsTimeStampOk);

  if VIsElevationOk then begin
    FElevation[FPointsCountPtr^] := AItem.Elevation;
  end else
  if Length(FElevation) > 0 then begin
    FElevation[FPointsCountPtr^] := 0;
  end;

  if VIsTimeStampOk then begin
    FTimeStamp[FPointsCountPtr^] := AItem.TimeStamp;
  end else
  if Length(FTimeStamp) > 0 then begin
    FTimeStamp[FPointsCountPtr^] := 0;
  end;
end;

procedure TDoublePointsMetaAggregator.AddItems(
  const AItems: PDoublePointsMeta;
  const ACount: Integer
);

  procedure FillZeroArrayOfDouble(A: PArrayOfDouble);
  begin
    FillChar(A[FPointsCountPtr^], ACount * SizeOf(Double), 0);
  end;

var
  VIsElevationOk: Boolean;
  VIsTimeStampOk: Boolean;
begin
  VIsElevationOk := (AItems <> nil) and (AItems.Elevation <> nil);
  VIsTimeStampOk := (AItems <> nil) and (AItems.TimeStamp <> nil);

  Grow(VIsElevationOk, VIsTimeStampOk);

  if VIsElevationOk then begin
    Move(AItems.Elevation[0], FElevation[FPointsCountPtr^], ACount * SizeOf(FElevation[0]));
  end else
  if Length(FElevation) > 0 then begin
    FillZeroArrayOfDouble(@FElevation[0]);
  end;

  if VIsTimeStampOk then begin
    Move(AItems.TimeStamp[0], FTimeStamp[FPointsCountPtr^], ACount * SizeOf(FTimeStamp[0]));
  end else
  if Length(FTimeStamp) > 0 then begin
    FillZeroArrayOfDouble(@FTimeStamp[0]);
  end;
end;

procedure TDoublePointsMetaAggregator.InsertItem(
  const AIndex: Integer;
  const AItem: PDoublePointsMetaItem
);
var
  VIsElevationOk: Boolean;
  VIsTimeStampOk: Boolean;
begin
  VIsElevationOk := (AItem <> nil) and AItem.IsElevationOk;
  VIsTimeStampOk := (AItem <> nil) and AItem.IsTimeStampOk;

  Grow(VIsElevationOk, VIsTimeStampOk);
  MoveRight(AIndex, 1, VIsElevationOk, VIsTimeStampOk);

  if VIsElevationOk then begin
    FElevation[AIndex] := AItem.Elevation;
  end;

  if VIsTimeStampOk then begin
    FTimeStamp[AIndex] := AItem.TimeStamp;
  end;
end;

procedure TDoublePointsMetaAggregator.InsertItems(
  const AIndex: Integer;
  const AItems: PDoublePointsMeta;
  const ACount: Integer
);
var
  VIsElevationOk: Boolean;
  VIsTimeStampOk: Boolean;
begin
  VIsElevationOk := (AItems <> nil) and (AItems.Elevation <> nil);
  VIsTimeStampOk := (AItems <> nil) and (AItems.TimeStamp <> nil);

  Grow(VIsElevationOk, VIsTimeStampOk);
  MoveRight(AIndex, ACount, VIsElevationOk, VIsTimeStampOk);

  if VIsElevationOk then begin
    Move(AItems.Elevation[0], FElevation[AIndex], ACount * SizeOf(FElevation[0]));
  end;

  if VIsTimeStampOk then begin
    Move(AItems.TimeStamp[0], FTimeStamp[AIndex], ACount * SizeOf(FTimeStamp[0]));
  end;
end;

procedure TDoublePointsMetaAggregator.DeleteItems(
  const AIndex: Integer;
  const ACount: Integer
);

  procedure MoveLeftArrayOfDouble(A: PArrayOfDouble);
  begin
    Move(A[AIndex + ACount], A[AIndex], (FPointsCountPtr^ - AIndex - ACount) * SizeOf(Double));
  end;

begin
  if Length(FElevation) > 0 then begin
    MoveLeftArrayOfDouble(@FElevation[0]);
  end;

  if Length(FTimeStamp) > 0 then begin
    MoveLeftArrayOfDouble(@FTimeStamp[0]);
  end;
end;

procedure TDoublePointsMetaAggregator.UpdateItem(
  const AIndex: Integer;
  const AItem: PDoublePointsMetaItem
);
var
  VIsElevationOk: Boolean;
  VIsTimeStampOk: Boolean;
begin
  VIsElevationOk := (AItem <> nil) and AItem.IsElevationOk;
  VIsTimeStampOk := (AItem <> nil) and AItem.IsTimeStampOk;

  Grow(VIsElevationOk, VIsTimeStampOk);

  if VIsElevationOk then begin
    FElevation[AIndex] := AItem.Elevation;
  end else
  if Length(FElevation) > 0 then begin
    FElevation[AIndex] := 0;
  end;

  if VIsTimeStampOk then begin
    FTimeStamp[AIndex] := AItem.TimeStamp;
  end else
  if Length(FTimeStamp) > 0 then begin
    FTimeStamp[AIndex] := 0;
  end;
end;

end.
