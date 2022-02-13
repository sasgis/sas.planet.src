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
    type
      TMetaRecInternal = record
        Elevation: array of Double;
        TimeStamp: array of TDateTime;
      end;
  private
    FMeta: TMetaRecInternal;
    FMetaPtr: PDoublePointsMeta;

    FPointsCountPtr: PInteger;
    FPointsCapacityPtr: PInteger;

    procedure Grow(
      const ACount: Integer;
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
    destructor Destroy; override;
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

  FMetaPtr := nil;
end;

destructor TDoublePointsMetaAggregator.Destroy;
begin
  if FMetaPtr <> nil then begin
    Dispose(FMetaPtr);
    FMetaPtr := nil;
  end;

  inherited Destroy;
end;

function TDoublePointsMetaAggregator.GetMeta: PDoublePointsMeta;

  procedure _LazyAlloc;
  begin
    if FMetaPtr = nil then begin
      New(FMetaPtr);
      FMetaPtr.Elevation := nil;
      FMetaPtr.TimeStamp := nil;
    end;
  end;

begin
  if Length(FMeta.Elevation) > 0 then begin
    _LazyAlloc;
    FMetaPtr.Elevation := @FMeta.Elevation[0];
  end;

  if Length(FMeta.TimeStamp) > 0 then begin
    _LazyAlloc;
    FMetaPtr.TimeStamp := @FMeta.TimeStamp[0];
  end;

  Result := FMetaPtr;
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
  if AIsElevationOk or (Length(FMeta.Elevation) > 0) then begin
    MoveRightArrayOfDouble(@FMeta.Elevation[0], AIsElevationOk);
  end;

  if AIsTimeStampOk or (Length(FMeta.TimeStamp) > 0) then begin
    MoveRightArrayOfDouble(@FMeta.TimeStamp[0], AIsTimeStampOk);
  end;
end;

procedure TDoublePointsMetaAggregator.Grow(
  const ACount: Integer;
  const AIsElevationOk: Boolean;
  const AIsTimeStampOk: Boolean
);
var
  VNewLen: Integer;
  VNewCapacity: Integer;
begin
  VNewLen := FPointsCountPtr^ + ACount;
  VNewCapacity := FPointsCapacityPtr^;

  if AIsElevationOk or (Length(FMeta.Elevation) > 0) then begin
    if Length(FMeta.Elevation) < VNewLen then begin
      SetLength(FMeta.Elevation, VNewCapacity);
    end;
  end;

  if AIsTimeStampOk or (Length(FMeta.TimeStamp) > 0) then begin
    if Length(FMeta.TimeStamp) < VNewLen then begin
      SetLength(FMeta.TimeStamp, VNewCapacity);
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

  Grow(1, VIsElevationOk, VIsTimeStampOk);

  if VIsElevationOk then begin
    FMeta.Elevation[FPointsCountPtr^] := AItem.Elevation;
  end else
  if Length(FMeta.Elevation) > 0 then begin
    FMeta.Elevation[FPointsCountPtr^] := 0;
  end;

  if VIsTimeStampOk then begin
    FMeta.TimeStamp[FPointsCountPtr^] := AItem.TimeStamp;
  end else
  if Length(FMeta.TimeStamp) > 0 then begin
    FMeta.TimeStamp[FPointsCountPtr^] := 0;
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

  Grow(ACount, VIsElevationOk, VIsTimeStampOk);

  if VIsElevationOk then begin
    Move(AItems.Elevation[0], FMeta.Elevation[FPointsCountPtr^], ACount * SizeOf(FMeta.Elevation[0]));
  end else
  if Length(FMeta.Elevation) > 0 then begin
    FillZeroArrayOfDouble(@FMeta.Elevation[0]);
  end;

  if VIsTimeStampOk then begin
    Move(AItems.TimeStamp[0], FMeta.TimeStamp[FPointsCountPtr^], ACount * SizeOf(FMeta.TimeStamp[0]));
  end else
  if Length(FMeta.TimeStamp) > 0 then begin
    FillZeroArrayOfDouble(@FMeta.TimeStamp[0]);
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

  Grow(1, VIsElevationOk, VIsTimeStampOk);
  MoveRight(AIndex, 1, VIsElevationOk, VIsTimeStampOk);

  if VIsElevationOk then begin
    FMeta.Elevation[AIndex] := AItem.Elevation;
  end;

  if VIsTimeStampOk then begin
    FMeta.TimeStamp[AIndex] := AItem.TimeStamp;
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

  Grow(ACount, VIsElevationOk, VIsTimeStampOk);
  MoveRight(AIndex, ACount, VIsElevationOk, VIsTimeStampOk);

  if VIsElevationOk then begin
    Move(AItems.Elevation[0], FMeta.Elevation[AIndex], ACount * SizeOf(FMeta.Elevation[0]));
  end;

  if VIsTimeStampOk then begin
    Move(AItems.TimeStamp[0], FMeta.TimeStamp[AIndex], ACount * SizeOf(FMeta.TimeStamp[0]));
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
  if Length(FMeta.Elevation) > 0 then begin
    MoveLeftArrayOfDouble(@FMeta.Elevation[0]);
  end;

  if Length(FMeta.TimeStamp) > 0 then begin
    MoveLeftArrayOfDouble(@FMeta.TimeStamp[0]);
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

  if VIsElevationOk then begin
    FMeta.Elevation[AIndex] := AItem.Elevation;
  end else
  if Length(FMeta.Elevation) > 0 then begin
    FMeta.Elevation[AIndex] := 0;
  end;

  if VIsTimeStampOk then begin
    FMeta.TimeStamp[AIndex] := AItem.TimeStamp;
  end else
  if Length(FMeta.TimeStamp) > 0 then begin
    FMeta.TimeStamp[AIndex] := 0;
  end;
end;

end.
