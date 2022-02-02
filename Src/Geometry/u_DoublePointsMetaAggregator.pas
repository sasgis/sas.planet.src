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

    FCapacityInternal: Integer;

    procedure Grow(
      const ACount: Integer;
      const AIsElevationRequired: Boolean;
      const AIsTimeStampRequired: Boolean
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

  FCapacityInternal := 0;

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

procedure TDoublePointsMetaAggregator.MoveRight(const AIndex, ACount: Integer;
  const AIsElevationOk, AIsTimeStampOk: Boolean);
begin
  if AIsElevationOk then begin
    Move(
      FMeta.Elevation[AIndex],
      FMeta.Elevation[AIndex + ACount],
      (FPointsCountPtr^ - AIndex) * SizeOf(FMeta.Elevation[0])
    );
  end;

  if AIsTimeStampOk then begin
    Move(
      FMeta.TimeStamp[AIndex],
      FMeta.TimeStamp[AIndex + ACount],
      (FPointsCountPtr^ - AIndex) * SizeOf(FMeta.TimeStamp[0])
    );
  end;
end;

procedure TDoublePointsMetaAggregator.Grow(
  const ACount: Integer;
  const AIsElevationRequired: Boolean;
  const AIsTimeStampRequired: Boolean
);
var
  VNewCapacity: Integer;
begin
  if not AIsElevationRequired and
     not AIsTimeStampRequired then
  begin
    Exit;
  end;

  VNewCapacity := FPointsCapacityPtr^;

  if FCapacityInternal < FPointsCountPtr^ + ACount then begin
    if AIsElevationRequired or (Length(FMeta.Elevation) > 0) then begin
      SetLength(FMeta.Elevation, VNewCapacity);
    end;

    if AIsTimeStampRequired or (Length(FMeta.TimeStamp) > 0) then begin
      SetLength(FMeta.TimeStamp, VNewCapacity);
    end;

    FCapacityInternal := VNewCapacity;
  end;
end;

procedure TDoublePointsMetaAggregator.AddItem(
  const AItem: PDoublePointsMetaItem
);
begin
  if AItem = nil then begin
    Exit;
  end;

  Grow(1, AItem.IsElevationOk, AItem.IsTimeStampOk);

  if AItem.IsElevationOk then begin
    FMeta.Elevation[FPointsCountPtr^] := AItem.Elevation;
  end;

  if AItem.IsTimeStampOk then begin
    FMeta.TimeStamp[FPointsCountPtr^] := AItem.TimeStamp;
  end;
end;

procedure TDoublePointsMetaAggregator.AddItems(
  const AItems: PDoublePointsMeta;
  const ACount: Integer
);
var
  VIsElevationOk: Boolean;
  VIsTimeStampOk: Boolean;
begin
  if AItems = nil then begin
    Exit;
  end;

  VIsElevationOk := AItems.Elevation <> nil;
  VIsTimeStampOk := AItems.TimeStamp <> nil;

  Grow(ACount, VIsElevationOk, VIsTimeStampOk);

  if VIsElevationOk then begin
    Move(AItems.Elevation[0], FMeta.Elevation[FPointsCountPtr^], ACount * SizeOf(FMeta.Elevation[0]));
  end;

  if VIsTimeStampOk then begin
    Move(AItems.TimeStamp[0], FMeta.TimeStamp[FPointsCountPtr^], ACount * SizeOf(FMeta.TimeStamp[0]));
  end;
end;

procedure TDoublePointsMetaAggregator.InsertItem(
  const AIndex: Integer;
  const AItem: PDoublePointsMetaItem
);
begin
  if AItem = nil then begin
    Exit;
  end;

  Grow(1, AItem.IsElevationOk, AItem.IsTimeStampOk);
  MoveRight(AIndex, 1, AItem.IsElevationOk, AItem.IsTimeStampOk);

  if AItem.IsElevationOk then begin
    FMeta.Elevation[AIndex] := AItem.Elevation;
  end;

  if AItem.IsTimeStampOk then begin
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
  if AItems = nil then begin
    Exit;
  end;

  VIsElevationOk := AItems.Elevation <> nil;
  VIsTimeStampOk := AItems.TimeStamp <> nil;

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
begin
  if Length(FMeta.Elevation) > 0 then begin
    Move(
      FMeta.Elevation[AIndex + ACount],
      FMeta.Elevation[AIndex],
      (FPointsCountPtr^ - AIndex - ACount) * SizeOf(FMeta.Elevation[0])
    );
  end;

  if Length(FMeta.TimeStamp) > 0 then begin
    Move(
      FMeta.TimeStamp[AIndex + ACount],
      FMeta.TimeStamp[AIndex],
      (FPointsCountPtr^ - AIndex - ACount) * SizeOf(FMeta.TimeStamp[0])
    );
  end;
end;

end.
