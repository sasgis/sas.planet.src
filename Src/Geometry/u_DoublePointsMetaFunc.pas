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

unit u_DoublePointsMetaFunc;

interface

uses
  t_GeoTypes,
  t_Hash,
  i_HashFunction;

function CopyMeta(
  const AMeta: PDoublePointsMeta;
  const ACount: Integer;
  const AStartIndex: Integer = 0
): PDoublePointsMeta;

procedure SliceMeta(
  const ASlice: PDoublePointsMeta;
  const AMeta: PDoublePointsMeta;
  const AStartIndex: Integer
);

procedure UpdateHashByMeta(
  var AHash: THashValue;
  const AHashFunc: IHashFunction;
  const AMeta: PDoublePointsMeta;
  const ACount: Integer
);

function IsSameMeta(
  const A, B: PDoublePointsMeta;
  const ACount: Integer
): Boolean;

function CreateMeta: PDoublePointsMeta;
procedure FreeAndNilMeta(var AMeta: PDoublePointsMeta);

procedure DeleteMetaItems(
  const AMeta: PDoublePointsMeta;
  const AItemsId: TDoublePointsMetaItemIds
);

procedure EraseMetaItems(
  const AMeta: PDoublePointsMeta;
  const ACount: Integer;
  const AItemsId: TDoublePointsMetaItemIds
);

procedure ResetMetaItem(const AItem: PDoublePointsMetaItem); inline;

procedure SetMetaItem(
  const AMeta: PDoublePointsMeta;
  const APointIndex: Integer;
  const AItem: PDoublePointsMetaItem
);

procedure SwapMetaItems(
  const AMeta: PDoublePointsMeta;
  const AIndexA: Integer;
  const AIndexB: Integer
); inline;

implementation

uses
  SysUtils;

function CreateMeta: PDoublePointsMeta;
begin
  GetMem(Result, SizeOf(TDoublePointsMeta));

  Result.Elevation := nil;
  Result.TimeStamp := nil;
end;

procedure FreeAndNilMeta(var AMeta: PDoublePointsMeta);
begin
  if AMeta = nil then begin
    Exit;
  end;

  FreeMem(AMeta.Elevation);
  FreeMem(AMeta.TimeStamp);

  FreeMem(AMeta);
  AMeta := nil;
end;

procedure DeleteMetaItems(
  const AMeta: PDoublePointsMeta;
  const AItemsId: TDoublePointsMetaItemIds
);
begin
  Assert(AMeta <> nil);

  if (AMeta.Elevation <> nil) and (miElevation in AItemsId) then begin
    FreeMem(AMeta.Elevation);
    AMeta.Elevation := nil;
  end;

  if (AMeta.TimeStamp <> nil) and (miTimeStamp in AItemsId) then begin
    FreeMem(AMeta.TimeStamp);
    AMeta.TimeStamp := nil;
  end;
end;

procedure EraseMetaItems(
  const AMeta: PDoublePointsMeta;
  const ACount: Integer;
  const AItemsId: TDoublePointsMetaItemIds
);
begin
  Assert(AMeta <> nil);

  if (AMeta.Elevation <> nil) and (miElevation in AItemsId) then begin
    FillChar(AMeta.Elevation[0], ACount * SizeOf(AMeta.Elevation[0]), 0);
  end;

  if (AMeta.TimeStamp <> nil) and (miTimeStamp in AItemsId) then begin
    FillChar(AMeta.TimeStamp[0], ACount * SizeOf(AMeta.TimeStamp[0]), 0);
  end;
end;

function CopyMeta(
  const AMeta: PDoublePointsMeta;
  const ACount: Integer;
  const AStartIndex: Integer
): PDoublePointsMeta;
var
  VSize: Integer;
begin
  Assert(AMeta <> nil);
  Assert(ACount > 0);

  Result := CreateMeta;

  if AMeta.Elevation <> nil then begin
    VSize := ACount * SizeOf(AMeta.Elevation[0]);
    GetMem(Result.Elevation, VSize);
    Move(AMeta.Elevation[AStartIndex], Result.Elevation[0], VSize);
  end;

  if AMeta.TimeStamp <> nil then begin
    VSize := ACount * SizeOf(AMeta.TimeStamp[0]);
    GetMem(Result.TimeStamp, VSize);
    Move(AMeta.TimeStamp[AStartIndex], Result.TimeStamp[0], VSize);
  end;
end;

procedure SliceMeta(
  const ASlice: PDoublePointsMeta;
  const AMeta: PDoublePointsMeta;
  const AStartIndex: Integer
);
begin
  Assert(ASlice <> nil);

  ASlice.Elevation := nil;
  ASlice.TimeStamp := nil;

  if AMeta = nil then begin
    Exit;
  end;

  if AMeta.Elevation <> nil then begin
    ASlice.Elevation := @AMeta.Elevation[AStartIndex];
  end;

  if AMeta.TimeStamp <> nil then begin
    ASlice.TimeStamp := @AMeta.TimeStamp[AStartIndex];
  end;
end;

procedure UpdateHashByMeta(
  var AHash: THashValue;
  const AHashFunc: IHashFunction;
  const AMeta: PDoublePointsMeta;
  const ACount: Integer
);
begin
  if AMeta = nil then begin
    Exit;
  end;
  if AMeta.Elevation <> nil then begin
    AHashFunc.UpdateHashByBuffer(AHash, AMeta.Elevation, ACount * SizeOf(AMeta.Elevation[0]));
  end;
  if AMeta.TimeStamp <> nil then begin
    AHashFunc.UpdateHashByBuffer(AHash, AMeta.TimeStamp, ACount * SizeOf(AMeta.TimeStamp[0]));
  end;
end;

function IsSameMeta(
  const A, B: PDoublePointsMeta;
  const ACount: Integer
): Boolean;
begin
  if (A = nil) and (B = nil) then begin
    Result := True;
    Exit;
  end else
  if (A <> nil) and (B <> nil) then begin
    // ok
  end else begin
    Result := False;
    Exit;
  end;

  if (A.Elevation <> nil) and (B.Elevation <> nil) then begin
    Result := CompareMem(A.Elevation, B.Elevation, ACount * SizeOf(A.Elevation[0]));
  end else
  if (A.Elevation = nil) and (B.Elevation = nil) then begin
    Result := True;
  end else begin
    Result := False;
  end;

  if not Result then begin
    Exit;
  end;

  if (A.TimeStamp <> nil) and (B.TimeStamp <> nil) then begin
    Result := CompareMem(A.TimeStamp, B.TimeStamp, ACount * SizeOf(A.TimeStamp[0]));
  end else
  if (A.TimeStamp = nil) and (B.TimeStamp = nil) then begin
    Result := True;
  end else begin
    Result := False;
  end;
end;

procedure ResetMetaItem(const AItem: PDoublePointsMetaItem);
begin
  with AItem^ do begin
    IsElevationOk := False;
    IsTimeStampOk := False;
  end;
end;

procedure SetMetaItem(
  const AMeta: PDoublePointsMeta;
  const APointIndex: Integer;
  const AItem: PDoublePointsMetaItem
);
begin
  if AMeta <> nil then begin
    AItem.IsElevationOk := AMeta.Elevation <> nil;
    if AItem.IsElevationOk then begin
      AItem.Elevation := AMeta.Elevation[APointIndex];
    end;

    AItem.IsTimeStampOk := AMeta.TimeStamp <> nil;
    if AItem.IsTimeStampOk then begin
      AItem.TimeStamp := AMeta.TimeStamp[APointIndex];
    end;
  end else begin
    ResetMetaItem(AItem);
  end;
end;

procedure SwapMetaItems(
  const AMeta: PDoublePointsMeta;
  const AIndexA: Integer;
  const AIndexB: Integer
);
var
  VElevation: Double;
  VTimeStamp: TDateTime;
begin
  Assert(AMeta <> nil);

  if AMeta.Elevation <> nil then begin
    VElevation := AMeta.Elevation[AIndexA];
    AMeta.Elevation[AIndexA] := AMeta.Elevation[AIndexB];
    AMeta.Elevation[AIndexB] := VElevation;
  end;

  if AMeta.TimeStamp <> nil then begin
    VTimeStamp := AMeta.TimeStamp[AIndexA];
    AMeta.TimeStamp[AIndexA] := AMeta.TimeStamp[AIndexB];
    AMeta.TimeStamp[AIndexB] := VTimeStamp;
  end;
end;

end.
