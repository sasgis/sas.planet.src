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
  t_GeoTypes;

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

function CreateMeta: PDoublePointsMeta;
procedure FreeAndNilMeta(var AMeta: PDoublePointsMeta);

procedure ResetMetaItem(const AItem: PDoublePointsMetaItem); inline;

procedure SetMetaItem(
  const AMeta: PDoublePointsMeta;
  const APointIndex: Integer;
  const AItem: PDoublePointsMetaItem
);

implementation

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

  if AMeta <> nil then begin
    ASlice.Elevation := @AMeta.Elevation[AStartIndex];
    ASlice.TimeStamp := @AMeta.TimeStamp[AStartIndex];
  end else begin
    ASlice.Elevation := nil;
    ASlice.TimeStamp := nil;
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

end.
