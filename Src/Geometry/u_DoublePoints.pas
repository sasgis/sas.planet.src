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

unit u_DoublePoints;

interface

uses
  t_GeoTypes,
  i_DoublePoints,
  u_BaseInterfacedObject;

type
  TDoublePoints = class(TBaseInterfacedObject, IDoublePoints)
  private
    FCount: Integer;
    FPoints: PDoublePointArray;
    FMeta: PDoublePointsMeta;
  private
    { IDoublePoints }
    function GetCount: Integer;
    function GetPoints: PDoublePointArray;
    function GetMeta: PDoublePointsMeta;
  public
    constructor CreateWithOwn(
      const APoints: PDoublePointArray;
      const AMeta: PDoublePointsMeta;
      const ACount: Integer
    );
    constructor Create(
      const APoints: PDoublePointArray;
      const AMeta: PDoublePointsMeta;
      const ACount: Integer
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_DoublePointsMeta;

{ TDoublePoints }

constructor TDoublePoints.Create(
  const APoints: PDoublePointArray;
  const AMeta: PDoublePointsMeta;
  const ACount: Integer
);
var
  VSize: Integer;
begin
  Assert(Assigned(APoints));
  Assert(ACount > 0);

  FCount := ACount;
  FPoints := nil;
  FMeta := nil;

  if FCount > 0 then begin
    VSize := FCount * SizeOf(TDoublePoint);
    GetMem(FPoints, VSize);
    Move(APoints[0], FPoints[0], VSize);

    if AMeta <> nil then begin
      FMeta := CopyMeta(AMeta, FCount);
    end;
  end;
end;

constructor TDoublePoints.CreateWithOwn(
  const APoints: PDoublePointArray;
  const AMeta: PDoublePointsMeta;
  const ACount: Integer
);
begin
  Assert(Assigned(APoints));
  Assert(ACount > 0);
  FCount := ACount;
  if FCount > 0 then begin
    FPoints := APoints;
    FMeta := AMeta;
  end else begin
    FPoints := nil;
    FMeta := nil;
  end;
end;

destructor TDoublePoints.Destroy;
begin
  FreeMem(FPoints);
  FreeAndNilMeta(FMeta);
  inherited Destroy;
end;

function TDoublePoints.GetCount: Integer;
begin
  Result := FCount;
end;

function TDoublePoints.GetPoints: PDoublePointArray;
begin
  Result := FPoints;
end;

function TDoublePoints.GetMeta: PDoublePointsMeta;
begin
  Result := FMeta;
end;

end.
