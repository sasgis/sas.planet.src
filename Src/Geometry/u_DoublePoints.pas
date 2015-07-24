{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2015, SAS.Planet development team.                      *}
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
  private
    function GetCount: Integer;
    function GetPoints: PDoublePointArray;
  public
    constructor CreateWithOwn(
      const APoints: PDoublePointArray;
      const ACount: Integer
    );
    constructor Create(
      const APoints: PDoublePointArray;
      const ACount: Integer
    );
    destructor Destroy; override;
  end;

implementation

{ TDoublePoints }

constructor TDoublePoints.Create(
  const APoints: PDoublePointArray;
  const ACount: Integer
);
var
  VSize: Integer;
begin
  Assert(Assigned(APoints));
  Assert(ACount > 0);
  FCount := ACount;
  if FCount > 0 then begin
    VSize := FCount * SizeOf(TDoublePoint);
    GetMem(FPoints, VSize);
    Move(APoints[0], FPoints[0], VSize);
  end else begin
    FPoints := nil;
  end;
end;

constructor TDoublePoints.CreateWithOwn(
  const APoints: PDoublePointArray;
  const ACount: Integer
);
begin
  Assert(Assigned(APoints));
  Assert(ACount > 0);
  FCount := ACount;
  if FCount > 0 then begin
    FPoints := APoints;
  end else begin
    FPoints := nil;
  end;
end;

destructor TDoublePoints.Destroy;
begin
  FreeMem(FPoints);
  inherited;
end;

function TDoublePoints.GetCount: Integer;
begin
  Result := FCount;
end;

function TDoublePoints.GetPoints: PDoublePointArray;
begin
  Result := FPoints;
end;

end.
