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
  i_DoublePointsAggregator,
  u_BaseInterfacedObject;

type
  TDoublePointsAggregator = class(TBaseInterfacedObject, IDoublePointsAggregator)
  private
    FPoints: array of TDoublePoint;
    FCount: Integer;
    FCapacity: Integer;
  private
    procedure Add(const APoint: TDoublePoint);
    procedure AddPoints(
      const APoints: PDoublePointArray;
      ACount: Integer
    );
    procedure Clear;

    function GetCount: Integer;
    function GetPoints: PDoublePointArray;
  public
    constructor Create(const ACapacity: Integer = 0);
    destructor Destroy; override;
  end;

implementation

{ TDoublePointsAggregator }

constructor TDoublePointsAggregator.Create(const ACapacity: Integer = 0);
begin
  Assert(ACapacity >= 0);
  inherited Create;
  FCount := 0;
  FCapacity := ACapacity;
  SetLength(FPoints, FCapacity);
end;

destructor TDoublePointsAggregator.Destroy;
begin
  FPoints := nil;
  FCount := 0;
  FCapacity := 0;
  inherited;
end;

procedure TDoublePointsAggregator.Add(const APoint: TDoublePoint);
var
  VSize: Integer;
begin
  if FCount >= FCapacity then begin
    if FCapacity < 256 then begin
      VSize := 256;
    end else if FCapacity < 4 * 1024 then begin
      VSize := FCapacity * 2;
    end else begin
      VSize := FCapacity + 4 * 1024;
    end;
    SetLength(FPoints, VSize);
    FCapacity := VSize;
  end;
  FPoints[FCount] := APoint;
  Inc(FCount);
end;

procedure TDoublePointsAggregator.AddPoints(
  const APoints: PDoublePointArray;
  ACount: Integer
);
var
  VNewCount: Integer;
  VSize: Integer;
begin
  if ACount > 0 then begin
    VNewCount := FCount + ACount;
    if VNewCount > FCapacity then begin
      VSize := FCapacity;
      while VNewCount > VSize do begin
        if VSize < 256 then begin
          VSize := 256;
        end else if VSize < 4 * 1024 then begin
          VSize := VSize * 2;
        end else begin
          VSize := VSize + 4 * 1024;
        end;
      end;
      SetLength(FPoints, VSize);
      FCapacity := VSize;
    end;
    Move(APoints[0], FPoints[FCount], ACount * SizeOf(TDoublePoint));
    FCount := VNewCount;
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
  Result := @FPoints[0];
end;

end.
