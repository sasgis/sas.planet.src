{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
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

unit u_DoublePointsMetaBuilder;

interface

uses
  t_GeoTypes,
  i_DoublePointsMeta;

type
  TDoublePointsMetaBuilder = class
  private
    FEle: array of Double;
    FTime: array of TDateTime;
    FCount: Integer;
  public
    procedure Add(const AMeta: PDoublePointsMeta; const ACount: Integer);

    procedure AddSeparationPoint; inline;
    procedure AddEmptyPoints(const ACount: Integer); inline;

    function Build: IDoublePointsMeta;
  end;

implementation

uses
  u_DoublePointsMeta;

{ TDoublePointsMetaBuilder }

procedure TDoublePointsMetaBuilder.Add(
  const AMeta: PDoublePointsMeta;
  const ACount: Integer
);
var
  I: Integer;
begin
  Assert(AMeta <> nil);

  I := FCount;

  if AMeta.Elevation <> nil then begin
    SetLength(FEle, I + ACount);
    Move(AMeta.Elevation[0], FEle[I], ACount * SizeOf(FEle[0]));
  end;

  if AMeta.TimeStamp <> nil then begin
    SetLength(FTime, I + ACount);
    Move(AMeta.TimeStamp[0], FTime[I], ACount * SizeOf(FTime[0]));
  end;

  Inc(FCount, ACount);
end;

procedure TDoublePointsMetaBuilder.AddSeparationPoint;
begin
  Inc(FCount);
end;

procedure TDoublePointsMetaBuilder.AddEmptyPoints(const ACount: Integer);
begin
  Inc(FCount, ACount);
end;

function TDoublePointsMetaBuilder.Build: IDoublePointsMeta;
var
  VMeta: TDoublePointsMeta;
begin
  if FCount = 0 then begin
    Result := nil;
    Exit;
  end;

  if Length(FEle) > 0 then begin
    SetLength(FEle, FCount);
    VMeta.Elevation := @Fele[0];
  end else begin
    VMeta.Elevation := nil;
  end;

  if Length(FTime) > 0 then begin
    SetLength(FTime, FCount);
    VMeta.TimeStamp := @FTime[0];
  end else begin
    VMeta.TimeStamp := nil;
  end;

  Result := TDoublePointsMetaImpl.Create(@VMeta, FCount);
end;

end.
