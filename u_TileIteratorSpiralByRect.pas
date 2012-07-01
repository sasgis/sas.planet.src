{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_TileIteratorSpiralByRect;

interface

uses
  Types,
  i_TileIterator,
  u_TileIteratorByRect;

type
  TTileIteratorSpiralByRect = class(TTileIteratorByRectBase, ITileIterator)
  private
    FCenterPoint: TPoint;
    FMaxRadius: Integer;

    FEOI: Boolean;
    FCurrentRing: Integer;
    FIndexInRing: Integer;

    function CheckPoint(const APoint: TPoint): Boolean;
    class function GetMaxRing(
      const ACenterPoint: TPoint;
      const ARect: TRect
    ): Integer;
    class function GetTilesInRingCount(ARad: Integer): Integer;
    class function GetDeltaByRingAndIndex(
      ARad: Integer;
      AIndex: Integer
    ): TPoint;
  private
    function Next(out ATile: TPoint): Boolean;
    procedure Reset;
  public
    constructor CreateWithCenter(
      const ARect: TRect;
      const APoint: TPoint
    );
    constructor Create(const ARect: TRect);
  end;

implementation

{ TTileIteratorSpiralByRect }

constructor TTileIteratorSpiralByRect.CreateWithCenter(
  const ARect: TRect;
  const APoint: TPoint
);
begin
  inherited Create(ARect);

  FCenterPoint := APoint;

  FMaxRadius := GetMaxRing(FCenterPoint, TilesRect);

  Reset;
end;

function TTileIteratorSpiralByRect.CheckPoint(const APoint: TPoint): Boolean;
begin
  Result :=
    (APoint.X >= TilesRect.Left) and
    (APoint.Y >= TilesRect.Top) and
    (APoint.X < TilesRect.Right) and
    (APoint.Y < TilesRect.Bottom);
end;

constructor TTileIteratorSpiralByRect.Create(const ARect: TRect);
begin
  CreateWithCenter(ARect, Point((ARect.Left + ARect.Right) div 2, (ARect.Top + ARect.Bottom) div 2));
end;

class function TTileIteratorSpiralByRect.GetDeltaByRingAndIndex(ARad,
  AIndex: Integer): TPoint;
var
  VTilesInLine: Integer;
  VLineIndex: Integer;
  VIndexInLine: Integer;
begin
  Result := Point(0, 0);
  if ARad > 0 then begin
    VTilesInLine := ARad;
    VLineIndex := AIndex div (VTilesInLine);
    VIndexInLine := AIndex - VLineIndex * VTilesInLine;
    case VLineIndex of
      0: begin
        Result.X := 1 + VIndexInLine;
        Result.Y := -ARad;
      end;
      1: begin
        Result.X := ARad;
        Result.Y := 1 - ARad + VIndexInLine;
      end;
      2: begin
        Result.X := ARad;
        Result.Y := 1 + VIndexInLine;
      end;
      3: begin
        Result.X := ARad - 1 - VIndexInLine;
        Result.Y := ARad;
      end;
      4: begin
        Result.X := -1 - VIndexInLine;
        Result.Y := ARad;
      end;
      5: begin
        Result.X := -ARad;
        Result.Y := ARad - 1 - VIndexInLine;
      end;
      6: begin
        Result.X := -ARad;
        Result.Y := -1 - VIndexInLine;
      end;
      7: begin
        Result.X := 1 - ARad + VIndexInLine;
        Result.Y := -ARad;
      end;
    end;
  end;
end;

class function TTileIteratorSpiralByRect.GetMaxRing(
  const ACenterPoint: TPoint;
  const ARect: TRect
): Integer;
var
  VRad: Integer;
begin
  Result := ACenterPoint.x - ARect.Left;
  VRad := ARect.Right - ACenterPoint.x;
  if (VRad > Result) then begin
    Result := VRad;
  end;
  VRad := ACenterPoint.y - ARect.Top;
  if (VRad > Result) then begin
    Result := VRad;
  end;
  VRad := ARect.Bottom - ACenterPoint.y;
  if (VRad > Result) then begin
    Result := VRad;
  end;
end;

class function TTileIteratorSpiralByRect.GetTilesInRingCount(ARad: Integer): Integer;
begin
  Result := 0;
  if ARad = 0 then begin
    Result := 1;
  end else if ARad > 0 then begin
    Result := (ARad * 2) * 4;
  end;
end;

function TTileIteratorSpiralByRect.Next(out ATile: TPoint): Boolean;
var
  VDelta: TPoint;
begin
  Result := False;
  while (not FEOI) and (not Result) do begin
    VDelta := GetDeltaByRingAndIndex(FCurrentRing, FIndexInRing);
    ATile.X := FCenterPoint.X + VDelta.X;
    ATile.Y := FCenterPoint.Y + VDelta.Y;
    Inc(FIndexInRing);
    if FIndexInRing >= GetTilesInRingCount(FCurrentRing) then begin
      Inc(FCurrentRing);
      FIndexInRing := 0;
      if FCurrentRing > FMaxRadius then begin
        FEOI := True;
      end;
    end;
    Result := CheckPoint(ATile);
  end;
end;

procedure TTileIteratorSpiralByRect.Reset;
begin
  FEOI := IsRectEmpty(TilesRect);
  FCurrentRing := 0;
  FIndexInRing := 0;
end;

end.
