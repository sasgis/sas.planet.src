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

unit u_TileIteratorSpiralByRect;

interface

uses
  Types,
  SysUtils,
  i_TileRect,
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
    procedure Seek(const APos: TPoint);
    function Clone: ITileIterator;
  private
    constructor CreateClone(
      const ARect: ITileRect;
      const ACurrentRing: Integer;
      const AIndexInRing: Integer;
      const AEOI: Boolean
    );
  public
    constructor CreateWithCenter(
      const ARect: ITileRect;
      const APoint: TPoint
    );
    constructor Create(const ARect: ITileRect);
  end;

implementation

{ TTileIteratorSpiralByRect }

constructor TTileIteratorSpiralByRect.CreateWithCenter(
  const ARect: ITileRect;
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

constructor TTileIteratorSpiralByRect.Create(const ARect: ITileRect);
begin
  CreateWithCenter(ARect, CenterPoint(ARect.Rect));
end;

constructor TTileIteratorSpiralByRect.CreateClone(
  const ARect: ITileRect;
  const ACurrentRing: Integer;
  const AIndexInRing: Integer;
  const AEOI: Boolean
);
begin
  Self.Create(ARect);
  FCurrentRing := ACurrentRing;
  FIndexInRing := AIndexInRing;
  FEOI := AEOI;
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

procedure TTileIteratorSpiralByRect.Seek(const APos: TPoint);
var
  VPoint: TPoint;
begin
  if PtInRect(TilesRect, APos) then begin
    FEOI := IsRectEmpty(TilesRect);
    //ToDo: Make this faster, if you can!
    if not FEOI then begin
      Reset;
      while Next(VPoint) do begin
        if (VPoint.X = APos.X) and (VPoint.Y = APos.Y) then begin
          Break;
        end;
      end;
    end;
  end else begin
    raise Exception.CreateFmt(
      'Point %d, %d not in Rect [%d, %d; %d, %d]',
      [APos.X, APos.Y, TilesRect.Left, TilesRect.Top, TilesRect.Right, TilesRect.Bottom]
    );
  end;
end;

function TTileIteratorSpiralByRect.Clone: ITileIterator;
begin
  Result :=
    TTileIteratorSpiralByRect.CreateClone(
      Self.GetTilesRect,
      FCurrentRing,
      FIndexInRing,
      FEOI
    );
end;

end.
