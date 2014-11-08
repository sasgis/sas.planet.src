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

unit u_LonLatRectByPoint;

interface

uses
  t_GeoTypes,
  i_LonLatRect,
  u_BaseInterfacedObject;

type
  TLonLatRectByPoint = class(TBaseInterfacedObject, ILonLatRect)
  private
    FPoint: TDoublePoint;
  private
    function GetLeft: Double;
    function GetTop: Double;
    function GetRight: Double;
    function GetBottom: Double;

    function GetTopLeft: TDoublePoint;
    function GetBottomRight: TDoublePoint;

    function GetRect: TDoubleRect;
    function CalcRectCenter: TDoublePoint;
    function IsEqual(const ARect: TDoubleRect): Boolean; overload;
    function IsEqual(const ARect: ILonLatRect): Boolean; overload;
    function IsPointInRect(const APoint: TDoublePoint): Boolean;
    function UnionWithRect(const ARect: TDoubleRect): TDoubleRect; overload;
    function UnionWithRect(const ARect: ILonLatRect): TDoubleRect; overload;
    function IntersecWithRect(
      out AResultRect: TDoubleRect;
      const ARect: TDoubleRect
    ): Boolean; overload;
    function IntersecWithRect(
      out AResultRect: TDoubleRect;
      const ARect: ILonLatRect
    ): Boolean; overload;
    function IsIntersecWithRect(const ARect: TDoubleRect): Boolean; overload;
    function IsIntersecWithRect(const ARect: ILonLatRect): Boolean; overload;
    function IsContainRect(const ARect: ILonLatRect): Boolean;
  public
    constructor Create(const APoint: TDoublePoint);
  end;

implementation

uses
  Math,
  u_GeoFunc;

{ TLonLatRectByPoint }

function TLonLatRectByPoint.CalcRectCenter: TDoublePoint;
begin
  Result := FPoint;
end;

constructor TLonLatRectByPoint.Create(const APoint: TDoublePoint);
begin
  inherited Create;
  Assert(not IsNan(APoint.X));
  Assert(not IsNan(APoint.Y));
  FPoint := APoint;
end;

function TLonLatRectByPoint.GetBottom: Double;
begin
  Result := FPoint.Y;
end;

function TLonLatRectByPoint.GetBottomRight: TDoublePoint;
begin
  Result := FPoint;
end;

function TLonLatRectByPoint.GetLeft: Double;
begin
  Result := FPoint.X;
end;

function TLonLatRectByPoint.GetRect: TDoubleRect;
begin
  Result.TopLeft := FPoint;
  Result.BottomRight := FPoint;
end;

function TLonLatRectByPoint.GetRight: Double;
begin
  Result := FPoint.X;
end;

function TLonLatRectByPoint.GetTop: Double;
begin
  Result := FPoint.Y;
end;

function TLonLatRectByPoint.GetTopLeft: TDoublePoint;
begin
  Result := FPoint;
end;

function TLonLatRectByPoint.IntersecWithRect(
  out AResultRect: TDoubleRect;
  const ARect: ILonLatRect
): Boolean;
begin
  Result := ARect.IsPointInRect(FPoint);
  if Result then begin
    AResultRect.TopLeft := FPoint;
    AResultRect.BottomRight := FPoint;
  end else begin
    FillChar(AResultRect, SizeOf(AResultRect), 0);
  end;
end;

function TLonLatRectByPoint.IntersecWithRect(
  out AResultRect: TDoubleRect;
  const ARect: TDoubleRect
): Boolean;
begin
  Result :=
    (FPoint.X <= ARect.Right) and
    (FPoint.X >= ARect.Left) and
    (FPoint.Y <= ARect.Top) and
    (FPoint.Y >= ARect.Bottom);
  if Result then begin
    AResultRect.TopLeft := FPoint;
    AResultRect.BottomRight := FPoint;
  end else begin
    FillChar(AResultRect, SizeOf(AResultRect), 0);
  end;
end;

function TLonLatRectByPoint.IsEqual(const ARect: ILonLatRect): Boolean;
var
  VRect: TDoubleRect;
begin
  if ARect = nil then begin
    Result := False;
  end else if ILonLatRect(Self) = ARect then begin
    Result := True;
  end else begin
    VRect := ARect.Rect;
    Result :=
      DoublePointsEqual(FPoint, VRect.TopLeft) and
      DoublePointsEqual(FPoint, VRect.BottomRight);
  end;
end;

function TLonLatRectByPoint.IsEqual(const ARect: TDoubleRect): Boolean;
begin
  Result :=
    DoublePointsEqual(FPoint, ARect.TopLeft) and
    DoublePointsEqual(FPoint, ARect.BottomRight);
end;

function TLonLatRectByPoint.IsIntersecWithRect(
  const ARect: TDoubleRect): Boolean;
begin
  Result :=
    (FPoint.X <= ARect.Right) and
    (FPoint.X >= ARect.Left) and
    (FPoint.Y <= ARect.Top) and
    (FPoint.Y >= ARect.Bottom);
end;

function TLonLatRectByPoint.IsIntersecWithRect(
  const ARect: ILonLatRect): Boolean;
begin
  if ARect = nil then begin
    Result := False;
  end else if ILonLatRect(Self) = ARect then begin
    Result := True;
  end else begin
    Result := ARect.IsPointInRect(FPoint);
  end;
end;

function TLonLatRectByPoint.IsPointInRect(const APoint: TDoublePoint): Boolean;
begin
  Result := DoublePointsEqual(FPoint, APoint);
end;

function TLonLatRectByPoint.UnionWithRect(
  const ARect: ILonLatRect): TDoubleRect;
begin
  if ARect = nil then begin
    Result.TopLeft := FPoint;
    Result.BottomRight := FPoint;
  end else if ILonLatRect(Self) = ARect then begin
    Result.TopLeft := FPoint;
    Result.BottomRight := FPoint;
  end else begin
    Result := ARect.Rect;
    if Result.Left > FPoint.X then begin
      Result.Left := FPoint.X;
    end;
    if Result.Right < FPoint.X then begin
      Result.Right := FPoint.X;
    end;
    if Result.Top < FPoint.Y then begin
      Result.Top := FPoint.Y;
    end;
    if Result.Bottom > FPoint.Y then begin
      Result.Bottom := FPoint.Y;
    end;
  end;
end;

function TLonLatRectByPoint.UnionWithRect(
  const ARect: TDoubleRect): TDoubleRect;
begin
  Result := ARect;
  if Result.Left > FPoint.X then begin
    Result.Left := FPoint.X;
  end;
  if Result.Right < FPoint.X then begin
    Result.Right := FPoint.X;
  end;
  if Result.Top < FPoint.Y then begin
    Result.Top := FPoint.Y;
  end;
  if Result.Bottom > FPoint.Y then begin
    Result.Bottom := FPoint.Y;
  end;
end;

function TLonLatRectByPoint.IsContainRect(const ARect: ILonLatRect): Boolean;
begin
  Result := IsEqual(ARect);
end;

end.
