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

unit u_LonLatRect;

interface

uses
  t_GeoTypes,
  i_LonLatRect,
  u_BaseInterfacedObject;

type
  TLonLatRect = class(TBaseInterfacedObject, ILonLatRect)
  private
    FRect: TDoubleRect;
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
    constructor Create(const ARect: TDoubleRect);
  end;

implementation

uses
  Math,
  u_GeoFunc;

{ TLonLatRect }

function TLonLatRect.CalcRectCenter: TDoublePoint;
begin
  Result := RectCenter(FRect);
end;

constructor TLonLatRect.Create(const ARect: TDoubleRect);
begin
  inherited Create;
  Assert(not IsNan(ARect.Left));
  Assert(not IsNan(ARect.Top));
  Assert(not IsNan(ARect.Right));
  Assert(not IsNan(ARect.Bottom));
  FRect := ARect;
end;

function TLonLatRect.GetBottom: Double;
begin
  Result := FRect.Bottom;
end;

function TLonLatRect.GetBottomRight: TDoublePoint;
begin
  Result := FRect.BottomRight;
end;

function TLonLatRect.GetLeft: Double;
begin
  Result := FRect.Left;
end;

function TLonLatRect.GetRect: TDoubleRect;
begin
  Result := FRect;
end;

function TLonLatRect.GetRight: Double;
begin
  Result := FRect.Right;
end;

function TLonLatRect.GetTop: Double;
begin
  Result := FRect.Top;
end;

function TLonLatRect.GetTopLeft: TDoublePoint;
begin
  Result := FRect.TopLeft;
end;

function TLonLatRect.IntersecWithRect(
  out AResultRect: TDoubleRect;
  const ARect: ILonLatRect
): Boolean;
begin
  if ARect = nil then begin
    Result := False;
    FillChar(AResultRect, SizeOf(AResultRect), 0);
  end else if IsEqual(ARect) then begin
    AResultRect := FRect;
    Result := True;
  end else begin
    Result := IntersecLonLatRect(AResultRect, FRect, ARect.Rect);
  end;
end;

function TLonLatRect.IntersecWithRect(
  out AResultRect: TDoubleRect;
  const ARect: TDoubleRect
): Boolean;
begin
  Result := IntersecLonLatRect(AResultRect, FRect, ARect);
end;

function TLonLatRect.IsEqual(const ARect: ILonLatRect): Boolean;
begin
  if ARect = nil then begin
    Result := False;
  end else if ILonLatRect(Self) = ARect then begin
    Result := True;
  end else begin
    Result := DoubleRectsEqual(FRect, ARect.Rect);
  end;
end;

function TLonLatRect.IsEqual(const ARect: TDoubleRect): Boolean;
begin
  Result := DoubleRectsEqual(FRect, ARect);
end;

function TLonLatRect.IsIntersecWithRect(const ARect: TDoubleRect): Boolean;
begin
  Result :=
    (FRect.Left <= ARect.Right) and
    (FRect.Right >= ARect.Left) and
    (FRect.Top >= ARect.Bottom) and
    (FRect.Bottom <= ARect.Top);
end;

function TLonLatRect.IsIntersecWithRect(const ARect: ILonLatRect): Boolean;
begin
  if ARect = nil then begin
    Result := False;
  end else if IsEqual(ARect) then begin
    Result := True;
  end else begin
    Result := IsIntersecWithRect(ARect.Rect);
  end;
end;

function TLonLatRect.IsPointInRect(const APoint: TDoublePoint): Boolean;
begin
  Result :=
    (APoint.X <= FRect.Right) and
    (APoint.X >= FRect.Left) and
    (APoint.Y <= FRect.Top) and
    (APoint.Y >= FRect.Bottom);
end;

function TLonLatRect.UnionWithRect(const ARect: ILonLatRect): TDoubleRect;
begin
  if ARect = nil then begin
    Result := FRect;
  end else if IsEqual(ARect) then begin
    Result := FRect;
  end else begin
    Result := UnionLonLatRects(FRect, ARect.Rect);
  end;
end;

function TLonLatRect.UnionWithRect(const ARect: TDoubleRect): TDoubleRect;
begin
  Result := UnionLonLatRects(FRect, ARect);
end;

function TLonLatRect.IsContainRect(const ARect: ILonLatRect): Boolean;
begin
  Result :=
    (FRect.Left <= ARect.Left) and
    (FRect.Top >= ARect.Top) and
    (FRect.Right >= ARect.Right) and
    (FRect.Bottom <= ARect.Bottom);
end;

end.
