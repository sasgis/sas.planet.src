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

unit u_TileRect;

interface

uses
  Types,
  i_TileRect,
  u_BaseInterfacedObject;

type
  TTileRect = class(TBaseInterfacedObject, ITileRect)
  private
    FRect: TRect;
    FZoom: Byte;
  private
    function GetLeft: Integer;
    function GetTop: Integer;
    function GetRight: Integer;
    function GetBottom: Integer;
    function GetTopLeft: TPoint;
    function GetBottomRight: TPoint;
    function GetRect: TRect;
    function GetZoom: Byte;

    function IsEqual(const ARect: TRect): Boolean; overload;
    function IsEqual(const ARect: ITileRect): Boolean; overload;
    function IsPointInRect(const APoint: TPoint): Boolean;
    function UnionWithRect(const ARect: TRect): TRect; overload;
    function UnionWithRect(const ARect: ITileRect): TRect; overload;
    function IntersecWithRect(
      out AResultRect: TRect;
      const ARect: TRect
    ): Boolean; overload;
    function IntersecWithRect(
      out AResultRect: TRect;
      const ARect: ITileRect
    ): Boolean; overload;
    function IsIntersecWithRect(const ARect: TRect): Boolean; overload;
    function IsIntersecWithRect(const ARect: ITileRect): Boolean; overload;
  public
    constructor Create(const ARect: TRect; const AZoom: Byte);
  end;

implementation

{ TTileRect }

constructor TTileRect.Create(const ARect: TRect; const AZoom: Byte);
begin
  inherited Create;
  FRect := ARect;
  FZoom := AZoom;
end;

function TTileRect.GetBottom: Integer;
begin
  Result := FRect.Bottom;
end;

function TTileRect.GetBottomRight: TPoint;
begin
  Result := FRect.BottomRight;
end;

function TTileRect.GetLeft: Integer;
begin
  Result := FRect.Left;
end;

function TTileRect.GetRect: TRect;
begin
  Result := FRect;
end;

function TTileRect.GetRight: Integer;
begin
  Result := FRect.Right;
end;

function TTileRect.GetTop: Integer;
begin
  Result := FRect.Top;
end;

function TTileRect.GetTopLeft: TPoint;
begin
  Result := FRect.TopLeft;
end;

function TTileRect.GetZoom: Byte;
begin
  Result := FZoom;
end;

function TTileRect.IntersecWithRect(out AResultRect: TRect;
  const ARect: ITileRect): Boolean;
begin
  if ARect = ITileRect(Self) then begin
    Result := True;
    AResultRect := FRect;
  end else if ARect = nil then begin
    Result := False;
  end else begin
    Result := IntersectRect(AResultRect, FRect, ARect.Rect);
  end;
end;

function TTileRect.IntersecWithRect(out AResultRect: TRect;
  const ARect: TRect): Boolean;
begin
  Result := IntersectRect(AResultRect, FRect, ARect);
end;

function TTileRect.IsEqual(const ARect: ITileRect): Boolean;
begin
  if ARect = ITileRect(Self) then begin
    Result := True;
  end else if ARect = nil then begin
    Result := False;
  end else begin
    Result := EqualRect(FRect, ARect.Rect);
  end;
end;

function TTileRect.IsEqual(const ARect: TRect): Boolean;
begin
  Result := EqualRect(FRect, ARect);
end;

function TTileRect.IsIntersecWithRect(const ARect: TRect): Boolean;
var
  VResultRect: TRect;
begin
  Result := IntersectRect(VResultRect, FRect, ARect);
end;

function TTileRect.IsIntersecWithRect(const ARect: ITileRect): Boolean;
var
  VResultRect: TRect;
begin
  if ARect = ITileRect(Self) then begin
    Result := True;
  end else if ARect = nil then begin
    Result := False;
  end else begin
    Result := IntersectRect(VResultRect, FRect, ARect.Rect);
  end;
end;

function TTileRect.IsPointInRect(const APoint: TPoint): Boolean;
begin
  Result := PtInRect(FRect, APoint);
end;

function TTileRect.UnionWithRect(const ARect: ITileRect): TRect;
begin
  if ARect = ITileRect(Self) then begin
    Result := FRect;
  end else if ARect = nil then begin
    Result := FRect;
  end else begin
    UnionRect(Result, FRect, ARect.Rect);
  end;
end;

function TTileRect.UnionWithRect(const ARect: TRect): TRect;
begin
  UnionRect(Result, FRect, ARect);
end;

end.
