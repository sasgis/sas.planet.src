{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2021, SAS.Planet development team.                      *}
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

unit u_HashMatrix;

interface

uses
  Types,
  t_Hash,
  i_HashMatrix,
  u_BaseInterfacedObject;

type
  THashMatrix = class(TBaseInterfacedObject, IHashMatrix)
  private type TMatrixType = array of THashValue;
  private
    FMatrix: TMatrixType;
    FRect: TRect;
  public
    procedure Reset(const ARect: TRect);
    procedure ChangeRect(const ARect: TRect);
    procedure SetHash(
      const APos: TPoint;
      const AHash: THashValue
    );
    function GetHash(const APos: TPoint): THashValue;
  public
    constructor Create;
  end;

implementation

function IndexByPos(
  const ARect: TRect;
  const APos: TPoint
): Integer; inline;
begin
  Result := APos.X - ARect.Left + (APos.Y - ARect.Top) * (ARect.Right - ARect.Left);
end;

{ TTileHashMatrix }

constructor THashMatrix.Create;
begin
  inherited Create;
  FMatrix := nil;
  FRect := Rect(0, 0, 0, 0);
end;

function THashMatrix.GetHash(const APos: TPoint): THashValue;
begin
  if PtInRect(FRect, APos) then begin
    Result := FMatrix[IndexByPos(FRect, APos)];
  end else begin
    Result := 0;
  end;
end;

procedure THashMatrix.ChangeRect(const ARect: TRect);
var
  VIntersectRect: TRect;
  VTargetLineWidth: Integer;
  VTargetSize: Integer;
  VIntersectWidth: Integer;
  i: Integer;
  VTargetMatrix: TMatrixType;
begin
  if EqualRect(ARect, FRect) then begin
    Exit;
  end else if not IntersectRect(VIntersectRect, ARect, FRect) then begin
    Reset(ARect);
  end else begin
    VTargetLineWidth := ARect.Right - ARect.Left;
    VIntersectWidth := VIntersectRect.Right - VIntersectRect.Left;
    VTargetSize := VTargetLineWidth * (ARect.Bottom - ARect.Top);
    SetLength(VTargetMatrix, VTargetSize);

    for i := VIntersectRect.Top to VIntersectRect.Bottom - 1 do begin
      Move(
        FMatrix[IndexByPos(FRect, Point(VIntersectRect.Left, i))],
        VTargetMatrix[IndexByPos(ARect, Point(VIntersectRect.Left, i))],
        VIntersectWidth * SizeOf(FMatrix[0])
      );
    end;
    FRect := ARect;
    FMatrix := VTargetMatrix;
  end;
end;

procedure THashMatrix.Reset(const ARect: TRect);
var
  VSize: Integer;
begin
  FRect := ARect;
  if not IsRectEmpty(FRect) then begin
    VSize := (FRect.Right - FRect.Left) * (FRect.Bottom - FRect.Top);
    SetLength(FMatrix, VSize);
    FillChar(FMatrix[0], VSize * SizeOf(FMatrix[0]), 0);
  end;
end;

procedure THashMatrix.SetHash(
  const APos: TPoint;
  const AHash: THashValue
);
begin
  if PtInRect(FRect, APos) then begin
    FMatrix[IndexByPos(FRect, APos)] := AHash;
  end;
end;

end.
