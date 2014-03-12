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

unit u_TileFileNameES;

interface

uses
  Types,
  i_TileFileNameParser,
  i_TileFileNameGenerator,
  u_BaseInterfacedObject;

type
  TTileFileNameES = class(
    TBaseInterfacedObject,
    ITileFileNameParser,
    ITileFileNameGenerator
    )
  private
    class function FullInt(
      I: Integer;
      AZoom: Byte
    ): string;
  private
    function GetTileFileName(
      AXY: TPoint;
      AZoom: Byte
    ): string;

    function GetTilePoint(
      const ATileFileName: string;
      out ATileXY: TPoint;
      out ATileZoom: Byte
    ): Boolean;
  end;

implementation

uses
  RegExpr,
  StrUtils,
  SysUtils;

const
  c_ES_Expr = '^(.+\\)?(\d+)-(\d+)-(\d+)(\..+)?$';

{ TTileFileNameES }

class function TTileFileNameES.FullInt(
  I: Integer;
  AZoom: Byte
): string;
begin
  Result := IntToStr(I);
  if AZoom < 4 then begin
  end else if AZoom < 7 then begin
    Result := RightStr('0' + Result, 2);
  end else if AZoom < 10 then begin
    Result := RightStr('00' + Result, 3);
  end else if AZoom < 14 then begin
    Result := RightStr('000' + Result, 4);
  end else if AZoom < 17 then begin
    Result := RightStr('0000' + Result, 5);
  end else if AZoom < 20 then begin
    Result := RightStr('00000' + Result, 6);
  end else begin
    Result := RightStr('000000' + Result, 7);
  end;
end;

function TTileFileNameES.GetTileFileName(
  AXY: TPoint;
  AZoom: Byte
): string;
var
  VZoomStr: string;
  VFileName: string;
begin
  inherited;
  if (AZoom >= 9) then begin
    VZoomStr := IntToStr(AZoom + 1);
  end else begin
    VZoomStr := '0' + IntToStr(AZoom + 1);
  end;
  VFileName := VZoomStr + '-' + FullInt(AXY.X, AZoom) + '-' + FullInt(AXY.Y, AZoom);
  if AZoom < 6 then begin
    Result := VZoomStr + PathDelim;
  end else if AZoom < 10 then begin
    Result := VZoomStr + PathDelim +
      Chr(60 + AZoom) + FullInt(AXY.X shr 5, AZoom - 5) + FullInt(AXY.Y shr 5, AZoom - 5) + PathDelim;
  end else begin
    Result := '10' + '-' + FullInt(AXY.X shr (AZoom - 9), 9) + '-' + FullInt(AXY.Y shr (AZoom - 9), 9) + PathDelim + VZoomStr + PathDelim + Chr(60 + AZoom) + FullInt(AXY.X shr 5, AZoom - 5) + FullInt(AXY.Y shr 5, AZoom - 5) + PathDelim;
  end;
  Result := Result + VFileName;
end;

function TTileFileNameES.GetTilePoint(
  const ATileFileName: string;
  out ATileXY: TPoint;
  out ATileZoom: Byte
): Boolean;
var
  VRegExpr: TRegExpr;
begin
  VRegExpr := TRegExpr.Create;
  try
    VRegExpr.Expression := c_ES_Expr;
    if VRegExpr.Exec(ATileFileName) then begin
      ATileZoom := StrToInt(VRegExpr.Match[2]) - 1;
      ATileXY.X := StrToInt(VRegExpr.Match[3]);
      ATileXY.Y := StrToInt(VRegExpr.Match[4]);
      Result := True;
    end else begin
      Result := False;
    end;
  finally
    VRegExpr.Free;
  end;
end;

end.
