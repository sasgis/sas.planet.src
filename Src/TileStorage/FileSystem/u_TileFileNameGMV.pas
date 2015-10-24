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

unit u_TileFileNameGMV;

interface

uses
  Types,
  i_TileFileNameParser,
  i_TileFileNameGenerator,
  u_TileFileNameBase;

type
  TTileFileNameGMV = class(TTileFileNameBase)
  protected
    function GetTileFileName(
      AXY: TPoint;
      AZoom: Byte
    ): string; override;

    function GetTilePoint(
      const ATileFileName: AnsiString;
      out ATileXY: TPoint;
      out ATileZoom: Byte
    ): Boolean; override;
  end;

implementation

uses
  RegExpr,
  SysUtils;

const
  c_GMV_Expr = '^(.+\\)?([tT][tsqrTSQR]*)(\..+)?$';

{ TTileFileNameGMV }

function TTileFileNameGMV.GetTileFileName(
  AXY: TPoint;
  AZoom: Byte
): string;
var
  i: Byte;
  VMask: Integer;
  c: Char;
begin
  if (AZoom >= 9) then begin
    Result := IntToStr(AZoom + 1);
  end else begin
    Result := '0' + IntToStr(AZoom + 1);
  end;
  Result := Result + PathDelim + 't';
  if AZoom > 0 then begin
    VMask := 1 shl (AZoom - 1);
    for i := 1 to AZoom do begin
      if (AXY.X and VMask) = 0 then begin
        if (AXY.Y and VMask) = 0 then begin
          c := 'q';
        end else begin
          c := 't';
        end;
      end else begin
        if (AXY.Y and VMask) = 0 then begin
          c := 'r';
        end else begin
          c := 's';
        end;
      end;
      Result := Result + c;
      VMask := VMask shr 1;
    end;
  end;
end;

function TTileFileNameGMV.GetTilePoint(
  const ATileFileName: AnsiString;
  out ATileXY: TPoint;
  out ATileZoom: Byte
): Boolean;

  function TSQR2XY(const ATSQRStr: AnsiString): TPoint;
  const
    TSQR_CHARSET: set of AnsiChar = ['t', 's', 'q', 'r', 'T', 'S', 'Q', 'R'];
  var
    I: integer;
    EWrongNameText: string;
  begin
    Result := Point(0, 0);
    EWrongNameText := 'Wrong name: ' + ATSQRStr;
    if ((Length(ATSQRStr) = 0) or not (ATSQRStr[1] in TSQR_CHARSET)) then begin
      raise Exception.Create(EWrongNameText);
    end;
    for I := 2 to Length(ATSQRStr) do begin
      Result.X := Result.X * 2;
      Result.Y := Result.Y * 2;
      if not (ATSQRStr[I] in TSQR_CHARSET) then begin
        raise Exception.Create(EWrongNameText);
      end;
      if ATSQRStr[I] in ['R', 'S', 'r', 's'] then begin
        Inc(Result.X);
      end;
      if ATSQRStr[I] in ['T', 'S', 't', 's'] then begin
        Inc(Result.Y);
      end;
    end;
  end;

var
  VRegExpr: TRegExpr;
  tsqr: AnsiString;
begin
  VRegExpr := TRegExpr.Create;
  try
    VRegExpr.Expression := c_GMV_Expr;
    if VRegExpr.Exec(ATileFileName) then begin
      tsqr := VRegExpr.Match[2];
      ATileZoom := Length(tsqr) - 1;
      ATileXY := TSQR2XY(tsqr);
      Result := True;
    end else begin
      Result := False;
    end;
  finally
    VRegExpr.Free;
  end;
end;

end.
