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

unit u_TileFileNameGMV;

interface

uses
  Types,
  i_TileFileNameGenerator;

type
  TTileFileNameGMV = class(TInterfacedObject, ITileFileNameGenerator)
  public
    function GetTileFileName(AXY: TPoint; Azoom: byte): string;
  end;

implementation

uses
  SysUtils;

{ TTileFileNameGMV }

function TTileFileNameGMV.GetTileFileName(AXY: TPoint;
  Azoom: byte): string;
var
  i: byte;
  VMask: Integer;
  c: Char;
begin
  if (Azoom >= 9) then begin
    Result := IntToStr(Azoom + 1);
  end else begin
    Result := '0' + IntToStr(Azoom + 1);
  end;
  Result := Result + PathDelim + 't';
  if Azoom > 0 then begin
    VMask := 1 shl (Azoom - 1);
    for i := 1 to Azoom do begin
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

end.
