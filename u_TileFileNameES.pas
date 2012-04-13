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

unit u_TileFileNameES;

interface

uses
  Types,
  i_TileFileNameGenerator;

type
  TTileFileNameES = class(TInterfacedObject, ITileFileNameGenerator)
  private
    class function FullInt(i: Integer; AZoom: byte): string;
  public
    function GetTileFileName(AXY: TPoint; Azoom: byte): string;
  end;

implementation

uses
  StrUtils,
  SysUtils;

{ TTileFileNameES }

class function TTileFileNameES.FullInt(i: Integer; AZoom: byte): string;
begin
  Result := IntToStr(i);
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

function TTileFileNameES.GetTileFileName(AXY: TPoint;
  Azoom: byte): string;
var
  VZoomStr: string;
  VFileName: string;
begin
  inherited;
  if (Azoom >= 9) then begin
    VZoomStr := IntToStr(Azoom + 1);
  end else begin
    VZoomStr := '0' + IntToStr(Azoom + 1);
  end;
  VFileName := VZoomStr + '-' + FullInt(AXY.X, AZoom) + '-' + FullInt(AXY.Y, AZoom);
  if Azoom < 6 then begin
    Result := VZoomStr + PathDelim;
  end else if Azoom < 10 then begin
    Result := VZoomStr + PathDelim +
      Chr(60 + Azoom) + FullInt(AXY.X shr 5, Azoom - 5) + FullInt(AXY.Y shr 5, Azoom - 5) + PathDelim;
  end else begin
    Result := '10' + '-' + FullInt(AXY.X shr (AZoom - 9), 9) + '-' + FullInt(AXY.Y shr (AZoom - 9), 9) + PathDelim + VZoomStr + PathDelim + Chr(60 + Azoom) + FullInt(AXY.X shr 5, Azoom - 5) + FullInt(AXY.Y shr 5, Azoom - 5) + PathDelim;
  end;
  Result := Result + VFileName;
end;

end.
