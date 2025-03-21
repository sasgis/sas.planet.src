{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
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

unit u_ETS_Tiles;

interface

uses
  Types;

function XYZ_to_QuadKey(
  const AXY: TPoint;
  const AZoom: Byte
): AnsiString;

implementation

function XYZ_to_QuadKey(
  const AXY: TPoint;
  const AZoom: Byte
): AnsiString;
var
  i: Byte;
  osX,osY,prX,prY: Integer;

  procedure _AddChar(const AChr: AnsiChar);
  begin
    Result := Result + AChr;
  end;

begin
  Result:='';

  // check zoom
  if (1<AZoom) and (AZoom<=24) then begin
    // correct zoom
    //osX:=round(intpower(2,pXYZ^.z-1)) div 2;
    osX := (1 shl (AZoom - 2));
    osY := osX;
    prX := osX;
    prY := osY;
    // loop
    for i := 2 to AZoom do
    begin
      prX := prX div 2;
      prY := prY div 2;
      if (AXY.x<osX) then begin
        osX := osX - prX;
        if (AXY.y<osY) then begin
          osY := osY - prY;
          _AddChar('0');
        end else begin
          osY := osY + prY;
          _AddChar('2');
        end;
      end else begin
        osX := osX + prX;
        if (AXY.y<osY) then begin
          osY := osY - prY;
          _AddChar('1');
        end else begin
          osY := osY + prY;
          _AddChar('3');
        end;
      end;
    end;
  end;
end;

end.
