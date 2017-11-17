{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2017, SAS.Planet development team.                      *}
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

unit t_SunCalcConfig;

interface

uses
  t_Bitmap32;

type
  TSunCalcShapesColors = record
    DayLineColor: TColor32;
    DayPolyLineColor: TColor32;

    DaySunriseLineColor: TColor32;
    DaySunsetLineColor: TColor32;

    YearCircleColor: TColor32;
    YearPolyLinesColor: TColor32;
    YearPolygonFillColor: TColor32;

    class operator Equal(const A, B: TSunCalcShapesColors): Boolean;
    class operator NotEqual(const A, B: TSunCalcShapesColors): Boolean;
  end;

  TSunCalcDayLineColors = array [0..5] of TColor32;

  TSunCalcTimeLineColors = record
    BgColor: TColor32;
    VertLinesColor: TColor32;
    YearLineColor: TColor32;
    DayLineColors: TSunCalcDayLineColors;

    class operator Equal(const A, B: TSunCalcTimeLineColors): Boolean;
    class operator NotEqual(const A, B: TSunCalcTimeLineColors): Boolean;
  end;

  TSunCalcDetailsPanelColsWidth = array [0..3] of Integer;

  TSunCalcDetailsPanelColors = record
    BgColor: TColor32;
    GridLinesColor: TColor32;

    class operator Equal(const A, B: TSunCalcDetailsPanelColors): Boolean;
    class operator NotEqual(const A, B: TSunCalcDetailsPanelColors): Boolean;
  end;

  TSunCalcFontInfo = record
    FontName: string;
    FontSize: Integer;
    TextColor: TColor32;
    BgColor: TColor32;

    class operator Equal(const A, B: TSunCalcFontInfo): Boolean;
    class operator NotEqual(const A, B: TSunCalcFontInfo): Boolean;
  end;

implementation

uses
  SysUtils;

{ TSunCalcShapesColors }

class operator TSunCalcShapesColors.Equal(const A, B: TSunCalcShapesColors): Boolean;
begin
  Result :=
    (A.DayLineColor = B.DayLineColor) and
    (A.DayPolyLineColor = B.DayPolyLineColor) and
    (A.DaySunriseLineColor = B.DaySunriseLineColor) and
    (A.DaySunsetLineColor = B.DaySunsetLineColor) and
    (A.YearCircleColor = B.YearCircleColor) and
    (A.YearPolyLinesColor = B.YearPolyLinesColor) and
    (A.YearPolygonFillColor = B.YearPolygonFillColor);
end;

class operator TSunCalcShapesColors.NotEqual(const A, B: TSunCalcShapesColors): Boolean;
begin
  Result := not (A = B);
end;

{ TSunCalcYearTimeLineColors }

class operator TSunCalcTimeLineColors.Equal(const A, B: TSunCalcTimeLineColors): Boolean;
var
  I: Integer;
begin
  Result :=
    (A.BgColor = B.BgColor) and
    (A.YearLineColor = B.YearLineColor) and
    (A.VertLinesColor = B.VertLinesColor);

  if not Result then begin
    Exit;
  end;

  for I := Low(A.DayLineColors) to High(A.DayLineColors) do begin
    if A.DayLineColors[I] <> B.DayLineColors[I] then begin
      Result := False;
      Exit;
    end;
  end;
end;

class operator TSunCalcTimeLineColors.NotEqual(const A, B: TSunCalcTimeLineColors): Boolean;
begin
  Result := not (A = B);
end;

{ TSunCalcDetailsPanelColors }

class operator TSunCalcDetailsPanelColors.Equal(const A, B: TSunCalcDetailsPanelColors): Boolean;
begin
  Result :=
    (A.BgColor = B.BgColor) and
    (A.GridLinesColor = B.GridLinesColor);
end;

class operator TSunCalcDetailsPanelColors.NotEqual(const A, B: TSunCalcDetailsPanelColors): Boolean;
begin
  Result := not (A = B);
end;

{ TSunCalcFontInfo }

class operator TSunCalcFontInfo.Equal(const A, B: TSunCalcFontInfo): Boolean;
begin
  Result :=
    (LowerCase(A.FontName) = LowerCase(B.FontName)) and
    (A.FontSize = B.FontSize) and
    (A.TextColor = B.TextColor) and
    (A.BgColor = B.BgColor);
end;

class operator TSunCalcFontInfo.NotEqual(const A, B: TSunCalcFontInfo): Boolean;
begin
  Result := not (A = B);
end;

end.
