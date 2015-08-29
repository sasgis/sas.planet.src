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

unit u_MultiPoligonParser;

interface

uses
  t_GeoTypes,
  i_GeometryLonLatFactory,
  i_DoublePointsAggregator,
  i_ProjectionType;

// parse POLYGON and MULTIPOLYGON coordinates
// and from JSON geometry
function ParsePointsToPolygonBuilder(
  const ABuilder: IGeometryLonLatPolygonBuilder;
  const ASourceText: String;
  const AProjectionType: IProjectionType;
  const AInMetr, AMakeHoles: Boolean;
  const APointsAggregator: IDoublePointsAggregator = nil;
  const AFromJSON: Boolean = FALSE
): Integer;

(*

Polygon with hole:

POLYGON((
6465070.6140027 8353056.70502428,
6450793.26254809 8365977.64618294,
...
6471059.38256601 8359565.78775429,
6465070.6140027 8353056.70502428
),(
6597117.44755939 8546992.63888382,
6595481.07789312 8545271.49034039,
...
6597461.56871858 8547441.64007438,
6597117.44755939 8546992.63888382
))

Multipolygon - 2 parts, the second has 2 holes:

MULTIPOLYGON(((
3330867.318648 6674715.089894,
3324072.916581 6678884.987633,
...
3331188.824966 6675175.172635,
3330867.318648 6674715.089894
)),((
3355925.703676 6849012.065766,
3363028.643568 6844534.981657,
...
3355768.152329 6848526.282447,
3355925.703676 6849012.065766
),(
3324592.102649 6727273.76987,
3325003.884715 6726752.179254,
...
3324701.9112 6727740.456211,
3324592.102649 6727273.76987
),(
3319293.840073 6722579.454324,
3319815.430689 6721563.725229,
...
3319376.196486 6723183.401353,
3319293.840073 6722579.454324
)))

JSON geometry:

"rings":[[
[6314062.67803955,8565684.43701172],
[6314138.86901855,8565667.88903809],
[6314139.45800781,8565666.66101074],
[6314135.6340332,8565640.12200928],
[6314132.5960083,8565617.23901367],
[6314131.11901855,8565606.43103027],
[6314107.35601807,8565609.72198486],
[6314105.33599854,8565610.01403809],
[6314098.68902588,8565611.72698975],
[6314096.19799805,8565603.27203369],
[6314053.89703369,8565610.88299561],
[6314046.52001953,8565614.6619873],
[6314062.67803955,8565684.43701172]
]]}

*)

implementation

uses
  SysUtils,
  u_DoublePointsAggregator,
  u_StrFunc,
  u_GeoFunc,
  u_GeoToStrFunc;

function _IsCoord(const ASym: Char): Boolean;
begin
  Result := CharInSet(ASym, ['0','1'..'9','.','-']);
end;

function ParsePointsToPolygonBuilder(
  const ABuilder: IGeometryLonLatPolygonBuilder;
  const ASourceText: String;
  const AProjectionType: IProjectionType;
  const AInMetr, AMakeHoles: Boolean;
  const APointsAggregator: IDoublePointsAggregator;
  const AFromJSON: Boolean
): Integer;
const
  c_JSON_NewSegment = ']],[[';
  c_JSON_Delimiter = '],[';
var
  VPos, VEnd, L: Integer;
  VOk: Byte;
  VPoint: TDoublePoint;
  VXYDelimiters: TSysCharSet;
  VDoublePointsAggregator: IDoublePointsAggregator;
begin
  Result := 0;
  if (0=Length(ASourceText)) then
    Exit;
  if Assigned(APointsAggregator) then begin
    VDoublePointsAggregator := APointsAggregator;
  end else begin
    VDoublePointsAggregator := TDoublePointsAggregator.Create;
  end;

  if AFromJSON then begin
    VXYDelimiters := [','];
  end else begin
    VXYDelimiters := [#32,#160];
  end;

  VPos := 1;
  L := Length(ASourceText);

  // skip preamble
  while (VPos<=L) and (not _IsCoord(ASourceText[VPos])) do
    Inc(VPos);

  try
  // run main loop
  while (VPos<=L) do begin
    // init
    VEnd := VPos;
    VOk := 0;

    // extract coords
    while (VEnd<=L) do begin
      if CharInSet(ASourceText[VEnd], VXYDelimiters) then begin
        // space between X and Y - get X
        VPoint.X := StrPointToFloat(System.Copy(ASourceText, VPos, VEnd-VPos));
        // skip delimiters
        while (VEnd<=L) and CharInSet(ASourceText[VEnd], VXYDelimiters) do
          Inc(VEnd);
        // skip spaces
        while (VEnd<=L) and CharInSet(ASourceText[VEnd], [#32,#160,#13,#10]) do
          Inc(VEnd);
        // first nonspace
        VPos := VEnd;
        VOk := VOk or $01;
      end else if _IsCoord(ASourceText[VEnd]) then begin
        // numeric - goto next
        Inc(VEnd);
      end else begin
        if VPos = VEnd then begin
          Break;
        end;
        // first non-numeric - get Y
        VPoint.Y := StrPointToFloat(System.Copy(ASourceText, VPos, VEnd-VPos));
        VOk := VOk or $02;
        break;
      end;
    end;

    // add coordinates
    if (VOk=$03) then begin
      // convert
      if AInMetr then begin
        VPoint := AProjectionType.Metr2LonLat(VPoint);
      end;
      VDoublePointsAggregator.Add(VPoint);
      Inc(Result);
    end;

    // check after
    // VEnd is the start of delimiter (actually ')' or ',' or ']')
    VPos := VEnd;
    while (VPos<=L) and (not _IsCoord(ASourceText[VPos])) do begin
      Inc(VPos);
    end;

    // if EOL
    if (VPos>L) then
      Exit;

    if AFromJSON then begin
      if System.Copy(ASourceText, VEnd, VPos - VEnd)=c_JSON_NewSegment then begin
        // start new segment
        // TODO: make new holes when available
        if VDoublePointsAggregator.Count > 0 then begin
          ABuilder.AddOuter(VDoublePointsAggregator.MakeStaticAndClear);
          Inc(Result);
        end;
      end;
    end else begin
      if (ASourceText[VEnd]=',') then begin
        // common coordinates delimiter - do nothing
      end else begin
        // if '),(' - start new hole
        // if ')),((' - start new part
        // TODO: make new holes when available
        if VDoublePointsAggregator.Count > 0 then begin
          ABuilder.AddOuter(VDoublePointsAggregator.MakeStaticAndClear);
          Inc(Result);
        end;
      end;
    end;
  end;
  finally
    if VDoublePointsAggregator.Count > 0 then begin
      ABuilder.AddOuter(VDoublePointsAggregator.MakeStaticAndClear);
    end;
  end;
end;

end.