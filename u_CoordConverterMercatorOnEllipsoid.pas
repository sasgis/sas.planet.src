{******************************************************************************}
{* SAS.Планета (SAS.Planet)                                                   *}
{* Copyright (C) 2007-2011, авторы программы SAS.Планета (SAS.Planet).        *}
{* Это программа является свободным программным обеспечением. Вы можете       *}
{* распространять и/или модифицировать её согласно условиям Стандартной       *}
{* Общественной Лицензии GNU, опубликованной Фондом Свободного Программного   *}
{* Обеспечения, версии 3. Эта программа распространяется в надежде, что она   *}
{* будет полезной, но БЕЗ ВСЯКИХ ГАРАНТИЙ, в том числе подразумеваемых        *}
{* гарантий ТОВАРНОГО СОСТОЯНИЯ ПРИ ПРОДАЖЕ и ГОДНОСТИ ДЛЯ ОПРЕДЕЛЁННОГО      *}
{* ПРИМЕНЕНИЯ. Смотрите Стандартную Общественную Лицензию GNU версии 3, для   *}
{* получения дополнительной информации. Вы должны были получить копию         *}
{* Стандартной Общественной Лицензии GNU вместе с программой. В случае её     *}
{* отсутствия, посмотрите http://www.gnu.org/licenses/.                       *}
{*                                                                            *}
{* http://sasgis.ru/sasplanet                                                 *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_CoordConverterMercatorOnEllipsoid;

interface

uses
  Types,
  t_GeoTypes,
  u_CoordConverterBasic;

type
  TCoordConverterMercatorOnEllipsoid = class(TCoordConverterBasic)
  protected
    FExct: Double;
    function LonLat2MetrInternal(const ALL: TDoublePoint): TDoublePoint; override;
    function LonLat2RelativeInternal(const XY: TDoublePoint): TDoublePoint; override; stdcall;
    function Relative2LonLatInternal(const XY: TDoublePoint): TDoublePoint; override; stdcall;
  public
    constructor Create(Aradiusa, Aradiusb: Double);
  end;

implementation

uses
  Math,
  u_Datum;

const
  MerkElipsK = 0.000000001;

{ TCoordConverterMercatorOnEllipsoid }

constructor TCoordConverterMercatorOnEllipsoid.Create(ARadiusA, Aradiusb: Double);
begin
  FExct := sqrt(ARadiusA * ARadiusA - ARadiusB * ARadiusB) / ARadiusA;
  if (Abs(ARadiusA - 6378137) < 1) and (Abs(ARadiusB - 6356752) < 1) then begin
    inherited Create(TDatum.Create(3395, Aradiusa, Aradiusb));
    FProjEPSG := 3395;
    FCellSizeUnits := CELL_UNITS_METERS;
  end else begin
    inherited Create(TDatum.Create(0, Aradiusa, Aradiusb));
    FProjEPSG := 0;
    FCellSizeUnits := CELL_UNITS_UNKNOWN;
  end;

end;

function TCoordConverterMercatorOnEllipsoid.LonLat2MetrInternal(const ALl: TDoublePoint): TDoublePoint;
var
  VLL: TDoublePoint;
  b, bs: extended;
begin
  VLL := ALL;
  Vll.x := Vll.x * (Pi / 180);
  Vll.y := Vll.y * (Pi / 180);
  result.x := FDatum.GetSpheroidRadiusA * Vll.x;

  bs := FExct * sin(VLl.y);
  b := Tan((Vll.y + PI / 2) / 2) * power((1 - bs) / (1 + bs), (FExct / 2));
  result.y := FDatum.GetSpheroidRadiusA * Ln(b);
end;

function TCoordConverterMercatorOnEllipsoid.LonLat2RelativeInternal(
  const XY: TDoublePoint): TDoublePoint;
var
  z, c: Extended;
  VLL: TDoublePoint;
begin
  VLL := XY;
  Result.x := (0.5 + VLl.x / 360);
  z := sin(VLl.y * Pi / 180);
  c := (1 / (2 * Pi));
  Result.y := (0.5 - c * (ArcTanh(z) - FExct * ArcTanh(FExct * z)));
end;

function TCoordConverterMercatorOnEllipsoid.Relative2LonLatInternal(
  const XY: TDoublePoint): TDoublePoint;
var
  zu, zum1, yy: extended;
  VXY: TDoublePoint;
  VSin: Extended;
  e_y: Extended;
begin
  VXY := XY;
  Result.X := (VXY.x - 0.5) * 360;

  if (VXY.y > 0.5) then begin
    yy := (VXY.y - 0.5);
  end else begin
    yy := (0.5 - VXY.y);
  end;
  yy := yy * (2 * PI);
  Zu := 2 * arctan(exp(yy)) - PI / 2;
  e_y := exp(2 * yy);
  Result.Y := Zu * (180 / Pi);
  repeat
    Zum1 := Zu;
    VSin := Sin(Zum1);
    Zu := arcsin(1 - (1 + VSin) * power((1 - FExct * VSin) / (1 + FExct * VSin), FExct) / e_y);
  until (abs(Zum1 - Zu) < MerkElipsK) or (isNAN(Zu));
  if not (isNAN(Zu)) then begin
    if VXY.y > 0.5 then begin
      result.Y := -zu * 180 / Pi;
    end else begin
      result.Y := zu * 180 / Pi;
    end;
  end;
end;

end.
