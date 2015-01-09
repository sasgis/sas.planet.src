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

unit u_LayerScaleLineHorizontal;

interface

uses
  Types,
  GR32,
  i_LocalCoordConverter,
  i_LocalCoordConverterChangeable,
  i_ScaleLineConfig,
  u_LayerScaleLine;

type
  TLayerScaleLineHorizontal = class(TLayerScaleLineBase)
  private
    procedure RedrawScaleLegend(const AVisualCoordConverter: ILocalCoordConverter);
    procedure DrawScaleLegend(
      ALineColor: TColor32;
      AOutLineColor: TColor32;
      ATextColor: TColor32;
      AScaleLegendWidth: Integer;
      const AHalfValue, AFullValue: string;
      ATargetBitmap: TBitmap32
    );
    procedure DrawScaleMarks(
      ALineColor: TColor32;
      AOutLineColor: TColor32;
      ATextColor: TColor32;
      const AText: string;
      AScalePos: Integer;
      ATargetBitmap: TBitmap32
    );
    function GetMetersPerLine(
      const AVisualCoordConverter: ILocalCoordConverter;
      ALineWidth: Integer
    ): Double;
    procedure ModifyLenAndWidth(
      var ALen: Double;
      var AWidth: Integer
    );
  protected
    function GetNewVisibility: boolean; override;
    function GetNewBitmapSize: TPoint; override;
    procedure DoUpdateBitmapDraw; override;
  end;

implementation

uses
  SysUtils,
  t_GeoTypes,
  i_CoordConverter,
  u_ResStrings,
  u_GeoFunc;

{ TLayerScaleLine }

procedure TLayerScaleLineHorizontal.DoUpdateBitmapDraw;
var
  VVisualCoordConverter: ILocalCoordConverter;
begin
  inherited;
  Layer.Bitmap.Clear(0);
  VVisualCoordConverter := View.GetStatic;
  if VVisualCoordConverter <> nil then begin
    RedrawScaleLegend(VVisualCoordConverter);
  end;
end;

procedure TLayerScaleLineHorizontal.RedrawScaleLegend(const AVisualCoordConverter: ILocalCoordConverter);
var
  VUnitsString: string;
  num: Double;
  rnum: Integer;
  VColor: TColor32;
  VOutLineColor: TColor32;
  VValidLegendWidth: Integer;
  VHalfValue, VFullValue: string;
begin
  VColor := Config.Color;
  VOutLineColor := Config.OutLineColor;

  VValidLegendWidth := (Config.Width div 4) * 4;

  num := GetMetersPerLine(AVisualCoordConverter, VValidLegendWidth);

  if Config.NumbersFormat = slnfNice then begin
    ModifyLenAndWidth(Num, VValidLegendWidth);
  end;

  if num > 10000 then begin
    num := num / 1000;
    VUnitsString := ' ' + SAS_UNITS_km + ' ';
  end else if num < 10 then begin
    num := num * 100;
    VUnitsString := ' ' + SAS_UNITS_sm + ' ';
  end else begin
    VUnitsString := ' ' + SAS_UNITS_m + ' ';
  end;

  case Config.NumbersFormat of
    slnfNice: begin
      VHalfValue := IntToStr(Round(num / 2)) + VUnitsString;
      VFullValue := IntToStr(Round(num)) + VUnitsString;
    end;
    slnfScienceRound: begin
      rnum := Round(num / 2);
      VHalfValue := IntToStr(rnum) + VUnitsString;
      VFullValue := IntToStr(rnum * 2) + VUnitsString;
    end;
  else begin
    VHalfValue := FloatToStrF(num / 2, ffFixed, 10, 2) + VUnitsString;
    VFullValue := FloatToStrF(num, ffFixed, 10, 2) + VUnitsString;
  end;
  end;

  DrawScaleLegend(
    VColor,
    VOutLineColor,
    VColor,
    VValidLegendWidth,
    VHalfValue,
    VFullValue,
    Layer.Bitmap
  );
end;

procedure TLayerScaleLineHorizontal.DrawScaleLegend(
  ALineColor: TColor32;
  AOutLineColor: TColor32;
  ATextColor: TColor32;
  AScaleLegendWidth: Integer;
  const AHalfValue, AFullValue: string;
  ATargetBitmap: TBitmap32
);
var
  I: Integer;
  VWidth: Integer;
  VBitmapSize: TPoint;
  VStartX: Integer;
  VText: string;
begin
  VWidth := (AScaleLegendWidth div 4) * 4;
  VBitmapSize := Types.Point(ATargetBitmap.Width, ATargetBitmap.Height);
  ATargetBitmap.Line(0, VBitmapSize.Y - 3, VWidth + 2, VBitmapSize.Y - 3, AOutLineColor);
  for I := 0 to 4 do begin
    VStartX := I * (VWidth div 4);
    case I of
      0: begin
        if not Config.Extended then begin
          VText := '0';
        end else begin
          VText := '';
        end;
      end;
      2: begin
        VText := AHalfValue;
      end;
      4: begin
        VText := AFullValue;
      end;
    else begin
      VText := '';
    end;
    end;
    DrawScaleMarks(
      ALineColor,
      AOutLineColor,
      ATextColor,
      VText,
      VStartX + 1,
      ATargetBitmap
    );
  end;
  ATargetBitmap.Line(1, VBitmapSize.Y - 2, VWidth + 2, VBitmapSize.Y - 2, ALineColor);
  ATargetBitmap.Line(0, VBitmapSize.Y - 1, VWidth + 2, VBitmapSize.Y - 1, AOutLineColor);
end;

procedure TLayerScaleLineHorizontal.DrawScaleMarks(
  ALineColor, AOutLineColor, ATextColor: TColor32;
  const AText: string;
  AScalePos: Integer;
  ATargetBitmap: TBitmap32
);
var
  VStartY: Integer;
  VHeight: Integer;
begin
  if Length(AText) > 0 then begin
    DrawOutLinedText(
      AScalePos,
      ATargetBitmap.Height - 36,
      AText,
      ATextColor,
      AOutLineColor,
      ATargetBitmap
    );
  end;
  VHeight := ATargetBitmap.Height;
  if Length(AText) = 0 then begin
    VStartY := VHeight - 10;
  end else begin
    VStartY := VHeight - 20;
  end;
  ATargetBitmap.Line(AScalePos - 1, VStartY, AScalePos - 1, VHeight - 1, AOutLineColor);
  ATargetBitmap.Line(AScalePos, VStartY, AScalePos, VHeight - 1, ALineColor);
  ATargetBitmap.Line(AScalePos + 1, VStartY, AScalePos + 1, VHeight - 1, AOutLineColor);
  ATargetBitmap.Line(AScalePos - 1, VStartY, AScalePos + 1, VStartY, AOutLineColor);
end;

function TLayerScaleLineHorizontal.GetMetersPerLine(
  const AVisualCoordConverter: ILocalCoordConverter;
  ALineWidth: Integer
): Double;
var
  VStartLonLat, VFinishLonLat: TDoublePoint;
  VStartPixel, VFinishPixel: TDoublePoint;
  VConverter: ICoordConverter;
  VZoom: Byte;
begin
  VZoom := AVisualCoordConverter.GetZoom;
  VConverter := AVisualCoordConverter.GetGeoConverter;
  VStartPixel := AVisualCoordConverter.GetCenterMapPixelFloat;
  VConverter.ValidatePixelPosFloatStrict(VStartPixel, VZoom, True);
  VFinishPixel := DoublePoint(VStartPixel.X + 1, VStartPixel.Y);
  VConverter.ValidatePixelPosFloat(VFinishPixel, VZoom, True);
  VStartLonLat := VConverter.PixelPosFloat2LonLat(VStartPixel, VZoom);
  VFinishLonLat := VConverter.PixelPosFloat2LonLat(VFinishPixel, VZoom);
  Result := VConverter.Datum.CalcDist(VStartLonLat, VFinishLonLat) * ALineWidth;
end;

procedure TLayerScaleLineHorizontal.ModifyLenAndWidth(
  var ALen: Double;
  var AWidth: Integer
);
var
  VNewLen: Double;
  VNewWidth: Integer;
begin
  if ALen > 0 then begin
    VNewLen := GetNiceLen(ALen);
    VNewWidth := Trunc(AWidth * VNewLen / ALen);
    ALen := VNewLen;
    AWidth := VNewWidth;
  end;
end;

function TLayerScaleLineHorizontal.GetNewBitmapSize: TPoint;
begin
  Result.X := Config.Width + 100;
  Result.Y := 50;
end;

function TLayerScaleLineHorizontal.GetNewVisibility: boolean;
begin
  Result := Config.Visible;
end;

end.
