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

unit u_LayerScaleLineVertical;

interface

uses
  Types,
  GR32,
  i_LocalCoordConverter,
  i_LocalCoordConverterChangeable,
  i_ScaleLineConfig,
  u_LayerScaleLine;

type
  TLayerScaleLineVertical = class(TLayerScaleLineBase)
  private
    procedure RedrawScaleLegend(const AVisualCoordConverter: ILocalCoordConverter);
    procedure DrawScaleLegend(
      ALineColor: TColor32;
      AOutLineColor: TColor32;
      ATextColor: TColor32;
      AScaleLegendHeight: Integer;
      const AHalfValue, AFullValue: string;
      ATargetBitmap: TBitmap32
    );
    procedure DrawScaleMarks(
      ALineColor, AOutLineColor, ATextColor: TColor32;
      const AText: string;
      AScalePos: Integer;
      ATargetBitmap: TBitmap32
    );
    procedure GetMetersPerLine(
      const AVisualCoordConverter: ILocalCoordConverter;
      ALineHeight: Integer;
      out AHalfLen: Double;
      out AFullLen: Double
    );
    procedure ModifyLenAndHeight(
      const AVisualCoordConverter: ILocalCoordConverter;
      var AFullLenght: Double;
      var AHeight: Integer
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

procedure TLayerScaleLineVertical.DoUpdateBitmapDraw;
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

procedure TLayerScaleLineVertical.RedrawScaleLegend(const AVisualCoordConverter: ILocalCoordConverter);
var
  VUnitsString: string;
  VFullLenght, VHalfLenght: Double;
  VColor: TColor32;
  VOutLineColor: TColor32;
  VValidLegendHeight: Integer;
  VHalfValue, VFullValue: string;
begin
  VColor := Config.Color;
  VOutLineColor := Config.OutLineColor;

  VValidLegendHeight := (Config.Width div 4) * 4;

  GetMetersPerLine(AVisualCoordConverter, VValidLegendHeight, VHalfLenght, VFullLenght);

  if Config.NumbersFormat = slnfNice then begin
    ModifyLenAndHeight(AVisualCoordConverter, VFullLenght, VValidLegendHeight);
  end;

  if (VHalfLenght < 0) or (VFullLenght < 0) then begin
    DrawScaleLegend(
      VColor,
      VOutLineColor,
      VColor,
      VValidLegendHeight,
      ' ',
      ' ',
      Layer.Bitmap
    );
    Exit;
  end else if VFullLenght > 10000 then begin
    VFullLenght := VFullLenght / 1000;
    VHalfLenght := VHalfLenght / 1000;
    VUnitsString := ' ' + SAS_UNITS_km + ' ';
  end else if VFullLenght < 10 then begin
    VFullLenght := VFullLenght * 100;
    VHalfLenght := VHalfLenght * 100;
    VUnitsString := ' ' + SAS_UNITS_sm + ' ';
  end else begin
    VUnitsString := ' ' + SAS_UNITS_m + ' ';
  end;

  case Config.NumbersFormat of
    slnfNice: begin
      VHalfValue := IntToStr(Round(VFullLenght / 2)) + VUnitsString;
      VFullValue := IntToStr(Round(VFullLenght)) + VUnitsString;
    end;
    slnfScienceRound: begin
      VHalfValue := IntToStr(Round(VHalfLenght)) + VUnitsString;
      VFullValue := IntToStr(Round(VFullLenght)) + VUnitsString;
    end;
  else begin
    VHalfValue := FloatToStrF(VHalfLenght, ffFixed, 10, 2) + VUnitsString;
    VFullValue := FloatToStrF(VFullLenght, ffFixed, 10, 2) + VUnitsString;
  end;
  end;

  DrawScaleLegend(
    VColor,
    VOutLineColor,
    VColor,
    VValidLegendHeight,
    VHalfValue,
    VFullValue,
    Layer.Bitmap
  );
end;

procedure TLayerScaleLineVertical.DrawScaleLegend(
  ALineColor: TColor32;
  AOutLineColor: TColor32;
  ATextColor: TColor32;
  AScaleLegendHeight: Integer;
  const AHalfValue, AFullValue: string;
  ATargetBitmap: TBitmap32
);
var
  I: Integer;
  VHeight: Integer;
  VBitmapSize: TPoint;
  VStartY: Integer;
  VText: string;
begin
  VHeight := (AScaleLegendHeight div 4) * 4;
  VBitmapSize := Types.Point(ATargetBitmap.Width, ATargetBitmap.Height);
  if VBitmapSize.Y > VHeight then begin
    ATargetBitmap.VertLineS(2, VBitmapSize.Y - 3, VBitmapSize.Y - VHeight - 3, AOutLineColor);
    for I := 1 to 4 do begin
      VStartY := (VBitmapSize.Y - 3) - I * (VHeight div 4);
      case I of
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
        VStartY + 1,
        ATargetBitmap
      );
    end;
    ATargetBitmap.VertLineS(1, VBitmapSize.Y - 3, VBitmapSize.Y - VHeight - 2, ALineColor);
    ATargetBitmap.VertLineS(0, VBitmapSize.Y - 3, VBitmapSize.Y - VHeight - 3, AOutLineColor);
  end;
end;

procedure TLayerScaleLineVertical.DrawScaleMarks(
  ALineColor, AOutLineColor, ATextColor: TColor32;
  const AText: string;
  AScalePos: Integer;
  ATargetBitmap: TBitmap32
);
var
  VStartX: Integer;
begin
  if Length(AText) > 0 then begin
    DrawOutLinedText(
      26,
      AScalePos,
      AText,
      ATextColor,
      AOutLineColor,
      ATargetBitmap
    );
  end;
  if Length(AText) = 0 then begin
    VStartX := 10;
  end else begin
    VStartX := 20;
  end;
  ATargetBitmap.HorzLineS(VStartX, AScalePos - 1, 0, AOutLineColor);
  ATargetBitmap.HorzLineS(VStartX, AScalePos, 0, ALineColor);
  ATargetBitmap.HorzLineS(VStartX, AScalePos + 1, 0, AOutLineColor);
  ATargetBitmap.VertLineS(VStartX, AScalePos - 1, AScalePos + 1, AOutLineColor);
end;

procedure TLayerScaleLineVertical.GetMetersPerLine(
  const AVisualCoordConverter: ILocalCoordConverter;
  ALineHeight: Integer;
  out AHalfLen, AFullLen: Double
);
var
  VStartLonLat, VFinishLonLat: TDoublePoint;
  VCenterPixelXY, VFinishPixelXY: TDoublePoint;
  VConverter: ICoordConverter;
  VZoom: Byte;
begin
  VZoom := AVisualCoordConverter.GetZoom;
  VConverter := AVisualCoordConverter.GetGeoConverter;

  VCenterPixelXY := AVisualCoordConverter.GetCenterMapPixelFloat;
  VConverter.ValidatePixelPosFloatStrict(VCenterPixelXY, VZoom, False);
  VStartLonLat := VConverter.PixelPosFloat2LonLat(VCenterPixelXY, VZoom);

  VFinishPixelXY := DoublePoint(VCenterPixelXY.X, VCenterPixelXY.Y - ALineHeight / 2);
  if VConverter.CheckPixelPosFloatStrict(VFinishPixelXY, VZoom) then begin
    VFinishLonLat := VConverter.PixelPosFloat2LonLat(
      VFinishPixelXY,
      VZoom
    );
    AHalfLen := VConverter.Datum.CalcDist(VStartLonLat, VFinishLonLat);
  end else begin
    AHalfLen := -1;
  end;

  VFinishPixelXY := DoublePoint(VCenterPixelXY.X, VCenterPixelXY.Y - ALineHeight);
  if VConverter.CheckPixelPosFloatStrict(VFinishPixelXY, VZoom) then begin
    VFinishLonLat := VConverter.PixelPosFloat2LonLat(
      VFinishPixelXY,
      VZoom
    );
    AFullLen := VConverter.Datum.CalcDist(VStartLonLat, VFinishLonLat);
  end else begin
    AFullLen := -1;
  end;
end;

procedure TLayerScaleLineVertical.ModifyLenAndHeight(
  const AVisualCoordConverter: ILocalCoordConverter;
  var AFullLenght: Double;
  var AHeight: Integer
);
var
  VStartLonLat, VFinishLonLat: TDoublePoint;
  VCenterPixelXY: TPoint;
  VFinishPixelXY: TDoublePoint;
  VConverter: ICoordConverter;
  VZoom: Byte;
  VHeight: Integer;
  VFullLenght: Double;
begin
  VFullLenght := GetNiceLen(AFullLenght);

  VZoom := AVisualCoordConverter.GetZoom;
  VConverter := AVisualCoordConverter.GetGeoConverter;

  VCenterPixelXY := AVisualCoordConverter.LocalPixel2MapPixel(
    AVisualCoordConverter.LonLat2LocalPixel(
      AVisualCoordConverter.GetCenterLonLat,
      prToTopLeft
    ),
    prToTopLeft
  );
  VConverter.ValidatePixelPosStrict(VCenterPixelXY, VZoom, False);
  VStartLonLat := VConverter.PixelPos2LonLat(VCenterPixelXY, VZoom);
  VFinishLonLat := VConverter.Datum.CalcFinishPosition(VStartLonLat, 0, VFullLenght);
  VConverter.ValidateLonLatPos(VFinishLonLat);
  VFinishPixelXY := VConverter.LonLat2PixelPosFloat(VFinishLonLat, VZoom);

  VHeight := Abs(VCenterPixelXY.Y - Round(VFinishPixelXY.Y));

  if VHeight <= 0 then begin
    AFullLenght := -1;
  end else begin
    AFullLenght := VFullLenght;
    AHeight := VHeight;
  end;
end;

function TLayerScaleLineVertical.GetNewBitmapSize: TPoint;
begin
  Result.X := 100;
  Result.Y := Config.Width + 10;
end;

function TLayerScaleLineVertical.GetNewVisibility: boolean;
begin
  Result := Config.Visible and Config.Extended;
end;

end.
