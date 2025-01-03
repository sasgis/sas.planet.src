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

unit u_WindowLayerScaleLineVertical;

interface

uses
  Types,
  GR32,
  i_LocalCoordConverter,
  u_WindowLayerScaleLineBase;

type
  TWindowLayerScaleLineVertical = class(TWindowLayerScaleLineBase)
  private
    procedure RedrawScaleLegend(
      const ABitmap: TBitmap32;
      const AVisualCoordConverter: ILocalCoordConverter
    );
    procedure DrawScaleLegend(
      const ABitmap: TBitmap32;
      const ALineColor: TColor32;
      const AOutLineColor: TColor32;
      const ATextColor: TColor32;
      const AScaleLegendHeight: Integer;
      const AHalfValue: string;
      const AFullValue: string
    );
    procedure DrawScaleMarks(
      const ABitmap: TBitmap32;
      const ALineColor: TColor32;
      const AOutLineColor: TColor32;
      const ATextColor: TColor32;
      const AText: string;
      const AScalePos: Integer
    );
    procedure GetMetersPerLine(
      const AVisualCoordConverter: ILocalCoordConverter;
      const ALineHeight: Integer;
      out AHalfLen: Double;
      out AFullLen: Double
    );
    procedure ModifyLenAndHeight(
      const AVisualCoordConverter: ILocalCoordConverter;
      var AFullLenght: Double;
      var AHeight: Integer
    );
  protected
    function GetNewVisibility: Boolean; override;
    function GetNewBitmapSize: TPoint; override;
    procedure DoUpdateBitmapDraw; override;
  end;

implementation

uses
  SysUtils,
  t_GeoTypes,
  i_Projection,
  i_ScaleLineConfig,
  u_ResStrings,
  u_GeoFunc;

{ TWindowLayerScaleLineVertical }

procedure TWindowLayerScaleLineVertical.DoUpdateBitmapDraw;
var
  VVisualCoordConverter: ILocalCoordConverter;
begin
  Layer.Bitmap.BeginUpdate;
  try
    Layer.Bitmap.Clear(0);
    VVisualCoordConverter := View.GetStatic;
    if VVisualCoordConverter <> nil then begin
      RedrawScaleLegend(Layer.Bitmap, VVisualCoordConverter);
    end;
  finally
    Layer.Bitmap.EndUpdate;
  end;
end;

procedure TWindowLayerScaleLineVertical.RedrawScaleLegend(
  const ABitmap: TBitmap32;
  const AVisualCoordConverter: ILocalCoordConverter
);
var
  VDigits: Integer;
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
      ABitmap,
      VColor,
      VOutLineColor,
      VColor,
      VValidLegendHeight,
      ' ',
      ' '
    );
    Exit;
  end;

  if VFullLenght > 10000 then begin
    VFullLenght := VFullLenght / 1000;
    VHalfLenght := VHalfLenght / 1000;
    VUnitsString := SAS_UNITS_km + ' ';
  end else
  if VFullLenght < 10 then begin
    VFullLenght := VFullLenght * 100;
    VHalfLenght := VHalfLenght * 100;
    VUnitsString := SAS_UNITS_sm + ' ';
  end else begin
    VUnitsString := SAS_UNITS_m + ' ';
  end;

  case Config.NumbersFormat of
    slnfNice: begin
      VHalfLenght := VFullLenght / 2;
      VDigits := 0;
    end;
    slnfScience: begin
      VDigits := 2;
    end;
    slnfScienceRound: begin
      VDigits := 0;
    end;
  else
    Assert(False);
    VDigits := 0;
  end;

  VHalfValue := ValueToStr(VHalfLenght, VDigits, VUnitsString);
  VFullValue := ValueToStr(VFullLenght, VDigits, VUnitsString);

  DrawScaleLegend(
    ABitmap,
    VColor,
    VOutLineColor,
    VColor,
    VValidLegendHeight,
    VHalfValue,
    VFullValue
  );
end;

procedure TWindowLayerScaleLineVertical.DrawScaleLegend(
  const ABitmap: TBitmap32;
  const ALineColor: TColor32;
  const AOutLineColor: TColor32;
  const ATextColor: TColor32;
  const AScaleLegendHeight: Integer;
  const AHalfValue: string;
  const AFullValue: string
);
var
  I: Integer;
  VHeight: Integer;
  VBitmapSize: TPoint;
  VStartY: Integer;
  VText: string;
begin
  VHeight := (AScaleLegendHeight div 4) * 4;
  VBitmapSize := Types.Point(ABitmap.Width, ABitmap.Height);
  if VBitmapSize.Y > VHeight then begin
    ABitmap.VertLineS(2, VBitmapSize.Y - 3, VBitmapSize.Y - VHeight - 3, AOutLineColor);
    for I := 1 to 4 do begin
      VStartY := (VBitmapSize.Y - 3) - I * (VHeight div 4);
      case I of
        2: begin
          VText := AHalfValue;
        end;
        4: begin
          VText := AFullValue;
        end;
      else
        VText := '';
      end;
      DrawScaleMarks(
        ABitmap,
        ALineColor,
        AOutLineColor,
        ATextColor,
        VText,
        VStartY + 1
      );
    end;
    ABitmap.VertLineS(1, VBitmapSize.Y - 3, VBitmapSize.Y - VHeight - 2, ALineColor);
    ABitmap.VertLineS(0, VBitmapSize.Y - 3, VBitmapSize.Y - VHeight - 3, AOutLineColor);
  end;
end;

procedure TWindowLayerScaleLineVertical.DrawScaleMarks(
  const ABitmap: TBitmap32;
  const ALineColor: TColor32;
  const AOutLineColor: TColor32;
  const ATextColor: TColor32;
  const AText: string;
  const AScalePos: Integer
);
var
  VStartX: Integer;
begin
  if Length(AText) > 0 then begin
    DrawOutLinedText(
      ABitmap,
      26,
      AScalePos,
      AText,
      ATextColor,
      AOutLineColor
    );
  end;
  if Length(AText) = 0 then begin
    VStartX := 10;
  end else begin
    VStartX := 20;
  end;
  ABitmap.HorzLineS(VStartX, AScalePos - 1, 0, AOutLineColor);
  ABitmap.HorzLineS(VStartX, AScalePos, 0, ALineColor);
  ABitmap.HorzLineS(VStartX, AScalePos + 1, 0, AOutLineColor);
  ABitmap.VertLineS(VStartX, AScalePos - 1, AScalePos + 1, AOutLineColor);
end;

procedure TWindowLayerScaleLineVertical.GetMetersPerLine(
  const AVisualCoordConverter: ILocalCoordConverter;
  const ALineHeight: Integer;
  out AHalfLen: Double;
  out AFullLen: Double
);
var
  VStartLonLat, VFinishLonLat: TDoublePoint;
  VCenterPixelXY, VFinishPixelXY: TDoublePoint;
  VProjection: IProjection;
begin
  VProjection := AVisualCoordConverter.Projection;

  VCenterPixelXY := AVisualCoordConverter.GetCenterMapPixelFloat;
  VProjection.ValidatePixelPosFloatStrict(VCenterPixelXY, False);
  VStartLonLat := VProjection.PixelPosFloat2LonLat(VCenterPixelXY);
  VProjection.ProjectionType.ValidateLonLatPos(VStartLonLat);

  VFinishPixelXY := DoublePoint(VCenterPixelXY.X, VCenterPixelXY.Y - ALineHeight / 2);
  if VProjection.CheckPixelPosFloatStrict(VFinishPixelXY) then begin
    VFinishLonLat := VProjection.PixelPosFloat2LonLat(VFinishPixelXY);
    VProjection.ProjectionType.ValidateLonLatPos(VFinishLonLat);
    AHalfLen := VProjection.ProjectionType.Datum.CalcDist(VStartLonLat, VFinishLonLat);
  end else begin
    AHalfLen := -1;
  end;

  VFinishPixelXY := DoublePoint(VCenterPixelXY.X, VCenterPixelXY.Y - ALineHeight);
  if VProjection.CheckPixelPosFloatStrict(VFinishPixelXY) then begin
    VFinishLonLat := VProjection.PixelPosFloat2LonLat(VFinishPixelXY);
    VProjection.ProjectionType.ValidateLonLatPos(VFinishLonLat);
    AFullLen := VProjection.ProjectionType.Datum.CalcDist(VStartLonLat, VFinishLonLat);
  end else begin
    AFullLen := -1;
  end;
end;

procedure TWindowLayerScaleLineVertical.ModifyLenAndHeight(
  const AVisualCoordConverter: ILocalCoordConverter;
  var AFullLenght: Double;
  var AHeight: Integer
);
var
  VStartLonLat, VFinishLonLat: TDoublePoint;
  VCenterPixelXY: TPoint;
  VFinishPixelXY: TDoublePoint;
  VProjection: IProjection;
  VHeight: Integer;
  VFullLenght: Double;
begin
  VFullLenght := GetNiceLen(AFullLenght);

  VProjection := AVisualCoordConverter.Projection;

  VCenterPixelXY := AVisualCoordConverter.LocalPixel2MapPixel(
    AVisualCoordConverter.LonLat2LocalPixel(
      AVisualCoordConverter.GetCenterLonLat,
      prToTopLeft
    ),
    prToTopLeft
  );
  VProjection.ValidatePixelPosStrict(VCenterPixelXY, False);
  VStartLonLat := VProjection.PixelPos2LonLat(VCenterPixelXY);
  VFinishLonLat := VProjection.ProjectionType.Datum.CalcFinishPosition(VStartLonLat, 0, VFullLenght);
  VProjection.ProjectionType.ValidateLonLatPos(VFinishLonLat);
  VFinishPixelXY := VProjection.LonLat2PixelPosFloat(VFinishLonLat);

  VHeight := Abs(VCenterPixelXY.Y - Round(VFinishPixelXY.Y));

  if VHeight <= 0 then begin
    AFullLenght := -1;
  end else begin
    AFullLenght := VFullLenght;
    AHeight := VHeight;
  end;
end;

function TWindowLayerScaleLineVertical.GetNewBitmapSize: TPoint;
begin
  Result.X := 100;
  Result.Y := Config.Width + 10;
end;

function TWindowLayerScaleLineVertical.GetNewVisibility: Boolean;
begin
  Result := Config.Visible and Config.Extended;
end;

end.
