{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
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

unit u_WindowLayerScaleLineHorizontal;

interface

uses
  Types,
  GR32,
  i_LocalCoordConverter,
  u_WindowLayerScaleLineBase;

type
  TWindowLayerScaleLineHorizontal = class(TWindowLayerScaleLineBase)
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
      const AScaleLegendWidth: Integer;
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
    function GetMetersPerLine(
      const AVisualCoordConverter: ILocalCoordConverter;
      const ALineWidth: Integer
    ): Double;
    procedure ModifyLenAndWidth(
      var ALen: Double;
      var AWidth: Integer
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

{ TWindowLayerScaleLineHorizontal }

procedure TWindowLayerScaleLineHorizontal.DoUpdateBitmapDraw;
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

procedure TWindowLayerScaleLineHorizontal.RedrawScaleLegend(
  const ABitmap: TBitmap32;
  const AVisualCoordConverter: ILocalCoordConverter
);
var
  VUnitsString: string;
  VFullLenght: Double;
  VColor: TColor32;
  VOutLineColor: TColor32;
  VValidLegendWidth: Integer;
  VDigits: Integer;
  VHalfValue, VFullValue: string;
begin
  VColor := Config.Color;
  VOutLineColor := Config.OutLineColor;

  VValidLegendWidth := (Config.Width div 4) * 4;

  VFullLenght := GetMetersPerLine(AVisualCoordConverter, VValidLegendWidth);
  if VFullLenght <= 0 then begin
    DrawScaleLegend(
      ABitmap,
      VColor,
      VOutLineColor,
      VColor,
      VValidLegendWidth,
      ' ',
      ' '
    );
    Exit;
  end;

  if Config.NumbersFormat = slnfNice then begin
    ModifyLenAndWidth(VFullLenght, VValidLegendWidth);
  end;

  if VFullLenght > 10000 then begin
    VFullLenght := VFullLenght / 1000;
    VUnitsString := SAS_UNITS_km + ' ';
  end else
  if VFullLenght < 10 then begin
    VFullLenght := VFullLenght * 100;
    VUnitsString := SAS_UNITS_sm + ' ';
  end else begin
    VUnitsString := SAS_UNITS_m + ' ';
  end;

  case Config.NumbersFormat of
    slnfNice: begin
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

  VHalfValue := ValueToStr(VFullLenght / 2, VDigits, VUnitsString);
  VFullValue := ValueToStr(VFullLenght, VDigits, VUnitsString);

  DrawScaleLegend(
    ABitmap,
    VColor,
    VOutLineColor,
    VColor,
    VValidLegendWidth,
    VHalfValue,
    VFullValue
  );
end;

procedure TWindowLayerScaleLineHorizontal.DrawScaleLegend(
  const ABitmap: TBitmap32;
  const ALineColor: TColor32;
  const AOutLineColor: TColor32;
  const ATextColor: TColor32;
  const AScaleLegendWidth: Integer;
  const AHalfValue, AFullValue: string
);
var
  I: Integer;
  VWidth: Integer;
  VBitmapSize: TPoint;
  VStartX: Integer;
  VText: string;
begin
  VWidth := (AScaleLegendWidth div 4) * 4;
  VBitmapSize := Types.Point(ABitmap.Width, ABitmap.Height);
  ABitmap.Line(0, VBitmapSize.Y - 3, VWidth + 2, VBitmapSize.Y - 3, AOutLineColor);
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
    else
      VText := '';
    end;
    DrawScaleMarks(
      ABitmap,
      ALineColor,
      AOutLineColor,
      ATextColor,
      VText,
      VStartX + 1
    );
  end;
  ABitmap.Line(1, VBitmapSize.Y - 2, VWidth + 2, VBitmapSize.Y - 2, ALineColor);
  ABitmap.Line(0, VBitmapSize.Y - 1, VWidth + 2, VBitmapSize.Y - 1, AOutLineColor);
end;

procedure TWindowLayerScaleLineHorizontal.DrawScaleMarks(
  const ABitmap: TBitmap32;
  const ALineColor, AOutLineColor, ATextColor: TColor32;
  const AText: string;
  const AScalePos: Integer
);
var
  VStartY: Integer;
  VHeight: Integer;
begin
  if Length(AText) > 0 then begin
    DrawOutLinedText(
      ABitmap,
      AScalePos,
      ABitmap.Height - 36,
      AText,
      ATextColor,
      AOutLineColor
    );
  end;
  VHeight := ABitmap.Height;
  if Length(AText) = 0 then begin
    VStartY := VHeight - 10;
  end else begin
    VStartY := VHeight - 20;
  end;
  ABitmap.Line(AScalePos - 1, VStartY, AScalePos - 1, VHeight - 1, AOutLineColor);
  ABitmap.Line(AScalePos, VStartY, AScalePos, VHeight - 1, ALineColor);
  ABitmap.Line(AScalePos + 1, VStartY, AScalePos + 1, VHeight - 1, AOutLineColor);
  ABitmap.Line(AScalePos - 1, VStartY, AScalePos + 1, VStartY, AOutLineColor);
end;

function TWindowLayerScaleLineHorizontal.GetMetersPerLine(
  const AVisualCoordConverter: ILocalCoordConverter;
  const ALineWidth: Integer
): Double;
var
  VStartLonLat, VFinishLonLat: TDoublePoint;
  VStartPixel, VFinishPixel: TDoublePoint;
  VProjection: IProjection;
begin
  VProjection := AVisualCoordConverter.Projection;
  VStartPixel := AVisualCoordConverter.GetCenterMapPixelFloat;
  VProjection.ValidatePixelPosFloatStrict(VStartPixel, True);
  VFinishPixel := DoublePoint(VStartPixel.X + 1, VStartPixel.Y);
  VProjection.ValidatePixelPosFloat(VFinishPixel, True);
  VStartLonLat := VProjection.PixelPosFloat2LonLat(VStartPixel);
  VProjection.ProjectionType.ValidateLonLatPos(VStartLonLat);
  VFinishLonLat := VProjection.PixelPosFloat2LonLat(VFinishPixel);
  VProjection.ProjectionType.ValidateLonLatPos(VFinishLonLat);
  Result := VProjection.ProjectionType.Datum.CalcDist(VStartLonLat, VFinishLonLat) * ALineWidth;
end;

procedure TWindowLayerScaleLineHorizontal.ModifyLenAndWidth(
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

function TWindowLayerScaleLineHorizontal.GetNewBitmapSize: TPoint;
begin
  Result.X := Config.Width + 100;
  Result.Y := 50;
end;

function TWindowLayerScaleLineHorizontal.GetNewVisibility: Boolean;
begin
  Result := Config.Visible;
end;

end.
