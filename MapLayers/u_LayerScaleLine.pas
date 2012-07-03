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

unit u_LayerScaleLine;

interface

uses
  Types,
  GR32,
  GR32_Image,
  i_LocalCoordConverter,
  i_InternalPerformanceCounter,
  i_ViewPortState,
  i_ScaleLineConfig,
  u_WindowLayerWithPos;

type
  TLayerScaleLine = class(TWindowLayerFixedSizeWithBitmap)
  private
    FConfig: IScaleLineConfig;
    FTmpBitmap: TBitmap32;
    procedure OnConfigChange;
    procedure DrawOutLinedText(
      X, Y: Integer;
      const Text: string;
      TextColor: TColor32;
      OutLineColor: TColor32;
      TargetBitmap: TBitmap32
    );
    // gorizontal
    procedure RedrawGorizontalScaleLegend(const AVisualCoordConverter: ILocalCoordConverter);
    procedure DrawGorizontalScaleLegend(
      ALineColor: TColor32;
      AOutLineColor: TColor32;
      ATextColor: TColor32;
      AScaleLegendWidth: Integer;
      const AHalfValue, AFullValue: string;
      ATargetBitmap: TBitmap32
    );
    procedure DrawGorizontalScaleMarks(
      ALineColor: TColor32;
      AOutLineColor: TColor32;
      ATextColor: TColor32;
      const AText: string;
      AScalePos: Integer;
      ATargetBitmap: TBitmap32
    );
    function GetMetersPerGorizontalLine(
      const AVisualCoordConverter: ILocalCoordConverter;
      ALineWidth: Integer
    ): Double;
    procedure ModifyLenAndWidth(
      var ALen: Double;
      var AWidth: Integer
    );
    // vertical
    procedure RedrawVerticalScaleLegend(const AVisualCoordConverter: ILocalCoordConverter);
    procedure DrawVerticalScaleLegend(
      ALineColor: TColor32;
      AOutLineColor: TColor32;
      ATextColor: TColor32;
      AScaleLegendHeight: Integer;
      const AHalfValue, AFullValue: string;
      ATargetBitmap: TBitmap32
    );
    procedure DrawVerticalScaleMarks(
      ALineColor, AOutLineColor, ATextColor: TColor32;
      const AText: string;
      AScalePos: Integer;
      ATargetBitmap: TBitmap32
    );
    procedure GetMetersPerVerticalLine(
      const AVisualCoordConverter: ILocalCoordConverter;
      ALineHeight: Integer;
      out AHalfLen: Double;
      out AFullLen: Double
    );
  protected
    procedure SetLayerCoordConverter(
      const AValue: ILocalCoordConverter
    ); override;
    function GetMapLayerLocationRect(const ANewVisualCoordConverter: ILocalCoordConverter): TFloatRect; override;
    procedure DoRedraw; override;
  public
    procedure StartThreads; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      AParentMap: TImage32;
      const AViewPortState: IViewPortState;
      const AConfig: IScaleLineConfig
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  GR32_Resamplers,
  i_CoordConverter,
  u_ListenerByEvent,
  u_ResStrings,
  u_GeoFun,
  t_GeoTypes;

{ TLayerScaleLine }

constructor TLayerScaleLine.Create(
  const APerfList: IInternalPerformanceCounterList;
  AParentMap: TImage32;
  const AViewPortState: IViewPortState;
  const AConfig: IScaleLineConfig
);
var
  VSize: TPoint;
begin
  inherited Create(APerfList, AParentMap, AViewPortState);
  FConfig := AConfig;
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FConfig.GetChangeNotifier
  );

  Layer.Bitmap.Font.Name := FConfig.FontName;
  Layer.Bitmap.Font.Size := FConfig.FontSize;

  FTmpBitmap := TBitmap32.Create;
  FTmpBitmap.Font := Layer.Bitmap.Font;
  FTmpBitmap.Font.Size := Layer.Bitmap.Font.Size;

  VSize.X := 400;
  VSize.Y := 400;

  Layer.Bitmap.SetSize(VSize.X, VSize.Y);
  DoUpdateLayerSize(VSize);
end;

destructor TLayerScaleLine.Destroy;
begin
  FTmpBitmap.Free;
  inherited Destroy;
end;

function TLayerScaleLine.GetMapLayerLocationRect(const ANewVisualCoordConverter: ILocalCoordConverter): TFloatRect;
var
  VSize: TPoint;
begin
  if ANewVisualCoordConverter <> nil then begin
    VSize := Point(Layer.Bitmap.Width, Layer.Bitmap.Height);
    Result.Left := 6;
    Result.Bottom := ANewVisualCoordConverter.GetLocalRectSize.Y - 6 - FConfig.BottomMargin;
    Result.Right := Result.Left + VSize.X;
    Result.Top := Result.Bottom - VSize.Y;
  end;
end;

procedure TLayerScaleLine.OnConfigChange;
begin
  ViewUpdateLock;
  try
    SetVisible(FConfig.Visible);
    SetNeedRedraw;
    SetNeedUpdateLocation;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TLayerScaleLine.SetLayerCoordConverter(const AValue: ILocalCoordConverter);
begin
  inherited;
  SetNeedRedraw;
end;

procedure TLayerScaleLine.StartThreads;
begin
  inherited;
  OnConfigChange;
end;

procedure TLayerScaleLine.DrawOutlinedText(
  X, Y: Integer;
  const Text: string;
  TextColor: TColor32;
  OutLineColor: TColor32;
  TargetBitmap: TBitmap32
);
var
  I, J: Integer;
begin
  FTmpBitmap.SetSize(FTmpBitmap.TextWidth(Text) + 4, FTmpBitmap.TextHeight(Text) + 4);
  FTmpBitmap.Clear(0);
  FTmpBitmap.RenderText(2, 2, Text, 0, TextColor);
  for I := 1 to FTmpBitmap.Width - 2 do begin
    for J := 1 to FTmpBitmap.Height - 2 do begin
      if (FTmpBitmap.Pixel[I, J] <> TextColor) and (FTmpBitmap.Pixel[I, J] <> OutLineColor) then begin
        if (FTmpBitmap.Pixel[I + 1, J] = TextColor) or
          (FTmpBitmap.Pixel[I - 1, J] = TextColor) or
          (FTmpBitmap.Pixel[I, J + 1] = TextColor) or
          (FTmpBitmap.Pixel[I, J - 1] = TextColor) or
          (FTmpBitmap.Pixel[I + 1, J + 1] = TextColor) or
          (FTmpBitmap.Pixel[I - 1, J + 1] = TextColor) or
          (FTmpBitmap.Pixel[I + 1, J - 1] = TextColor) or
          (FTmpBitmap.Pixel[I - 1, J - 1] = TextColor) then begin
          FTmpBitmap.Pixel[I, J] := OutLineColor;
        end;
      end;
    end;
  end;
  BlockTransfer(
    TargetBitmap,
    X, Y,
    TargetBitmap.ClipRect,
    FTmpBitmap,
    FTmpBitmap.BoundsRect,
    dmOpaque
  );
end;

procedure TLayerScaleLine.DoRedraw;
var
  VVisualCoordConverter: ILocalCoordConverter;
begin
  inherited;
  Layer.Bitmap.Clear(0);
  VVisualCoordConverter := LayerCoordConverter;
  if VVisualCoordConverter <> nil then begin
    RedrawGorizontalScaleLegend(VVisualCoordConverter);
    if FConfig.Extended then begin
      RedrawVerticalScaleLegend(VVisualCoordConverter);
    end;
  end;
end;

{$REGION 'Gorizontal Scale Legend'}

procedure TLayerScaleLine.RedrawGorizontalScaleLegend(const AVisualCoordConverter: ILocalCoordConverter);
var
  VUnitsString: string;
  num: Double;
  rnum: Integer;
  VColor: TColor32;
  VOutLineColor: TColor32;
  VValidLegendWidth: Integer;
  VHalfValue, VFullValue: string;
begin
  VColor := FConfig.Color;
  VOutLineColor := FConfig.OutLineColor;

  VValidLegendWidth := (FConfig.Width div 4) * 4;

  num := GetMetersPerGorizontalLine(AVisualCoordConverter, VValidLegendWidth);

  if FConfig.NumbersFormat = slnfNice then begin
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

  case FConfig.NumbersFormat of
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

  DrawGorizontalScaleLegend(
    VColor,
    VOutLineColor,
    VColor,
    VValidLegendWidth,
    VHalfValue,
    VFullValue,
    Layer.Bitmap
  );
end;

procedure TLayerScaleLine.DrawGorizontalScaleLegend(
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
  VBitmapSize := Point(ATargetBitmap.Width, ATargetBitmap.Height);
  ATargetBitmap.Line(0, VBitmapSize.Y - 3, VWidth + 2, VBitmapSize.Y - 3, AOutLineColor);
  for I := 0 to 4 do begin
    VStartX := I * (VWidth div 4);
    case I of
      0: begin
        if not FConfig.Extended then begin
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
    DrawGorizontalScaleMarks(
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

procedure TLayerScaleLine.DrawGorizontalScaleMarks(
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
    DrawOutlinedText(
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

function TLayerScaleLine.GetMetersPerGorizontalLine(
  const AVisualCoordConverter: ILocalCoordConverter;
  ALineWidth: Integer
): Double;
var
  VStartLonLat, VFinishLonLat: TDoublePoint;
  VStartPixel, VFinishPixel: TPoint;
  VConverter: ICoordConverter;
  VZoom: Byte;
begin
  VZoom := AVisualCoordConverter.GetZoom;
  VConverter := AVisualCoordConverter.GetGeoConverter;
  VStartPixel := PointFromDoublePoint(AVisualCoordConverter.GetCenterMapPixelFloat, prToTopLeft);
  VFinishPixel := Point(VStartPixel.X + 1, VStartPixel.Y);
  VStartLonLat := VConverter.PixelPos2LonLat(VStartPixel, VZoom);
  VFinishLonLat := VConverter.PixelPos2LonLat(VFinishPixel, VZoom);
  Result := VConverter.Datum.CalcDist(VStartLonLat, VFinishLonLat) * ALineWidth;
end;

procedure TLayerScaleLine.ModifyLenAndWidth(
  var ALen: Double;
  var AWidth: Integer
);
  function GetNiceLen(ALen: Double): Double;
  const
    CNiceValues: array [0..54] of Double =
      (
      40000000,
      30000000,
      20000000,
      15000000,
      10000000,
      8000000,
      5000000,
      4000000,
      3000000,
      2000000,
      1500000,
      1000000,
      800000,
      500000,
      400000,
      300000,
      200000,
      150000,
      100000,
      80000,
      50000,
      40000,
      30000,
      20000,
      15000,
      10000,
      8000,
      5000,
      4000,
      3000,
      2000,
      1500,
      1000,
      800,
      500,
      400,
      300,
      200,
      150,
      100,
      80,
      50,
      40,
      30,
      20,
      15,
      10,
      8,
      6,
      4,
      3,
      2,
      1.5,
      1,
      0.5
      );
  var
    i: Integer;
  begin
    for i := 0 to Length(CNiceValues) - 1 do begin
      Result := CNiceValues[i];
      if ALen > Result then begin
        Break;
      end;
    end;
  end;

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

{$ENDREGION}// Gorizontal Scale Legend

{$REGION 'Vertical Scale Legend'}

procedure TLayerScaleLine.RedrawVerticalScaleLegend(const AVisualCoordConverter: ILocalCoordConverter);
var
  VUnitsString: string;
  VFullLenght, VHalfLenght: Double;
  VColor: TColor32;
  VOutLineColor: TColor32;
  VValidLegendWidth: Integer;
  VHalfValue, VFullValue: string;
begin
  VColor := FConfig.Color;
  VOutLineColor := FConfig.OutLineColor;

  VValidLegendWidth := (FConfig.Width div 4) * 4;

  GetMetersPerVerticalLine(AVisualCoordConverter, VValidLegendWidth, VHalfLenght, VFullLenght);

  if VFullLenght > 10000 then begin
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

  case FConfig.NumbersFormat of

    // slnfNice: // TODO

    slnfScienceRound: begin
      VHalfValue := IntToStr(Round(VHalfLenght)) + VUnitsString;
      VFullValue := IntToStr(Round(VFullLenght)) + VUnitsString;
    end else begin
    VHalfValue := FloatToStrF(VHalfLenght, ffFixed, 10, 2) + VUnitsString;
    VFullValue := FloatToStrF(VFullLenght, ffFixed, 10, 2) + VUnitsString;
  end;
  end;

  DrawVerticalScaleLegend(
    VColor,
    VOutLineColor,
    VColor,
    VValidLegendWidth,
    VHalfValue,
    VFullValue,
    Layer.Bitmap
  );
end;

procedure TLayerScaleLine.DrawVerticalScaleLegend(
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
  VBitmapSize := Point(ATargetBitmap.Width, ATargetBitmap.Height);
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
      DrawVerticalScaleMarks(
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

procedure TLayerScaleLine.DrawVerticalScaleMarks(
  ALineColor, AOutLineColor, ATextColor: TColor32;
  const AText: string;
  AScalePos: Integer;
  ATargetBitmap: TBitmap32
);
var
  VStartX: Integer;
begin
  if Length(AText) > 0 then begin
    DrawOutlinedText(
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

procedure TLayerScaleLine.GetMetersPerVerticalLine(
  const AVisualCoordConverter: ILocalCoordConverter;
  ALineHeight: Integer;
  out AHalfLen, AFullLen: Double
);
var
  VStartLonLat, VFinishLonLat: TDoublePoint;
  VCenterPixelXY: TPoint;
  VConverter: ICoordConverter;
  VZoom: Byte;
begin
  VZoom := AVisualCoordConverter.GetZoom;
  VConverter := AVisualCoordConverter.GetGeoConverter;

  VCenterPixelXY := AVisualCoordConverter.LocalPixel2MapPixel(
    AVisualCoordConverter.LonLat2LocalPixel(
      AVisualCoordConverter.GetCenterLonLat
    )
  );

  VStartLonLat := VConverter.PixelPos2LonLat(VCenterPixelXY, VZoom);
  VFinishLonLat := VConverter.PixelPos2LonLat(
    Point(VCenterPixelXY.X, VCenterPixelXY.Y - (ALineHeight div 2)),
    VZoom
  );
  AHalfLen := VConverter.Datum.CalcDist(VStartLonLat, VFinishLonLat);

  VStartLonLat := VConverter.PixelPos2LonLat(VCenterPixelXY, VZoom);
  VFinishLonLat := VConverter.PixelPos2LonLat(
    Point(VCenterPixelXY.X, VCenterPixelXY.Y - ALineHeight),
    VZoom
  );
  AFullLen := VConverter.Datum.CalcDist(VStartLonLat, VFinishLonLat);
end;

{$ENDREGION}// Vertical Scale Legend

end.
