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
  Controls,
  Classes,
  GR32,
  GR32_Image,
  i_NotifierOperation,
  i_LocalCoordConverter,
  i_LocalCoordConverterChangeable,
  i_InternalPerformanceCounter,
  i_ViewPortState,
  i_ScaleLineConfig,
  i_LanguageManager,
  u_LayerScaleLinePopupMenu,
  u_WindowLayerWithPos;

type
  TLayerScaleLine = class(TWindowLayerWithBitmapBase)
  private
    FConfig: IScaleLineConfig;
    FView: ILocalCoordConverterChangeable;
    FTmpBitmap: TBitmap32;
    FPopupMenu: TLayerScaleLinePopupMenu;
    procedure OnConfigChange;
    procedure OnPosChange;
    procedure OnMouseDown(
      Sender: TObject;
      Button: TMouseButton;
      Shift: TShiftState;
      X, Y: Integer
    );

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
    procedure ModifyLenAndHeight(
      const AVisualCoordConverter: ILocalCoordConverter;
      var AFullLenght: Double;
      var AHeight: Integer
    );
  protected
    function GetNewBitmapSize: TPoint; override;
    function GetNewLayerLocation: TFloatRect; override;
    procedure DoUpdateBitmapDraw; override;
    procedure DoUpdateLayerVisibility; override;
    procedure StartThreads; override;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      AParentMap: TImage32;
      const AView: ILocalCoordConverterChangeable;
      const AOnOptionsClick: TNotifyEvent;
      const AConfig: IScaleLineConfig
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  GR32_Layers,
  GR32_Resamplers,
  i_CoordConverter,
  u_ListenerByEvent,
  u_ResStrings,
  u_GeoFun,
  t_GeoTypes;

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

{ TLayerScaleLine }

constructor TLayerScaleLine.Create(
  const ALanguageManager: ILanguageManager;
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const AView: ILocalCoordConverterChangeable;
  const AOnOptionsClick: TNotifyEvent;
  const AConfig: IScaleLineConfig
);
begin
  inherited Create(
    APerfList,
    AAppStartedNotifier,
    AAppClosingNotifier,
    TBitmapLayer.Create(AParentMap.Layers)
  );
  FConfig := AConfig;
  FView := AView;

  FPopupMenu := TLayerScaleLinePopupMenu.Create(
    ALanguageManager,
    AParentMap,
    AConfig,
    AOnOptionsClick
  );

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnPosChange),
    FView.GetChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FConfig.GetChangeNotifier
  );

  Layer.AlphaHit := True;
  Layer.OnMouseDown := OnMouseDown;

  Layer.Bitmap.Font.Name := FConfig.FontName;
  Layer.Bitmap.Font.Size := FConfig.FontSize;

  FTmpBitmap := TBitmap32.Create;
  FTmpBitmap.Font := Layer.Bitmap.Font;
  FTmpBitmap.Font.Size := Layer.Bitmap.Font.Size;
end;

destructor TLayerScaleLine.Destroy;
begin
  FTmpBitmap.Free;
  FPopupMenu.Free;
  inherited Destroy;
end;

procedure TLayerScaleLine.OnConfigChange;
begin
  ViewUpdateLock;
  try
    Visible := FConfig.Visible;
    SetNeedUpdateBitmapDraw;
    SetNeedUpdateLayerLocation;
    SetNeedUpdateLayerVisibility;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TLayerScaleLine.OnPosChange;
begin
  ViewUpdateLock;
  try
    SetNeedUpdateBitmapDraw;
    SetNeedUpdateLayerLocation;
    SetNeedUpdateLayerVisibility;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TLayerScaleLine.OnMouseDown(
  Sender: TObject;
  Button: TMouseButton;
  Shift: TShiftState;
  X, Y: Integer
);
begin
  if Button = mbRight then begin
    FPopupMenu.PopUp;
  end;
end;

procedure TLayerScaleLine.StartThreads;
begin
  inherited;
  OnConfigChange;
end;

procedure TLayerScaleLine.DoUpdateLayerVisibility;
begin
  inherited;
  Layer.MouseEvents := Visible;
end;

procedure TLayerScaleLine.DrawOutLinedText(
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
    FTmpBitmap.Bits,
    FTmpBitmap.Width,
    FTmpBitmap.Height,
    FTmpBitmap.BoundsRect,
    dmOpaque
  );
end;

procedure TLayerScaleLine.DoUpdateBitmapDraw;
var
  VVisualCoordConverter: ILocalCoordConverter;
begin
  inherited;
  Layer.Bitmap.Clear(0);
  VVisualCoordConverter := FView.GetStatic;
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
  VBitmapSize := Types.Point(ATargetBitmap.Width, ATargetBitmap.Height);
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
  VConverter.CheckPixelPosStrict(VStartPixel, VZoom, True);
  VFinishPixel := Types.Point(VStartPixel.X + 1, VStartPixel.Y);
  VConverter.CheckPixelPos(VFinishPixel, VZoom, True);
  VStartLonLat := VConverter.PixelPos2LonLat(VStartPixel, VZoom);
  VFinishLonLat := VConverter.PixelPos2LonLat(VFinishPixel, VZoom);
  Result := VConverter.Datum.CalcDist(VStartLonLat, VFinishLonLat) * ALineWidth;
end;

procedure TLayerScaleLine.ModifyLenAndWidth(
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

{$ENDREGION}// Gorizontal Scale Legend

{$REGION 'Vertical Scale Legend'}

procedure TLayerScaleLine.RedrawVerticalScaleLegend(const AVisualCoordConverter: ILocalCoordConverter);
var
  VUnitsString: string;
  VFullLenght, VHalfLenght: Double;
  VColor: TColor32;
  VOutLineColor: TColor32;
  VValidLegendHeight: Integer;
  VHalfValue, VFullValue: string;
begin
  VColor := FConfig.Color;
  VOutLineColor := FConfig.OutLineColor;

  VValidLegendHeight := (FConfig.Width div 4) * 4;

  GetMetersPerVerticalLine(AVisualCoordConverter, VValidLegendHeight, VHalfLenght, VFullLenght);

  if FConfig.NumbersFormat = slnfNice then begin
    ModifyLenAndHeight(AVisualCoordConverter, VFullLenght, VValidLegendHeight);
  end;

  if (VHalfLenght < 0) or (VFullLenght < 0) then begin
    DrawVerticalScaleLegend(
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

  case FConfig.NumbersFormat of
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

  DrawVerticalScaleLegend(
    VColor,
    VOutLineColor,
    VColor,
    VValidLegendHeight,
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

procedure TLayerScaleLine.GetMetersPerVerticalLine(
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

  VCenterPixelXY := RectCenter(AVisualCoordConverter.GetRectInMapPixel);

  VStartLonLat := VConverter.PixelPosFloat2LonLat(VCenterPixelXY, VZoom);

  VFinishPixelXY := DoublePoint(VCenterPixelXY.X, VCenterPixelXY.Y - ALineHeight / 2);
  if VConverter.CheckPixelPosFloatStrict(VFinishPixelXY, VZoom, False) then begin
    VFinishLonLat := VConverter.PixelPosFloat2LonLat(
      VFinishPixelXY,
      VZoom
    );
    AHalfLen := VConverter.Datum.CalcDist(VStartLonLat, VFinishLonLat);
  end else begin
    AHalfLen := -1;
  end;

  VFinishPixelXY := DoublePoint(VCenterPixelXY.X, VCenterPixelXY.Y - ALineHeight);
  if VConverter.CheckPixelPosFloatStrict(VFinishPixelXY, VZoom, False) then begin
    VFinishLonLat := VConverter.PixelPosFloat2LonLat(
      VFinishPixelXY,
      VZoom
    );
    AFullLen := VConverter.Datum.CalcDist(VStartLonLat, VFinishLonLat);
  end else begin
    AFullLen := -1;
  end;
end;

procedure TLayerScaleLine.ModifyLenAndHeight(
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

  VStartLonLat := VConverter.PixelPos2LonLat(VCenterPixelXY, VZoom);
  VFinishLonLat := VConverter.Datum.CalcFinishPosition(VStartLonLat, 0, VFullLenght);
  VConverter.CheckLonLatPos(VFinishLonLat);
  VFinishPixelXY := VConverter.LonLat2PixelPosFloat(VFinishLonLat, VZoom);

  VHeight := Abs(VCenterPixelXY.Y - Round(VFinishPixelXY.Y));

  if VHeight <= 0 then begin
    AFullLenght := -1;
  end else begin
    AFullLenght := VFullLenght;
    AHeight := VHeight;
  end;
end;

function TLayerScaleLine.GetNewBitmapSize: TPoint;
begin
  Result.X := 400;
  Result.Y := 400;
end;

function TLayerScaleLine.GetNewLayerLocation: TFloatRect;
var
  VSize: TPoint;
begin
  VSize := Types.Point(Layer.Bitmap.Width, Layer.Bitmap.Height);
  Result.Left := 6;
  Result.Bottom := FView.GetStatic.GetLocalRect.Bottom - 6 - FConfig.BottomMargin;
  Result.Right := Result.Left + VSize.X;
  Result.Top := Result.Bottom - VSize.Y;
end;

{$ENDREGION}// Vertical Scale Legend

end.
