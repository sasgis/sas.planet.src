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
      Text: string;
      TextColor: TColor32;
      OutLineColor: TColor32;
      TargetBitmap: TBitmap32
    );
    function GetMetersPerGorizontalLine(ALineWidth: Integer): Double;
    procedure DrawExtendedScaleLegend(
      ALineColor: TColor32;
      AOutLineColor: TColor32;
      ATextColor: TColor32;
      AScaleLegendWidth: Integer;
      AHalfValue, AFullValue: string;
      ATargetBitmap: TBitmap32
    );
    procedure ModifyLenAndWidth(var ALen: Double; var AWidth: Integer);
    procedure DrawScaleMarks(
      ALineColor: TColor32;
      AOutLineColor: TColor32;
      ATextColor: TColor32;
      AText: string;
      AScalePos: Integer;
      ATargetBitmap: TBitmap32
    );
  protected
    procedure SetLayerCoordConverter(AValue: ILocalCoordConverter); override;
    function GetMapLayerLocationRect: TFloatRect; override;
    procedure DoRedraw; override;
  public
    procedure StartThreads; override;
  public
    constructor Create(
      APerfList: IInternalPerformanceCounterList;
      AParentMap: TImage32;
      AViewPortState: IViewPortState;
      AConfig: IScaleLineConfig
    );
    destructor Destroy; override;
  end;

implementation

uses
  Math,
  SysUtils,
  i_CoordConverter,
  u_NotifyEventListener,
  u_ResStrings,
  t_GeoTypes;

function RoundTo(Value, N: Integer): Integer;
{=========================================================
  «Округление» до ближайшего кратного.

  Функция возвращает ближайшее к Value число, которoе без
  остатка делится на N. Если Value находится посередине
  между двумя кратными, функция вернёт большее значение.
=========================================================}
asm
   push ebx
   mov ebx, eax
   mov ecx, edx
   cdq
   idiv ecx
   imul ecx

   add ecx, eax
   mov edx, ebx
   sub ebx, eax
   jg @@10
   neg ebx
@@10:
   sub edx, ecx
   jg @@20
   neg edx
@@20:
   cmp ebx, edx
   jl @@30
   mov eax, ecx
@@30:
   pop ebx
end;

{ TLayerScaleLine }

constructor TLayerScaleLine.Create(
  APerfList: IInternalPerformanceCounterList;
  AParentMap: TImage32;
  AViewPortState: IViewPortState;
  AConfig: IScaleLineConfig
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

  FLayer.Bitmap.Font.Name := FConfig.FontName;
  FLayer.Bitmap.Font.Size := FConfig.FontSize;

  FTmpBitmap := TBitmap32.Create;
  FTmpBitmap.Font := FLayer.Bitmap.Font;
  FTmpBitmap.Font.Size := FLayer.Bitmap.Font.Size;

  VSize.X := 400;
  VSize.Y := 40;

  FLayer.Bitmap.SetSize(VSize.X, VSize.Y);
  DoUpdateLayerSize(VSize);
end;

destructor TLayerScaleLine.Destroy;
begin
  FTmpBitmap.Free;
  inherited Destroy;
end;

procedure TLayerScaleLine.DoRedraw;
var
  VUnitsString: string;
  num: Double;
  rnum, rnum_nice: Integer;
  VColor: TColor32;
  VOutLineColor: TColor32;
  VValidLegendWidth: Integer;
  VHalfValue, VFullValue: string;
begin
  inherited;

  VColor := FConfig.Color;
  VOutLineColor := FConfig.OutLineColor;

  VValidLegendWidth := (FConfig.Width div 4) * 4;

  num := GetMetersPerGorizontalLine(VValidLegendWidth);
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
    slnfNice:
      begin
        rnum := Round(num/2);
        if rnum > 1000 then begin
          rnum_nice := RoundTo(rnum, 10);
        end else begin
          rnum_nice := RoundTo(rnum, 5);
        end;
        VHalfValue := IntToStr(rnum_nice) + VUnitsString;
        VFullValue := IntToStr(rnum_nice*2) + VUnitsString;
      end;
    slnfScienceRound:
      begin
        rnum := Round(num/2);
        VHalfValue := IntToStr(rnum) + VUnitsString;
        VFullValue := IntToStr(rnum*2) + VUnitsString;
      end
  else
    begin
      VHalfValue := FloatToStrF(num/2, ffFixed, 10, 2) + VUnitsString;
      VFullValue := FloatToStrF(num, ffFixed, 10, 2) + VUnitsString;
    end;
  end;

  DrawExtendedScaleLegend(
    VColor,
    VOutLineColor,
    VColor,
    VValidLegendWidth,
    VHalfValue,
    VFullValue,
    FLayer.Bitmap
  );
end;

procedure TLayerScaleLine.DrawExtendedScaleLegend(
  ALineColor: TColor32;
  AOutLineColor: TColor32;
  ATextColor: TColor32;
  AScaleLegendWidth: Integer;
  AHalfValue, AFullValue: string;
  ATargetBitmap: TBitmap32
);
var
  I: Integer;
  VWidth: Integer;
  VBitmapSize: TPoint;
  VStartX: Integer;
  VText: string;
begin
  ATargetBitmap.Clear(0);
  VWidth := (AScaleLegendWidth div 4) * 4;

  VBitmapSize := Point(ATargetBitmap.Width, ATargetBitmap.Height);

  // длинная горизонталь снизу
  ATargetBitmap.Line(0, VBitmapSize.Y - 3, VWidth + 2, VBitmapSize.Y - 3, AOutLineColor);

  // заборчик: длинная/короткая вертикали
  DrawScaleMarks(
    ALineColor,
    AOutLineColor,
    ATextColor,
    '0',
    1,
    ATargetBitmap
  );

  for I := 1 to 4 do begin
    VStartX := 1 + I * (VWidth div 4);
    if (I = 2) then begin
      VText := AHalfValue;
    end else if I = 4 then begin
      VText := AFullValue;
    end else begin
      VText := '';
    end;
    DrawScaleMarks(
      ALineColor,
      AOutLineColor,
      ATextColor,
      VText,
      VStartX,
      ATargetBitmap
    );
  end;

  // дорисовываем горизонталь снизу
  ATargetBitmap.Line(1, VBitmapSize.Y - 2, VWidth + 2, VBitmapSize.Y - 2, ALineColor);
  ATargetBitmap.Line(0, VBitmapSize.Y - 1, VWidth + 2, VBitmapSize.Y - 1, AOutLineColor);
end;

procedure TLayerScaleLine.DrawScaleMarks(
  ALineColor, AOutLineColor, ATextColor: TColor32;
  AText: string;
  AScalePos: Integer;
  ATargetBitmap: TBitmap32
);
var
  VStartY: Integer;
  VHeight: Integer;
begin
  VHeight := ATargetBitmap.Height;
  if Length(AText) = 0 then begin
    VStartY := VHeight - 10; // короткая вертикаль
  end else begin
    VStartY := VHeight - 20; // длинная вертикаль
  end;
  // вертикаль
  ATargetBitmap.Line(AScalePos - 1, VStartY, AScalePos - 1, VHeight - 1, AOutLineColor);
  ATargetBitmap.Line(AScalePos,     VStartY, AScalePos,     VHeight - 1, ALineColor);
  ATargetBitmap.Line(AScalePos + 1, VStartY, AScalePos + 1, VHeight - 1, AOutLineColor);
  // "шапка"
  ATargetBitmap.Line(AScalePos - 1, VStartY, AScalePos + 1, VStartY, AOutLineColor);
  if Length(AText) > 0 then begin
    DrawOutlinedText(
      AScalePos,
      4,
      AText,
      ATextColor,
      AOutLineColor,
      ATargetBitmap
    );
  end;
end;

function TLayerScaleLine.GetMapLayerLocationRect: TFloatRect;
var
  VSize: TPoint;
begin
  VSize := Point(FLayer.Bitmap.Width, FLayer.Bitmap.Height);
  Result.Left := 6;
  Result.Bottom := ViewCoordConverter.GetLocalRectSize.Y - 6 - FConfig.BottomMargin;
  Result.Right := Result.Left + VSize.X;
  Result.Top := Result.Bottom - VSize.Y;
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
  ViewUpdate;
end;

procedure TLayerScaleLine.SetLayerCoordConverter(AValue: ILocalCoordConverter);
begin
  inherited;
  SetNeedRedraw;
end;

procedure TLayerScaleLine.StartThreads;
begin
  inherited;
  OnConfigChange;
end;

procedure TLayerScaleLine.DrawOutlinedText(X, Y: Integer; Text: string;
  TextColor: TColor32; OutLineColor: TColor32; TargetBitmap: TBitmap32);
var
  I, J: Integer;
begin
  FTmpBitmap.SetSize(FTmpBitmap.TextWidth(Text) + 4, FTmpBitmap.TextHeight(Text) + 4);
  FTmpBitmap.Clear(0);
  FTmpBitmap.DrawMode := dmOpaque;
  FTmpBitmap.RenderText(2, 2, Text, 0, TextColor);
  for I := 1 to FTmpBitmap.Width - 2 do begin
    for J := 1 to FTmpBitmap.Height - 2 do begin
      if (FTmpBitmap.Pixel[I, J] <> TextColor) and (FTmpBitmap.Pixel[I, J] <> OutLineColor) then begin
        if
          (FTmpBitmap.Pixel[I + 1, J] = TextColor) or
          (FTmpBitmap.Pixel[I - 1, J] = TextColor) or
          (FTmpBitmap.Pixel[I, J + 1] = TextColor) or
          (FTmpBitmap.Pixel[I, J - 1] = TextColor) or
          (FTmpBitmap.Pixel[I + 1, J + 1] = TextColor) or
          (FTmpBitmap.Pixel[I - 1, J + 1] = TextColor) or
          (FTmpBitmap.Pixel[I + 1, J - 1] = TextColor) or
          (FTmpBitmap.Pixel[I - 1, J - 1] = TextColor)
        then begin
          FTmpBitmap.Pixel[I, J] := OutLineColor;
        end;
      end;
    end;
  end;
  FTmpBitmap.DrawTo(TargetBitmap, X, Y);
end;

function TLayerScaleLine.GetMetersPerGorizontalLine(ALineWidth: Integer): Double;
const
  // Константа для преобразования градусов в радианы
  CDegreeToRadians: Double = 0.017453292519943295769236907684886;
var
  VRad: Double;
  VLat: Double;
  VConverter: ICoordConverter;
  VPixelsAtZoom: Double;
  VZoom: Byte;
  VVisualCoordConverter: ILocalCoordConverter;
begin
  VVisualCoordConverter := LayerCoordConverter;
  VZoom := VVisualCoordConverter.GetZoom;
  VLat := VVisualCoordConverter.GetCenterLonLat.Y;
  VConverter := VVisualCoordConverter.GetGeoConverter;
  VRad := VConverter.Datum.GetSpheroidRadiusA;
  VPixelsAtZoom := VConverter.PixelsAtZoomFloat(VZoom);
  Result := ALineWidth / ((VPixelsAtZoom / (2 * PI)) / (VRad * cos(VLat * CDegreeToRadians)));
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

end.
