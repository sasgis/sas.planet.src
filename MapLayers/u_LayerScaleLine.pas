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
    procedure DrawOutLinedText(X, Y: Integer; Text: string;
      TextColor: TColor32; OutLineColor: TColor32; TargetBitmap: TBitmap32);
    function GetMetersPerGorizontalLine(ALineWidth: Integer): Double;
    procedure DrawExtendedScaleLegend(
      ALineColor: TColor32;
      AOutLineColor: TColor32;
      AScaleLegendWidth: Integer;
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
  num: real;
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

  FLayer.Bitmap.Clear(SetAlpha(clWhite32, 0));

  DrawOutlinedText(
    0,
    4,
    '0' + VUnitsString,
    VColor,
    VOutLineColor,
    FLayer.Bitmap
  );

  DrawOutlinedText(
    VValidLegendWidth div 2,
    4,
    VHalfValue,
    VColor,
    VOutLineColor,
    FLayer.Bitmap
  );

  DrawOutlinedText(
    VValidLegendWidth,
    4,
    VFullValue,
    VColor,
    VOutLineColor,
    FLayer.Bitmap
  );

  DrawExtendedScaleLegend(
    VColor,
    VOutLineColor,
    VValidLegendWidth,
    FLayer.Bitmap
  );
end;

procedure TLayerScaleLine.DrawExtendedScaleLegend(
  ALineColor: TColor32;
  AOutLineColor: TColor32;
  AScaleLegendWidth: Integer;
  ATargetBitmap: TBitmap32
);
var
  I: Integer;
  VWidth: Integer;
  VBitmapSize: TPoint;
  VStartX, VStartY: Integer;
begin
  VWidth := (AScaleLegendWidth div 4) * 4;

  VBitmapSize := Point(ATargetBitmap.Width, ATargetBitmap.Height);

  // длинная горизонталь снизу
  ATargetBitmap.Line(0, VBitmapSize.Y - 3, VWidth + 2, VBitmapSize.Y - 3, AOutLineColor);

  // заборчик: длинная/короткая вертикали
  for I := 0 to 4 do begin
    VStartX := 1 + I * (VWidth div 4);
    if (I = 1) or (I = 3) then begin
      VStartY := VBitmapSize.Y - 10; // короткая вертикаль
    end else begin
      VStartY := VBitmapSize.Y - 20; // длинная вертикаль
    end;
    // вертикаль
    ATargetBitmap.Line(VStartX - 1, VStartY, VStartX - 1, VBitmapSize.Y - 1, AOutLineColor);
    ATargetBitmap.Line(VStartX,     VStartY, VStartX,     VBitmapSize.Y - 1, ALineColor);
    ATargetBitmap.Line(VStartX + 1, VStartY, VStartX + 1, VBitmapSize.Y - 1, AOutLineColor);
    // "шапка"
    ATargetBitmap.Line(VStartX - 1, VStartY, VStartX + 1, VStartY, AOutLineColor);
  end;

  // дорисовываем горизонталь снизу
  ATargetBitmap.Line(1, VBitmapSize.Y - 2, VWidth + 2, VBitmapSize.Y - 2, ALineColor);
  ATargetBitmap.Line(0, VBitmapSize.Y - 1, VWidth + 2, VBitmapSize.Y - 1, AOutLineColor);
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

end.
