{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2017, SAS.Planet development team.                      *}
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

unit u_WindowLayerSunCalcYearTimeLine;

interface

uses
  GR32,  
  t_SunCalcConfig,
  i_SunCalcConfig,
  u_TimeZoneInfo,
  u_WindowLayerSunCalcTimeLineBase;

type
  TWindowLayerSunCalcYearTimeLine = class(TWindowLayerSunCalcTimeLineBase)
  private
    FColors: TSunCalcTimeLineColors;
    FFixedDates: array [0..3] of TDateTime;
    procedure UpdateFixedDatesIfNeed; inline;
  protected
    function PosToUtcDateTime(const X: Integer): TDateTime; override;
    function UtcDateTimeToPosF(const ADateTime: TDateTime): Double; override;
    procedure OnSunCalcConfigChange; override;
  protected
    procedure DoUpdateBitmapDraw; override;
  public
    procedure AfterConstruction; override;
  end;

implementation

uses
  SysUtils,
  DateUtils,
  Solstice,
  u_MarkerSimpleConfigStatic,
  u_MarkerDrawableSimpleSquare;

resourcestring
  rsSolstice = 'Solstice at';
  rsEquinox = 'Equinox at';

{ TWindowLayerSunCalcYearTimeLine }

procedure TWindowLayerSunCalcYearTimeLine.AfterConstruction;
const
  cMonthText: array [0..11] of string = (
     'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
     'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'
  );
var
  I: Integer;
begin
  inherited;

  FBorder.Left   := 5;
  FBorder.Right  := 5;
  FBorder.Top    := 3;
  FBorder.Bottom := 4;

  FTimeLineHeight := 4;

  FMarker :=
    TMarkerDrawableSimpleSquare.Create(
      TMarkerSimpleConfigStatic.Create(8, clYellow32, clRed32)
    );

  SetLength(FRenderedText, Length(cMonthText));
  for I := Low(FRenderedText) to High(FRenderedText) do begin
    FRenderedText[I].Text := cMonthText[I];
    FRenderedText[I].Bitmap := nil;
  end;

  for I := Low(FFixedDates) to High(FFixedDates) do begin
    FFixedDates[I] := 0;
  end;

  FRedrawOnDateChanged := True;
  FRedrawOnTzOffsetChanged := True;
end;

procedure TWindowLayerSunCalcYearTimeLine.UpdateFixedDatesIfNeed;
var
  VYear: Word;
begin
  VYear := YearOf(FDateTime);
  if (FFixedDates[0] = 0) or (VYear <> YearOf(FFixedDates[0])) then begin
    FFixedDates[0] := Solstice.March(VYear);
    FFixedDates[1] := Solstice.June(VYear);
    FFixedDates[2] := Solstice.September(VYear);
    FFixedDates[3] := Solstice.December(VYear);
  end;
end;

function TWindowLayerSunCalcYearTimeLine.UtcDateTimeToPosF(
  const ADateTime: TDateTime
): Double;
var
  VMonthPos: Integer;
  VMonthWidth: Integer;
  VLocalDate: TDateTime;
begin
  Assert(ADateTime <> 0);

  VLocalDate := TTimeZoneInfo.UTCToTzLocalTime(ADateTime, FTzOffset);

  VMonthWidth := FTimeLineWidth div 12;
  VMonthPos := (MonthOfTheYear(VLocalDate) - 1) * VMonthWidth;

  Result := FBorder.Left + VMonthPos +
    (VMonthWidth - 1) * (DayOfTheMonth(VLocalDate) - 1) / (DaysInMonth(VLocalDate) - 1);
end;

function TWindowLayerSunCalcYearTimeLine.PosToUtcDateTime(
  const X: Integer
): TDateTime;
var
  VPos: Integer;
  VLocalDate: TDateTime;
  VMonthPos: Integer;
  VMonthWidth: Integer;
  VMonthOfTheYear: Integer;
  VDaysInMonth: Integer;
  VDayOfTheMonth: Double;
begin
  VLocalDate := TTimeZoneInfo.UTCToTzLocalTime(FDateTime, FTzOffset);

  VPos := X - FBorder.Left;
  if VPos < 0 then begin
    VPos := 0;
  end else if VPos >= FTimeLineWidth then begin
    VPos := FTimeLineWidth - 1;
  end;

  VMonthWidth := FTimeLineWidth div 12;
  VMonthOfTheYear := (VPos div VMonthWidth) + 1;
  VMonthPos := (VMonthOfTheYear - 1) * VMonthWidth;
  VDaysInMonth := DaysInAMonth(YearOf(VLocalDate), VMonthOfTheYear);

  VDayOfTheMonth := (VPos - VMonthPos) * (VDaysInMonth - 1) / (VMonthWidth - 1) + 1;

  VLocalDate := RecodeDate(VLocalDate, YearOf(VLocalDate), VMonthOfTheYear, Round(VDayOfTheMonth));
  Result := TTimeZoneInfo.TzLocalTimeToUTC(VLocalDate, FTzOffset);
end;

procedure TWindowLayerSunCalcYearTimeLine.DoUpdateBitmapDraw;
var
  I: Integer;
  VRect: TRect;
  VDate: TDateTime;
  VFixedDate: TDateTime;
  VMarkerText: string;
begin
  inherited;

  UpdateFixedDatesIfNeed;

  Layer.Bitmap.BeginUpdate;
  try
    Layer.Bitmap.Clear(FColors.BgColor);

    // Time Line
    VRect.Left   := FBorder.Left;
    VRect.Top    := FTimeLineTop;
    VRect.Right  := FBorder.Left + FTimeLineWidth;
    VRect.Bottom := VRect.Top + FTimeLineHeight;

    Layer.Bitmap.FillRectS(VRect, FColors.YearLineColor);

    // Month lines and captions
    DrawScaleItems(FColors.VertLinesColor);

    // Fixed days lines
    for I := Low(FFixedDates) to High(FFixedDates) do begin
      Layer.Bitmap.VertLineS(
        Round(UtcDateTimeToPosF(FFixedDates[I])),
        FTimeLineTop - 6,
        FTimeLineTop - 1,
        clYellow32
      );
    end;

    // Day Marker
    if FIsShowMarkerCaption then begin
      VMarkerText := '';
      VDate := TTimeZoneInfo.UTCToTzLocalTime(FDateTime, FTzOffset);
      for I := Low(FFixedDates) to High(FFixedDates) do begin
        VFixedDate := TTimeZoneInfo.UTCToTzLocalTime(FFixedDates[I], FTzOffset);
        if SameDate(VDate, VFixedDate) then begin
          if (I = 0) or (I = 2) then begin
            VMarkerText := rsEquinox;
          end else begin
            VMarkerText := rsSolstice;
          end;
          VMarkerText := ', ' + VMarkerText + ' ' + FormatDateTime('hh:mm', VFixedDate) +
            TTimeZoneInfo.UTCOffsetToString(FTzOffset);
          Break;
        end;
      end;
      VMarkerText := DateToStr(VDate) + VMarkerText;
    end else begin
      VMarkerText := '';
    end;
    DrawMarker(VMarkerText);
  finally
    Layer.Bitmap.EndUpdate;
    Layer.Bitmap.Changed;
  end;
end;

procedure TWindowLayerSunCalcYearTimeLine.OnSunCalcConfigChange;
var
  VColorSchema: ISunCalcColorSchema;
  VColorSchemaStatic: ISunCalcColorSchemaStatic;
begin
  ViewUpdateLock;
  try
    FSunCalcConfig.LockRead;
    try
      Visible := FSunCalcConfig.Visible;

      FHeight := FSunCalcConfig.YearTimeLineHight;
      FTimeLineTop := (FHeight - 1) - FBorder.Bottom - FTimeLineHeight;

      VColorSchema := FSunCalcConfig.ColorSchemaList.GetActiveColorSchema;
      VColorSchemaStatic := VColorSchema.GetStatic;

      FColors := VColorSchemaStatic.TimeLineColors;
      FFont := VColorSchemaStatic.TimeLineFont;
      FMarkerCaptionFont := VColorSchemaStatic.TimeLineHintFont;

      ClearRenderedText;
    finally
      FSunCalcConfig.UnlockRead;
    end;

    SetNeedUpdateBitmapSize;
    SetNeedUpdateLayerLocation;
    SetNeedUpdateBitmapDraw;
    SetNeedUpdateLayerVisibility;
  finally
    ViewUpdateUnlock;
  end;
end;

end.
