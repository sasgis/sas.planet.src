{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2019, SAS.Planet development team.                      *}
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
  t_SunCalcDataProvider,
  i_SunCalcConfig,
  i_SunCalcDataProvider,
  i_MarkerDrawable,
  u_TimeZoneInfo,
  u_WindowLayerSunCalcTimeLineBase;

type
  TWindowLayerSunCalcYearTimeLine = class(TWindowLayerSunCalcTimeLineBase)
  private
    FColors: TSunCalcTimeLineColors;
  protected
    function PosToUtcDateTime(const X: Integer): TDateTime; override;
    function UtcDateTimeToPosF(const ADateTime: TDateTime): Double; override;

    procedure OnSunCalcConfigChange; override;
    function GetMarker: IMarkerDrawable; override;
  protected
    procedure DoUpdateBitmapDraw; override;
  public
    procedure AfterConstruction; override;
  end;

implementation

uses
  SysUtils,
  DateUtils;

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

  SetLength(FRenderedText, Length(cMonthText));
  for I := Low(FRenderedText) to High(FRenderedText) do begin
    FRenderedText[I].Text := cMonthText[I];
    FRenderedText[I].Bitmap := nil;
  end;

  FRedrawOnDateChanged := True;
  FRedrawOnTzOffsetChanged := True;
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
  VYearEvents: TSunCalcYearEvents;
  VEventDate: TDateTime;
  VMarkerText: string;
begin
  inherited;

  VYearEvents := FSunCalcDataProvider.GetYearEvents(FDateTime, FLocation);

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

    // Year event days lines
    for I := Low(VYearEvents) to High(VYearEvents) do begin
      Layer.Bitmap.VertLineS(
        Round(UtcDateTimeToPosF(VYearEvents[I].Date)),
        FTimeLineTop - 6,
        FTimeLineTop - 1,
        clYellow32
      );
    end;

    // Day Marker
    if FIsShowMarkerCaption then begin
      VMarkerText := '';
      VDate := TTimeZoneInfo.UTCToTzLocalTime(FDateTime, FTzOffset);
      for I := Low(VYearEvents) to High(VYearEvents) do begin
        VEventDate := TTimeZoneInfo.UTCToTzLocalTime(VYearEvents[I].Date, FTzOffset);
        if SameDate(VDate, VEventDate) then begin
          VMarkerText :=
            ', ' +
            VYearEvents[I].Name + ' ' +
            FormatDateTime('hh:nn', VEventDate) +
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

function TWindowLayerSunCalcYearTimeLine.GetMarker: IMarkerDrawable;
begin
  Result := FSunCalcDataProvider.YearMarker;
end;

end.
