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

unit u_WindowLayerSunCalcDayTimeLine;

interface

uses
  GR32,  
  t_SunCalcConfig,
  i_SunCalcConfig,
  i_SunCalcDataProvider,
  u_TimeZoneInfo,
  u_WindowLayerSunCalcTimeLineBase;

type
  TColoredLineItemRec = record
    Pos: Integer;
    ItemColorIndex: Integer;
    NextColorIndex: Integer;
  end;

  TWindowLayerSunCalcDayTimeLine = class(TWindowLayerSunCalcTimeLineBase)
  private
    FColors: TSunCalcTimeLineColors;
    FLineItems: array of TColoredLineItemRec;
    FIsDetailedView: Boolean;
    procedure SortLineItems; inline;
    procedure DrawColoredTimeLine;
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
  DateUtils;

{ TWindowLayerSunCalcDayTimeLine }

procedure TWindowLayerSunCalcDayTimeLine.AfterConstruction;
var
  I: Integer;
begin
  inherited;

  FMargins.Left   := 0;
  FMargins.Top    := FSunCalcConfig.YearTimeLineHight;
  FMargins.Right  := 0;
  FMargins.Bottom := 0;

  FBorder.Left   := 5;
  FBorder.Right  := 5;
  FBorder.Top    := 8;
  FBorder.Bottom := 6;

  FTimeLineHeight := 8;

  FMarker := FSunCalcDataProvider.DayMarker;

  SetLength(FRenderedText, 24);
  for I := Low(FRenderedText) to High(FRenderedText) do begin
    FRenderedText[I].Text := IntToStr(I) + ':00';
    FRenderedText[I].Bitmap := nil;
  end;

  FRedrawOnDateChanged := True;
  FRedrawOnDateTimeChanged := True;
  FRedrawOnLocationChanged := True;

  FIsDetailedView := False;
end;

function TWindowLayerSunCalcDayTimeLine.UtcDateTimeToPosF(
  const ADateTime: TDateTime
): Double;
var
  VLocalTime: TDateTime;
begin
  Assert(ADateTime <> 0);

  VLocalTime := TTimeZoneInfo.UTCToTzLocalTime(ADateTime, FTzOffset);
  Result := FBorder.Left +
    (FTimeLineWidth - 1) * MinuteOfTheDay(VLocalTime) / (MinsPerDay - 1);
end;

function TWindowLayerSunCalcDayTimeLine.PosToUtcDateTime(
  const X: Integer
): TDateTime;
var
  VPos: Integer;
  VDayWidth: Integer;
  VLocalTime: TDateTime;
  VMinuteOfTheDay: Double;
begin
  VLocalTime := TTimeZoneInfo.UTCToTzLocalTime(FDateTime, FTzOffset);

  VDayWidth := FTimeLineWidth;

  VPos := X - FBorder.Left;
  if VPos < 0 then begin
    VPos := 0;
  end else if VPos >= VDayWidth then begin
    VPos := VDayWidth - 1;
  end;

  VMinuteOfTheDay := (VPos / (VDayWidth - 1)) * (MinsPerDay - 1);

  VLocalTime := IncMinute(StartOfTheDay(VLocalTime), Round(VMinuteOfTheDay));
  Result := TTimeZoneInfo.TzLocalTimeToUTC(VLocalTime, FTzOffset);
end;

procedure TWindowLayerSunCalcDayTimeLine.SortLineItems;
var
  I, J, K: Integer;
  VItem: TColoredLineItemRec;
begin
  // selection sort
  for I := Low(FLineItems) to High(FLineItems) - 1 do begin
    K := I;
    for J := I + 1 to High(FLineItems) do begin
      if FLineItems[J].Pos < FLineItems[K].Pos then begin
        K := J;
      end;
    end;
    if K <> I then begin
      // swap
      VItem := FLineItems[I];
      FLineItems[I] := FLineItems[K];
      FLineItems[K] := VItem;
    end;
  end;
end;

procedure TWindowLayerSunCalcDayTimeLine.DrawColoredTimeLine;
var
  I: Integer;
  VDay: TDateTime;
  VRect: TRect;
  VColor: TColor32;
  VDayEvents: TSunCalcDayEvents;
  VTransitPos: TSunCalcProviderPosition;
  VTransitDate: TDateTime;
  VIsTransitFound: Boolean;
  VTimeLineEndPos: Integer;
begin
  VTransitDate := 0;
  VIsTransitFound := False;

  VDay := StartOfTheDay(TTimeZoneInfo.UTCToTzLocalTime(FDateTime, FTzOffset));
  VDayEvents := FSunCalcDataProvider.GetDayEvents(VDay, FLocation, FIsDetailedView);

  SetLength(FLineItems, Length(VDayEvents));

  for I := 0 to Length(VDayEvents) - 1 do begin
    if VDayEvents[I].Date > 0 then begin
      FLineItems[I].Pos := Round(UtcDateTimeToPosF(VDayEvents[I].Date));
    end else begin
      FLineItems[I].Pos := 0;
    end;

    FLineItems[I].ItemColorIndex := VDayEvents[I].ColorIndex;
    FLineItems[I].NextColorIndex := VDayEvents[I].NextColorIndex;

    if (VDayEvents[I].IsTransit) and (VDayEvents[I].Date <> 0) then begin
      VTransitDate := VDayEvents[I].Date;
      VTransitPos := FSunCalcDataProvider.GetPosition(VTransitDate, FLocation);
      VIsTransitFound := True;
    end;
  end;

  SortLineItems;

  if VIsTransitFound and (VTransitPos.Altitude >= 0) then begin
    VColor := FColors.DayLineColors[Length(FColors.DayLineColors)-1];
  end else begin
    VColor := FColors.DayLineColors[0];
  end;

  VTimeLineEndPos := FBorder.Left + FTimeLineWidth;

  VRect.Left := FBorder.Left;
  VRect.Top := FTimeLineTop;
  VRect.Right := VTimeLineEndPos;
  VRect.Bottom := VRect.Top + FTimeLineHeight;

  for I := Low(FLineItems) to High(FLineItems) do begin
    if (FLineItems[I].Pos > 0) and (FLineItems[I].ItemColorIndex >= 0) then begin
      VColor := FColors.DayLineColors[FLineItems[I].ItemColorIndex];

      VRect.Right := FLineItems[I].Pos;
      Layer.Bitmap.FillRectS(VRect, VColor);

      VRect.Left := VRect.Right;
      VRect.Right := VTimeLineEndPos;

      VColor := FColors.DayLineColors[FLineItems[I].NextColorIndex];
    end;
  end;

  if VRect.Left <> VRect.Right then begin
    Layer.Bitmap.FillRectS(VRect, VColor);
  end;

  // Transit vert line
  if VIsTransitFound then begin
    Layer.Bitmap.VertLineS(
      Round(UtcDateTimeToPosF(VTransitDate)),
      FTimeLineTop - 6,
      FTimeLineTop - 1,
      clYellow32
    );
  end;
end;

procedure TWindowLayerSunCalcDayTimeLine.DoUpdateBitmapDraw;
var
  VTime: TDateTime;
  VMarkerText: string;
begin
  inherited;

  Layer.Bitmap.BeginUpdate;
  try
    Layer.Bitmap.Clear(FColors.BgColor);

    // Bottom line
    Layer.Bitmap.HorzLineS(0, FHeight - 1, Layer.Bitmap.Width - 1, SetAlpha(FColors.BgColor, 255));

    // Time Line
    DrawColoredTimeLine;

    // Hour lines and captions
    DrawScaleItems(FColors.VertLinesColor);

    // Sun Marker
    if FIsShowMarkerCaption then begin
      VTime := TTimeZoneInfo.UTCToTzLocalTime(FDateTime, FTzOffset);
      VMarkerText := FormatDateTime('hh:nn', VTime) + TTimeZoneInfo.UTCOffsetToString(FTzOffset);
    end else begin
      VMarkerText := '';
    end;
    DrawMarker(VMarkerText);
  finally
    Layer.Bitmap.EndUpdate;
    Layer.Bitmap.Changed;
  end;
end;

procedure TWindowLayerSunCalcDayTimeLine.OnSunCalcConfigChange;
var
  VColorSchema: ISunCalcColorSchema;
  VColorSchemaStatic: ISunCalcColorSchemaStatic;
begin
  ViewUpdateLock;
  try
    FSunCalcConfig.LockRead;
    try
      Visible := FSunCalcConfig.Visible;

      FIsDetailedView := FSunCalcConfig.IsDetailedView;

      FHeight := FSunCalcConfig.DayTimeLineHight;
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
