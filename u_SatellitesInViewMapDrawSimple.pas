unit u_SatellitesInViewMapDrawSimple;

interface

uses
  Windows,
  GR32,
  i_GPS,
  i_SatellitesInViewMapDraw;

type
  TSatellitesInViewMapDrawSimple = class(TInterfacedObject, ISatellitesInViewMapDraw)
  private
    FSatFixedColor: TColor32;
    FSatNotFixedColor: TColor32;
    FSatNotVisibleColor: TColor32;
    FSkyMapBgColor: TColor32;
    FSkyMapGridColor: TColor32;
    FSkyMapGridCirclesMaxCount: Integer;
    FSkyMapGridCirclesMinDelta: Integer;
    FSkyMapMargin: Integer;
    FSkyMapSatRdius: Integer;

    FSignalBarsCount: Integer;
    FSignalBarsBGColor: TColor32;
    FSignalBarsFontColor: TColor32;
    FSignalBarBorderColor: TColor32;
    FSignalBarMinHeight: Integer;
    FSignalBarMinWidth: Integer;
    FSignalBarVertSpaces: Integer;
    FSignalBarMinHorizSpaces: Integer;

    function GetSignalBarRowsCount(ABitmap: TBitmap32): Integer;
    function GetSignalBarHeight(
      ABitmap: TBitmap32;
      ARowCount: Integer
    ): Integer;
    procedure GetSignalBarWidthAndHorizSpace(
      ABitmap: TBitmap32;
      ARowCount: Integer;
      out AWidth: Integer;
      out AHorizSpace: Integer
    );
    function GetSignalBarRect(
      ABitmap: TBitmap32;
      AIndex: Integer;
      ARowCount: Integer;
      ABarHeight:  Integer;
      AWidth: Integer;
      AHorizSpace: Integer
    ): TRect;
    procedure GetCircleCenterAndRadius(ABitmap: TBitmap32; ARowCount,
      ABarHeight: Integer; out ACenter: TPoint; out ARadius: Integer);

    procedure DrawEmptySkyMap(
      ABitmap: TBitmap32;
      ACenter: TPoint;
      ARadius: Integer
    );
    procedure DrawEmptySignalBars(
      ABitmap: TBitmap32;
      ARowCount: Integer;
      ABarHeight:  Integer;
      AWidth: Integer;
      AHorizSpace: Integer
    );
    procedure DrawSkyMap(
      ABitmap: TBitmap32;
      ACenter: TPoint;
      ARadius: Integer;
      ASatellites: IGPSSatellitesInView
    );
    procedure DrawSignalBars(
      ABitmap: TBitmap32;
      ARowCount: Integer;
      ABarHeight:  Integer;
      AWidth: Integer;
      AHorizSpace: Integer;
      ASatellites: IGPSSatellitesInView
    );
  protected
    procedure Draw(ABitmap: TBitmap32; ASatellites: IGPSSatellitesInView);
  public
    constructor Create;
  end;

implementation

uses
  SysUtils,
  GR32_VectorUtils,
  GR32_PolygonsEx;

{ TSatellitesInViewMapDrawSimple }

constructor TSatellitesInViewMapDrawSimple.Create;
begin
  FSatFixedColor := clGreen32;
  FSatNotFixedColor := clYellow32;
  FSatNotVisibleColor := clRed32;
  FSkyMapBgColor := clWhite32;
  FSkyMapGridColor := clBlack32;
  FSkyMapMargin := 10;
  FSkyMapGridCirclesMaxCount := 5;
  FSkyMapGridCirclesMinDelta := 20;
  FSkyMapSatRdius := 8;

  FSignalBarsCount := 12;
  FSignalBarsBGColor := clWhite32;
  FSignalBarBorderColor := clBlue32;
  FSignalBarsFontColor := clBlack32;
  FSignalBarMinHeight := 32;
  FSignalBarMinWidth := 4;
  FSignalBarVertSpaces := 10;
  FSignalBarMinHorizSpaces := 2;
end;

procedure TSatellitesInViewMapDrawSimple.Draw(ABitmap: TBitmap32;
  ASatellites: IGPSSatellitesInView);
var
  VRowCount: Integer;
  VBarHeight: Integer;
  VWidth: Integer;
  VHorizSpace: Integer;
  VCenter: TPoint;
  VRadius: Integer;
begin
  ABitmap.Lock;
  try
    ABitmap.Clear(FSkyMapBgColor);
    VRowCount := GetSignalBarRowsCount(ABitmap);
    GetSignalBarWidthAndHorizSpace(ABitmap, VRowCount, VWidth, VHorizSpace);
    VBarHeight := GetSignalBarHeight(ABitmap, VRowCount);
    GetCircleCenterAndRadius(ABitmap, VRowCount, VBarHeight, VCenter, VRadius);
    if VRadius > 0 then begin
      DrawEmptySkyMap(ABitmap, VCenter, VRadius);
    end;
    DrawEmptySignalBars(ABitmap, VRowCount, VBarHeight, VWidth, VHorizSpace);
    if VRadius > 0 then begin
      DrawSkyMap(ABitmap, VCenter, VRadius, ASatellites);
    end;
    DrawSignalBars(ABitmap, VRowCount, VBarHeight, VWidth, VHorizSpace, ASatellites);
  finally
    ABitmap.Unlock;
  end;
end;

procedure TSatellitesInViewMapDrawSimple.DrawEmptySignalBars(ABitmap: TBitmap32;
  ARowCount, ABarHeight, AWidth, AHorizSpace: Integer);
var
  i: Integer;
  VRect: TRect;
begin
  VRect.Left := 0;
  VRect.Right := ABitmap.Width;
  VRect.Bottom := ABitmap.Height;
  VRect.Top := VRect.Bottom - ARowCount * (ABarHeight + FSignalBarVertSpaces);
  ABitmap.FillRectS(VRect, FSignalBarsBGColor);
  for i := 0 to FSignalBarsCount - 1 do begin
    VRect := GetSignalBarRect(ABitmap, i, ARowCount, ABarHeight, AWidth, AHorizSpace);
    ABitmap.FrameRectS(VRect, FSignalBarBorderColor);
  end;
end;

procedure TSatellitesInViewMapDrawSimple.DrawEmptySkyMap(ABitmap: TBitmap32;
  ACenter: TPoint; ARadius: Integer);
var
  VPoints: TArrayOfFloatPoint;
  VCirclesCount: Integer;
  VCirlcesDelta: Single;
  VRadius: Single;
  i: Integer;
begin
  if ARadius / FSkyMapGridCirclesMaxCount < FSkyMapGridCirclesMinDelta then begin
    VCirclesCount := ARadius div FSkyMapGridCirclesMinDelta;
  end else begin
    VCirclesCount := FSkyMapGridCirclesMaxCount;
  end;
  if VCirclesCount <= 0 then begin
    VCirclesCount := 1;
  end;
  VCirlcesDelta := ARadius / VCirclesCount;
  for i := 0 to VCirclesCount - 1 do begin
    VRadius := ARadius - VCirlcesDelta * i;
    VPoints := Ellipse(ACenter.X, ACenter.Y, VRadius, VRadius);
    PolylineFS(ABitmap, VPoints, FSkyMapGridColor, True);
  end;
  VPoints := Ellipse(ACenter.X, ACenter.Y, ARadius, ARadius, 16);
  for i := 0 to Length(VPoints) - 1 do begin
    ABitmap.LineFS(ACenter.X, ACenter.Y, VPoints[i].X, VPoints[i].Y, FSkyMapGridColor);
  end;
end;

procedure TSatellitesInViewMapDrawSimple.DrawSignalBars(ABitmap: TBitmap32;
  ARowCount, ABarHeight, AWidth, AHorizSpace: Integer;
  ASatellites: IGPSSatellitesInView);
var
  i: Integer;
  VSatellite: IGPSSatelliteInfo;
  VRect: TRect;
  VColor: TColor32;
  VText: string;
  VTextSize: TSize;
  VTextPos: TPoint;
  VIndex: Integer;
begin
  VIndex := 0;
  for i := 0 to ASatellites.Count - 1 do begin
    VSatellite := ASatellites.Item[i];
    if VSatellite.SignalToNoiseRatio > 0 then begin
      if VSatellite.IsFix then begin
        VColor := FSatFixedColor;
      end else begin
        VColor := FSatNotFixedColor;
      end;
      VRect := GetSignalBarRect(ABitmap, VIndex, ARowCount, ABarHeight, AWidth, AHorizSpace);
      Inc(VRect.Left);
      Inc(VRect.Top);
      Dec(VRect.Right);
      Dec(VRect.Bottom);
      VText := IntToStr(VSatellite.PseudoRandomCode);
      VTextSize := ABitmap.TextExtent(VText);
      VTextPos.X := Trunc((VRect.Left + VRect.Right) / 2 - VTextSize.cx / 2);
      VTextPos.Y := VRect.Bottom;
      ABitmap.RenderText(VTextPos.X, VTextPos.Y, VText, 4, clBlack32);

      VRect.Top := VRect.Bottom - Trunc((VRect.Bottom - VRect.Top) * VSatellite.SignalToNoiseRatio /100);
      ABitmap.FillRectS(VRect, VColor);
      Inc(VIndex);
    end;
    if VIndex >= FSignalBarsCount then begin
      Break;
    end;
  end;
  for i := 0 to ASatellites.Count - 1 do begin
    VSatellite := ASatellites.Item[i];
    if VSatellite.SignalToNoiseRatio <=0 then begin
      VRect := GetSignalBarRect(ABitmap, VIndex, ARowCount, ABarHeight, AWidth, AHorizSpace);
      Inc(VRect.Left);
      Inc(VRect.Top);
      Dec(VRect.Right);
      Dec(VRect.Bottom);
      VText := IntToStr(VSatellite.PseudoRandomCode);
      VTextSize := ABitmap.TextExtent(VText);
      VTextPos.X := Trunc((VRect.Left + VRect.Right) / 2 - VTextSize.cx / 2);
      VTextPos.Y := VRect.Bottom;
      ABitmap.RenderText(VTextPos.X, VTextPos.Y, VText, 4, clBlack32);

      Inc(VIndex);
    end;
    if VIndex >= FSignalBarsCount then begin
      Break;
    end;
  end;
end;

procedure TSatellitesInViewMapDrawSimple.DrawSkyMap(ABitmap: TBitmap32;
  ACenter: TPoint; ARadius: Integer; ASatellites: IGPSSatellitesInView);
var
  i: Integer;
  VSatellite: IGPSSatelliteInfo;
  VColor: TColor32;
  VSatAtRadius: TFloat;
  VSatPos: TFloatPoint;
  VPoints: TArrayOfFloatPoint;
  VText: string;
  VTextSize: TSize;
  VTextPos: TPoint;
begin
  for i := 0 to ASatellites.Count - 1 do begin
    VSatellite := ASatellites.Item[i];
    if VSatellite.IsFix then begin
      VColor := FSatFixedColor;
    end else begin
      if VSatellite.SignalToNoiseRatio > 0 then begin
        VColor := FSatNotFixedColor;
      end else begin
        VColor := FSatNotVisibleColor;
      end;
    end;
    VSatAtRadius := ARadius * ((90 - VSatellite.Elevation) / 90);
    VSatPos.x := ACenter.x + VSatAtRadius * cos((VSatellite.Azimuth - 90) * (Pi / 180));
    VSatPos.y := ACenter.y + VSatAtRadius * sin((VSatellite.Azimuth - 90) * (Pi / 180));
    VPoints := Ellipse(VSatPos.X, VSatPos.Y, FSkyMapSatRdius, FSkyMapSatRdius, 20);
    PolygonFS(ABitmap, VPoints, VColor);
    VText := IntToStr(VSatellite.PseudoRandomCode);
    VTextSize := ABitmap.TextExtent(VText);
    VTextPos.X := Trunc(VSatPos.X - VTextSize.cx / 2);
    VTextPos.Y := Trunc(VSatPos.Y - VTextSize.cy / 2);
    ABitmap.RenderText(VTextPos.X, VTextPos.Y, VText, 4, FSkyMapGridColor);
    PolylineFS(ABitmap, VPoints, FSkyMapGridColor, true);
  end;
end;

procedure TSatellitesInViewMapDrawSimple.GetCircleCenterAndRadius(
  ABitmap: TBitmap32;
  ARowCount: Integer;
  ABarHeight:  Integer;
  out ACenter: TPoint;
  out ARadius: Integer);
var
  VHeight: Integer;
begin
  VHeight := ABitmap.Height - ARowCount * (ABarHeight + FSignalBarVertSpaces);
  if VHeight > FSkyMapMargin * 2 then begin
    ACenter.X := ABitmap.Width div 2;
    ACenter.Y := VHeight div 2;

    if ACenter.X > ACenter.Y then begin
      ARadius := ACenter.Y;
    end else begin
      ARadius := ACenter.X;
    end;
    ARadius := ARadius -  FSkyMapMargin;
  end else begin
    ARadius := 0;
    ACenter.X := 0;
    ACenter.Y := 0;
  end;
end;

function TSatellitesInViewMapDrawSimple.GetSignalBarHeight(
  ABitmap: TBitmap32; ARowCount: Integer): Integer;
begin
  if ARowCount > 1 then begin
    Result := (ABitmap.Height - ABitmap.Width - FSignalBarVertSpaces * 2) div 2;
  end else begin
    if ABitmap.Height > ABitmap.Width + FSignalBarVertSpaces + FSignalBarMinHeight then begin
      Result := ABitmap.Height - (ABitmap.Width + FSignalBarVertSpaces);
    end else begin
      Result := FSignalBarMinHeight;
    end;
  end;
end;

function TSatellitesInViewMapDrawSimple.GetSignalBarRect(ABitmap: TBitmap32;
  AIndex: Integer; ARowCount: Integer; ABarHeight:  Integer; AWidth: Integer; AHorizSpace: Integer): TRect;
var
  VRowIndex: Integer;
  VColIndex: Integer;
  VColCount: Integer;
  VLeftMargin: Integer;
begin
  if ARowCount > 1 then begin
    VColCount := FSignalBarsCount div 2;
    if AIndex < VColCount then begin
      VRowIndex := 1;
    end else begin
      VRowIndex := 0;
    end;
  end else begin
    VColCount := FSignalBarsCount;
    VRowIndex := 0;
  end;
  VColIndex := AIndex mod VColCount;
  VLeftMargin := FSignalBarMinHorizSpaces + (ABitmap.Width - VColCount * AWidth - (VColCount - 1) * AHorizSpace - 2 * FSignalBarMinHorizSpaces ) div 2;

  Result.Bottom := ABitmap.Height - (FSignalBarVertSpaces + VRowIndex * (ABarHeight + FSignalBarVertSpaces));
  Result.Top := Result.Bottom - ABarHeight;
  Result.Left := VLeftMargin + VColIndex * (AWidth + AHorizSpace);
  Result.Right := Result.Left + AWidth;
end;

function TSatellitesInViewMapDrawSimple.GetSignalBarRowsCount(
  ABitmap: TBitmap32): Integer;
begin
  if ABitmap.Height - (FSignalBarMinHeight * 2 + FSignalBarVertSpaces * 2) > ABitmap.Width then begin
    Result := 2;
  end else begin
    Result := 1;
  end;
end;

procedure TSatellitesInViewMapDrawSimple.GetSignalBarWidthAndHorizSpace(
  ABitmap: TBitmap32; ARowCount: Integer; out AWidth, AHorizSpace: Integer);
var
  VBarWithSpace: Integer;
  VColCount: Integer;
begin
  if ARowCount > 1 then begin
    VColCount := FSignalBarsCount div 2;
  end else begin
    VColCount := FSignalBarsCount;
  end;
  VBarWithSpace := (ABitmap.Width - 2 * FSignalBarMinHorizSpaces) div VColCount;
  AWidth := FSignalBarMinWidth + (VBarWithSpace - FSignalBarMinWidth - FSignalBarMinHorizSpaces) div 2;
  AHorizSpace := VBarWithSpace - AWidth;
end;

end.
