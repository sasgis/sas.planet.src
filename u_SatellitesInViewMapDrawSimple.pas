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

unit u_SatellitesInViewMapDrawSimple;

interface

uses
  Windows,
  GR32,
  i_GPS,
  i_SatellitesInViewMapDraw,
  u_BaseInterfacedObject;

type
  TSatellitesInViewMapDrawSimple = class(TBaseInterfacedObject, ISatellitesInViewMapDraw)
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

    procedure AdjustSignalSatBarsCount(const ASatCount: Integer);

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
      ABarHeight: Integer;
      AWidth: Integer;
      AHorizSpace: Integer
    ): TRect;
    procedure GetCircleCenterAndRadius(
      ABitmap: TBitmap32;
      ARowCount, ABarHeight: Integer;
      out ACenter: TPoint;
      out ARadius: Integer
    );

    procedure DrawEmptySkyMap(
      ABitmap: TBitmap32;
      const ACenter: TPoint;
      ARadius: Integer
    );
    procedure DrawEmptySignalBars(
      ABitmap: TBitmap32;
      ARowCount: Integer;
      ABarHeight: Integer;
      AWidth: Integer;
      AHorizSpace: Integer
    );
    procedure DrawSkyMap(
      ABitmap: TBitmap32;
      const ACenter: TPoint;
      ARadius: Integer;
      const ASatellites: IGPSSatellitesInView
    );
    procedure DrawSignalBars(
      ABitmap: TBitmap32;
      ARowCount: Integer;
      ABarHeight: Integer;
      AWidth: Integer;
      AHorizSpace: Integer;
      const ASatellites: IGPSSatellitesInView
    );
  private
    procedure Draw(
      ABitmap: TBitmap32;
      const ASatellites: IGPSSatellitesInView
    );
  public
    constructor Create;
  end;

implementation

uses
  SysUtils,
  GR32_VectorUtils,
  GR32_PolygonsEx,
  vsagps_public_base;

const
  cDefault_FSignalBarsCount = 12;

{ TSatellitesInViewMapDrawSimple }

procedure TSatellitesInViewMapDrawSimple.AdjustSignalSatBarsCount(const ASatCount: Integer);
var
  rem: Integer;
begin
  FSignalBarsCount := cDefault_FSignalBarsCount;
  if (cDefault_FSignalBarsCount < ASatCount) then begin
    // calc reminder
    FSignalBarsCount := ASatCount;
    rem := (ASatCount mod 4);
    if (0 <> rem) then begin
      FSignalBarsCount := FSignalBarsCount - rem + 4;
    end;
  end;
end;

constructor TSatellitesInViewMapDrawSimple.Create;
begin
  inherited Create;
  FSatFixedColor := clGreen32;
  FSatNotFixedColor := clYellow32;
  FSatNotVisibleColor := clRed32;
  FSkyMapBgColor := clWhite32;
  FSkyMapGridColor := clBlack32;
  FSkyMapMargin := 10;
  FSkyMapGridCirclesMaxCount := 5;
  FSkyMapGridCirclesMinDelta := 20;
  FSkyMapSatRdius := 8;

  FSignalBarsCount := cDefault_FSignalBarsCount; // default value
  FSignalBarsBGColor := clWhite32;
  FSignalBarBorderColor := clBlue32;
  FSignalBarsFontColor := clBlack32;
  FSignalBarMinHeight := 32;
  FSignalBarMinWidth := 4;
  FSignalBarVertSpaces := 10;
  FSignalBarMinHorizSpaces := 2;
end;

procedure TSatellitesInViewMapDrawSimple.Draw(
  ABitmap: TBitmap32;
  const ASatellites: IGPSSatellitesInView
);
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

procedure TSatellitesInViewMapDrawSimple.DrawEmptySignalBars(
  ABitmap: TBitmap32;
  ARowCount, ABarHeight, AWidth, AHorizSpace: Integer
);
var
  i: Byte;
  VRect: TRect;
begin
  VRect.Left := 0;
  VRect.Right := ABitmap.Width;
  VRect.Bottom := ABitmap.Height;
  VRect.Top := VRect.Bottom - ARowCount * (ABarHeight + FSignalBarVertSpaces);
  ABitmap.FillRectS(VRect, FSignalBarsBGColor);
  if (0 < FSignalBarsCount) then begin
    for i := 0 to FSignalBarsCount - 1 do begin
      VRect := GetSignalBarRect(ABitmap, i, ARowCount, ABarHeight, AWidth, AHorizSpace);
      ABitmap.FrameRectS(VRect, FSignalBarBorderColor);
    end;
  end;
end;

procedure TSatellitesInViewMapDrawSimple.DrawEmptySkyMap(
  ABitmap: TBitmap32;
  const ACenter: TPoint;
  ARadius: Integer
);
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

procedure TSatellitesInViewMapDrawSimple.DrawSignalBars(
  ABitmap: TBitmap32;
  ARowCount, ABarHeight, AWidth, AHorizSpace: Integer;
  const ASatellites: IGPSSatellitesInView
);
var
  VSatFixed: Boolean;
  VSatFixibility: TSingleSatFixibilityData;
  VIndex: Integer;

  function InternalDrawLast: Boolean;
  var
    VRect: TRect;
    VColor: TColor32;

    procedure InternalDrawText(
    const AText: String;
    const AUpper: Boolean;
    const AColor32: TColor32
    );
    var
      VTextSize: TSize;
      VTextPos: TPoint;
    begin
      VTextSize := ABitmap.TextExtent(AText);
      VTextPos.X := Trunc((VRect.Left + VRect.Right) / 2 - VTextSize.cx / 2);
      VTextPos.Y := VRect.Bottom;
      if AUpper then begin
        VTextPos.Y := VTextPos.Y - VTextSize.cy;
      end; // - 1;
      ABitmap.RenderText(VTextPos.X, VTextPos.Y, AText, 4, AColor32);
    end;

  begin
    Result := FALSE;

    // rect
    VRect := GetSignalBarRect(ABitmap, VIndex, ARowCount, ABarHeight, AWidth, AHorizSpace);
    Inc(VRect.Left);
    Inc(VRect.Top);
    Dec(VRect.Right);
    Dec(VRect.Bottom);

    // sat number
    if (VSatFixibility.sat_info.svid > 0) then begin
      // sat number ok
      InternalDrawText(IntToStr(VSatFixibility.sat_info.svid), FALSE, clBlack32);
    end;

    // if snr > 0 - draw colored stripe
    if (VSatFixibility.snr > 0) then begin
      // select color
      if (VSatFixibility.status > 0) and
        (cGarmin_Flag_InSolution = (VSatFixibility.flags and cGarmin_Flag_InSolution)) and
        (0 = (VSatFixibility.flags and cGarmin_Flag_HasEphemeris)) then begin
        VColor := FSignalBarBorderColor; // in solution but without ephemeris - garmin workaround
      end else if VSatFixed then begin
        VColor := FSatFixedColor;
      end else begin
        VColor := FSatNotFixedColor;
      end;

      // draw colored stripe (if > 100% - truncate to 100%, so fill full bar)
      if (VSatFixibility.snr < 100) then begin
        VRect.Top := VRect.Bottom - Trunc((VRect.Bottom - VRect.Top) * VSatFixibility.snr / 100);
      end;
      ABitmap.FillRectS(VRect, VColor);

      // draw D for DGPS
      if (cGarmin_Flag_Differential = (VSatFixibility.flags and cGarmin_Flag_Differential)) then begin
        InternalDrawText('D', TRUE, FSignalBarsBGColor);
      end;
    end;

    // check count of bars
    Inc(VIndex);
    if VIndex >= FSignalBarsCount then begin
      Result := TRUE;
    end;
  end;

var
  i: Byte;
  VCountForAllTalkerIDs, VTalkerIDCount: Byte;
  VTalkerID: AnsiString;
begin
  if (not Assigned(ASatellites)) then begin
    Exit;
  end;

  VIndex := 0;

  // get count of satellites for all constellations
  VCountForAllTalkerIDs := ASatellites.GetCountForAllTalkerIDs(FALSE);

  // Adjust count of bars
  AdjustSignalSatBarsCount(VCountForAllTalkerIDs);

  // no sats - nothing to do
  if (0 = VCountForAllTalkerIDs) then begin
    Exit;
  end;

  // loop (for many constellations)
  VTalkerID := '';
  while ASatellites.EnumerateTalkerID(VTalkerID) do begin
    // count of satellites for current talker_id
    VTalkerIDCount := ASatellites.Count[VTalkerID];

    // draw items
    if (0 < VTalkerIDCount) then begin
      // first step
      for i := 0 to VTalkerIDCount - 1 do begin
        if ASatellites.GetAllSatelliteParams(i, VTalkerID, VSatFixed, @VSatFixibility) then begin
          if (VSatFixibility.snr > 0) then begin
            // for sats with signals (Signal-to-Noise Ratio > 0)
            if InternalDrawLast then begin
              Exit;
            end;
          end;
        end;
      end;

      // second step - if some bars are available for drawing
      if (VIndex < FSignalBarsCount) then begin
        for i := 0 to VTalkerIDCount - 1 do begin
          if ASatellites.GetAllSatelliteParams(i, VTalkerID, VSatFixed, @VSatFixibility) then begin
            if (VSatFixibility.snr <= 0) then begin
              // satellites without signal - to the end
              if InternalDrawLast then begin
                Exit;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TSatellitesInViewMapDrawSimple.DrawSkyMap(
  ABitmap: TBitmap32;
  const ACenter: TPoint;
  ARadius: Integer;
  const ASatellites: IGPSSatellitesInView
);
var
  VSatFixed: Boolean;
  VSatFixibility: TSingleSatFixibilityData;
  VSatSky: TSingleSatSkyData;
  VColor: TColor32;
  VSatAtRadius: TFloat;
  VSatPos: TFloatPoint;
  VPoints: TArrayOfFloatPoint;
  VText: string;
  VTextSize: TSize;
  VTextPos: TPoint;
  VTalkerID: AnsiString;
  // VCountForAllTalkerIDs: Byte;
  VTalkerIDCount, i: Byte;
begin
  if not Assigned(ASatellites) then begin
    DrawEmptySkyMap(ABitmap, ACenter, ARadius);
    Exit;
  end;

  {
  // get count of satellites for all constellations
  VCountForAllTalkerIDs:=ASatellites.GetCountForAllTalkerIDs;

  // no sats - nothing to do
  if (0=VCountForAllTalkerIDs) then
    Exit;
  }

  // loop (for many constellations)
  VTalkerID := '';
  while ASatellites.EnumerateTalkerID(VTalkerID) do begin
    // count of satellites
    VTalkerIDCount := ASatellites.Count[VTalkerID];
    // loop
    if (0 < VTalkerIDCount) then begin
      for i := 0 to VTalkerIDCount - 1 do begin
        if ASatellites.GetAllSatelliteParams(i, VTalkerID, VSatFixed, @VSatFixibility, @VSatSky) then begin
          // select color
          if VSatFixed then begin
            VColor := FSatFixedColor;
          end else if VSatFixibility.snr > 0 then begin
            VColor := FSatNotFixedColor;
          end else begin
            VColor := FSatNotVisibleColor;
          end;

          // sat position
          VSatAtRadius := ARadius * ((90 - VSatSky.elevation) / 90);
          VSatPos.x := ACenter.x + VSatAtRadius * cos((VSatSky.azimuth - 90) * (Pi / 180));
          VSatPos.y := ACenter.y + VSatAtRadius * sin((VSatSky.azimuth - 90) * (Pi / 180));
          VPoints := Ellipse(VSatPos.X, VSatPos.Y, FSkyMapSatRdius, FSkyMapSatRdius, 20);
          PolygonFS(ABitmap, VPoints, VColor);

          if (VSatFixibility.sat_info.svid > 0) then begin
            VText := IntToStr(VSatFixibility.sat_info.svid);
            VTextSize := ABitmap.TextExtent(VText);
            VTextPos.X := Trunc(VSatPos.X - VTextSize.cx / 2);
            VTextPos.Y := Trunc(VSatPos.Y - VTextSize.cy / 2);
            ABitmap.RenderText(VTextPos.X, VTextPos.Y, VText, 4, FSkyMapGridColor);
          end;

          PolylineFS(ABitmap, VPoints, FSkyMapGridColor, true);
        end;
      end;
    end;
  end;
end;

procedure TSatellitesInViewMapDrawSimple.GetCircleCenterAndRadius(
  ABitmap: TBitmap32;
  ARowCount: Integer;
  ABarHeight: Integer;
  out ACenter: TPoint;
  out ARadius: Integer
);
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
    ARadius := ARadius - FSkyMapMargin;
  end else begin
    ARadius := 0;
    ACenter.X := 0;
    ACenter.Y := 0;
  end;
end;

function TSatellitesInViewMapDrawSimple.GetSignalBarHeight(
  ABitmap: TBitmap32;
  ARowCount: Integer
): Integer;
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

function TSatellitesInViewMapDrawSimple.GetSignalBarRect(
  ABitmap: TBitmap32;
  AIndex: Integer;
  ARowCount: Integer;
  ABarHeight: Integer;
  AWidth: Integer;
  AHorizSpace: Integer
): TRect;
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
  VLeftMargin := FSignalBarMinHorizSpaces + (ABitmap.Width - VColCount * AWidth - (VColCount - 1) * AHorizSpace - 2 * FSignalBarMinHorizSpaces) div 2;

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
  ABitmap: TBitmap32;
  ARowCount: Integer;
  out AWidth, AHorizSpace: Integer
);
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
