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

unit u_WindowLayerSunCalcTimeInfo;

interface

uses
  Types,
  GR32,
  t_SunCalcConfig,
  u_WindowLayerSunCalcInfoBase;

type
  TSunCalcTimeInfoCaption = class
  private
    FText: string;
    FBitmap: TBitmap32;
  public
    procedure SetText(const AText: string; const AFont: TSunCalcFontInfo; const ABgColor: TColor32);
    function GetBoundsForPosition(const APosition: TPoint): TRect;
    procedure DrawToBitmap(ABuffer: TBitmap32; const APosition: TPoint);
 public
    constructor Create;
    destructor Destroy; override;
  end;

  TWindowLayerSunCalcTimeInfo = class(TWindowLayerSunCalcInfoBase)
  private
    FIsValid: Boolean;
    FRect: TRect;
    FCenter: TFloatPoint;
    FCurrentPos: TFloatPoint;
    FCaption: TSunCalcTimeInfoCaption;
  protected
    procedure InvalidateLayer; override;
    procedure PaintLayer(ABuffer: TBitmap32); override;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
  end;

implementation

uses
  Math,
  SysUtils,
  t_GeoTypes,
  t_SunCalcDataProvider,
  i_SunCalcDataProvider,
  u_GeoFunc,
  u_GR32Func;

const
  CLineWidth = 4;

resourcestring
  rsAzimuth = 'Azimuth';
  rsAltitude = 'Altitude';

{ TWindowLayerSunCalcTimeInfo }

procedure TWindowLayerSunCalcTimeInfo.AfterConstruction;
begin
  inherited;
  FRepaintOnDayChange := True;
  FRepaintOnTimeChange := True;
  FRepaintOnLocationChange := True;
  FCaption := TSunCalcTimeInfoCaption.Create;
end;

destructor TWindowLayerSunCalcTimeInfo.Destroy;
begin
  FreeAndNil(FCaption);
  inherited;
end;

procedure TWindowLayerSunCalcTimeInfo.InvalidateLayer;
var
  VSunPos: TSunCalcProviderPosition;
  VAzimuth: string;
  VAltitude: string;
  VText: string;
  VRect: TFloatRect;
  VMarkerRect: TRect;
begin
  if FIsValid then begin
    FIsValid := False;
    DoInvalidateRect(FRect); // erase
  end;

  FIsValid := Visible and FShapesGenerator.IsIntersectScreenRect;

  if FIsValid then begin
    FShapesGenerator.ValidateCache;
    FShapesGenerator.GetTimeInfoPoints(FCurrentPos, FCenter);

    FIsValid := (FCurrentPos.X > 0) and (FCurrentPos.Y > 0);
    if not FIsValid then begin
      Exit;
    end;

    if FSunCalcConfig.ShowCaptionNearSun then begin
      VSunPos := FSunCalcDataProvider.GetPosition(FDateTime, FLocation);

      VAzimuth := Format('%.2f°', [RadToDeg(VSunPos.Azimuth + Pi)]);
      VAltitude := Format('%.2f°', [RadToDeg(VSunPos.Altitude)]);
      VText := Format('%s: %s; %s: %s', [rsAltitude, VAltitude, rsAzimuth, VAzimuth]);

      FCaption.SetText(VText, FFont, FColor.BgColor);

      VRect := FloatRect(FCaption.GetBoundsForPosition(GR32.Point(FCurrentPos)));
      UpdateRectByFloatPoint(VRect, FCenter);
    end else begin
      VRect := FloatRect(FCenter, FCenter);
    end;

    UpdateRectByFloatPoint(VRect, FCurrentPos);

    FRect := MakeRect(VRect, GR32.TRectRounding.rrOutside);
    GR32.InflateRect(FRect, CLineWidth, CLineWidth);

    if FSunCalcConfig.IsRealTime then begin
      VMarkerRect :=
        FSunCalcDataProvider.DayMarker.GetBoundsForPosition(
          DoublePoint(GR32.Point(FCurrentPos))
        );
      FRect := UnionRect(FRect, VMarkerRect);
    end;

    // draw
    if FMainFormState.IsMapMoving then begin
      DoInvalidateFull;
    end else begin
      DoInvalidateRect(FRect);
    end;
  end;
end;

procedure TWindowLayerSunCalcTimeInfo.PaintLayer(ABuffer: TBitmap32);
begin
  if not FIsValid then begin
    Exit;
  end;

  if ABuffer.MeasuringMode then begin
    ABuffer.Changed(FRect);
    Exit;
  end;

  ABuffer.BeginUpdate;
  try
    // Draw line to the current sun position
    DrawThickLine(ABuffer, FCenter, FCurrentPos, FShapesColors.DayLineColor, CLineWidth);

    // Draw sun marker
    if FSunCalcConfig.IsRealTime then begin
      FSunCalcDataProvider.DayMarker.DrawToBitmap(
        ABuffer,
        DoublePoint(GR32.Point(FCurrentPos))
      );
    end;

    // Draw caption with Azimuth and Altitude info
    if FSunCalcConfig.ShowCaptionNearSun then begin
      FCaption.DrawToBitmap(ABuffer, GR32.Point(FCurrentPos));
    end;
  finally
    ABuffer.EndUpdate;
  end;
end;

{ TSunCalcTimeInfoCaption }

constructor TSunCalcTimeInfoCaption.Create;
begin
  inherited Create;
  FText := '';
  FBitmap := nil;
end;

destructor TSunCalcTimeInfoCaption.Destroy;
begin
  FreeAndNil(FBitmap);
  inherited Destroy;
end;

function TSunCalcTimeInfoCaption.GetBoundsForPosition(const APosition: TPoint): TRect;
begin
  if FText <> '' then begin
    Result.Left := APosition.X + 12;
    Result.Top := APosition.Y;
    Result.Right := Result.Left + FBitmap.Width;
    Result.Bottom := Result.Top + FBitmap.Height;
  end else begin
    Result := Rect(APosition.X, APosition.Y, APosition.X, APosition.Y);
    Assert(False);
  end;
end;

procedure TSunCalcTimeInfoCaption.DrawToBitmap(ABuffer: TBitmap32; const APosition: TPoint);
var
  VRect: TRect;
begin
  VRect := GetBoundsForPosition(APosition);

  if ABuffer.MeasuringMode then begin
    ABuffer.Changed(VRect);
  end else begin
    FBitmap.DrawTo(ABuffer, VRect.Left, VRect.Top);
  end;
end;

procedure TSunCalcTimeInfoCaption.SetText(const AText: string; const AFont: TSunCalcFontInfo; const ABgColor: TColor32);
var
  VTextSize: TSize;
begin
  if FText = AText then begin
    Exit;
  end;

  FText := AText;

  if not Assigned(FBitmap) then begin
    FBitmap := TBitmap32.Create;
  end;

  FBitmap.Font.Size := AFont.FontSize;
  FBitmap.Font.Name := AFont.FontName;
  FBitmap.Font.Color := WinColor(AFont.TextColor);

  VTextSize := FBitmap.TextExtent(FText);

  FBitmap.SetSize(VTextSize.cx + 8, VTextSize.cy + 4);

  FBitmap.Clear(ABgColor);
  FBitmap.RenderText(4, 2, FText, 0, AFont.TextColor);
  FBitmap.DrawMode := dmBlend;
end;

end.
