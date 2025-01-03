{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
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

unit u_MapCaptionDrawable;

interface

uses
  Types,
  UITypes,
  GR32;

type
  // Set AUseTextout = True if you need TFontQuality other then fqAntialiased or fqNonAntialiased

  TMapCaptionDrawable = class
  private const
    CDefaultFontQuality = TFontQuality.fqNonAntialiased;
  private
    FText: string;
    FBitmap: TBitmap32;
    FTextSize: TSize;
    FBgColor: TColor32;
    FUseTextout: Boolean;
    function GetBitmapWidth: Integer; inline;
    function GetBitmapHeight: Integer; inline;
  public
    procedure SetText(
      const AText: string;
      const ABgColor: TColor32;
      const AFontName: string;
      const AFontSize: Integer;
      const AFontColor: TColor32;
      const AFontQuality: TFontQuality = CDefaultFontQuality
    );
    function GetBoundsForPosition(const APosition: TPoint): TRect;
    procedure DrawToBitmap(ABuffer: TBitmap32; const APosition: TPoint);
  public
    constructor Create(
      const AUseTextout: Boolean = False
    );
    destructor Destroy; override;
  end;

implementation

uses
  Windows,
  SysUtils,
  Graphics;

const
  CBorderSize = 2;
  CLeftOffset = 12;

{ TMapCaptionDrawable }

constructor TMapCaptionDrawable.Create(
  const AUseTextout: Boolean
);
begin
  inherited Create;
  FText := '';
  FBitmap := nil;
  FTextSize := TSize.Create(0, 0); // record initialization
  FUseTextout := AUseTextout;
end;

destructor TMapCaptionDrawable.Destroy;
begin
  FreeAndNil(FBitmap);
  inherited Destroy;
end;

function TMapCaptionDrawable.GetBitmapWidth: Integer;
begin
  Result := FTextSize.cx + CBorderSize * 2;
end;

function TMapCaptionDrawable.GetBitmapHeight: Integer;
begin
  Result:= FTextSize.cy + CBorderSize * 2;
end;

procedure TMapCaptionDrawable.SetText(
  const AText: string;
  const ABgColor: TColor32;
  const AFontName: string;
  const AFontSize: Integer;
  const AFontColor: TColor32;
  const AFontQuality: TFontQuality
);
var
  VFont: TFont;
begin
  if FText = AText then begin
    Exit;
  end;

  FText := AText;
  FBgColor := ABgColor;

  if not Assigned(FBitmap) then begin
    FBitmap := TBitmap32.Create;
  end;

  VFont := FBitmap.Font;

  VFont.Size := AFontSize;
  VFont.Name := AFontName;
  VFont.Color := WinColor(AFontColor);
  VFont.Quality := AFontQuality;

  FBitmap.Font := VFont;

  FTextSize := FBitmap.TextExtent(FText);

  if not FUseTextout then begin
    FBitmap.SetSize(GetBitmapWidth, GetBitmapHeight);
    FBitmap.Clear(FBgColor);
    FBitmap.RenderText(CBorderSize, CBorderSize, FText, AFontColor, AFontQuality = fqAntialiased);
    FBitmap.DrawMode := dmBlend;
  end;
end;

function TMapCaptionDrawable.GetBoundsForPosition(const APosition: TPoint): TRect;
begin
  if FText <> '' then begin
    Result.Left := APosition.X + CLeftOffset;
    Result.Top := APosition.Y;
    Result.Right := Result.Left + GetBitmapWidth;
    Result.Bottom := Result.Top + GetBitmapHeight;
  end else begin
    Result := Rect(APosition.X, APosition.Y, APosition.X, APosition.Y);
    Assert(False);
  end;
end;

procedure TMapCaptionDrawable.DrawToBitmap(ABuffer: TBitmap32; const APosition: TPoint);
var
  VRect: TRect;
begin
  VRect := GetBoundsForPosition(APosition);
  if ABuffer.MeasuringMode then begin
    ABuffer.Changed(VRect);
  end else begin
    if FUseTextout then begin
      ABuffer.BeginUpdate;
      try
        ABuffer.FillRectTS(VRect, FBgColor);
        ABuffer.Font := FBitmap.Font;
        ABuffer.Textout(VRect.Left + CBorderSize, VRect.Top + CBorderSize, FText);
      finally
        ABuffer.EndUpdate;
      end;
    end else begin
      FBitmap.DrawTo(ABuffer, VRect.Left, VRect.Top);
    end;
  end;
end;

end.
