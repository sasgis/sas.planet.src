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

unit u_MapCaptionDrawable;

interface

uses
  Types,
  GR32;

type
  TMapCaptionDrawable = class
  private
    FText: string;
    FBitmap: TBitmap32;
  public
    procedure SetText(
      const AText: string;
      const AFontName: string;
      const AFontSize: Integer;
      const AFontColor: TColor32;
      const ABgColor: TColor32;
      const AAntiAlias: Boolean = False
    );
    function GetBoundsForPosition(const APosition: TPoint): TRect;
    procedure DrawToBitmap(ABuffer: TBitmap32; const APosition: TPoint);
 public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  Graphics;

{ TMapCaptionDrawable }

constructor TMapCaptionDrawable.Create;
begin
  inherited Create;
  FText := '';
  FBitmap := nil;
end;

destructor TMapCaptionDrawable.Destroy;
begin
  FreeAndNil(FBitmap);
  inherited Destroy;
end;

procedure TMapCaptionDrawable.SetText(
  const AText: string;
  const AFontName: string;
  const AFontSize: Integer;
  const AFontColor: TColor32;
  const ABgColor: TColor32;
  const AAntiAlias: Boolean
);
const
  CBorderSize = 2; // pixels
var
  VFont: TFont;
  VTextSize: TSize;
begin
  if FText = AText then begin
    Exit;
  end;

  FText := AText;

  if not Assigned(FBitmap) then begin
    FBitmap := TBitmap32.Create;
  end;

  VFont := FBitmap.Font;

  VFont.Size := AFontSize;
  VFont.Name := AFontName;
  VFont.Color := WinColor(AFontColor);

  FBitmap.Font := VFont;

  VTextSize := FBitmap.TextExtent(FText);

  FBitmap.SetSize(
    VTextSize.cx + CBorderSize * 2,
    VTextSize.cy + CBorderSize * 2
  );

  FBitmap.Clear(ABgColor);
  FBitmap.RenderText(CBorderSize, CBorderSize, FText, AFontColor, AAntiAlias);
  FBitmap.DrawMode := dmBlend;
end;

function TMapCaptionDrawable.GetBoundsForPosition(const APosition: TPoint): TRect;
const
  CLeftOffset = 12; // pixels
begin
  if FText <> '' then begin
    Result.Left := APosition.X + CLeftOffset;
    Result.Top := APosition.Y;
    Result.Right := Result.Left + FBitmap.Width;
    Result.Bottom := Result.Top + FBitmap.Height;
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
    FBitmap.DrawTo(ABuffer, VRect.Left, VRect.Top);
  end;
end;

end.
