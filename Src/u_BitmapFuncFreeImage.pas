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

unit u_BitmapFuncFreeImage;

interface

uses
  GR32,
  FreeImage,
  i_Bitmap32Static;

function Bitmap32StaticToFiBitmap(const ABitmap: IBitmap32Static): PFIBITMAP;
procedure FiBitmapToBitmap32(const AFiBitmap: PFIBITMAP; const ABitmap: TCustomBitmap32);

implementation

function Bitmap32StaticToFiBitmap(const ABitmap: IBitmap32Static): PFIBITMAP;
var
  VWidth, VHeight: Integer;
  VSrc: PColor32Array;
  VDest: PByte;
begin
  VWidth := ABitmap.Size.X;
  VHeight := ABitmap.Size.Y;

  Result := FreeImage_Allocate(VWidth, VHeight, 32, FI_RGBA_RED_MASK, FI_RGBA_GREEN_MASK, FI_RGBA_BLUE_MASK);
  if not Assigned(Result) then begin
    Assert(False);
    Exit;
  end;

  VSrc := ABitmap.Data;
  VDest := FreeImage_GetBits(Result);

  if Assigned(VSrc) and Assigned(VDest) then begin
    Move(VSrc^, VDest^, VWidth * VHeight * SizeOf(TColor32));
  end else begin
    Assert(False);
  end;
end;

procedure FiBitmapToBitmap32(const AFiBitmap: PFIBITMAP; const ABitmap: TCustomBitmap32);
var
  VWidth, VHeight: Integer;
  VSrc: PByte;
  VDest: PColor32Array;
begin
  VWidth := FreeImage_GetWidth(AFiBitmap);
  VHeight := FreeImage_GetHeight(AFiBitmap);

  ABitmap.SetSize(VWidth, VHeight);

  VSrc := FreeImage_GetBits(AFiBitmap);
  VDest := ABitmap.Bits;

  if Assigned(VSrc) and Assigned(VDest) then begin
    Move(VSrc^, VDest^, VWidth * VHeight * SizeOf(TColor32));
  end else begin
    Assert(False);
  end;
end;

end.
