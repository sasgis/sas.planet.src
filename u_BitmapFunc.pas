{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
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

unit u_BitmapFunc;

interface

uses
  GR32,
  i_Bitmap32Static;

procedure AssignStaticToBitmap32(
  ADst: TCustomBitmap32;
  const ASource: IBitmap32Static
);

procedure StretchTransferFull(
  ADst: TCustomBitmap32;
  const ADstRect: TRect;
  const ASource: IBitmap32Static;
  AResampler: TCustomResampler;
  ACombineOp: TDrawMode;
  ACombineMode: TCombineMode = cmMerge;
  AMasterAlpha: Cardinal = 255;
  AOuterColor: TColor32 = 0
);

procedure StretchTransfer(
  ADst: TCustomBitmap32;
  const ADstRect: TRect;
  const ASource: IBitmap32Static;
  const ASrcRect: TRect;
  AResampler: TCustomResampler;
  ACombineOp: TDrawMode;
  ACombineMode: TCombineMode = cmMerge;
  AMasterAlpha: Cardinal = 255;
  AOuterColor: TColor32 = 0
);
       
procedure BlockTransferFull(
  ADst: TCustomBitmap32;
  ADstX: Integer;
  ADstY: Integer;
  const ASource: IBitmap32Static;
  ACombineOp: TDrawMode;
  ACombineMode: TCombineMode = cmMerge;
  AMasterAlpha: Cardinal = 255;
  AOuterColor: TColor32 = 0
);

procedure BlockTransfer(
  ADst: TCustomBitmap32;
  ADstX: Integer;
  ADstY: Integer;
  const ASource: IBitmap32Static;
  const ASrcRect: TRect;
  ACombineOp: TDrawMode;
  ACombineMode: TCombineMode = cmMerge;
  AMasterAlpha: Cardinal = 255;
  AOuterColor: TColor32 = 0
);

procedure CopyBitmap32StaticToBitmap32(
  const ASourceBitmap: IBitmap32Static;
  ATarget: TCustomBitmap32
);

implementation

uses
  Types,
  Math,
  GR32_LowLevel,
  GR32_Resamplers;

procedure AssignStaticToBitmap32(
  ADst: TCustomBitmap32;
  const ASource: IBitmap32Static
);
var
  VSize: TPoint;
begin
  VSize := ASource.Size;
  ADst.SetSize(VSize.X, VSize.Y);
  MoveLongword(ASource.Data[0], ADst.Bits[0], VSize.X * VSize.Y);
end;

procedure StretchTransferFull(
  ADst: TCustomBitmap32;
  const ADstRect: TRect;
  const ASource: IBitmap32Static;
  AResampler: TCustomResampler;
  ACombineOp: TDrawMode;
  ACombineMode: TCombineMode = cmMerge;
  AMasterAlpha: Cardinal = 255;
  AOuterColor: TColor32 = 0
);
var
  VSize: TPoint;
begin
  VSize := ASource.Size;
  GR32_Resamplers.StretchTransfer(
    ADst,
    ADstRect,
    ADst.ClipRect,
    ASource.Data,
    VSize.X,
    VSize.Y,
    Bounds(0, 0, VSize.X, VSize.Y),
    AResampler,
    ACombineOp,
    ACombineMode,
    AMasterAlpha,
    AOuterColor
  );
end;

procedure StretchTransfer(
  ADst: TCustomBitmap32;
  const ADstRect: TRect;
  const ASource: IBitmap32Static;
  const ASrcRect: TRect;
  AResampler: TCustomResampler;
  ACombineOp: TDrawMode;
  ACombineMode: TCombineMode;
  AMasterAlpha: Cardinal;
  AOuterColor: TColor32
);
var
  VSize: TPoint;
begin
  VSize := ASource.Size;
  GR32_Resamplers.StretchTransfer(
    ADst,
    ADstRect,
    ADst.ClipRect,
    ASource.Data,
    VSize.X,
    VSize.Y,
    ASrcRect,
    AResampler,
    ACombineOp,
    ACombineMode,
    AMasterAlpha,
    AOuterColor
  );
end;

procedure BlockTransferFull(
  ADst: TCustomBitmap32;
  ADstX: Integer;
  ADstY: Integer;
  const ASource: IBitmap32Static;
  ACombineOp: TDrawMode;
  ACombineMode: TCombineMode;
  AMasterAlpha: Cardinal;
  AOuterColor: TColor32
);
var
  VSize: TPoint;
begin
  VSize := ASource.Size;
  GR32_Resamplers.BlockTransfer(
    ADst,
    ADstX,
    ADstY,
    ADst.ClipRect,
    ASource.Data,
    VSize.X,
    VSize.Y,
    Bounds(0, 0, VSize.X, VSize.Y),
    ACombineOp,
    ACombineMode,
    AMasterAlpha,
    AOuterColor
  );
end;

procedure BlockTransfer(
  ADst: TCustomBitmap32;
  ADstX: Integer;
  ADstY: Integer;
  const ASource: IBitmap32Static;
  const ASrcRect: TRect;
  ACombineOp: TDrawMode;
  ACombineMode: TCombineMode;
  AMasterAlpha: Cardinal;
  AOuterColor: TColor32
);
var
  VSize: TPoint;
begin
  VSize := ASource.Size;
  GR32_Resamplers.BlockTransfer(
    ADst,
    ADstX,
    ADstY,
    ADst.ClipRect,
    ASource.Data,
    VSize.X,
    VSize.Y,
    ASrcRect,
    ACombineOp,
    ACombineMode,
    AMasterAlpha,
    AOuterColor
  );
end;

procedure CopyBitmap32StaticToBitmap32(
  const ASourceBitmap: IBitmap32Static;
  ATarget: TCustomBitmap32
);
var
  VSourceSize: TPoint;
  VScale: Double;
  VSourceRect: TRect;
  VDstRect: TRect;
  VResampler: TCustomResampler;
begin
  VSourceSize := ASourceBitmap.Size;
  if (VSourceSize.X > 0) and (VSourceSize.Y > 0) then begin
    ATarget.Clear(clWhite32);
    VScale := Min(ATarget.Width / VSourceSize.X, ATarget.Height / VSourceSize.Y);
    VSourceRect := Rect(0, 0, VSourceSize.X, VSourceSize.Y);
    VDstRect :=
      Rect(
        Trunc((ATarget.Width - VSourceSize.X * VScale) / 2),
        Trunc((ATarget.Height - VSourceSize.Y * VScale) / 2),
        Trunc((ATarget.Width + VSourceSize.X * VScale) / 2),
        Trunc((ATarget.Height + VSourceSize.Y * VScale) / 2)
      );
    VResampler := TLinearResampler.Create;
    try
      StretchTransfer(
        ATarget,
        VDstRect,
        ASourceBitmap,
        VSourceRect,
        VResampler,
        dmBlend,
        cmBlend
      );
    finally
      VResampler.Free;
    end;
  end;
end;

end.
