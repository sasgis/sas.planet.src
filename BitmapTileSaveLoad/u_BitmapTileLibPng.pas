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

unit u_BitmapTileLibPng;

interface

uses
  Types,
  Classes,
  GR32,
  i_InternalPerformanceCounter,
  i_BinaryData,
  i_Bitmap32Static,
  i_BitmapTileSaveLoad,
  i_Bitmap32To8Converter,
  u_BaseInterfacedObject;

type
  TLibPngTileSaver = class(TBaseInterfacedObject, IBitmapTileSaver)
  private
    FSaveCounter: IInternalPerformanceCounter;
    FCompression: Integer;
    FBitsPerPixel: Integer;
    FBitmap32To8Converter: IBitmap32To8Converter;
    function GetLineCallBack(
      const ARowNumber: Integer;
      const ALineSize: Integer;
      const AUserInfo: Pointer
    ): Pointer;
  private
    function Save(const ABitmap: IBitmap32Static): IBinaryData;
  public
    constructor Create(
      const APerfCounterList: IInternalPerformanceCounterList;
      const ACompression: Integer = 2;
      const ABitsPerPixel: Integer = 32;
      const ABitmap32To8Converter: IBitmap32To8Converter = nil
    );
  end;

implementation

uses
  LibPngWriter,
  i_Bitmap8Static,
  u_BinaryDataByMemStream;

type
  TColor32Rec = packed record
    B, G, R, A: Byte;
  end;
  PColor32Rec = ^TColor32Rec;

  TLibPngTileSaverInfo = record
    FBpp: Integer;
    FData: Pointer;
    FSize: TPoint;
    FLine: Pointer;
    FLineSize: Integer;
  end;
  PLibPngTileSaverInfo = ^TLibPngTileSaverInfo;

{ TLibPngTileSaver }

constructor TLibPngTileSaver.Create(
  const APerfCounterList: IInternalPerformanceCounterList;
  const ACompression: Integer;
  const ABitsPerPixel: Integer;
  const ABitmap32To8Converter: IBitmap32To8Converter
);
begin
  inherited Create;
  FSaveCounter := APerfCounterList.CreateAndAddNewCounter('LibPng/SaveStream');
  FCompression := ACompression;
  FBitsPerPixel := ABitsPerPixel;
  FBitmap32To8Converter := ABitmap32To8Converter;
  if FBitsPerPixel = 8 then begin
    Assert(FBitmap32To8Converter <> nil);
  end;
end;

function TLibPngTileSaver.Save(const ABitmap: IBitmap32Static): IBinaryData;
var
  VCounterContext: TInternalPerformanceCounterContext;
  VPng: TLibPngWriter;
  VMemStream: TMemoryStream;
  VBitmap8Static: IBitmap8Static;
  VPalette: PColor32Array;
  VPaletteSize: Integer;
  VInfo: TLibPngTileSaverInfo;
begin
  Result := nil;

  VCounterContext := FSaveCounter.StartOperation;
  try
    VMemStream := TMemoryStream.Create;
    try
      FillChar(VInfo, SizeOf(TLibPngTileSaverInfo), 0);
      VInfo.FBpp := FBitsPerPixel;
      if FBitsPerPixel = 8 then begin
        VBitmap8Static := FBitmap32To8Converter.Convert(ABitmap);
        Assert(VBitmap8Static <> nil);
        VInfo.FData := VBitmap8Static.Data;
        VInfo.FSize := VBitmap8Static.Size;
        VInfo.FLineSize := VInfo.FSize.X;
        VPalette := VBitmap8Static.Palette;
        VPaletteSize := VBitmap8Static.PaletteSize;
      end else begin
        VInfo.FData := ABitmap.Data;
        VInfo.FSize := ABitmap.Size;
        VPalette := nil;
        VPaletteSize := 0;
      end;
      VInfo.FLineSize := VInfo.FSize.X * (FBitsPerPixel div 8);
      VInfo.FLine := GetMemory(VInfo.FLineSize);
      try
        VPng := TLibPngWriter.Create;
        try
          VPng.Write(
            VMemStream,
            VInfo.FSize.X,
            VInfo.FSize.Y,
            FBitsPerPixel,
            Self.GetLineCallBack,
            @VInfo,
            FCompression,
            VPalette,
            VPaletteSize
          );
          Result := TBinaryDataByMemStream.CreateWithOwn(VMemStream);
          VMemStream := nil;
        finally
          VPng.Free;
        end;
      finally
        FreeMemory(VInfo.FLine);
      end;
    finally
      VMemStream.Free;
    end;
  finally
    FSaveCounter.FinishOperation(VCounterContext);
  end;
end;

function TLibPngTileSaver.GetLineCallBack(
  const ARowNumber: Integer;
  const ALineSize: Integer;
  const AUserInfo: Pointer
): Pointer;
var
  I, J: Integer;
  VLine: PByte;
  VInfo: PLibPngTileSaverInfo;
  VBGRALine: PColor32Array;
  VBGRAPixel: TColor32Rec;
begin
  VInfo := PLibPngTileSaverInfo(AUserInfo);
  J := ARowNumber * VInfo.FSize.X;

  if VInfo.FBpp = 8 then begin
    VLine := PByte(VInfo.FData);
    Inc(VLine, J);
  end else begin // 24 and 32 bpp
    VLine := PByte(VInfo.FLine);
    VBGRALine := PColor32Array(VInfo.FData);

    // BGR to RGB conversion (can optimize it?)
    for I := 0 to VInfo.FSize.X - 1 do begin
      VBGRAPixel := TColor32Rec(VBGRALine[I + J]);

      VLine^ := VBGRAPixel.R;
      Inc(VLine);

      VLine^ := VBGRAPixel.G;
      Inc(VLine);

      VLine^ := VBGRAPixel.B;
      Inc(VLine);

      if VInfo.FBpp = 32 then begin
        VLine^ := VBGRAPixel.A;
        Inc(VLine);
      end;
    end;

    VLine := VInfo.FLine;
  end;

  Result := VLine;
end;

end.
