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

unit u_BitmapTileFreeImage;

interface

uses
  FreeImage,
  i_BinaryData,
  i_Bitmap32Static,
  i_BitmapTileSaveLoad,
  i_InternalPerformanceCounter;

type
  TBitmapTileFreeImageLoader = class (TInterfacedObject, IBitmapTileLoader)
  private
    FCounter: IInternalPerformanceCounter;
  private
    function Load(const AData: IBinaryData): IBitmap32Static;
  public
    constructor Create(const APerfCounterList: IInternalPerformanceCounterList);
    destructor Destroy; override;
  end;

  TBitmapTileFreeImageLoaderBmp = class(TBitmapTileFreeImageLoader)
  public
    constructor Create(
      const APerfCounterList: IInternalPerformanceCounterList
    );
  end;

  TBitmapTileFreeImageLoaderIco = class(TBitmapTileFreeImageLoader)
  public
    constructor Create(
      const APerfCounterList: IInternalPerformanceCounterList
    );
  end;

  TBitmapTileFreeImageLoaderGif = class(TBitmapTileFreeImageLoader)
  public
    constructor Create(
      const APerfCounterList: IInternalPerformanceCounterList
    );
  end;

  TBitmapTileFreeImageLoaderPng = class(TBitmapTileFreeImageLoader)
  public
    constructor Create(
      const APerfCounterList: IInternalPerformanceCounterList
    );
  end;

  TBitmapTileFreeImageSaver = class (TInterfacedObject, IBitmapTileSaver)
  private
    FFormat: FREE_IMAGE_FORMAT;
    FBitPerPixel: Byte;
    FFlag: Integer;
    FCounter: IInternalPerformanceCounter;
  private
    function Save(const ABitmap: IBitmap32Static): IBinaryData;
  public
    constructor Create(
      const AFormat: FREE_IMAGE_FORMAT;
      const APngCompress: Byte;
      const ABitPerPixel: Byte;
      const APerfCounterList: IInternalPerformanceCounterList
    );
    destructor Destroy; override;
  end;

  TBitmapTileFreeImageSaverBmp = class(TBitmapTileFreeImageSaver)
  public
    constructor Create(
      const APerfCounterList: IInternalPerformanceCounterList
    );
  end;

  TBitmapTileFreeImageSaverGif = class(TBitmapTileFreeImageSaver)
  public
    constructor Create(
      const APerfCounterList: IInternalPerformanceCounterList
    );
  end;

  TBitmapTileFreeImageSaverPng = class(TBitmapTileFreeImageSaver)
  public
    constructor Create(
      const APngCompress: Byte;
      const ABitPerPixel: Integer;
      const APerfCounterList: IInternalPerformanceCounterList
    );
  end;

implementation

uses
  Windows,
  Classes,
  SysUtils,
  GR32,
  FreeBitmap,
  u_Bitmap32Static,
  u_BinaryDataByMemStream;

type
  EBitmapTileFreeImageLoader = class (Exception);
  EBitmapTileFreeImageSaver = class (Exception);

const
  cPngCompressLevelUndef = 255;
  
{ TBitmapTileFreeImageLoader }

constructor TBitmapTileFreeImageLoader.Create(
  const APerfCounterList: IInternalPerformanceCounterList
);
begin
  inherited Create;
  FCounter := APerfCounterList.CreateAndAddNewCounter('Load');
end;

destructor TBitmapTileFreeImageLoader.Destroy;
begin
  FCounter := nil;
  inherited Destroy;
end;

function TBitmapTileFreeImageLoader.Load(
  const AData: IBinaryData
): IBitmap32Static;
var
  I: Integer;
  VScanLineSize: Integer;
  VBitmap: TCustomBitmap32;
  VFreeBitmap: TFreeWinBitmap;
  VFreeMemoryIO: TFreeMemoryIO;
  VCounterContext: TInternalPerformanceCounterContext;
begin
  Result := nil;
  VCounterContext := FCounter.StartOperation;
  try
    VFreeMemoryIO := TFreeMemoryIO.Create(AData.Buffer, AData.Size);
    try
      VFreeBitmap := TFreeWinBitmap.Create(FIT_BITMAP);
      try
        if VFreeBitmap.LoadFromMemory(VFreeMemoryIO) then begin
          if VFreeBitmap.ConvertTo32Bits then begin
            VBitmap := TCustomBitmap32.Create;
            try
              VBitmap.Width := VFreeBitmap.GetWidth;
              VBitmap.Height := VFreeBitmap.GetHeight;
              VScanLineSize := VFreeBitmap.GetScanWidth;

              Assert(VScanLineSize = VBitmap.Width * 4);

              for I := 0 to VBitmap.Height - 1 do begin
                Move(
                  VFreeBitmap.GetScanLine(VBitmap.Height - 1 - I)^,
                  VBitmap.ScanLine[I]^,
                  VScanLineSize
                );
              end;
              Result := TBitmap32Static.CreateWithOwn(VBitmap);
              VBitmap := nil;
            finally
              VBitmap.Free;
            end;
          end else begin
            raise EBitmapTileFreeImageLoader.Create(
              'FreeBitmap.ConvertTo32Bits FAIL!'
            );
          end;
        end else begin
          raise EBitmapTileFreeImageLoader.Create(
            'FreeBitmap.LoadFromMemory FAIL!'
          );
        end;
      finally
        VFreeBitmap.Free;
      end;
    finally
      VFreeMemoryIO.Free;
    end;
  finally
    FCounter.FinishOperation(VCounterContext);
  end;
end;

{ TBitmapTileFreeImageLoaderBmp }

constructor TBitmapTileFreeImageLoaderBmp.Create(
  const APerfCounterList: IInternalPerformanceCounterList
);
begin
  inherited Create(
    APerfCounterList.CreateAndAddNewSubList('FreeImage/Bmp')
  );
end;

{ TBitmapTileFreeImageLoaderIco }

constructor TBitmapTileFreeImageLoaderIco.Create(
  const APerfCounterList: IInternalPerformanceCounterList
);
begin
  inherited Create(
    APerfCounterList.CreateAndAddNewSubList('FreeImage/Ico')
  );
end;

{ TBitmapTileFreeImageLoaderGif }

constructor TBitmapTileFreeImageLoaderGif.Create(
  const APerfCounterList: IInternalPerformanceCounterList
);
begin
  inherited Create(
    APerfCounterList.CreateAndAddNewSubList('FreeImage/Gif')
  );
end;

{ TBitmapTileFreeImageLoaderPng }

constructor TBitmapTileFreeImageLoaderPng.Create(
  const APerfCounterList: IInternalPerformanceCounterList
);
begin
  inherited Create(
    APerfCounterList.CreateAndAddNewSubList('FreeImage/Png')
  );
end;

{ TBitmapTileFreeImageSaver }

constructor TBitmapTileFreeImageSaver.Create(
  const AFormat: FREE_IMAGE_FORMAT;
  const APngCompress: Byte;
  const ABitPerPixel: Byte;
  const APerfCounterList: IInternalPerformanceCounterList
);
begin
  inherited Create;
  FFormat := AFormat;
  FBitPerPixel := ABitPerPixel;
  FCounter := APerfCounterList.CreateAndAddNewCounter('Save');
  case FFormat of
    FIF_PNG:
      case APngCompress  of
        1..4:
          FFlag := PNG_Z_BEST_SPEED;
        5..7:
          FFlag := PNG_Z_DEFAULT_COMPRESSION;
        8, 9:
          FFlag := PNG_Z_BEST_COMPRESSION;
      else // 0
        FFlag := PNG_Z_NO_COMPRESSION;
      end;
  else // FIF_BMP, FIF_GIF
    FFlag := 0;
  end;
end;

destructor TBitmapTileFreeImageSaver.Destroy;
begin
  FCounter := nil;
  inherited Destroy;
end;

function TBitmapTileFreeImageSaver.Save(
  const ABitmap: IBitmap32Static
): IBinaryData;
var
  I: Integer;
  VScanLineSize: Integer;
  VMemStream: TMemoryStream;
  VFreeBitmap: TFreeWinBitmap;
  VCounterContext: TInternalPerformanceCounterContext;
  VPalette: PRGBQUAD;
  VPaletteSize: Integer;
  VTransparencyTable: array [0..255] of Byte;
  VSize: TPoint;
begin
  VCounterContext := FCounter.StartOperation;
  try
    VSize := ABitmap.Size;
    VFreeBitmap := TFreeWinBitmap.Create(
      FIT_BITMAP,
      VSize.X,
      VSize.Y,
      32
    );
    try
      VScanLineSize := VFreeBitmap.GetScanWidth;

      Assert(VScanLineSize = VSize.X * 4);

      for I := 0 to VSize.Y - 1 do begin
        Move(
          ABitmap.Data[I * VSize.X],
          VFreeBitmap.GetScanLine(VSize.Y - 1 - I)^,
          VScanLineSize
        );
      end;

      if FBitPerPixel = 8 then begin // PNG with palette, GIF
        if VFreeBitmap.ConvertTo24Bits then begin
          if VFreeBitmap.ColorQuantize(FIQ_WUQUANT) then begin
            // Restore transparent background 
            VPalette := VFreeBitmap.GetPalette;
            VPaletteSize := VFreeBitmap.GetPaletteSize div SizeOf(PRGBQUAD);
            if (VPalette <> nil) and (VPaletteSize <= 256) then begin
              for I := 0 to VPaletteSize - 1 do begin
                if (VPalette.rgbGreen >= $FE) and
                   (VPalette.rgbBlue = $00) and
                   (VPalette.rgbRed = $00) then
                begin
                  VTransparencyTable[I] := $00;
                end else begin
                  VTransparencyTable[I] := $FF;
                end;
                Inc(VPalette);
              end;
              VFreeBitmap.SetTransparencyTable(
                @VTransparencyTable[0],
                VPaletteSize
              );
            end;
          end else begin
            raise EBitmapTileFreeImageSaver.Create(
              'FreeBitmap.ColorQuantize FAIL!'
            );
          end;
        end else begin
          raise EBitmapTileFreeImageSaver.Create(
            'Palette FreeBitmap.ConvertTo24Bits FAIL!'
          );
        end;
      end else if FBitPerPixel = 24 then begin // PNG without alfa
        if not VFreeBitmap.ConvertTo24Bits then begin
          raise EBitmapTileFreeImageSaver.Create(
            'FreeBitmap.ConvertTo24Bits FAIL!'
          );
        end;
      end;

      VMemStream := TMemoryStream.Create;
      try
        if not VFreeBitmap.SaveToStream(FFormat, VMemStream, FFlag) then begin
          raise EBitmapTileFreeImageSaver.Create(
            'FreeBitmap.SaveToStream FAIL!'
          );
        end;
        Result := TBinaryDataByMemStream.CreateWithOwn(VMemStream);
        VMemStream := nil;
      finally
        VMemStream.Free;
      end;
    finally
      VFreeBitmap.Free;
    end;
  finally
    FCounter.FinishOperation(VCounterContext);
  end;
end;

{ TBitmapTileFreeImageSaverBmp }

constructor TBitmapTileFreeImageSaverBmp.Create(
  const APerfCounterList: IInternalPerformanceCounterList
);
begin
  inherited Create(
    FIF_BMP,
    cPngCompressLevelUndef,
    32,
    APerfCounterList.CreateAndAddNewSubList('FreeImage/Bmp')
  );
end;

{ TBitmapTileFreeImageSaverGif }

constructor TBitmapTileFreeImageSaverGif.Create(
  const APerfCounterList: IInternalPerformanceCounterList
);
begin
  inherited Create(
    FIF_GIF,
    cPngCompressLevelUndef,
    8,
    APerfCounterList.CreateAndAddNewSubList('FreeImage/Gif')
  );
end;

{ TBitmapTileFreeImageSaverPng }

constructor TBitmapTileFreeImageSaverPng.Create(
  const APngCompress: Byte;
  const ABitPerPixel: Integer;
  const APerfCounterList: IInternalPerformanceCounterList
);
begin
  inherited Create(
    FIF_PNG,
    APngCompress,
    ABitPerPixel,
    APerfCounterList.CreateAndAddNewSubList('FreeImage/Png')
  );
end;

end.
