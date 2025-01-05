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

unit u_BitmapTileSaveLoadFactory;

interface

uses
  i_BitmapTileSaveLoad,
  i_BitmapTileSaveLoadFactory,
  i_Bitmap32BufferFactory,
  i_Bitmap32To8Converter,
  i_InternalPerformanceCounter,
  u_BaseInterfacedObject;

type
  TBitmapTileSaveLoadFactory = class(
    TBaseInterfacedObject,
    IBitmapTileSaveLoadFactory
  )
  private
    FBitmap32StaticFactory: IBitmap32StaticFactory;
    FBitmap32To8Converter: IBitmap32To8Converter;
  private
     // BMP
    function CreateBmpLoader(
      const APerfCounterList: IInternalPerformanceCounterList = nil
    ): IBitmapTileLoader;

    function CreateBmpSaver(
      const APerfCounterList: IInternalPerformanceCounterList = nil
    ): IBitmapTileSaver;

    // GIF
    function CreateGifLoader(
      const APerfCounterList: IInternalPerformanceCounterList = nil
    ): IBitmapTileLoader;

    function CreateGifSaver(
      const APerfCounterList: IInternalPerformanceCounterList = nil
    ): IBitmapTileSaver;

    // PNG
    function CreatePngLoader(
      const APerfCounterList: IInternalPerformanceCounterList = nil
    ): IBitmapTileLoader;

    function CreatePngSaver(
      const AColorDepth: TImageColorBitPerPix = i32bpp;
      const ACompressionLevel: Byte = 6;
      const APerfCounterList: IInternalPerformanceCounterList = nil
    ): IBitmapTileSaver;

    // JPEG
    function CreateJpegLoader(
      const APerfCounterList: IInternalPerformanceCounterList = nil
    ): IBitmapTileLoader;

    function CreateJpegSaver(
      const ACompressionQuality: Byte = 75;
      const APerfCounterList: IInternalPerformanceCounterList = nil
    ): IBitmapTileSaver;

    // TIFF
    function CreateTiffLoader(
      const APerfCounterList: IInternalPerformanceCounterList = nil
    ): IBitmapTileLoader;

    function CreateTiffSaver(
      const AColorDepth: TImageColorBitPerPix;
      const ACompressionType: TImageTiffCompression;
      const APerfCounterList: IInternalPerformanceCounterList = nil
    ): IBitmapTileSaver;
    
    // WebP
    function CreateWebpLoader(
      const APerfCounterList: IInternalPerformanceCounterList = nil
    ): IBitmapTileLoader;

    function CreateWebpSaver(
      const ACompressionQuality: Byte = 75;
      const APerfCounterList: IInternalPerformanceCounterList = nil
    ): IBitmapTileSaver;

    function CreateWebpLosslessSaver(
      const APerfCounterList: IInternalPerformanceCounterList = nil
    ): IBitmapTileSaver;
  public
    constructor Create(
      const ABitmap32StaticFactory: IBitmap32StaticFactory
    );
  end;

implementation

uses
  SysUtils,
  FreeImage,
  u_BitmapTileLibJpeg,
  u_BitmapTileFreeImage,
  u_Bitmap32To8ConverterByFreeImage,
  u_Bitmap32To8ConverterByLibImageQuant,
  u_InternalPerformanceCounterFake;

function GetValidPerfCounterList(
  const APerfCounterList: IInternalPerformanceCounterList
): IInternalPerformanceCounterList;
begin
  Result := APerfCounterList;
  if not Assigned(Result) then begin
    Result := TInternalPerformanceCounterFake.Create;
  end;
end;

{ TBitmapTileSaveLoadFactory }

constructor TBitmapTileSaveLoadFactory.Create(
  const ABitmap32StaticFactory: IBitmap32StaticFactory
);
begin
  Assert(Assigned(ABitmap32StaticFactory));
  inherited Create;
  FBitmap32StaticFactory := ABitmap32StaticFactory;
  try
    FBitmap32To8Converter := TBitmap32To8ConverterByLibImageQuant.Create;
  except
    FBitmap32To8Converter := TBitmap32To8ConverterByFreeImage.Create;
  end;
end;

function TBitmapTileSaveLoadFactory.CreateBmpLoader(
  const APerfCounterList: IInternalPerformanceCounterList
): IBitmapTileLoader;
begin
  Result := TBitmapTileFreeImageLoaderBmp.Create(
    GetValidPerfCounterList(APerfCounterList),
    FBitmap32StaticFactory
  );
end;

function TBitmapTileSaveLoadFactory.CreateBmpSaver(
  const APerfCounterList: IInternalPerformanceCounterList
): IBitmapTileSaver;
begin
  Result := TBitmapTileFreeImageSaverBmp.Create(
    GetValidPerfCounterList(APerfCounterList)
  );
end;

function TBitmapTileSaveLoadFactory.CreateGifLoader(
  const APerfCounterList: IInternalPerformanceCounterList
): IBitmapTileLoader;
begin
  Result := TBitmapTileFreeImageLoaderGif.Create(
    GetValidPerfCounterList(APerfCounterList),
    FBitmap32StaticFactory
  );
end;

function TBitmapTileSaveLoadFactory.CreateGifSaver(
  const APerfCounterList: IInternalPerformanceCounterList
): IBitmapTileSaver;
begin
  Result := TBitmapTileFreeImageSaverGif.Create(
    FBitmap32To8Converter,
    GetValidPerfCounterList(APerfCounterList)
  );
end;

function TBitmapTileSaveLoadFactory.CreatePngLoader(
  const APerfCounterList: IInternalPerformanceCounterList
): IBitmapTileLoader;
begin
  Result := TBitmapTileFreeImageLoaderPng.Create(
    GetValidPerfCounterList(APerfCounterList),
    FBitmap32StaticFactory
  );
end;

function TBitmapTileSaveLoadFactory.CreatePngSaver(
  const AColorDepth: TImageColorBitPerPix;
  const ACompressionLevel: Byte;
  const APerfCounterList: IInternalPerformanceCounterList
): IBitmapTileSaver;
var
  VBpp: Byte;
begin
  case AColorDepth of
    i8bpp  : VBpp := 8;
    i24bpp : VBpp := 24;
    i32bpp : VBpp := 32;
  else
    raise Exception.CreateFmt('Unexpected ColorDepth value: "%d"', [Integer(AColorDepth)]);
  end;
  Result := TBitmapTileFreeImageSaverPng.Create(
    ACompressionLevel,
    VBpp,
    FBitmap32To8Converter,
    GetValidPerfCounterList(APerfCounterList)
  );
end;

function TBitmapTileSaveLoadFactory.CreateJpegLoader(
  const APerfCounterList: IInternalPerformanceCounterList
): IBitmapTileLoader;
begin
  Result := TLibJpegTileLoader.Create(
    GetValidPerfCounterList(APerfCounterList),
    FBitmap32StaticFactory
  );
end;

function TBitmapTileSaveLoadFactory.CreateJpegSaver(
  const ACompressionQuality: Byte;
  const APerfCounterList: IInternalPerformanceCounterList
): IBitmapTileSaver;
begin
  Result := TLibJpegTileSaver.Create(
    ACompressionQuality,
    GetValidPerfCounterList(APerfCounterList)
  );
end;

function TBitmapTileSaveLoadFactory.CreateTiffLoader(
  const APerfCounterList: IInternalPerformanceCounterList = nil
): IBitmapTileLoader;
begin
  Result := TBitmapTileFreeImageLoaderTiff.Create(
    GetValidPerfCounterList(APerfCounterList),
    FBitmap32StaticFactory
  );
end;

function TBitmapTileSaveLoadFactory.CreateTiffSaver(
  const AColorDepth: TImageColorBitPerPix;
  const ACompressionType: TImageTiffCompression;
  const APerfCounterList: IInternalPerformanceCounterList = nil
): IBitmapTileSaver;
var
  VBpp: Byte;
  VCompression: Cardinal;
begin
  case AColorDepth of
    i8bpp  : VBpp := 8;
    i24bpp : VBpp := 24;
    i32bpp : VBpp := 32;
  else
    raise Exception.CreateFmt('Unexpected ColorDepth value: "%d"', [Integer(AColorDepth)]);
  end;
  case ACompressionType of
    itcNone    : VCompression := TIFF_NONE;
    itcDeflate : VCompression := TIFF_ADOBE_DEFLATE;
    itcLZW     : VCompression := TIFF_LZW;
    itcJPEG    : VCompression := TIFF_JPEG;
  else
    raise Exception.CreateFmt('Unexpected CompressionType value: "%d"', [Integer(ACompressionType)]);
  end;
  Result := TBitmapTileFreeImageSaverTiff.Create(
    VCompression,
    VBpp,
    GetValidPerfCounterList(APerfCounterList)
  );
end;

function TBitmapTileSaveLoadFactory.CreateWebpLoader(
  const APerfCounterList: IInternalPerformanceCounterList
): IBitmapTileLoader;
begin
  Result := TBitmapTileFreeImageLoaderWebp.Create(
    GetValidPerfCounterList(APerfCounterList),
    FBitmap32StaticFactory
  );
end;

function TBitmapTileSaveLoadFactory.CreateWebpSaver(
  const ACompressionQuality: Byte;
  const APerfCounterList: IInternalPerformanceCounterList
): IBitmapTileSaver;
begin
  Result := TBitmapTileFreeImageSaverWebp.Create(
    ACompressionQuality,
    GetValidPerfCounterList(APerfCounterList)
  );
end;

function TBitmapTileSaveLoadFactory.CreateWebpLosslessSaver(
  const APerfCounterList: IInternalPerformanceCounterList
): IBitmapTileSaver;
begin
  Result := TBitmapTileFreeImageSaverWebpLossless.Create(
    GetValidPerfCounterList(APerfCounterList)
  );
end;

end.
