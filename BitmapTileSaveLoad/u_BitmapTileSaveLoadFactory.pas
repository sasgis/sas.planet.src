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

unit u_BitmapTileSaveLoadFactory;

interface

uses
  i_BitmapTileSaveLoad,
  i_ARGBToPaletteConverter,
  i_BitmapTileSaveLoadFactory,
  i_InternalPerformanceCounter;

type
  TBitmapTileSaveLoadFactory = class(
    TInterfacedObject,
    IBitmapTileSaveLoadFactory
  )
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
      const ACompressionLevel: Byte = 2;
      const AARGBToPaletteConverter: IARGBToPaletteConverter = nil;
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
  end;

implementation

{$DEFINE USE_FREE_IMAGE}

uses
  u_BitmapTileLibJpeg,
  {$IFNDEF USE_FREE_IMAGE}
  u_BitmapTileVampyreSaver,
  u_BitmapTileVampyreLoader,
  {$ELSE}
  u_BitmapTileFreeImage,
  {$ENDIF}
  u_InternalPerformanceCounterList;

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

function TBitmapTileSaveLoadFactory.CreateBmpLoader(
  const APerfCounterList: IInternalPerformanceCounterList = nil
): IBitmapTileLoader;
begin
  {$IFNDEF USE_FREE_IMAGE}
  Result := TVampyreBasicBitmapTileLoaderBMP.Create(
    GetValidPerfCounterList(APerfCounterList)
  );
  {$ELSE}
  Result := TBitmapTileFreeImageLoaderBmp.Create(
    GetValidPerfCounterList(APerfCounterList)
  );
  {$ENDIF}
end;

function TBitmapTileSaveLoadFactory.CreateBmpSaver(
  const APerfCounterList: IInternalPerformanceCounterList = nil
): IBitmapTileSaver;
begin
  {$IFNDEF USE_FREE_IMAGE}
  Result := TVampyreBasicBitmapTileSaverBMP.Create(
    GetValidPerfCounterList(APerfCounterList)
  );
  {$ELSE}
  Result := TBitmapTileFreeImageSaverBmp.Create(
    GetValidPerfCounterList(APerfCounterList)
  );
  {$ENDIF}
end;

function TBitmapTileSaveLoadFactory.CreateGifLoader(
  const APerfCounterList: IInternalPerformanceCounterList = nil
): IBitmapTileLoader;
begin
  {$IFNDEF USE_FREE_IMAGE}
  Result := TVampyreBasicBitmapTileLoaderGIF.Create(
    GetValidPerfCounterList(APerfCounterList)
  );
  {$ELSE}
  Result := TBitmapTileFreeImageLoaderGif.Create(
    GetValidPerfCounterList(APerfCounterList)
  );
  {$ENDIF}
end;

function TBitmapTileSaveLoadFactory.CreateGifSaver(
  const APerfCounterList: IInternalPerformanceCounterList = nil
): IBitmapTileSaver;
begin
  {$IFNDEF USE_FREE_IMAGE}
  Result := TVampyreBasicBitmapTileSaverGIF.Create(
    GetValidPerfCounterList(APerfCounterList)
  );
  {$ELSE}
  Result := TBitmapTileFreeImageSaverGif.Create(
    GetValidPerfCounterList(APerfCounterList)
  );
  {$ENDIF}
end;

function TBitmapTileSaveLoadFactory.CreatePngLoader(
  const APerfCounterList: IInternalPerformanceCounterList = nil
): IBitmapTileLoader;
begin
  {$IFNDEF USE_FREE_IMAGE}
  Result := TVampyreBasicBitmapTileLoaderPNG.Create(
    GetValidPerfCounterList(APerfCounterList)
  );
  {$ELSE}
  Result := TBitmapTileFreeImageLoaderPng.Create(
    GetValidPerfCounterList(APerfCounterList)
  );
  {$ENDIF}
end;

function TBitmapTileSaveLoadFactory.CreatePngSaver(
  const AColorDepth: TImageColorBitPerPix = i32bpp;
  const ACompressionLevel: Byte = 2;
  const AARGBToPaletteConverter: IARGBToPaletteConverter = nil;
  const APerfCounterList: IInternalPerformanceCounterList = nil
): IBitmapTileSaver;
begin
  case AColorDepth of
    i8bpp:
      begin
        {$IFNDEF USE_FREE_IMAGE}
        Result := TVampyreBasicBitmapTileSaverPNGPalette.Create(
          AARGBToPaletteConverter,
          ACompressionLevel,
          GetValidPerfCounterList(APerfCounterList)
        );
        {$ELSE}
        Result := TBitmapTileFreeImageSaverPng.Create(
          ACompressionLevel,
          8,
          GetValidPerfCounterList(APerfCounterList)
        );
        {$ENDIF}
      end;

    i24bpp:
      begin
        {$IFNDEF USE_FREE_IMAGE}
        Result := TVampyreBasicBitmapTileSaverPNGRGB.Create(
          ACompressionLevel,
          GetValidPerfCounterList(APerfCounterList)
        );
        {$ELSE}
        Result := TBitmapTileFreeImageSaverPng.Create(
          ACompressionLevel,
          24,
          GetValidPerfCounterList(APerfCounterList)
        );
        {$ENDIF}
      end;

  else // i32bpp
    begin
      {$IFNDEF USE_FREE_IMAGE}
      Result := TVampyreBasicBitmapTileSaverPNG.Create(
        ACompressionLevel,
        GetValidPerfCounterList(APerfCounterList)
      );
      {$ELSE}
      Result := TBitmapTileFreeImageSaverPng.Create(
        ACompressionLevel,
        32,
        GetValidPerfCounterList(APerfCounterList)
      );
      {$ENDIF}
    end;
  end;
end;

function TBitmapTileSaveLoadFactory.CreateJpegLoader(
  const APerfCounterList: IInternalPerformanceCounterList = nil
): IBitmapTileLoader;
begin
  Result := TLibJpegTileLoader.Create(
    GetValidPerfCounterList(APerfCounterList)
  );
end;

function TBitmapTileSaveLoadFactory.CreateJpegSaver(
  const ACompressionQuality: Byte = 75;
  const APerfCounterList: IInternalPerformanceCounterList = nil
): IBitmapTileSaver;
begin
  Result := TLibJpegTileSaver.Create(
    ACompressionQuality,
    GetValidPerfCounterList(APerfCounterList)
  );
end;

end.
