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

unit i_BitmapTileSaveLoadFactory;

interface

uses
  i_BitmapTileSaveLoad,
  i_InternalPerformanceCounter;

type
  TImageColorBitPerPix = (i8bpp = 0, i24bpp = 1, i32bpp = 2);

  IBitmapTileSaveLoadFactory = interface
    ['{2F2F9CA4-4642-47D9-902B-6514F1D882E9}']
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

end.
