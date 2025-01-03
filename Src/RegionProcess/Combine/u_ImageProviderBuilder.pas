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

unit u_ImageProviderBuilder;

interface

uses
  Windows,
  t_Bitmap32,
  i_BitmapTileProvider,
  i_Bitmap32BufferFactory,
  i_BitmapTileSaveLoadFactory,
  i_ImageLineProvider,
  i_ImageTileProvider,
  i_InternalPerformanceCounter,
  i_TileStorage,
  i_MapVersionRequest;

type
  TImageProviderBuilder = record
    class function BuildLineProvider(
      const APrepareDataCounter: IInternalPerformanceCounter;
      const AGetLineCounter: IInternalPerformanceCounter;
      const AImageProvider: IBitmapTileProvider;
      const AWithAlpha: Boolean;
      const AMapRect: TRect;
      const AThreadCount: Integer = 1
    ): IImageLineProvider; static;

    class function BuildTileProvider(
      const AGetTileCounter: IInternalPerformanceCounter;
      const AImageProvider: IBitmapTileProvider;
      const ABitmapFactory: IBitmap32StaticFactory;
      const AWithAlpha: Boolean;
      const AThreadCount: Integer = 1
    ): IImageTileProvider; static;

    class function BuildTileProviderRawJpeg(
      const AGetTileCounter: IInternalPerformanceCounter;
      const AImageProvider: IBitmapTileProvider;
      const ABitmapFactory: IBitmap32StaticFactory;
      const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
      const ATileStorage: ITileStorage;
      const AMapVersionRequest: IMapVersionRequest;
      const ABgColor: TColor32;
      const AJpegQuality: Byte;
      const AZoom: Byte
    ): IImageTileProvider; static;
  end;

implementation

uses
  u_ImageLineProvider,
  u_ImageLineProviderMultiThread,
  u_ImageTileProvider;

{ TImageProviderBuilder }

class function TImageProviderBuilder.BuildLineProvider(
  const APrepareDataCounter, AGetLineCounter: IInternalPerformanceCounter;
  const AImageProvider: IBitmapTileProvider;
  const AWithAlpha: Boolean;
  const AMapRect: TRect;
  const AThreadCount: Integer
): IImageLineProvider;
begin
  if AWithAlpha then begin
    if AThreadCount > 1 then begin
      Result :=
        TImageLineProviderRGBAMultiThread.Create(
          APrepareDataCounter,
          AGetLineCounter,
          AImageProvider,
          AThreadCount,
          AMapRect
        );
    end else begin
      Result :=
        TImageLineProviderRGBA.Create(
          APrepareDataCounter,
          AGetLineCounter,
          AImageProvider,
          AMapRect
        );
    end;
  end else begin
    if AThreadCount > 1 then begin
      Result :=
        TImageLineProviderRGBMultiThread.Create(
          APrepareDataCounter,
          AGetLineCounter,
          AImageProvider,
          AThreadCount,
          AMapRect
        );
    end else begin
      Result :=
        TImageLineProviderRGB.Create(
          APrepareDataCounter,
          AGetLineCounter,
          AImageProvider,
          AMapRect
        );
    end;
  end;
end;

class function TImageProviderBuilder.BuildTileProvider(
  const AGetTileCounter: IInternalPerformanceCounter;
  const AImageProvider: IBitmapTileProvider;
  const ABitmapFactory: IBitmap32StaticFactory;
  const AWithAlpha: Boolean;
  const AThreadCount: Integer
): IImageTileProvider;
begin
  // ToDo: Add multi-threaded tile providers

  if AWithAlpha then begin
    Result :=
      TImageTileProviderRGBA.Create(
        AGetTileCounter,
        AImageProvider,
        ABitmapFactory
      );
  end else begin
    Result :=
      TImageTileProviderRGB.Create(
        AGetTileCounter,
        AImageProvider,
        ABitmapFactory
      );
  end;
end;

class function TImageProviderBuilder.BuildTileProviderRawJpeg(
  const AGetTileCounter: IInternalPerformanceCounter;
  const AImageProvider: IBitmapTileProvider;
  const ABitmapFactory: IBitmap32StaticFactory;
  const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
  const ATileStorage: ITileStorage;
  const AMapVersionRequest: IMapVersionRequest;
  const ABgColor: TColor32;
  const AJpegQuality: Byte;
  const AZoom: Byte
): IImageTileProvider;
begin
  Result :=
    TImageTileProviderRawJpeg.Create(
      AGetTileCounter,
      AImageProvider,
      ABitmapFactory,
      ABitmapTileSaveLoadFactory,
      ATileStorage,
      AMapVersionRequest,
      ABgColor,
      AJpegQuality,
      AZoom
    );
end;

end.
