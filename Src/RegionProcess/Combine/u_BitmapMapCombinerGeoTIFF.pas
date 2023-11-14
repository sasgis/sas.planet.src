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

unit u_BitmapMapCombinerGeoTIFF;

interface

uses
  i_InternalPerformanceCounter,
  i_BitmapMapCombiner,
  i_RegionProcessParamsFrame,
  u_BitmapMapCombinerFactoryBase;

type
  TBitmapMapCombinerFactoryGeoTiffStripped = class(TBitmapMapCombinerFactoryBase)
  private
    FSaveRectCounter: IInternalPerformanceCounter;
    FPrepareDataCounter: IInternalPerformanceCounter;
    FGetLineCounter: IInternalPerformanceCounter;
  protected
    function PrepareMapCombiner(
      const AParams: IRegionProcessParamsFrameMapCombine;
      const AProgressInfo: IBitmapCombineProgressUpdate
    ): IBitmapMapCombiner; override;
  public
    constructor Create(
      const ACounterList: IInternalPerformanceCounterList
    );
  end;

  TBitmapMapCombinerFactoryGeoTiffTiled = class(TBitmapMapCombinerFactoryBase)
  private
    FSaveRectCounter: IInternalPerformanceCounter;
    FGetTileCounter: IInternalPerformanceCounter;
  protected
    function PrepareMapCombiner(
      const AParams: IRegionProcessParamsFrameMapCombine;
      const AProgressInfo: IBitmapCombineProgressUpdate
    ): IBitmapMapCombiner; override;
  public
    constructor Create(
      const ACounterList: IInternalPerformanceCounterList
    );
  end;

implementation

uses
  SysUtils,
  Classes,
  Types,
  gnugettext,
  GeoTiffWriter,
  t_GeoTIFF,
  t_Bitmap32,
  t_CommonTypes,
  t_MapCombineOptions,
  c_CoordConverter,
  i_Projection,
  i_ImageLineProvider,
  i_NotifierOperation,
  i_BitmapTileProvider,
  i_Bitmap32Static,
  u_BaseInterfacedObject,
  u_CalcWFileParams,
  u_ImageLineProvider,
  u_ImageLineProviderMultiThread,
  u_ResStrings,
  u_GeoFunc;

type
  TBitmapMapCombinerGeoTiffBase = class(TBaseInterfacedObject, IBitmapMapCombiner)
  protected
    FProgressUpdate: IBitmapCombineProgressUpdate;
    FWidth: Integer;
    FHeight: Integer;
    FWithAlpha: Boolean;
    FFileFormat: TGeoTiffFileFormat;
    FCompression: TGeoTiffCompression;
    FOperationID: Integer;
    FCancelNotifier: INotifierOperation;
    FSaveRectCounter: IInternalPerformanceCounter;
    function GetTiffType: TTiffType;
  protected
    procedure DoSaveRect(
      const AFileName: string;
      const AImageProvider: IBitmapTileProvider;
      const AMapRect: TRect
    ); virtual; abstract;
  private
    { IBitmapMapCombiner }
    procedure SaveRect(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const AFileName: string;
      const AImageProvider: IBitmapTileProvider;
      const AMapRect: TRect
    );
  public
    constructor Create(
      const AProgressUpdate: IBitmapCombineProgressUpdate;
      const ASaveRectCounter: IInternalPerformanceCounter;
      const AWithAlpha: Boolean;
      const AFileFormat: TGeoTiffFileFormat;
      const ACompression: TGeoTiffCompression
    );
  end;

  TBitmapMapCombinerGeoTiffStripped = class(TBitmapMapCombinerGeoTiffBase)
  private
    FThreadNumber: Integer;
    FLineProvider: IImageLineProvider;
    FPrepareDataCounter: IInternalPerformanceCounter;
    FGetLineCounter: IInternalPerformanceCounter;
  private
    function GetLineCallBack(
      const ARowNumber: Integer;
      const ALineSize: Integer;
      const AUserInfo: Pointer
    ): Pointer;
  protected
    procedure DoSaveRect(
      const AFileName: string;
      const AImageProvider: IBitmapTileProvider;
      const AMapRect: TRect
    ); override;
  public
    constructor Create(
      const AProgressUpdate: IBitmapCombineProgressUpdate;
      const ASaveRectCounter: IInternalPerformanceCounter;
      const APrepareDataCounter: IInternalPerformanceCounter;
      const AGetLineCounter: IInternalPerformanceCounter;
      const AThreadNumber: Integer;
      const AWithAlpha: Boolean;
      const AFileFormat: TGeoTiffFileFormat;
      const ACompression: TGeoTiffCompression
    );
  end;

  TBitmapMapCombinerGeoTiffTiled = class(TBitmapMapCombinerGeoTiffBase)
  private
    FData: Pointer;
    FTileSizePix: TPoint;
    FGetTileCounter: IInternalPerformanceCounter;
    FImageProvider: IBitmapTileProvider;
    FFullTileRect: TRect;
    FTotalTiles: UInt64;
    FProcessedTiles: UInt64;
  private
    function GetTileCallBack(
      const X, Y, Z: Integer;
      const ASize: Integer;
      const AUserInfo: Pointer
    ): Pointer;
  protected
    procedure DoSaveRect(
      const AFileName: string;
      const AImageProvider: IBitmapTileProvider;
      const AMapRect: TRect
    ); override;
  public
    constructor Create(
      const AProgressUpdate: IBitmapCombineProgressUpdate;
      const ASaveRectCounter: IInternalPerformanceCounter;
      const AGetTileCounter: IInternalPerformanceCounter;
      const AWithAlpha: Boolean;
      const AFileFormat: TGeoTiffFileFormat;
      const ACompression: TGeoTiffCompression
    );
  end;

{ TBitmapMapCombinerGeoTiffBase }

constructor TBitmapMapCombinerGeoTiffBase.Create(
  const AProgressUpdate: IBitmapCombineProgressUpdate;
  const ASaveRectCounter: IInternalPerformanceCounter;
  const AWithAlpha: Boolean;
  const AFileFormat: TGeoTiffFileFormat;
  const ACompression: TGeoTiffCompression
);
begin
  inherited Create;
  FProgressUpdate := AProgressUpdate;
  FSaveRectCounter := ASaveRectCounter;
  FWithAlpha := AWithAlpha;
  FFileFormat := AFileFormat;
  FCompression := ACompression;
end;

function TBitmapMapCombinerGeoTiffBase.GetTiffType: TTiffType;
var
  VSize: Int64;
  VOldTiffMaxFileSize: Int64;
  VBytesPerPix: Integer;
begin
  case FFileFormat of
    gtfOld: Result := ttOldTiff;
    gtfBig: Result := ttBigTiff;
  else
    begin
      if FWithAlpha then begin
        VBytesPerPix := 4;
      end else begin
        VBytesPerPix := 3;
      end;
      VSize := Int64(FWidth) * FHeight * VBytesPerPix;
      VOldTiffMaxFileSize := Int64(4000) * 1024 * 1024; // 4000 MB
      if VSize >= VOldTiffMaxFileSize then begin
        Result := ttBigTiff;
      end else begin
        Result := ttOldTiff;
      end;
    end;
  end;
end;

procedure TBitmapMapCombinerGeoTiffBase.SaveRect(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const AFileName: string;
  const AImageProvider: IBitmapTileProvider;
  const AMapRect: TRect
);
var
  VContext: TInternalPerformanceCounterContext;
begin
  FOperationID := AOperationID;
  FCancelNotifier := ACancelNotifier;

  VContext := FSaveRectCounter.StartOperation;
  try
    DoSaveRect(AFileName, AImageProvider, AMapRect);
  finally
    FSaveRectCounter.FinishOperation(VContext);
  end;
end;

{ TBitmapMapCombinerGeoTiffStripped }

constructor TBitmapMapCombinerGeoTiffStripped.Create(
  const AProgressUpdate: IBitmapCombineProgressUpdate;
  const ASaveRectCounter: IInternalPerformanceCounter;
  const APrepareDataCounter: IInternalPerformanceCounter;
  const AGetLineCounter: IInternalPerformanceCounter;
  const AThreadNumber: Integer;
  const AWithAlpha: Boolean;
  const AFileFormat: TGeoTiffFileFormat;
  const ACompression: TGeoTiffCompression
);
begin
  inherited Create(AProgressUpdate, ASaveRectCounter, AWithAlpha, AFileFormat, ACompression);

  FPrepareDataCounter := APrepareDataCounter;
  FGetLineCounter := AGetLineCounter;
  FThreadNumber := AThreadNumber;
end;

procedure TBitmapMapCombinerGeoTiffStripped.DoSaveRect(
  const AFileName: string;
  const AImageProvider: IBitmapTileProvider;
  const AMapRect: TRect
);
const
  TIFF_JPG_MAX_WIDTH = 65536;
  TIFF_JPG_MAX_HEIGHT = 65536;
var
  VCurrentPieceRect: TRect;
  VMapPieceSize: TPoint;
  VFullTileRect: TRect;
  VTileRectSize: TPoint;
  VProjection: IProjection;
  VCellIncrementX, VCellIncrementY, VOriginX, VOriginY: Double;
  VGeoTiffWriter: TGeoTiffWriter;
  VCompression: TTiffCompression;
  VProjInfo: TProjectionInfo;
  VThreadNumber: Integer;
  VErrorMessage: string;
begin
  VCurrentPieceRect := AMapRect;
  VMapPieceSize := RectSize(VCurrentPieceRect);

  FWidth := VMapPieceSize.X;
  FHeight := VMapPieceSize.Y;

  if (FCompression = gtcJPEG) and
     ((FWidth >= TIFF_JPG_MAX_WIDTH) or (FHeight >= TIFF_JPG_MAX_HEIGHT)) then
  begin
    raise Exception.CreateFmt(
      SAS_ERR_GeoTiffWithJpegResolutionIsTooHigh,
      [FWidth, TIFF_JPG_MAX_WIDTH, FHeight, TIFF_JPG_MAX_HEIGHT]
    );
  end;

  VProjection := AImageProvider.Projection;
  VFullTileRect := VProjection.PixelRect2TileRect(VCurrentPieceRect);
  VTileRectSize := RectSize(VFullTileRect);

  CalculateWFileParams(
    VProjection.PixelPos2LonLat(VCurrentPieceRect.TopLeft),
    VProjection.PixelPos2LonLat(VCurrentPieceRect.BottomRight),
    VMapPieceSize.X, VMapPieceSize.Y, VProjection.ProjectionType,
    VCellIncrementX, VCellIncrementY, VOriginX, VOriginY
  );

  VProjInfo.EPSG := VProjection.ProjectionType.ProjectionEPSG;
  VProjInfo.IsGeographic := (VProjInfo.EPSG = CGELonLatProjectionEPSG);
  VProjInfo.CellIncrementX := VCellIncrementX;
  VProjInfo.CellIncrementY := -VCellIncrementY;
  VProjInfo.OriginX := VOriginX;
  VProjInfo.OriginY := VOriginY;

  case FCompression of
    gtcZIP: VCompression := tcZip;
    gtcLZW: VCompression := tcLZW;
    gtcJPEG: VCompression := tcJPG;
  else
    VCompression := tcNone;
  end;

  VThreadNumber := FThreadNumber;
  if (VThreadNumber > 1) and (VTileRectSize.X <= VThreadNumber) then begin
    VThreadNumber := 1;
  end;

  if FWithAlpha then begin
    if VThreadNumber > 1 then begin
      FLineProvider :=
        TImageLineProviderRGBAMultiThread.Create(
          FPrepareDataCounter,
          FGetLineCounter,
          AImageProvider,
          VThreadNumber,
          VCurrentPieceRect
        );
    end else begin
      FLineProvider :=
        TImageLineProviderRGBA.Create(
          FPrepareDataCounter,
          FGetLineCounter,
          AImageProvider,
          VCurrentPieceRect
        );
    end;
  end else begin
    if VThreadNumber > 1 then begin
      FLineProvider :=
        TImageLineProviderRGBMultiThread.Create(
          FPrepareDataCounter,
          FGetLineCounter,
          AImageProvider,
          VThreadNumber,
          VCurrentPieceRect
        );
    end else begin
      FLineProvider :=
        TImageLineProviderRGB.Create(
          FPrepareDataCounter,
          FGetLineCounter,
          AImageProvider,
          VCurrentPieceRect
        );
    end;
  end;

  VGeoTiffWriter := TGeoTiffWriter.Create;
  try
    VGeoTiffWriter.WriteStripped(
      Self.GetTiffType,
      AFileName,
      FWidth,
      FHeight,
      VCompression,
      Self.GetLineCallBack,
      FWithAlpha,
      @VProjInfo,
      nil,
      VErrorMessage
    );
  finally
    VGeoTiffWriter.Free;
  end;
end;

function TBitmapMapCombinerGeoTiffStripped.GetLineCallBack(
  const ARowNumber: Integer;
  const ALineSize: Integer;
  const AUserInfo: Pointer
): Pointer;
begin
  if ARowNumber mod 256 = 0 then begin
    FProgressUpdate.Update(ARowNumber / FHeight);
  end;
  if not FCancelNotifier.IsOperationCanceled(FOperationID) then begin
    Result := FLineProvider.GetLine(FOperationID, FCancelNotifier, ARowNumber);
  end else begin
    Result := nil;
  end;
end;

{ TBitmapMapCombinerGeoTiffTiled }

constructor TBitmapMapCombinerGeoTiffTiled.Create(
  const AProgressUpdate: IBitmapCombineProgressUpdate;
  const ASaveRectCounter, AGetTileCounter: IInternalPerformanceCounter;
  const AWithAlpha: Boolean;
  const AFileFormat: TGeoTiffFileFormat;
  const ACompression: TGeoTiffCompression
);
begin
  inherited Create(AProgressUpdate, ASaveRectCounter, AWithAlpha, AFileFormat, ACompression);

  FGetTileCounter := AGetTileCounter;
end;

procedure TBitmapMapCombinerGeoTiffTiled.DoSaveRect(
  const AFileName: string;
  const AImageProvider: IBitmapTileProvider;
  const AMapRect: TRect
);
var
  VBytesPerPix: Integer;
  VTileRectSize: TPoint;
  VProjection: IProjection;
  VCellIncrementX, VCellIncrementY, VOriginX, VOriginY: Double;
  VGeoTiffWriter: TGeoTiffWriter;
  VCompression: TTiffCompression;
  VProjInfo: TProjectionInfo;
  VErrorMessage: string;
begin
  FImageProvider := AImageProvider;

  VProjection := AImageProvider.Projection;
  FFullTileRect := VProjection.PixelRect2TileRect(AMapRect);
  VTileRectSize := RectSize(FFullTileRect);

  FTileSizePix := VProjection.GetTileSize(FFullTileRect.TopLeft);

  Assert(FTileSizePix.X = FTileSizePix.Y);

  FWidth := VTileRectSize.X * FTileSizePix.X;
  FHeight := VTileRectSize.Y * FTileSizePix.Y;

  FTotalTiles := VTileRectSize.X * VTileRectSize.Y;
  FProcessedTiles := 0;

  CalculateWFileParams(
    VProjection.TilePos2LonLat(FFullTileRect.TopLeft),
    VProjection.TilePos2LonLat(FFullTileRect.BottomRight),
    FWidth, FHeight, VProjection.ProjectionType,
    VCellIncrementX, VCellIncrementY, VOriginX, VOriginY
  );

  VProjInfo.EPSG := VProjection.ProjectionType.ProjectionEPSG;
  VProjInfo.IsGeographic := (VProjInfo.EPSG = CGELonLatProjectionEPSG);
  VProjInfo.CellIncrementX := VCellIncrementX;
  VProjInfo.CellIncrementY := -VCellIncrementY;
  VProjInfo.OriginX := VOriginX;
  VProjInfo.OriginY := VOriginY;

  case FCompression of
    gtcZIP: VCompression := tcZip;
    gtcLZW: VCompression := tcLZW;
    gtcJPEG: VCompression := tcJPG;
  else
    VCompression := tcNone;
  end;

  if FWithAlpha then begin
    VBytesPerPix := 4;
  end else begin
    VBytesPerPix := 3;
  end;

  GetMem(FData, FTileSizePix.X * FTileSizePix.Y * VBytesPerPix);
  try
    VGeoTiffWriter := TGeoTiffWriter.Create('', FTileSizePix.X, FTileSizePix.Y);
    try
      VGeoTiffWriter.WriteTiled(
        Self.GetTiffType,
        AFileName,
        FWidth,
        FHeight,
        '', // todo
        VCompression,
        Self.GetTileCallBack,
        FWithAlpha,
        @VProjInfo,
        nil,
        VErrorMessage
      );
    finally
      VGeoTiffWriter.Free;
    end;
  finally
    FreeMem(FData);
  end;
end;

function TBitmapMapCombinerGeoTiffTiled.GetTileCallBack(
  const X, Y, Z: Integer;
  const ASize: Integer;
  const AUserInfo: Pointer
): Pointer;

type
  TRGB  = packed record R,G,B: Byte; end;
  TRGBA = packed record R,G,B,A: Byte; end;

  function ToRGB(const AData: PColor32Array): Pointer;
  var
    I: Integer;
    VSource: PColor32Entry;
    VTarget: ^TRGB;
  begin
    VSource := PColor32Entry(AData);
    VTarget := FData;
    for I := 0 to FTileSizePix.X * FTileSizePix.Y - 1 do begin
      VTarget.B := VSource.B;
      VTarget.G := VSource.G;
      VTarget.R := VSource.R;
      Inc(VSource);
      Inc(VTarget);
    end;
    Result := FData;
  end;

  function ToRGBA(const AData: PColor32Array): Pointer;
  var
    I: Integer;
    VSource: PColor32Entry;
    VTarget: ^TRGBA;
  begin
    VSource := PColor32Entry(AData);
    VTarget := FData;
    for I := 0 to FTileSizePix.X * FTileSizePix.Y - 1 do begin
      VTarget.B := VSource.B;
      VTarget.G := VSource.G;
      VTarget.R := VSource.R;
      VTarget.A := VSource.A;
      Inc(VSource);
      Inc(VTarget);
    end;
    Result := FData;
  end;

var
  VTile: TPoint;
  VBitmap: IBitmap32Static;
begin
  Result := nil;

  Inc(FProcessedTiles);
  if FProcessedTiles mod 100 = 0 then begin
    FProgressUpdate.Update(FProcessedTiles / FTotalTiles);
  end;

  if not FCancelNotifier.IsOperationCanceled(FOperationID) then begin
    VTile.X := FFullTileRect.Left + X;
    VTile.Y := FFullTileRect.Top + Y;

    VBitmap := FImageProvider.GetTile(FOperationID, FCancelNotifier, VTile);

    if Assigned(VBitmap) then begin
      if FWithAlpha then begin
        Result := ToRGBA(VBitmap.Data);
      end else begin
        Result := ToRGB(VBitmap.Data);
      end;
    end;
  end;
end;

{ TBitmapMapCombinerFactoryGeoTiffStripped }

constructor TBitmapMapCombinerFactoryGeoTiffStripped.Create(
  const ACounterList: IInternalPerformanceCounterList
);
var
  VCounterList: IInternalPerformanceCounterList;
begin
  inherited Create(
    Point(0, 0),
    Point(1000000, MaxInt),
    stsUnicode,
    'tif',
    gettext_NoExtract('GeoTIFF (Stripped)'),
    [mcAlphaUncheck, mcGeoTiff, mcThreadCount]
  );
  VCounterList := ACounterList.CreateAndAddNewSubList('GeoTIFF (Stripped)');
  FSaveRectCounter := VCounterList.CreateAndAddNewCounter('SaveRect');
  FPrepareDataCounter := VCounterList.CreateAndAddNewCounter('PrepareData');
  FGetLineCounter := VCounterList.CreateAndAddNewCounter('GetLine');
end;

function TBitmapMapCombinerFactoryGeoTiffStripped.PrepareMapCombiner(
  const AParams: IRegionProcessParamsFrameMapCombine;
  const AProgressInfo: IBitmapCombineProgressUpdate
): IBitmapMapCombiner;
begin
  Result :=
    TBitmapMapCombinerGeoTiffStripped.Create(
      AProgressInfo,
      FSaveRectCounter,
      FPrepareDataCounter,
      FGetLineCounter,
      AParams.CustomOptions.ThreadCount,
      AParams.CustomOptions.IsSaveAlfa,
      AParams.CustomOptions.GeoTiffFormat,
      AParams.CustomOptions.GeoTiffCompression
    );
end;

{ TBitmapMapCombinerFactoryGeoTiffTiled }

constructor TBitmapMapCombinerFactoryGeoTiffTiled.Create(
  const ACounterList: IInternalPerformanceCounterList
);
var
  VCounterList: IInternalPerformanceCounterList;
begin
  inherited Create(
    Point(0, 0),
    Point(MaxInt, MaxInt),
    stsUnicode,
    'tif',
    gettext_NoExtract('GeoTIFF (Tiled)'),
    [mcAlphaUncheck, mcGeoTiff]
  );
  VCounterList := ACounterList.CreateAndAddNewSubList('GeoTIFF (Tiled)');
  FSaveRectCounter := VCounterList.CreateAndAddNewCounter('SaveRect');
  FGetTileCounter := VCounterList.CreateAndAddNewCounter('GetTile');
end;

function TBitmapMapCombinerFactoryGeoTiffTiled.PrepareMapCombiner(
  const AParams: IRegionProcessParamsFrameMapCombine;
  const AProgressInfo: IBitmapCombineProgressUpdate
): IBitmapMapCombiner;
begin
  Result :=
    TBitmapMapCombinerGeoTiffTiled.Create(
      AProgressInfo,
      FSaveRectCounter,
      FGetTileCounter,
      AParams.CustomOptions.IsSaveAlfa,
      AParams.CustomOptions.GeoTiffFormat,
      AParams.CustomOptions.GeoTiffCompression
    );
end;

end.
