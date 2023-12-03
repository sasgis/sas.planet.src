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
  libtiff.writer,
  t_GeoTIFF,
  t_GeoTypes,
  t_CommonTypes,
  t_MapCombineOptions,
  c_CoordConverter,
  i_Projection,
  i_ProjectionType,
  i_ImageLineProvider,
  i_ImageTileProvider,
  i_NotifierOperation,
  i_BitmapTileProvider,
  u_BaseInterfacedObject,
  u_CalcWFileParams,
  u_ImageLineProvider,
  u_ImageLineProviderMultiThread,
  u_ImageTileProvider,
  u_ResStrings,
  u_GeoFunc;

type
  TBitmapMapCombinerGeoTiffBase = class(TBaseInterfacedObject, IBitmapMapCombiner)
  protected
    FProgressUpdate: IBitmapCombineProgressUpdate;
    FWidth: Integer;
    FHeight: Integer;
    FWithAlpha: Boolean;
    FGeoTiffOptions: TGeoTiffOptions;
    FProjInfo: TProjectionInfo;
    FOperationID: Integer;
    FCancelNotifier: INotifierOperation;
    FSaveRectCounter: IInternalPerformanceCounter;
    function GetTiffType: TTiffType;
    function GetTiffCompression: TTiffCompression;
    function GetProjInfo(
      const ALonLatRect: TDoubleRect;
      const AImageSizePix: TPoint;
      const AProjectionType: IProjectionType
    ): PProjectionInfo;
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
      const AGeoTiffOptions: TGeoTiffOptions
    );
  end;

  TBitmapMapCombinerGeoTiffStripped = class(TBitmapMapCombinerGeoTiffBase)
  private
    FThreadNumber: Integer;
    FLineProvider: IImageLineProvider;
    FLineSizeInBytes: NativeInt;
    FPrepareDataCounter: IInternalPerformanceCounter;
    FGetLineCounter: IInternalPerformanceCounter;
  private
    function OnGetLineCallBack(
      const ARowNumber: Integer;
      const AOverView: Integer;
      out AData: Pointer;
      out ADataSize: NativeInt
    ): Boolean;
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
      const AGeoTiffOptions: TGeoTiffOptions
    );
  end;

  TBitmapMapCombinerGeoTiffTiled = class(TBitmapMapCombinerGeoTiffBase)
  private
    FGetTileCounter: IInternalPerformanceCounter;
    FTileProvider: IImageTileProvider;
    FTileSizeInBytes: NativeInt;
    FFullTileRect: TRect;
    FTotalTiles: UInt64;
    FProcessedTiles: UInt64;
  private
    function OnGetTileCallBack(
      const X, Y: Integer;
      const AOverView: Integer;
      out AData: Pointer;
      out ADataSize: NativeInt
    ): Boolean;
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
      const AGeoTiffOptions: TGeoTiffOptions
    );
  end;

{ TBitmapMapCombinerGeoTiffBase }

constructor TBitmapMapCombinerGeoTiffBase.Create(
  const AProgressUpdate: IBitmapCombineProgressUpdate;
  const ASaveRectCounter: IInternalPerformanceCounter;
  const AWithAlpha: Boolean;
  const AGeoTiffOptions: TGeoTiffOptions
);
begin
  inherited Create;
  FProgressUpdate := AProgressUpdate;
  FSaveRectCounter := ASaveRectCounter;
  FWithAlpha := AWithAlpha;
  FGeoTiffOptions := AGeoTiffOptions;
end;

function TBitmapMapCombinerGeoTiffBase.GetTiffType: TTiffType;
var
  VSize: Int64;
  VOldTiffMaxFileSize: Int64;
  VBytesPerPix: Integer;
begin
  case FGeoTiffOptions.FileFormatType of
    gtfClassic: Result := ttClassicTiff;
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
        Result := ttClassicTiff;
      end;
    end;
  end;
end;

function TBitmapMapCombinerGeoTiffBase.GetTiffCompression: TTiffCompression;
begin
  case FGeoTiffOptions.CompressionType of
    gtcZip  : Result := tcZip;
    gtcLzw  : Result := tcLzw;
    gtcJpeg : Result := tcJpeg;
  else
    Result := tcNone;
  end;
end;

function TBitmapMapCombinerGeoTiffBase.GetProjInfo(
  const ALonLatRect: TDoubleRect;
  const AImageSizePix: TPoint;
  const AProjectionType: IProjectionType
): PProjectionInfo;
var
  VCellIncrementX, VCellIncrementY, VOriginX, VOriginY: Double;
begin
  CalculateWFileParams(
    ALonLatRect.TopLeft, ALonLatRect.BottomRight,
    AImageSizePix.X, AImageSizePix.Y, AProjectionType,
    VCellIncrementX, VCellIncrementY, VOriginX, VOriginY
  );

  FProjInfo.EPSG := AProjectionType.ProjectionEPSG;
  FProjInfo.IsGeographic := (FProjInfo.EPSG = CGELonLatProjectionEPSG);
  FProjInfo.CellIncrementX := VCellIncrementX;
  FProjInfo.CellIncrementY := -VCellIncrementY;
  FProjInfo.OriginX := VOriginX;
  FProjInfo.OriginY := VOriginY;

  Result := @FProjInfo;
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
  const AGeoTiffOptions: TGeoTiffOptions
);
begin
  inherited Create(AProgressUpdate, ASaveRectCounter, AWithAlpha, AGeoTiffOptions);

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
  VProjInfo: PProjectionInfo;
  VTiffWriterParams: TTiffWriterParams;
  VThreadNumber: Integer;
  VErrorMessage: string;
begin
  VCurrentPieceRect := AMapRect;
  VMapPieceSize := RectSize(VCurrentPieceRect);

  FWidth := VMapPieceSize.X;
  FHeight := VMapPieceSize.Y;

  if (FGeoTiffOptions.CompressionType = gtcJpeg) and
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

  VProjInfo := Self.GetProjInfo(
    DoubleRect(
      VProjection.PixelPos2LonLat(VCurrentPieceRect.TopLeft),
      VProjection.PixelPos2LonLat(VCurrentPieceRect.BottomRight)
    ),
    VMapPieceSize,
    VProjection.ProjectionType
  );

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

  FLineSizeInBytes := FLineProvider.ImageSize.X * FLineProvider.BytesPerPixel;

  VTiffWriterParams := CTiffWriterParamsEmpty;
  with VTiffWriterParams do begin
    TiffType := Self.GetTiffType;
    OutputFileName := AFileName;
    ImageWidth := FWidth;
    ImageHeight := FHeight;
    Compression := Self.GetTiffCompression;
    StoreAlphaChanel := FWithAlpha;
    ProjectionInfo := VProjInfo;
    GetLineCallBack := Self.OnGetLineCallBack;
  end;

  with TTiffWriter.Create do
  try
    if not WriteStripped(@VTiffWriterParams, VErrorMessage) then begin
      if not FCancelNotifier.IsOperationCanceled(FOperationID) then begin
        raise Exception.CreateFmt('TTiffWriter.WriteStripped error: "%s"', [VErrorMessage]);
      end;
    end;
  finally
    Free;
  end;
end;

function TBitmapMapCombinerGeoTiffStripped.OnGetLineCallBack(
  const ARowNumber: Integer;
  const AOverView: Integer;
  out AData: Pointer;
  out ADataSize: NativeInt
): Boolean;
begin
  AData := FLineProvider.GetLine(FOperationID, FCancelNotifier, ARowNumber);
  ADataSize := FLineSizeInBytes;

  if ARowNumber mod 256 = 0 then begin
    FProgressUpdate.Update(ARowNumber / FHeight);
  end;

  Result := AData <> nil;
end;

{ TBitmapMapCombinerGeoTiffTiled }

constructor TBitmapMapCombinerGeoTiffTiled.Create(
  const AProgressUpdate: IBitmapCombineProgressUpdate;
  const ASaveRectCounter, AGetTileCounter: IInternalPerformanceCounter;
  const AWithAlpha: Boolean;
  const AGeoTiffOptions: TGeoTiffOptions
);
begin
  inherited Create(AProgressUpdate, ASaveRectCounter, AWithAlpha, AGeoTiffOptions);

  FGetTileCounter := AGetTileCounter;
end;

procedure TBitmapMapCombinerGeoTiffTiled.DoSaveRect(
  const AFileName: string;
  const AImageProvider: IBitmapTileProvider;
  const AMapRect: TRect
);
var
  VTileSizePix: TPoint;
  VTileRectSize: TPoint;
  VProjection: IProjection;
  VProjInfo: PProjectionInfo;
  VTiffWriterParams: TTiffWriterParams;
  VErrorMessage: string;
begin
  if FWithAlpha then begin
    FTileProvider :=
      TImageTileProviderRGBA.Create(
        FGetTileCounter,
        AImageProvider
      );
  end else begin
    FTileProvider :=
      TImageTileProviderRGB.Create(
        FGetTileCounter,
        AImageProvider
      );
  end;

  VTileSizePix := FTileProvider.TileSize;
  Assert(VTileSizePix.X = VTileSizePix.Y);

  VProjection := AImageProvider.Projection;
  FFullTileRect := VProjection.PixelRect2TileRect(AMapRect);
  VTileRectSize := RectSize(FFullTileRect);

  FWidth := VTileRectSize.X * VTileSizePix.X;
  FHeight := VTileRectSize.Y * VTileSizePix.Y;

  FTotalTiles := VTileRectSize.X * VTileRectSize.Y;
  FProcessedTiles := 0;

  VProjInfo := Self.GetProjInfo(
    DoubleRect(
      VProjection.TilePos2LonLat(FFullTileRect.TopLeft),
      VProjection.TilePos2LonLat(FFullTileRect.BottomRight)
    ),
    Types.Point(FWidth, FHeight),
    VProjection.ProjectionType
  );

  FTileSizeInBytes := FTileProvider.TileSize.X * FTileProvider.TileSize.Y * FTileProvider.BytesPerPixel;

  VTiffWriterParams := CTiffWriterParamsEmpty;
  with VTiffWriterParams do begin
    TiffType := Self.GetTiffType;
    OutputFileName := AFileName;
    ImageWidth := FWidth;
    ImageHeight := FHeight;
    Compression := Self.GetTiffCompression;
    StoreAlphaChanel := FWithAlpha;
    ProjectionInfo := VProjInfo;
    GetTileCallBack := Self.OnGetTileCallBack;
  end;

  with TTiffWriter.Create do
  try
    if not WriteTiled(@VTiffWriterParams, VErrorMessage) then begin
      if not FCancelNotifier.IsOperationCanceled(FOperationID) then begin
        raise Exception.CreateFmt('TTiffWriter.WriteTiled error: "%s"', [VErrorMessage]);
      end;
    end;
  finally
    Free;
  end;
end;

function TBitmapMapCombinerGeoTiffTiled.OnGetTileCallBack(
  const X, Y: Integer;
  const AOverView: Integer;
  out AData: Pointer;
  out ADataSize: NativeInt
): Boolean;
var
  VTile: TPoint;
begin
  VTile.X := FFullTileRect.Left + X;
  VTile.Y := FFullTileRect.Top + Y;

  AData := FTileProvider.GetTile(FOperationID, FCancelNotifier, VTile, 0);
  ADataSize := FTileSizeInBytes;

  Inc(FProcessedTiles);
  if FProcessedTiles mod 100 = 0 then begin
    FProgressUpdate.Update(FProcessedTiles / FTotalTiles);
  end;

  Result := AData <> nil;
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
    gettext_NoExtract('GeoTIFF'),
    [mcAlphaUncheck, mcGeoTiffStripped, mcThreadCount]
  );
  VCounterList := ACounterList.CreateAndAddNewSubList('GeoTIFF');
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
      AParams.CustomOptions.GeoTiffOptions
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
    [mcAlphaUncheck, mcGeoTiffTiled]
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
      AParams.CustomOptions.GeoTiffOptions
    );
end;

end.
