{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2016, SAS.Planet development team.                      *}
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

unit u_BitmapMapCombinerGeoTIFF;

interface

uses
  i_InternalPerformanceCounter,
  i_BitmapMapCombiner,
  i_RegionProcessParamsFrame,
  u_BitmapMapCombinerFactoryBase;

type
  TBitmapMapCombinerFactoryGeoTIFF = class(TBitmapMapCombinerFactoryBase)
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

implementation

uses
  SysUtils,
  Classes,
  Types,
  gnugettext,
  GeoTiffWriter,
  t_GeoTIFF,
  t_CommonTypes,
  t_MapCombineOptions,
  c_CoordConverter,
  i_Projection,
  i_ImageLineProvider,
  i_NotifierOperation,
  i_BitmapTileProvider,
  u_BaseInterfacedObject,
  u_CalcWFileParams,
  u_ImageLineProvider,
  u_ImageLineProviderMultiThread,
  u_GeoFunc;

type
  TBitmapMapCombinerGeoTIFF = class(TBaseInterfacedObject, IBitmapMapCombiner)
  private
    FProgressUpdate: IBitmapCombineProgressUpdate;
    FWidth: Integer;
    FHeight: Integer;
    FThreadNumber: Integer;
    FWithAlpha: Boolean;
    FFileFormat: TGeoTiffFileFormat;
    FCompression: TGeoTiffCompression;
    FLineProvider: IImageLineProvider;
    FOperationID: Integer;
    FCancelNotifier: INotifierOperation;
    FSaveRectCounter: IInternalPerformanceCounter;
    FPrepareDataCounter: IInternalPerformanceCounter;
    FGetLineCounter: IInternalPerformanceCounter;
  private
    function _GetTiffType: TTiffType;
    function GetLineCallBack(
      const ARowNumber: Integer;
      const ALineSize: Integer;
      const AUserInfo: Pointer
    ): Pointer;
  private
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
      const APrepareDataCounter: IInternalPerformanceCounter;
      const AGetLineCounter: IInternalPerformanceCounter;
      const AThreadNumber: Integer;
      const AWithAlpha: Boolean = True;
      const AFileFormat: TGeoTiffFileFormat = gtfOld;
      const ACompression: TGeoTiffCompression = gtcLZW
    );
  end;

{ TBitmapMapCombinerGeoTIFF }

constructor TBitmapMapCombinerGeoTIFF.Create(
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
  inherited Create;
  FProgressUpdate := AProgressUpdate;
  FSaveRectCounter := ASaveRectCounter;
  FPrepareDataCounter := APrepareDataCounter;
  FGetLineCounter := AGetLineCounter;
  FThreadNumber := AThreadNumber;
  FWithAlpha := AWithAlpha;
  FFileFormat := AFileFormat;
  FCompression := ACompression;
end;

function TBitmapMapCombinerGeoTIFF._GetTiffType: TTiffType;
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

procedure TBitmapMapCombinerGeoTIFF.SaveRect(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const AFileName: string;
  const AImageProvider: IBitmapTileProvider;
  const AMapRect: TRect
);
var
  VCurrentPieceRect: TRect;
  VMapPieceSize: TPoint;
  VFullTileRect: TRect;
  VTileRectSize: TPoint;
  VProjection: IProjection;
  VCellIncrementX, VCellIncrementY, VOriginX, VOriginY: Double;
  VGeoTiffWriter: TGeoTiffWriter;
  VTiffType: TTiffType;
  VCompression: TTiffCompression;
  VProjInfo: TProjectionInfo;
  VContext: TInternalPerformanceCounterContext;
  VThreadNumber: Integer;
begin
  FOperationID := AOperationID;
  FCancelNotifier := ACancelNotifier;

  VContext := FSaveRectCounter.StartOperation;
  try
  VCurrentPieceRect := AMapRect;
  VMapPieceSize := RectSize(VCurrentPieceRect);

  FWidth := VMapPieceSize.X;
  FHeight := VMapPieceSize.Y;

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

  VTiffType := _GetTiffType;

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

  VGeoTiffWriter := TGeoTiffWriter.Create('SAS.Planet');
  try
    VGeoTiffWriter.Write(
      VTiffType,
      AFileName,
      FWidth,
      FHeight,
      VCompression,
      Self.GetLineCallBack,
      FWithAlpha,
      @VProjInfo
    );
  finally
    VGeoTiffWriter.Free;
  end;
  finally
    FSaveRectCounter.FinishOperation(VContext);
  end;
end;

function TBitmapMapCombinerGeoTIFF.GetLineCallBack(
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

{ TBitmapMapCombinerFactoryGeoTIFF }

constructor TBitmapMapCombinerFactoryGeoTIFF.Create(
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
    gettext_NoExtract('GeoTIFF (Tagged Image File Format)'),
    [mcAlphaUncheck, mcGeoTiff, mcThreadCount]
  );
  VCounterList := ACounterList.CreateAndAddNewSubList('GeoTIFF');
  FSaveRectCounter := VCounterList.CreateAndAddNewCounter('SaveRect');
  FPrepareDataCounter := VCounterList.CreateAndAddNewCounter('PrepareData');
  FGetLineCounter := VCounterList.CreateAndAddNewCounter('GetLine');
end;

function TBitmapMapCombinerFactoryGeoTIFF.PrepareMapCombiner(
  const AParams: IRegionProcessParamsFrameMapCombine;
  const AProgressInfo: IBitmapCombineProgressUpdate
): IBitmapMapCombiner;
begin
  Result :=
    TBitmapMapCombinerGeoTIFF.Create(
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

end.
