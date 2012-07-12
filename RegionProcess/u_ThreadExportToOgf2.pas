unit u_ThreadExportToOgf2;

interface

uses
  Types,
  SysUtils,
  Classes,
  i_NotifierOperation,
  i_BitmapTileSaveLoad,
  i_BitmapLayerProvider,
  i_BinaryData,
  i_RegionProcessProgressInfo,
  i_VectorItemLonLat,
  i_CoordConverter,
  i_CoordConverterFactory,
  i_LocalCoordConverterFactorySimpe,
  i_VectorItmesFactory,
  u_ResStrings,
  u_ThreadRegionProcessAbstract;

type
  TThreadExportToOgf2 = class(TThreadRegionProcessAbstract)
  private
    FZoom: Byte;
    FOgf2TileWidth: Integer;
    FOgf2TileHeight: Integer;
    FTileSaver: IBitmapTileSaver;
    FTargetFile: string;
    FImageProvider: IBitmapLayerProvider;
    FCoordConverterFactory: ICoordConverterFactory;
    FLocalConverterFactory: ILocalCoordConverterFactorySimpe;
    FProjectionFactory: IProjectionInfoFactory;
    FVectorItmesFactory: IVectorItmesFactory;
    function GetMapPreview(
      const ABitmapSaver: IBitmapTileSaver;
      out AMapPreviewWidth: Integer;
      out AMapPreviewHeight: Integer
    ): IBinaryData;
    function GetEmptyTile(
      const ABitmapSaver: IBitmapTileSaver
    ): IBinaryData;
    procedure SaveOziCalibrationMap(
      const AGeoConvert: ICoordConverter;
      const ATileRect: TRect;
      const AZoom: Byte
    );
    procedure ProgressFormUpdateOnProgress(AProcessed, AToProcess: Int64);
  protected
    procedure ProcessRegion; override;
  public
    constructor Create(
      const ACancelNotifier: INotifierOperation;
      AOperationID: Integer;
      const AProgressInfo: IRegionProcessProgressInfoInternal;
      const ACoordConverterFactory: ICoordConverterFactory;
      const ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
      const AProjectionFactory: IProjectionInfoFactory;
      const AVectorItmesFactory: IVectorItmesFactory;
      const ATargetFile: string;
      const APolygon: ILonLatPolygon;
      const AImageProvider: IBitmapLayerProvider;
      AZoom: Byte;
      const ATileSize: TPoint;
      const ATileSaver: IBitmapTileSaver
    );
  end;

implementation

uses
  GR32,
  GR32_Resamplers,
  Ogf2Writer,
  t_GeoTypes,
  c_CoordConverter,
  i_Bitmap32Static,
  i_TileIterator,
  i_VectorItemProjected,
  u_Bitmap32Static,
  u_TileIteratorByRect,
  u_MapCalibrationOzi,
  u_GeoFun;

const
  cBackGroundColor = $CCCCCCCC;

{ TThreadExportToOgf2 }

constructor TThreadExportToOgf2.Create(
  const ACancelNotifier: INotifierOperation;
  AOperationID: Integer;
  const AProgressInfo: IRegionProcessProgressInfoInternal;
  const ACoordConverterFactory: ICoordConverterFactory;
  const ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
  const AProjectionFactory: IProjectionInfoFactory;
  const AVectorItmesFactory: IVectorItmesFactory;
  const ATargetFile: string;
  const APolygon: ILonLatPolygon;
  const AImageProvider: IBitmapLayerProvider;
  AZoom: Byte;
  const ATileSize: TPoint;
  const ATileSaver: IBitmapTileSaver
);
begin
  inherited Create(
    ACancelNotifier,
    AOperationID,
    AProgressInfo,
    APolygon
  );
  FImageProvider := AImageProvider;
  FZoom := AZoom;
  FTargetFile := ATargetFile;
  FCoordConverterFactory := ACoordConverterFactory;
  FLocalConverterFactory := ALocalConverterFactory;
  FProjectionFactory := AProjectionFactory;
  FVectorItmesFactory := AVectorItmesFactory;
  FTileSaver := ATileSaver;
  FOgf2TileWidth := ATileSize.X;
  FOgf2TileHeight := ATileSize.Y;
end;

procedure TThreadExportToOgf2.SaveOziCalibrationMap(
  const AGeoConvert: ICoordConverter;
  const ATileRect: TRect;
  const AZoom: Byte
);
var
  VOziCalibrationMap: TMapCalibrationOzi;
begin
  VOziCalibrationMap := TMapCalibrationOzi.Create;
  try
    VOziCalibrationMap.SaveCalibrationInfo(
      FTargetFile,
      ATileRect.TopLeft,
      ATileRect.BottomRight,
      AZoom,
      AGeoConvert
    );
  finally
    VOziCalibrationMap.Free;
  end;
end;


function TThreadExportToOgf2.GetMapPreview(
  const ABitmapSaver: IBitmapTileSaver;
  out AMapPreviewWidth: Integer;
  out AMapPreviewHeight: Integer
): IBinaryData;
var
  VBitmap: TCustomBitmap32;
  VBitmapStatic: IBitmap32Static;
begin
  VBitmap := TCustomBitmap32.Create;
  try
    //TODO: generate some preview and make it sizeble

    AMapPreviewWidth := 256;
    AMapPreviewHeight := 256;

    VBitmap.SetSize(AMapPreviewWidth, AMapPreviewHeight);
    VBitmap.Clear(cBackGroundColor);

    VBitmapStatic := TBitmap32Static.CreateWithOwn(VBitmap);
    VBitmap := nil;
    Result := ABitmapSaver.Save(VBitmapStatic);
  finally
    VBitmap.Free;
  end;
end;

function TThreadExportToOgf2.GetEmptyTile(
  const ABitmapSaver: IBitmapTileSaver
): IBinaryData;
var
  VBitmap: TCustomBitmap32;
  VBitmapStatic: IBitmap32Static;
begin
  VBitmap := TCustomBitmap32.Create;
  try
    VBitmap.SetSize(FOgf2TileWidth, FOgf2TileHeight);
    VBitmap.Clear(cBackGroundColor);
    VBitmapStatic := TBitmap32Static.CreateWithOwn(VBitmap);
    VBitmap := nil;
    Result := ABitmapSaver.Save(VBitmapStatic);
  finally
    VBitmap.Free;
  end;
end;

procedure TThreadExportToOgf2.ProcessRegion;
var
  VOfg2FileStream: TFileStream;
  VPreviewImageWidth: Integer;
  VPreviewImageHeight: Integer;
  VPreviewImageData: IBinaryData;
  VEmptyTile: IBinaryData;
  VBitmap: TCustomBitmap32;
  VBitmapTile: IBitmap32Static;
  VZoom: Byte;
  VTile: TPoint;
  VTileIterator: ITileIterator;
  VSaver: IBitmapTileSaver;
  VGeoConvert: ICoordConverter;
  VWriter: TOgf2Writer;
  VTilesToProcess: Int64;
  VTilesProcessed: Int64;
  VProjected: IProjectedPolygon;
  VLine: IProjectedPolygonLine;
  VBounds: TDoubleRect;
  VPixelRect: TRect;
  VTileRect: TRect;
  I, J: Integer;
  VStaticBitmapCrop: IBitmap32Static;
  VDataToSave: IBinaryData;
begin
  inherited;
  VTilesProcessed := 0;
  VTilesToProcess := 0;

  VZoom := FZoom;
  VSaver := FTileSaver;
  VGeoConvert :=
    FCoordConverterFactory.GetCoordConverterByCode(
      CGoogleProjectionEPSG, // Merkator, WSG84, EPSG = 3785
      CTileSplitQuadrate256x256
    );

  VProjected :=
    FVectorItmesFactory.CreateProjectedPolygonByLonLatPolygon(
      FProjectionFactory.GetByConverterAndZoom(VGeoConvert, VZoom),
      Self.PolygLL
    );

  VLine := VProjected.Item[0];
  VBounds := VLine.Bounds;
  VPixelRect := RectFromDoubleRect(VBounds, rrOutside);
  VTileRect := VGeoConvert.PixelRect2TileRect(VPixelRect, VZoom);

  SaveOziCalibrationMap(
    VGeoConvert,
    VPixelRect,
    VZoom
  );

  VTileIterator := TTileIteratorByRect.Create(VTileRect);
  try
    VTilesToProcess := VTilesToProcess + VTileIterator.TilesTotal;

    ProgressInfo.SetCaption(SAS_STR_ExportTiles);
    ProgressInfo.SetFirstLine(
      SAS_STR_AllSaves + ' ' +
      IntToStr(VTilesToProcess) + ' ' +
      SAS_STR_Files
    );

    ProgressFormUpdateOnProgress(VTilesProcessed, VTilesToProcess);

      VPreviewImageData :=
        GetMapPreview(
          VSaver,
          VPreviewImageWidth,
          VPreviewImageHeight
        );

      VEmptyTile := GetEmptyTile(VSaver);

      if (VPreviewImageData <> nil) and (VEmptyTile <> nil) then begin

        VOfg2FileStream := TFileStream.Create(FTargetFile, fmCreate);
        try
          VWriter := TOgf2Writer.Create(
            VOfg2FileStream,
            (VTileRect.Right - VTileRect.Left) * 256,
            (VTileRect.Bottom - VTileRect.Top) * 256,
            FOgf2TileWidth,
            FOgf2TileHeight,
            VPreviewImageWidth,
            VPreviewImageHeight,
            VPreviewImageData.Buffer,
            VPreviewImageData.Size,
            VEmptyTile.Buffer,
            VEmptyTile.Size
          );
          try
            VBitmap := TCustomBitmap32.Create;
            try
              VBitmap.Width := FOgf2TileWidth;
              VBitmap.Height := FOgf2TileHeight;

              while VTileIterator.Next(VTile) do begin
                if CancelNotifier.IsOperationCanceled(OperationID) then begin
                  Exit;
                end;

                VBitmapTile :=
                  FImageProvider.GetBitmapRect(
                    OperationID,
                    CancelNotifier,
                    FLocalConverterFactory.CreateForTile(VTile, VZoom, VGeoConvert)
                  );

                for I := 0 to (256 div FOgf2TileWidth) - 1 do begin
                  for J := 0 to (256 div FOgf2TileHeight) - 1 do begin
                    if VBitmapTile <> nil then begin
                      VBitmap.Clear(cBackGroundColor);

                      BlockTransfer(
                        VBitmap,
                        0,
                        0,
                        VBitmap.ClipRect,
                        VBitmapTile.Bitmap,
                        Bounds(FOgf2TileWidth * I, FOgf2TileHeight * J, FOgf2TileWidth, FOgf2TileHeight),
                        dmOpaque
                      );

                      VStaticBitmapCrop := TBitmap32Static.CreateWithCopy(VBitmap);
                      VDataToSave := VSaver.Save(VStaticBitmapCrop);

                      VWriter.Add(
                        (VTile.X * 2) + I,
                        (VTile.Y * 2) + J,
                        VDataToSave.Buffer,
                        VDataToSave.Size
                      );
                    end else begin
                      VWriter.AddEmpty(
                        (VTile.X * 2) + I,
                        (VTile.Y * 2) + J
                      );
                    end;
                  end;
                end;

                Inc(VTilesProcessed);

                if VTilesProcessed mod 100 = 0 then begin
                  ProgressFormUpdateOnProgress(VTilesProcessed, VTilesToProcess);
                end;
              end;

              VWriter.SaveAllocationTable; // finalize export

            finally
              VBitmap.Free;
            end;
          finally
            VWriter.Free;
          end;
        finally
          VOfg2FileStream.Free;
        end;
      end;
  finally
    VTileIterator := nil;
  end;
end;

procedure TThreadExportToOgf2.ProgressFormUpdateOnProgress(AProcessed,
  AToProcess: Int64);
begin
  ProgressInfo.SetProcessedRatio(AProcessed / AToProcess);
  ProgressInfo.SetSecondLine(SAS_STR_Processed + ' ' + inttostr(AProcessed));
end;

end.
