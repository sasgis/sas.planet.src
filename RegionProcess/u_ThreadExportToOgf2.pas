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
  i_Bitmap32StaticFactory,
  i_RegionProcessProgressInfo,
  i_GeometryLonLat,
  i_CoordConverter,
  i_CoordConverterFactory,
  i_LocalCoordConverterFactorySimpe,
  i_GeometryProjectedFactory,
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
    FBitmapFactory: IBitmap32StaticFactory;
    FVectorGeometryProjectedFactory: IGeometryProjectedFactory;
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
      const APixelRect: TRect;
      const AZoom: Byte
    );
    procedure ProgressFormUpdateOnProgress(AProcessed, AToProcess: Int64);
  protected
    procedure ProcessRegion; override;
  public
    constructor Create(
      const AProgressInfo: IRegionProcessProgressInfoInternal;
      const ACoordConverterFactory: ICoordConverterFactory;
      const ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
      const AProjectionFactory: IProjectionInfoFactory;
      const ABitmapFactory: IBitmap32StaticFactory;
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
      const ATargetFile: string;
      const APolygon: IGeometryLonLatMultiPolygon;
      const AImageProvider: IBitmapLayerProvider;
      AZoom: Byte;
      const ATileSize: TPoint;
      const ATileSaver: IBitmapTileSaver
    );
  end;

implementation

uses
  GR32,
  Ogf2Writer,
  t_GeoTypes,
  c_CoordConverter,
  i_MapCalibration,
  i_Bitmap32Static,
  i_TileIterator,
  i_GeometryProjected,
  u_TileIteratorByRect,
  u_MapCalibrationOzi,
  u_BitmapFunc,
  u_GeoFunc;

const
  cBackGroundColor = $CCCCCCCC;

{ TThreadExportToOgf2 }

constructor TThreadExportToOgf2.Create(
  const AProgressInfo: IRegionProcessProgressInfoInternal;
  const ACoordConverterFactory: ICoordConverterFactory;
  const ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
  const AProjectionFactory: IProjectionInfoFactory;
  const ABitmapFactory: IBitmap32StaticFactory;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  const ATargetFile: string;
  const APolygon: IGeometryLonLatMultiPolygon;
  const AImageProvider: IBitmapLayerProvider;
  AZoom: Byte;
  const ATileSize: TPoint;
  const ATileSaver: IBitmapTileSaver
);
begin
  inherited Create(
    AProgressInfo,
    APolygon,
    Self.ClassName
  );
  FImageProvider := AImageProvider;
  FZoom := AZoom;
  FTargetFile := ATargetFile;
  FCoordConverterFactory := ACoordConverterFactory;
  FLocalConverterFactory := ALocalConverterFactory;
  FProjectionFactory := AProjectionFactory;
  FBitmapFactory := ABitmapFactory;
  FVectorGeometryProjectedFactory := AVectorGeometryProjectedFactory;
  FTileSaver := ATileSaver;
  FOgf2TileWidth := ATileSize.X;
  FOgf2TileHeight := ATileSize.Y;
end;

procedure TThreadExportToOgf2.SaveOziCalibrationMap(
  const AGeoConvert: ICoordConverter;
  const APixelRect: TRect;
  const AZoom: Byte
);
var
  VOziCalibrationMap: IMapCalibration;
begin
  VOziCalibrationMap := TMapCalibrationOzi.Create;
  VOziCalibrationMap.SaveCalibrationInfo(
    FTargetFile,
    APixelRect.TopLeft,
    APixelRect.BottomRight,
    AZoom,
    AGeoConvert
  );
end;


function TThreadExportToOgf2.GetMapPreview(
  const ABitmapSaver: IBitmapTileSaver;
  out AMapPreviewWidth: Integer;
  out AMapPreviewHeight: Integer
): IBinaryData;
var
  VBitmapStatic: IBitmap32Static;
begin
  AMapPreviewWidth := 256;
  AMapPreviewHeight := 256;
  VBitmapStatic :=
    FBitmapFactory.BuildEmptyClear(
      Point(AMapPreviewWidth, AMapPreviewHeight),
      cBackGroundColor
    );
  //TODO: generate some preview and make it sizeble
  Result := ABitmapSaver.Save(VBitmapStatic);
end;

function TThreadExportToOgf2.GetEmptyTile(
  const ABitmapSaver: IBitmapTileSaver
): IBinaryData;
var
  VBitmapStatic: IBitmap32Static;
begin
  VBitmapStatic :=
    FBitmapFactory.BuildEmptyClear(
      Point(FOgf2TileWidth, FOgf2TileHeight),
      cBackGroundColor
    );
  Result := ABitmapSaver.Save(VBitmapStatic);
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
  VProjected: IGeometryProjectedMultiPolygon;
  VLine: IGeometryProjectedPolygon;
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
    FVectorGeometryProjectedFactory.CreateProjectedPolygonByLonLatPolygon(
      FProjectionFactory.GetByConverterAndZoom(VGeoConvert, VZoom),
      Self.PolygLL
    );

  VLine := VProjected.Item[0];
  VBounds := VLine.Bounds;
  VPixelRect := RectFromDoubleRect(VBounds, rrOutside);
  VTileRect := VGeoConvert.PixelRect2TileRect(VPixelRect, VZoom);

  SaveOziCalibrationMap(
    VGeoConvert,
    VGeoConvert.TileRect2PixelRect(VTileRect, VZoom),
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
                      VBitmapTile,
                      Bounds(FOgf2TileWidth * I, FOgf2TileHeight * J, FOgf2TileWidth, FOgf2TileHeight),
                      dmOpaque
                    );

                    VStaticBitmapCrop :=
                      FBitmapFactory.Build(
                        Point(FOgf2TileWidth, FOgf2TileHeight),
                        VBitmap.Bits
                      );
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
