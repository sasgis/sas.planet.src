unit u_ThreadExportToJNX;

interface

uses
  t_GeoTypes,
  i_RegionProcessProgressInfo,
  i_GeometryLonLat,
  i_CoordConverterFactory,
  i_GeometryProjectedFactory,
  u_ExportToJnxTask,
  u_ThreadRegionProcessAbstract;

type
  TThreadExportToJnx = class(TThreadRegionProcessAbstract)
  private
    FTasks: TExportTaskJnxArray;
    FTargetFile: string;
    FProjectionFactory: IProjectionInfoFactory;
    FVectorGeometryProjectedFactory: IGeometryProjectedFactory;
    FProductName: string; // копирайт
    FMapName: string;  // имя карты
    FJNXVersion: byte;  // 3..4
    FZorder: integer;   // для 4 версии
    FProductID: integer; // 0,2,3,4,5,6,7,8,9
  protected
    procedure ProcessRegion; override;
  public
    constructor Create(
      const AProgressInfo: IRegionProcessProgressInfoInternal;
      const AProjectionFactory: IProjectionInfoFactory;
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
      const ATargetFile: string;
      const APolygon: IGeometryLonLatMultiPolygon;
      const ATasks: TExportTaskJnxArray;
      const AProductName: string;
      const AMapName: string;
      AJNXVersion: integer;
      AZorder: integer;
      AProductID: integer
    );
  end;

implementation

uses
  Types,
  SysUtils,
  ALString,
  JNXlib,
  i_TileStorage,
  i_TileInfoBasic,
  i_ContentTypeInfo,
  i_CoordConverter,
  i_Bitmap32Static,
  i_TileIterator,
  i_BinaryData,
  i_MapVersionRequest,
  i_BitmapTileSaveLoad,
  i_GeometryProjected,
  u_ResStrings,
  u_TileIteratorByPolygon;

constructor TThreadExportToJnx.Create(
  const AProgressInfo: IRegionProcessProgressInfoInternal;
  const AProjectionFactory: IProjectionInfoFactory;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  const ATargetFile: string;
  const APolygon: IGeometryLonLatMultiPolygon;
  const ATasks: TExportTaskJnxArray;
  const AProductName: string;
  const AMapName: string;
  AJNXVersion: integer;
  AZorder: integer;
  AProductID: integer
);
begin
  inherited Create(
    AProgressInfo,
    APolygon,
    Self.ClassName
  );
  FTargetFile := ATargetFile;
  FProjectionFactory := AProjectionFactory;
  FVectorGeometryProjectedFactory := AVectorGeometryProjectedFactory;
  FTasks := ATasks;
  FProductName := AProductName;
  FMapName := AMapName;
  FJNXVersion := AJNXVersion;
  FZorder := AZorder;
  FProductID := AProductID;
end;

procedure TThreadExportToJnx.ProcessRegion;
const
  ZoomToScale: array [0..26] of integer = (
    2446184, 1834628, 1223072, 611526, 458642, 305758, 152877, 114657, 76437,
    38218, 28664, 19109, 9554, 7166, 4777, 2388, 1791, 1194,
    597, 448, 298, 149, 112, 75, 37, 28, 19
  );

var
  i: integer;
  VBitmapTile: IBitmap32Static;
  VZoom: Byte;
  VTile: TPoint;
  VTileIterators: array of ITileIterator;
  VTileIterator: ITileIterator;
  VSaver: IBitmapTileSaver;
  VGeoConvert: ICoordConverter;
  VStringStream: TALStringStream;
  VWriter: TMultiVolumeJNXWriter;
  VTileBounds: TJNXRect;
  VTopLeft: TDoublePoint;
  VBottomRight: TDoublePoint;
  VProjectedPolygon: IGeometryProjectedMultiPolygon;
  VTilesToProcess: Int64;
  VTilesProcessed: Int64;
  VData: IBinaryData;
  VTileStorage: ITileStorage;
  VVersion: IMapVersionRequest;
  VTileInfo: ITileInfoWithData;
  VContentTypeInfoBitmap: IContentTypeInfoBitmap;
  VRecompress: Boolean;
begin
  inherited;
  VTilesToProcess := 0;
  SetLength(VTileIterators, Length(FTasks));
  for i := 0 to Length(FTasks) - 1 do begin
    VZoom := FTasks[i].FZoom;
    VGeoConvert := FTasks[i].FTileStorage.CoordConverter;
    VProjectedPolygon :=
      FVectorGeometryProjectedFactory.CreateProjectedPolygonByLonLatPolygon(
        FProjectionFactory.GetByConverterAndZoom(VGeoConvert, VZoom),
        PolygLL
      );
    VTileIterators[i] := TTileIteratorByPolygon.Create(VProjectedPolygon);
    VTilesToProcess := VTilesToProcess + VTileIterators[i].TilesTotal;
  end;

  VWriter := TMultiVolumeJNXWriter.Create(FTargetFile);
  try
    VWriter.Levels := Length(FTasks);
    VWriter.ProductName := FProductName;
    VWriter.MapName := FMapName;
    VWriter.Version := FJNXVersion;
    VWriter.ZOrder := FZorder;
    VWriter.ProductID := FProductID;

    for i := 0 to Length(FTasks) - 1 do begin
      VWriter.LevelScale[i] := ZoomToScale[FTasks[i].FScale];
      VWriter.TileCount[i] := VTileIterators[i].TilesTotal;
      VWriter.LevelDescription[i] := FTasks[i].FLevelDesc;
      VWriter.LevelName[i] := FTasks[i].FLevelName;
      VWriter.LevelCopyright[i] := FTasks[i].FLevelCopyright;
      VWriter.LevelZoom[i] := FTasks[i].FZoom;
    end;

    try
      ProgressInfo.SetCaption(SAS_STR_ExportTiles);
      ProgressInfo.SetFirstLine(SAS_STR_AllSaves + ' ' + inttostr(VTilesToProcess) + ' ' + SAS_STR_Files);
      VStringStream := TALStringStream.Create('');
      try
        VTilesProcessed := 0;
        ProgressFormUpdateOnProgress(VTilesProcessed, VTilesToProcess);
        for i := 0 to Length(FTasks) - 1 do begin
          VSaver := FTasks[i].FSaver;
          VRecompress := FTasks[i].FRecompress;

          VTileStorage := FTasks[i].FTileStorage;
          VVersion := FTasks[i].FMapVersion;
          VZoom := FTasks[i].FZoom;
          VGeoConvert := VTileStorage.CoordConverter;
          VTileIterator := VTileIterators[i];
          while VTileIterator.Next(VTile) do begin
            if CancelNotifier.IsOperationCanceled(OperationID) then begin
              exit;
            end;

            if Supports(VTileStorage.GetTileInfoEx(VTile, VZoom, VVersion, gtimWithData), ITileInfoWithData, VTileInfo) then begin
              VData := Nil;
              if VRecompress or not SameText(VTileInfo.ContentType.GetContentType, 'image/jpg') then begin
                if Supports(VTileInfo.ContentType, IContentTypeInfoBitmap, VContentTypeInfoBitmap) then begin
                  VBitmapTile := VContentTypeInfoBitmap.GetLoader.Load(VTileInfo.TileData);
                  if Assigned(VBitmapTile) then begin
                    VData := VSaver.Save(VBitmapTile);
                  end;
                end;
              end else begin
                VData := VTileInfo.TileData;
              end;

              if Assigned(VData) then begin
                VTopLeft := VGeoConvert.TilePos2LonLat(Point(VTile.X, VTile.Y + 1), VZoom);
                VBottomRight := VGeoConvert.TilePos2LonLat(Point(VTile.X + 1, VTile.Y), VZoom);

                VTileBounds := JNXRect(
                  WGS84CoordToJNX(VBottomRight.Y),
                  WGS84CoordToJNX(VBottomRight.X),
                  WGS84CoordToJNX(VTopLeft.Y),
                  WGS84CoordToJNX(VTopLeft.X)
                );

                VStringStream.Size := 0;
                VStringStream.WriteBuffer(VData.Buffer^, VData.Size);

                VWriter.WriteTile(
                  i,
                  256,
                  256,
                  VTileBounds,
                  VStringStream.DataString
                );
              end;
            end;
            inc(VTilesProcessed);
            if VTilesProcessed mod 100 = 0 then begin
              ProgressFormUpdateOnProgress(VTilesProcessed, VTilesToProcess);
            end;
          end;
        end;
      finally
        VStringStream.Free;
      end;
    finally
      for i := 0 to Length(VTileIterators) - 1 do begin
        VTileIterators[i] := nil;
      end;
      VTileIterators := nil;
    end;
  finally
    VWriter.Free;
  end;
end;

end.
