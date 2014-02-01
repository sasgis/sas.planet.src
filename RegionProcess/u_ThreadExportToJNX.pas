unit u_ThreadExportToJNX;

interface

uses
  Types,
  SysUtils,
  Classes,
  JNXlib,
  t_GeoTypes,
  i_MapTypes,
  i_MapTypeListStatic,
  i_NotifierOperation,
  i_RegionProcessProgressInfo,
  i_GeometryLonLat,
  i_CoordConverterFactory,
  i_VectorGeometryProjectedFactory,
  i_BitmapTileSaveLoadFactory,
  i_StringListStatic,
  i_TileStorage,
  i_TileInfoBasic,
  i_ContentTypeInfo,
  u_ResStrings,
  u_ThreadExportAbstract;

type
  TThreadExportToJnx = class(TThreadExportAbstract)
  private
    FTargetFile: string;
    FCoordConverterFactory: ICoordConverterFactory;
    FProjectionFactory: IProjectionInfoFactory;
    FVectorGeometryProjectedFactory: IGeometryProjectedFactory;
    FBitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
    FProductName: string; // копирайт
    FMapName: string;  // имя карты
    FJNXVersion: byte;  // 3..4
    FZorder: integer;   // для 4 версии
    FProductID: integer; // 0,2,3,4,5,6,7,8,9
    FJpgQuality: IStringListStatic; // 10..100 TODO
    FLevelsDesc: IStringListStatic; // Levels Descriptions
    FMapList: IMapTypeListStatic;
    FLayersList: IMapTypeListStatic;
    FZoomList: TByteDynArray;
    FScaleArr: TByteDynArray;
    FRecompressArr: TBooleanDynArray;

  protected
    procedure ProcessRegion; override;
  public
    constructor Create(
      const AProgressInfo: IRegionProcessProgressInfoInternal;
      const ACoordConverterFactory: ICoordConverterFactory;
      const AProjectionFactory: IProjectionInfoFactory;
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
      const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
      const ATargetFile: string;
      const APolygon: IGeometryLonLatMultiPolygon;
      const Azoomarr: TByteDynArray;
      const AProductName: string;
      const AMapName: string;
      AJNXVersion: integer;
      AZorder: integer;
      AProductID: integer;
      const AJpgQuality: IStringListStatic;
      const ALevelsDesc: IStringListStatic;
      const AMapList: IMapTypeListStatic;
      const ALayerList: IMapTypeListStatic;
      const AScaleArr: TByteDynArray;
      const ARecompressArr: TBooleanDynArray
    );
  end;

implementation

uses
  ALString,
  i_CoordConverter,
  i_Bitmap32Static,
  i_TileIterator,
  i_BinaryData,
  i_GeometryProjected,
  i_BitmapTileSaveLoad,
  i_MapVersionInfo,
  u_TileIteratorByPolygon;

constructor TThreadExportToJnx.Create(
  const AProgressInfo: IRegionProcessProgressInfoInternal;
  const ACoordConverterFactory: ICoordConverterFactory;
  const AProjectionFactory: IProjectionInfoFactory;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
  const ATargetFile: string;
  const APolygon: IGeometryLonLatMultiPolygon;
  const Azoomarr: TByteDynArray;
  const AProductName: string;
  const AMapName: string;
  AJNXVersion: integer;
  AZorder: integer;
  AProductID: integer;
  const AJpgQuality: IStringListStatic;
  const ALevelsDesc: IStringListStatic;
  const AMapList: IMapTypeListStatic;
  const ALayerList: IMapTypeListStatic;
  const AScaleArr: TByteDynArray;
  const ARecompressArr: TBooleanDynArray
);
begin
  inherited Create(
    AProgressInfo,
    APolygon,
    Azoomarr,
    Self.ClassName
  );
  FTargetFile := ATargetFile;
  FCoordConverterFactory := ACoordConverterFactory;
  FProjectionFactory := AProjectionFactory;
  FVectorGeometryProjectedFactory := AVectorGeometryProjectedFactory;
  FBitmapTileSaveLoadFactory := ABitmapTileSaveLoadFactory;
  FProductName := AProductName;
  FMapName := AMapName;
  FJNXVersion := AJNXVersion;
  FZorder := AZorder;
  FProductID := AProductID;
  FJpgQuality := AJpgQuality;
  FLevelsDesc := ALevelsDesc;
  FMapList := AMapList;
  FLayersList := ALayerList;
  FZoomList := Azoomarr;
  FScaleArr := AScaleArr;
  FRecompressArr := ARecompressArr;
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
  VMapVersionInfo: IMapVersionInfo;
  VTileInfo: ITileInfoWithData;
  VContentTypeInfoBitmap: IContentTypeInfoBitmap;
begin
  inherited;
  VTilesToProcess := 0;
  SetLength(VTileIterators, Length(FZoomList));
  for i := 0 to FMapList.Count - 1 do begin
    VZoom := FZoomList[i];
    VGeoConvert := FMapList.Items[i].MapType.GeoConvert;
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
    VWriter.Levels := Length(FZoomList);
    VWriter.ProductName := FProductName;
    VWriter.MapName := FMapName;
    VWriter.Version := FJNXVersion;
    VWriter.ZOrder := FZorder;
    VWriter.ProductID := FProductID;

    for i := 0 to FMapList.Count - 1 do begin
      VWriter.LevelScale[i] := ZoomToScale[FScaleArr[i]];
      VWriter.TileCount[i] := VTileIterators[i].TilesTotal;
      VWriter.LevelDescription[i] := FLevelsDesc.items[i * 3];
      VWriter.LevelName[i] := FLevelsDesc.Items[i * 3 + 1];
      VWriter.LevelCopyright[i] := FLevelsDesc.items[i * 3 + 2];
      VWriter.LevelZoom[i] := FZoomList[i];
    end;

    try
      ProgressInfo.SetCaption(SAS_STR_ExportTiles);
      ProgressInfo.SetFirstLine(SAS_STR_AllSaves + ' ' + inttostr(VTilesToProcess) + ' ' + SAS_STR_Files);
      VStringStream := TALStringStream.Create('');
      try
        VTilesProcessed := 0;
        ProgressFormUpdateOnProgress(VTilesProcessed, VTilesToProcess);
        for i := 0 to Length(FZoomList) - 1 do begin
          VSaver := FBitmapTileSaveLoadFactory.CreateJpegSaver(StrToInt(FJpgQuality.Items[i]));

          VTileStorage := FMapList.Items[i].MapType.TileStorage;
          VMapVersionInfo := FMapList.Items[i].MapType.VersionConfig.Version;
          VZoom := FZoomList[i];
          VGeoConvert := FMapList.Items[i].MapType.GeoConvert;
          VTileIterator := VTileIterators[i];
          while VTileIterator.Next(VTile) do begin
            if CancelNotifier.IsOperationCanceled(OperationID) then begin
              exit;
            end;

            if Supports(VTileStorage.GetTileInfo(VTile, VZoom, VMapVersionInfo, gtimWithData), ITileInfoWithData, VTileInfo) then begin
              VData := Nil;
              if FRecompressArr[i] or not SameText(VTileInfo.ContentType.GetContentType, 'image/jpg') then begin
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
      for i := 0 to Length(FZoomList) - 1 do begin
        VTileIterators[i] := nil;
      end;
      VTileIterators := nil;
    end;
  finally
    VWriter.Free;
  end;
end;

end.
