unit u_ThreadExportToJNX;

interface

uses
  Types,
  SysUtils,
  Classes,
  JNXlib,
  t_GeoTypes,
  i_MapTypes,
  i_OperationNotifier,
  i_RegionProcessProgressInfo,
  i_VectorItemLonLat,
  i_CoordConverterFactory,
  i_VectorItmesFactory,
  i_StringListStatic,
  u_MapType,
  u_ResStrings,
  u_ThreadExportAbstract;

type
  TThreadExportToJnx = class(TThreadExportAbstract)
  private
//  FMapType: TMapType;
    FTargetFile: string;
    FCoordConverterFactory: ICoordConverterFactory;
    FProjectionFactory: IProjectionInfoFactory;
    FVectorItmesFactory: IVectorItmesFactory;
    FProductName: string; // копирайт
    FMapName: string;  // имя карты
    FJNXversion: byte;  // 3..4
    FZorder: integer;   // для 4 версии
    FProductID: integer; // 0,2,3,4,5,6,7,8,9
    FJpgQuality: IStringListStatic; // 10..100 TODO
    FLevelsDesc: IStringListStatic; // Levels Descriptions
    FMapList: IMapTypeListStatic;
    FLayersList: IMapTypeListStatic;
    FZoomList: TByteDynArray;
    FScaleArr: TByteDynArray;

  protected
    procedure ProcessRegion; override;
  public
    constructor Create(
      const ACancelNotifier: IOperationNotifier;
      AOperationID: Integer;
      const AProgressInfo: IRegionProcessProgressInfoInternal;
      const ACoordConverterFactory: ICoordConverterFactory;
      const AProjectionFactory: IProjectionInfoFactory;
      const AVectorItmesFactory: IVectorItmesFactory;
      const ATargetFile: string;
      const APolygon: ILonLatPolygon;
      const Azoomarr: TByteDynArray;
      AMapType: TMapType;
      const AProductName: string;
      const AMapName: string;
      AJNXVersion: integer;
      AZorder: integer;
      AProductID: integer;
      AJpgQuality: IStringListStatic;
      ALevelsDesc: IStringListStatic;
      const AMapList: IMapTypeListStatic;
      const ALayerList: IMapTypeListStatic;
      const AScaleArr: TByteDynArray
    );
  end;

implementation

uses
  c_CoordConverter,
  i_CoordConverter,
  i_Bitmap32Static,
  i_TileIterator,
  i_BinaryData,
  i_VectorItemProjected,
  i_BitmapTileSaveLoad,
  u_BitmapTileVampyreSaver,

  u_TileIteratorByPolygon;

constructor TThreadExportToJnx.Create(
  const ACancelNotifier: IOperationNotifier;
  AOperationID: Integer;
  const AProgressInfo: IRegionProcessProgressInfoInternal;
  const ACoordConverterFactory: ICoordConverterFactory;
  const AProjectionFactory: IProjectionInfoFactory;
  const AVectorItmesFactory: IVectorItmesFactory;
  const ATargetFile: string;
  const APolygon: ILonLatPolygon;
  const Azoomarr: TByteDynArray;
  AMapType: TMapType;
  const AProductName: string;
  const AMapName: string;
  AJNXVersion: integer;
  AZorder: integer;
  AProductID: integer;
  AJpgQuality: IStringListStatic;
  ALevelsDesc: IStringListStatic;
  const AMapList: IMapTypeListStatic;
  const ALayerList: IMapTypeListStatic;
  const AScaleArr: TByteDynArray

);
begin
  inherited Create(
    ACancelNotifier,
    AOperationID,
    AProgressInfo,
    APolygon,
    Azoomarr
  );
  FTargetFile := ATargetFile;
//FMapType := AMapType;
  FCoordConverterFactory := ACoordConverterFactory;
  FProjectionFactory := AProjectionFactory;
  FVectorItmesFactory := AVectorItmesFactory;
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
end;

procedure TThreadExportToJnx.ProcessRegion;
const
  ZoomToScale: array [0..32] of integer = (
2083334, 2083334, 2083334, 2083334, 2083334, 2083334, 2083334, 1302084,
781250,  520834,  312500,  208334,  130209,   78125,    52084,   31250,
 20834,   13021,    7813,    5209,    3125,    2084,     1303,     782,
   521,     313,    209,      131,      79,      52,       32,      21,
   14
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
  VStringStream: TStringStream;
  VWriter: TMultiVolumeJNXWriter;
  VTileBounds: TJNXRect;
  VTopLeft: TDoublePoint;
  VBottomRight: TDoublePoint;
  VProjectedPolygon: IProjectedPolygon;
  VTilesToProcess: Int64;
  VTilesProcessed: Int64;
  VData: IBinaryData;
begin
  inherited;
  VTilesToProcess := 0;
  VGeoConvert := FCoordConverterFactory.GetCoordConverterByCode(CGELonLatProjectionEPSG, CTileSplitQuadrate256x256);
  SetLength(VTileIterators, Length(FZooms));
  for i := 0 to FMapList.Count - 1 do begin
    VZoom := FZooms[i];
    VProjectedPolygon :=
      FVectorItmesFactory.CreateProjectedPolygonByLonLatPolygon(
        FProjectionFactory.GetByConverterAndZoom(VGeoConvert, VZoom),
        PolygLL
      );
    VTileIterators[i] := TTileIteratorByPolygon.Create(VProjectedPolygon);
    VTilesToProcess := VTilesToProcess + VTileIterators[i].TilesTotal;
  end;

  VWriter := TMultiVolumeJNXWriter.Create(FTargetFile);
  try
    VWriter.Levels := Length(FZooms);
    VWriter.ProductName := FProductName;
    VWriter.MapName := FmapName;
    VWriter.Version := FJNXVersion;
    VWriter.ZOrder := FZorder;
    VWriter.ProductID := FProductID;
    
    for i := 0 to FMapList.Count - 1 do begin
      VWriter.LevelScale[i] := ZoomToScale[FScaleArr[i]];;
      VWriter.TileCount[i] := VTileIterators[i].TilesTotal;
      VWriter.LevelDescription[i] := FLevelsDesc.items[i * 3];
      VWriter.LevelName[i] := FLevelsDesc.Items[i * 3 + 1];
      VWriter.LevelCopyright[i] := FLevelsDesc.items[i * 3 + 2];
      VWriter.LevelZoom[i] := FZoomList[i];
    end;

    try
      ProgressInfo.SetCaption(SAS_STR_ExportTiles);
      ProgressInfo.SetFirstLine(SAS_STR_AllSaves + ' ' + inttostr(VTilesToProcess) + ' ' + SAS_STR_Files);
      VStringStream := TStringStream.Create('');
      try
        VTilesProcessed := 0;
        ProgressFormUpdateOnProgress(VTilesProcessed, VTilesToProcess);
        for i := 0 to Length(FZooms) - 1 do begin
          VSaver := TVampyreBasicBitmapTileSaverJPG.Create(strtoint(FJpgQuality.Items[i]));
          VZoom := FZoomList[i];
          VTileIterator := VTileIterators[i];
          while VTileIterator.Next(VTile) do begin
            if CancelNotifier.IsOperationCanceled(OperationID) then begin
              exit;
            end;
//            VBitmapTile := FMapType.LoadTileUni(VTile, VZoom, VGeoConvert, False, False, True);
            VBitmapTile := FMapList.Items[i].MapType.LoadTileUni(VTile, VZoom, VGeoConvert, False, False, True);
            if VBitmapTile <> nil then begin
              VData := VSaver.Save(VBitmapTile);

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
                I,
                256,
                256,
                VTileBounds,
                VStringStream.DataString
              );

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
      for i := 0 to Length(FZooms) - 1 do begin
        VTileIterators[i] := nil;
      end;
      VTileIterators := nil;
    end;
  finally
    VWriter.Free;
  end;
end;

end.
