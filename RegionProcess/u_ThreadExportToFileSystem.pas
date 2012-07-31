unit u_ThreadExportToFileSystem;

interface

uses
  Types,
  SysUtils,
  Classes,
  i_TileFileNameGenerator,
  i_NotifierOperation,
  i_RegionProcessProgressInfo,
  i_CoordConverterFactory,
  i_VectorItmesFactory,
  i_VectorItemLonLat,
  i_MapTypes,
  u_MapType,
  u_ResStrings,
  u_ThreadExportAbstract;

type
  TThreadExportToFileSystem = class(TThreadExportAbstract)
  private
    FMapTypeArr: IMapTypeListStatic;
    FTileNameGen: ITileFileNameGenerator;
    FProjectionFactory: IProjectionInfoFactory;
    FVectorItmesFactory: IVectorItmesFactory;

    FIsMove: boolean;
    FIsReplace: boolean;
    FPathExport: string;
  protected
    procedure ProcessRegion; override;
  public
    constructor Create(
      const ACancelNotifier: INotifierOperation;
      AOperationID: Integer;
      const AProgressInfo: IRegionProcessProgressInfoInternal;
      const APath: string;
      const AProjectionFactory: IProjectionInfoFactory;
      const AVectorItmesFactory: IVectorItmesFactory;
      const APolygon: ILonLatPolygon;
      const AZoomArr: TByteDynArray;
      const AMapTypeArr: IMapTypeListStatic;
      AMove: boolean;
      AReplace: boolean;
      const ATileNameGen: ITileFileNameGenerator
    );
  end;

implementation

uses
  i_VectorItemProjected,
  i_CoordConverter,
  i_TileIterator,
  u_TileIteratorByPolygon;

constructor TThreadExportToFileSystem.Create(
  const ACancelNotifier: INotifierOperation;
  AOperationID: Integer;
  const AProgressInfo: IRegionProcessProgressInfoInternal;
  const APath: string;
  const AProjectionFactory: IProjectionInfoFactory;
  const AVectorItmesFactory: IVectorItmesFactory;
  const APolygon: ILonLatPolygon;
  const AZoomArr: TByteDynArray;
  const AMapTypeArr: IMapTypeListStatic;
  AMove, AReplace: boolean;
  const ATileNameGen: ITileFileNameGenerator
);
begin
  inherited Create(
    ACancelNotifier,
    AOperationID,
    AProgressInfo,
    APolygon,
    AZoomArr,
    Self.ClassName
  );
  FProjectionFactory := AProjectionFactory;
  FVectorItmesFactory := AVectorItmesFactory;
  FPathExport := APath;
  FIsMove := AMove;
  FTileNameGen := ATileNameGen;
  FIsReplace := AReplace;
  FMapTypeArr := AMapTypeArr;
end;

procedure TThreadExportToFileSystem.ProcessRegion;
var
  i, j: integer;
  VZoom: Byte;
  pathto: string;
  VExt: string;
  VPath: string;
  VTile: TPoint;
  VMapType: TMapType;
  VGeoConvert: ICoordConverter;
  VTileIterators: array of array of ITileIterator;
  VTileIterator: ITileIterator;
  VProjectedPolygon: IProjectedPolygon;
  VTilesToProcess: Int64;
  VTilesProcessed: Int64;
begin
  inherited;
  SetLength(VTileIterators, FMapTypeArr.Count, Length(FZooms));
  VTilesToProcess := 0;
  for j := 0 to FMapTypeArr.Count - 1 do begin
    for i := 0 to Length(FZooms) - 1 do begin
      VZoom := FZooms[i];
      VGeoConvert := FMapTypeArr.Items[j].MapType.GeoConvert;
      VProjectedPolygon :=
        FVectorItmesFactory.CreateProjectedPolygonByLonLatPolygon(
          FProjectionFactory.GetByConverterAndZoom(VGeoConvert, VZoom),
          PolygLL
        );
      VTileIterators[j, i] := TTileIteratorByPolygon.Create(VProjectedPolygon);
      VTilesToProcess := VTilesToProcess + VTileIterators[j, i].TilesTotal;
    end;
  end;
  try
    ProgressInfo.SetCaption(SAS_STR_ExportTiles);
    ProgressInfo.SetFirstLine(
      SAS_STR_AllSaves + ' ' + inttostr(VTilesToProcess) + ' ' + SAS_STR_Files
    );
    VTilesProcessed := 0;
    ProgressFormUpdateOnProgress(VTilesProcessed, VTilesToProcess);
    for i := 0 to Length(FZooms) - 1 do begin
      VZoom := FZooms[i];
      for j := 0 to FMapTypeArr.Count - 1 do begin
        VMapType := FMapTypeArr.Items[j].MapType;
        VGeoConvert := VMapType.GeoConvert;
        VExt := VMapType.StorageConfig.TileFileExt;
        VPath := IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(FPathExport) + VMapType.GetShortFolderName);
        VTileIterator := VTileIterators[j, i];
        while VTileIterator.Next(VTile) do begin
          if CancelNotifier.IsOperationCanceled(OperationID) then begin
            exit;
          end;
          if VMapType.TileExists(VTile, VZoom) then begin
            pathto := VPath + FTileNameGen.GetTileFileName(VTile, VZoom) + VExt;
            if VMapType.TileExportToFile(VTile, VZoom, pathto, FIsReplace) then begin
              if FIsMove then begin
                VMapType.DeleteTile(VTile, VZoom);
              end;
            end;
          end;
          inc(VTilesProcessed);
          if VTilesProcessed mod 100 = 0 then begin
            ProgressFormUpdateOnProgress(VTilesProcessed, VTilesToProcess);
          end;
        end;
      end;
    end;
  finally
    for j := 0 to FMapTypeArr.Count - 1 do begin
      for i := 0 to Length(FZooms) - 1 do begin
        VTileIterators[j, i] := nil;
      end;
    end;
    VTileIterators := nil;
  end;
  FTileNameGen := nil;
end;

end.
