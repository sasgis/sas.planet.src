unit u_ThreadExportToStorage;

interface

uses
  Types,
  SysUtils,
  Classes,
  i_NotifierOperation,
  i_RegionProcessProgressInfo,
  i_CoordConverterFactory,
  i_ContentTypeManager,
  i_VectorItemsFactory,
  i_VectorItemLonLat,
  i_TileInfoBasic,
  i_TileStorage,
  i_MapTypeListStatic,
  u_MapType,
  u_ResStrings,
  u_ThreadExportAbstract;

type
  TThreadExportToStorage = class(TThreadExportAbstract)
  private
    FMapTypeArr: IMapTypeListStatic;
    FContentTypeManager: IContentTypeManager;
    FProjectionFactory: IProjectionInfoFactory;
    FVectorGeometryProjectedFactory: IVectorGeometryProjectedFactory;
    FConfigPath, FExportPath: string;

    FIsMove: boolean;
    FIsReplace: boolean;
    FSetTargetVersionEnabled: Boolean;
    FSetTargetVersionValue: String;

    FTargetStorage: ITileStorage;
  protected
    // enum source tiles
    procedure ProcessRegion; override;
    // create storage
    procedure CreateTargetStorage(const ASourceMapType: TMapType); virtual; abstract;
  public
    constructor Create(
      const AProgressInfo: IRegionProcessProgressInfoInternal;
      const AConfigPath, AExportPath: string;
      const AContentTypeManager: IContentTypeManager;
      const AProjectionFactory: IProjectionInfoFactory;
      const AVectorGeometryProjectedFactory: IVectorGeometryProjectedFactory;
      const APolygon: ILonLatPolygon;
      const AZoomArr: TByteDynArray;
      const AMapTypeArr: IMapTypeListStatic;
      const ASetTargetVersionEnabled: Boolean;
      const ASetTargetVersionValue: String;
      const AMove, AReplace: boolean
    );
  end;

  TThreadExportToDBMS = class(TThreadExportToStorage)
  protected
    procedure CreateTargetStorage(const ASourceMapType: TMapType); override;
  end;

implementation

uses
  i_VectorItemProjected,
  i_CoordConverter,
  i_MapVersionInfo,
  i_TileIterator,
  u_TileStorageDBMS,
  u_TileIteratorByPolygon;

constructor TThreadExportToStorage.Create(
  const AProgressInfo: IRegionProcessProgressInfoInternal;
  const AConfigPath, AExportPath: string;
  const AContentTypeManager: IContentTypeManager;
  const AProjectionFactory: IProjectionInfoFactory;
  const AVectorGeometryProjectedFactory: IVectorGeometryProjectedFactory;
  const APolygon: ILonLatPolygon;
  const AZoomArr: TByteDynArray;
  const AMapTypeArr: IMapTypeListStatic;
  const ASetTargetVersionEnabled: Boolean;
  const ASetTargetVersionValue: String;
  const AMove, AReplace: boolean
);
begin
  inherited Create(
    AProgressInfo,
    APolygon,
    AZoomArr,
    Self.ClassName
  );
  FTargetStorage := nil;
  FContentTypeManager := AContentTypeManager;
  FProjectionFactory := AProjectionFactory;
  FVectorGeometryProjectedFactory := AVectorGeometryProjectedFactory;
  FConfigPath := AConfigPath;
  FExportPath := AExportPath;
  FIsMove := AMove;
  FIsReplace := AReplace;
  FSetTargetVersionEnabled := ASetTargetVersionEnabled;
  FSetTargetVersionValue := ASetTargetVersionValue;
  FMapTypeArr := AMapTypeArr;
end;

procedure TThreadExportToStorage.ProcessRegion;
var
  i, j: integer;
  VZoom: Byte;
  //pathto: string;
  //VExt: string;
  //VPath: string;
  VTile: TPoint;
  VMapType: TMapType;
  VGeoConvert: ICoordConverter;
  VTileIterators: array of array of ITileIterator;
  VTileIterator: ITileIterator;
  VProjectedPolygon: IProjectedPolygon;
  VTilesToProcess: Int64;
  VTilesProcessed: Int64;
  VTileInfo: ITileInfoWithData;
  VSourceVersion: IMapVersionInfo;
  VTargetVersion: IMapVersionInfo;
  VAllowToSave: Boolean;
  VTileInfoTarget: ITileInfoBasic;
begin
  inherited;
  SetLength(VTileIterators, FMapTypeArr.Count, Length(FZooms));
  VTilesToProcess := 0;

  for j := 0 to FMapTypeArr.Count - 1 do begin
    for i := 0 to Length(FZooms) - 1 do begin
      VZoom := FZooms[i];
      VGeoConvert := FMapTypeArr.Items[j].MapType.GeoConvert;
      VProjectedPolygon :=
        FVectorGeometryProjectedFactory.CreateProjectedPolygonByLonLatPolygon(
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
    for j := 0 to FMapTypeArr.Count - 1 do begin
      // source map
      VMapType := FMapTypeArr.Items[j].MapType;
      // source version
      VSourceVersion := VMapType.VersionConfig.Version;
      // build storage
      CreateTargetStorage(VMapType);

      // target version
      if FSetTargetVersionEnabled then begin
        VTargetVersion := VMapType.VersionConfig.VersionFactory.CreateByStoreString(FSetTargetVersionValue);
      end else begin
        VTargetVersion := VSourceVersion;
      end;

      // loop through zooms
      for i := 0 to Length(FZooms) - 1 do begin
        VZoom := FZooms[i];
        //VGeoConvert := VMapType.GeoConvert;
        //VExt := VMapType.StorageConfig.TileFileExt;
        //VPath := IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(FPathExport) + VMapType.GetShortFolderName);
        VTileIterator := VTileIterators[j, i];
        while VTileIterator.Next(VTile) do begin
          if CancelNotifier.IsOperationCanceled(OperationID) then begin
            exit;
          end;
          if Supports(VMapType.TileStorage.GetTileInfo(VTile, VZoom, VSourceVersion, gtimWithData), ITileInfoWithData, VTileInfo) then begin
            // replace or not
            VAllowToSave := TRUE;
            if (not FIsReplace) then begin
              // check tile in destination storage
              VTileInfoTarget := FTargetStorage.GetTileInfo(VTile, VZoom, VTargetVersion, gtimWithoutData);
              if (VTileInfoTarget.IsExists) then
                VAllowToSave := FALSE;
            end;

            if VAllowToSave then begin
              // save
              FTargetStorage.SaveTile(VTile, VZoom, VTargetVersion, VTileInfo.LoadDate, VTileInfo.ContentType, VTileInfo.TileData);
              // saved
              if FIsMove then begin
                // delete source
                VMapType.TileStorage.DeleteTile(VTile, VZoom, VSourceVersion);
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
    FTargetStorage := nil;
  end;
end;

{ TThreadExportToDBMS }

procedure TThreadExportToDBMS.CreateTargetStorage(const ASourceMapType: TMapType);
begin
  // хранилище создаётся с теми же настройками типов и версий, что и исходное
  FTargetStorage := TTileStorageDBMS.Create(
    ASourceMapType.GeoConvert,
    FConfigPath,
    FExportPath,
    nil,
    nil,
    FContentTypeManager,
    ASourceMapType.VersionConfig.VersionFactory,
    ASourceMapType.ContentType
  );
end;

end.


