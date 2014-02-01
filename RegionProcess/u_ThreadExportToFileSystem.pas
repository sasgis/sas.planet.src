unit u_ThreadExportToFileSystem;

interface

uses
  Windows,
  Types,
  SysUtils,
  Classes,
  i_TileFileNameGenerator,
  i_NotifierOperation,
  i_RegionProcessProgressInfo,
  i_CoordConverterFactory,
  i_VectorGeometryProjectedFactory,
  i_GeometryLonLat,
  i_TileInfoBasic,
  i_MapTypeListStatic,
  u_MapType,
  u_ResStrings,
  u_ThreadExportAbstract;

type
  TThreadExportToFileSystem = class(TThreadExportAbstract)
  private
    FMapTypeArr: IMapTypeListStatic;
    FTileNameGen: ITileFileNameGenerator;
    FProjectionFactory: IProjectionInfoFactory;
    FVectorGeometryProjectedFactory: IGeometryProjectedFactory;

    FIsMove: boolean;
    FIsReplace: boolean;
    FPathExport: string;
    FPlaceInSubFolder: Boolean;

    function SaveTileToFile(
      const ATileInfo: ITileInfoWithData;
      const AFileName: string;
      OverWrite: boolean
    ): boolean;
  protected
    procedure ProcessRegion; override;
  public
    constructor Create(
      const AProgressInfo: IRegionProcessProgressInfoInternal;
      const APath: string;
      const APlaceInSubFolder: Boolean;
      const AProjectionFactory: IProjectionInfoFactory;
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
      const APolygon: IGeometryLonLatMultiPolygon;
      const AZoomArr: TByteDynArray;
      const AMapTypeArr: IMapTypeListStatic;
      AMove: boolean;
      AReplace: boolean;
      const ATileNameGen: ITileFileNameGenerator
    );
  end;

implementation

uses
  i_GeometryProjected,
  i_CoordConverter,
  i_MapVersionInfo,
  i_BinaryData,
  i_TileIterator,
  i_TileStorage,
  u_TileIteratorByPolygon;

constructor TThreadExportToFileSystem.Create(
  const AProgressInfo: IRegionProcessProgressInfoInternal;
  const APath: string;
  const APlaceInSubFolder: Boolean;
  const AProjectionFactory: IProjectionInfoFactory;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  const APolygon: IGeometryLonLatMultiPolygon;
  const AZoomArr: TByteDynArray;
  const AMapTypeArr: IMapTypeListStatic;
  AMove, AReplace: boolean;
  const ATileNameGen: ITileFileNameGenerator
);
begin
  inherited Create(
    AProgressInfo,
    APolygon,
    AZoomArr,
    Self.ClassName
  );
  FProjectionFactory := AProjectionFactory;
  FVectorGeometryProjectedFactory := AVectorGeometryProjectedFactory;
  FPathExport := APath;
  FPlaceInSubFolder := APlaceInSubFolder;
  FIsMove := AMove;
  FTileNameGen := ATileNameGen;
  FIsReplace := AReplace;
  FMapTypeArr := AMapTypeArr;
end;

function TThreadExportToFileSystem.SaveTileToFile(
  const ATileInfo: ITileInfoWithData;
  const AFileName: string;
  OverWrite: boolean
): boolean;
var
  VFileStream: TFileStream;
  VFileExists: Boolean;
  VExportPath: string;
  VData: IBinaryData;
begin
  Result := False;
  VFileExists := FileExists(AFileName);
  if not VFileExists or OverWrite then begin
    if VFileExists then begin
      DeleteFile(AFileName);
    end else begin
      VExportPath := ExtractFilePath(AFileName);
      ForceDirectories(VExportPath);
    end;
    VData := ATileInfo.TileData;
    VFileStream := TFileStream.Create(AFileName, fmCreate);
    try
      VFileStream.WriteBuffer(VData.Buffer^, VData.Size);
      FileSetDate(AFileName, DateTimeToFileDate(ATileInfo.GetLoadDate));
    finally
      VFileStream.Free;
    end;
    Result := True;
  end;
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
  VProjectedPolygon: IGeometryProjectedMultiPolygon;
  VTilesToProcess: Int64;
  VTilesProcessed: Int64;
  VTileInfo: ITileInfoWithData;
  VVersion: IMapVersionInfo;
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
    for i := 0 to Length(FZooms) - 1 do begin
      VZoom := FZooms[i];
      for j := 0 to FMapTypeArr.Count - 1 do begin
        VMapType := FMapTypeArr.Items[j].MapType;
        VVersion := VMapType.VersionConfig.Version;
        VGeoConvert := VMapType.GeoConvert;
        VExt := VMapType.StorageConfig.TileFileExt;
        VPath := IncludeTrailingPathDelimiter(FPathExport);
        if FPlaceInSubFolder then begin
          VPath := IncludeTrailingPathDelimiter(VPath + VMapType.GetShortFolderName);
        end;
        VTileIterator := VTileIterators[j, i];
        while VTileIterator.Next(VTile) do begin
          if CancelNotifier.IsOperationCanceled(OperationID) then begin
            exit;
          end;
          if Supports(VMapType.TileStorage.GetTileInfo(VTile, VZoom, VVersion, gtimWithData), ITileInfoWithData, VTileInfo) then begin
            pathto := VPath + FTileNameGen.GetTileFileName(VTile, VZoom) + VExt;
            if SaveTileToFile(VTileInfo, pathto, FIsReplace) then begin
              if FIsMove then begin
                VMapType.TileStorage.DeleteTile(VTile, VZoom, VVersion);
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
