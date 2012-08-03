unit u_ThreadExportToZip;

interface

uses
  Types,
  SysUtils,
  Classes,
  GR32,
  i_CoordConverterFactory,
  i_NotifierOperation,
  i_RegionProcessProgressInfo,
  i_VectorItmesFactory,
  i_VectorItemLonLat,
  i_TileFileNameGenerator,
  i_ArchiveReadWriteFactory,
  u_MapType,
  u_ResStrings,
  u_ThreadExportAbstract;

type
  TThreadExportToZip = class(TThreadExportAbstract)
  private
    FMapType: TMapType;
    FTileNameGen: ITileFileNameGenerator;
    FProjectionFactory: IProjectionInfoFactory;
    FVectorItmesFactory: IVectorItmesFactory;
    FArchiveReadWriteFactory: IArchiveReadWriteFactory;
    FTargetFile: string;
  protected
    procedure ProcessRegion; override;
  public
    constructor Create(
      const ACancelNotifier: INotifierOperation;
      AOperationID: Integer;
      const AProgressInfo: IRegionProcessProgressInfoInternal;
      const ATargetFile: string;
      const AProjectionFactory: IProjectionInfoFactory;
      const AVectorItmesFactory: IVectorItmesFactory;
      const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
      const APolygon: ILonLatPolygon;
      const Azoomarr: TByteDynArray;
      AMapType: TMapType;
      const ATileNameGen: ITileFileNameGenerator
    );
    destructor Destroy; override;
  end;

implementation

uses
  i_BinaryData,
  i_ArchiveReadWrite,
  i_VectorItemProjected,
  i_TileIterator,
  i_TileInfoBasic,
  u_StreamReadOnlyByBinaryData,
  u_TileIteratorByPolygon,
  u_TileStorageAbstract;

constructor TThreadExportToZip.Create(
  const ACancelNotifier: INotifierOperation;
  AOperationID: Integer;
  const AProgressInfo: IRegionProcessProgressInfoInternal;
  const ATargetFile: string;
  const AProjectionFactory: IProjectionInfoFactory;
  const AVectorItmesFactory: IVectorItmesFactory;
  const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
  const APolygon: ILonLatPolygon;
  const Azoomarr: TByteDynArray;
  AMapType: TMapType;
  const ATileNameGen: ITileFileNameGenerator
);
begin
  inherited Create(
    ACancelNotifier,
    AOperationID,
    AProgressInfo,
    APolygon,
    Azoomarr,
    Self.ClassName
  );
  FTargetFile := ATargetFile;
  FProjectionFactory := AProjectionFactory;
  FVectorItmesFactory := AVectorItmesFactory;
  FTileNameGen := ATileNameGen;
  FMapType := AMapType;
  FArchiveReadWriteFactory := AArchiveReadWriteFactory;
end;

destructor TThreadExportToZip.Destroy;
begin
  inherited Destroy;
end;

procedure TThreadExportToZip.ProcessRegion;
var
  i: integer;
  VZoom: Byte;
  VExt: string;
  VPath: string;
  VTile: TPoint;
  VTileIterators: array of ITileIterator;
  VTileIterator: ITileIterator;
  VTileStorage: TTileStorageAbstract;
  VTileInfo: ITileInfoBasic;
  VProjectedPolygon: IProjectedPolygon;
  VTilesToProcess: Int64;
  VTilesProcessed: Int64;
  VData: IBinaryData;
  VZip: IArchiveWriter;
begin
  inherited;
  VTilesToProcess := 0;
  SetLength(VTileIterators, Length(FZooms));
  for i := 0 to Length(FZooms) - 1 do begin
    VZoom := FZooms[i];
    VProjectedPolygon :=
      FVectorItmesFactory.CreateProjectedPolygonByLonLatPolygon(
        FProjectionFactory.GetByConverterAndZoom(FMapType.GeoConvert, VZoom),
        PolygLL
      );
    VTileIterators[i] := TTileIteratorByPolygon.Create(VProjectedPolygon);
    VTilesToProcess := VTilesToProcess + VTileIterators[i].TilesTotal;
  end;
  try
    ProgressInfo.SetCaption(SAS_STR_ExportTiles);
    ProgressInfo.SetFirstLine(
      SAS_STR_AllSaves + ' ' + inttostr(VTilesToProcess) + ' ' + SAS_STR_Files
    );
    VTileStorage := FMapType.TileStorage;
    VZip := FArchiveReadWriteFactory.CreateZipWriterByName(FTargetFile);

    VTilesProcessed := 0;
    ProgressFormUpdateOnProgress(VTilesProcessed, VTilesToProcess);
    for i := 0 to Length(FZooms) - 1 do begin
      VZoom := FZooms[i];
      VExt := FMapType.StorageConfig.TileFileExt;
      VPath := IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(FTargetFile) + FMapType.GetShortFolderName);
      VTileIterator := VTileIterators[i];
      while VTileIterator.Next(VTile) do begin
        if CancelNotifier.IsOperationCanceled(OperationID) then begin
          exit;
        end;
        VData := VTileStorage.LoadTile(VTile, VZoom, nil, VTileInfo);
        if VData <> nil then begin
          VZip.AddFile(
            VData,
            FTileNameGen.GetTileFileName(VTile, VZoom) + VExt,
            VTileInfo.GetLoadDate
          );
        end;
        inc(VTilesProcessed);
        if VTilesProcessed mod 100 = 0 then begin
          ProgressFormUpdateOnProgress(VTilesProcessed, VTilesToProcess);
        end;
      end;
    end;
  finally
    for i := 0 to Length(FZooms) - 1 do begin
      VTileIterators[i] := nil;
    end;
    VTileIterators := nil;
  end;
  FTileNameGen := nil;
end;

end.
