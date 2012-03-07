unit u_ThreadExportToZip;

interface

uses
  SysUtils,
  Classes,
  KAZip,
  GR32,
  i_CoordConverterFactory,
  i_OperationNotifier,
  i_RegionProcessProgressInfo,
  i_VectorItmesFactory,
  i_VectorItemLonLat,
  i_TileFileNameGenerator,
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

    FTargetFile: string;
    FZip: TKaZip;
  protected
    procedure ProcessRegion; override;
  public
    constructor Create(
      ACancelNotifier: IOperationNotifier;
      AOperationID: Integer;
      AProgressInfo: IRegionProcessProgressInfo;
      ATargetFile: string;
      AProjectionFactory: IProjectionInfoFactory;
      AVectorItmesFactory: IVectorItmesFactory;
      APolygon: ILonLatPolygon;
      Azoomarr: array of boolean;
      AMapType: TMapType;
      ATileNameGen: ITileFileNameGenerator
    );
    destructor Destroy; override;
  end;

implementation

uses
  i_BinaryData,
  i_VectorItemProjected,
  i_TileIterator,
  i_TileInfoBasic,
  u_TileIteratorByPolygon,
  u_TileStorageAbstract;

constructor TThreadExportToZip.Create(
  ACancelNotifier: IOperationNotifier;
  AOperationID: Integer;
  AProgressInfo: IRegionProcessProgressInfo;
  ATargetFile: string;
  AProjectionFactory: IProjectionInfoFactory;
  AVectorItmesFactory: IVectorItmesFactory;
  APolygon: ILonLatPolygon;
  Azoomarr: array of boolean;
  AMapType: TMapType;
  ATileNameGen: ITileFileNameGenerator);
begin
  inherited Create(
    ACancelNotifier,
    AOperationID,
    AProgressInfo,
    APolygon,
    Azoomarr
  );
  FTargetFile := ATargetFile;
  FProjectionFactory := AProjectionFactory;
  FVectorItmesFactory := AVectorItmesFactory;
  FTileNameGen := ATileNameGen;
  FMapType := AMapType;
  FZip := TKaZip.Create(nil);
end;

destructor TThreadExportToZip.Destroy;
begin
  inherited;
  FZip.free;
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
  VMemStream: TMemoryStream;
  VFileTime: TDateTime;
  VTileInfo: ITileInfoBasic;
  VProjectedPolygon: IProjectedPolygon;
  VTilesToProcess: Int64;
  VTilesProcessed: Int64;
  VData: IBinaryData;
begin
  inherited;
  VTilesToProcess := 0;
  SetLength(VTileIterators, Length(FZooms));
  for i := 0 to Length(FZooms) - 1 do begin
    VZoom := FZooms[i];
    VProjectedPolygon :=
      FVectorItmesFactory.CreateProjectedPolygonByLonLatPolygon(
        FProjectionFactory.GetByConverterAndZoom(
          FMapType.GeoConvert,
          VZoom
        ),
        PolygLL
      );
    VTileIterators[i] := TTileIteratorByPolygon.Create(VProjectedPolygon);
    VTilesToProcess := VTilesToProcess + VTileIterators[i].TilesTotal;
  end;
  try
    ProgressInfo.Caption := SAS_STR_ExportTiles;
    ProgressInfo.FirstLine := SAS_STR_AllSaves + ' ' + inttostr(VTilesToProcess) + ' ' + SAS_STR_Files;
    VMemStream := TMemoryStream.Create;
    try
      VTileStorage := FMapType.TileStorage;
      FZip.FileName := FTargetFile;
      FZip.CreateZip(FTargetFile);
      FZip.CompressionType := ctFast;
      FZip.Active := true;

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
              VFileTime := VTileInfo.GetLoadDate;
              VMemStream.SetSize(0);
              VMemStream.WriteBuffer(VData.Buffer^, VData.Size);
              {$WARN SYMBOL_PLATFORM OFF}
              FZip.AddStream(
                FTileNameGen.GetTileFileName(VTile, VZoom)+ VExt,
                faArchive,
                VFileTime,
                VMemStream
              );
              {$WARN SYMBOL_PLATFORM ON}
            end;
            inc(VTilesProcessed);
            if VTilesProcessed mod 100 = 0 then begin
              ProgressFormUpdateOnProgress(VTilesProcessed, VTilesToProcess);
            end;
          end;
      end;
    finally
      VMemStream.Free;
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
