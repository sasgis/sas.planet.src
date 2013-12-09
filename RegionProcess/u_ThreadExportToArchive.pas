unit u_ThreadExportToArchive;

interface

uses
  Types,
  SysUtils,
  Classes,
  GR32,
  i_TileFileNameGenerator,
  i_NotifierOperation,
  i_RegionProcessProgressInfo,
  i_CoordConverterFactory,
  i_VectorItemsFactory,
  i_VectorItemLonLat,
  i_ArchiveReadWrite,
  i_TileStorage,
  u_ThreadExportAbstract;

type
  TThreadExportToArchive = class(TThreadExportAbstract)
  private
    FTileStorage: ITileStorage;
    FArchive: IArchiveWriter;
    FTileNameGen: ITileFileNameGenerator;
    FProjectionFactory: IProjectionInfoFactory;
    FVectorGeometryProjectedFactory: IVectorGeometryProjectedFactory;
  protected
    procedure ProcessRegion; override;
  public
    constructor Create(
      const AProgressInfo: IRegionProcessProgressInfoInternal;
      const AArchiveWriter: IArchiveWriter;
      const AProjectionFactory: IProjectionInfoFactory;
      const AVectorGeometryProjectedFactory: IVectorGeometryProjectedFactory;
      const APolygon: IGeometryLonLatMultiPolygon;
      const Azoomarr: TByteDynArray;
      const ATileStorage: ITileStorage;
      const ATileNameGen: ITileFileNameGenerator
    );
  end;

implementation

uses
  i_VectorItemProjected,
  i_TileIterator,
  i_TileInfoBasic,
  u_TileIteratorByPolygon,
  u_ResStrings;

{ TThreadExportToArchive }

constructor TThreadExportToArchive.Create(
  const AProgressInfo: IRegionProcessProgressInfoInternal;
  const AArchiveWriter: IArchiveWriter;
  const AProjectionFactory: IProjectionInfoFactory;
  const AVectorGeometryProjectedFactory: IVectorGeometryProjectedFactory;
  const APolygon: IGeometryLonLatMultiPolygon;
  const Azoomarr: TByteDynArray;
  const ATileStorage: ITileStorage;
  const ATileNameGen: ITileFileNameGenerator
);
begin
  inherited Create(
    AProgressInfo,
    APolygon,
    Azoomarr,
    Self.ClassName
  );
  FProjectionFactory := AProjectionFactory;
  FVectorGeometryProjectedFactory := AVectorGeometryProjectedFactory;
  FTileNameGen := ATileNameGen;
  FTileStorage := ATileStorage;
  FArchive := AArchiveWriter;
end;

procedure TThreadExportToArchive.ProcessRegion;
var
  I: Integer;
  VZoom: Byte;
  VExt: string;
  VTile: TPoint;
  VTileIterators: array of ITileIterator;
  VTileIterator: ITileIterator;
  VTileInfo: ITileInfoWithData;
  VProjectedPolygon: IProjectedPolygon;
  VTilesToProcess: Int64;
  VTilesProcessed: Int64;
begin
  inherited;
  VTilesToProcess := 0;
  SetLength(VTileIterators, Length(FZooms));
  for I := 0 to Length(FZooms) - 1 do begin
    VZoom := FZooms[I];
    VProjectedPolygon :=
      FVectorGeometryProjectedFactory.CreateProjectedPolygonByLonLatPolygon(
        FProjectionFactory.GetByConverterAndZoom(FTileStorage.CoordConverter, VZoom),
        PolygLL
      );
    VTileIterators[I] := TTileIteratorByPolygon.Create(VProjectedPolygon);
    VTilesToProcess := VTilesToProcess + VTileIterators[I].TilesTotal;
  end;
  try
    ProgressInfo.SetCaption(SAS_STR_ExportTiles);
    ProgressInfo.SetFirstLine(
      SAS_STR_AllSaves + ' ' + inttostr(VTilesToProcess) + ' ' + SAS_STR_Files
    );
    VTilesProcessed := 0;
    ProgressFormUpdateOnProgress(VTilesProcessed, VTilesToProcess);
    for I := 0 to Length(FZooms) - 1 do begin
      VZoom := FZooms[I];
      VTileIterator := VTileIterators[I];
      while VTileIterator.Next(VTile) do begin
        if CancelNotifier.IsOperationCanceled(OperationID) then begin
          Exit;
        end;
        if Supports(FTileStorage.GetTileInfo(VTile, VZoom, nil, gtimWithData), ITileInfoWithData, VTileInfo) then begin
          VExt := VTileInfo.ContentType.GetDefaultExt;
          FArchive.AddFile(
            VTileInfo.TileData,
            FTileNameGen.GetTileFileName(VTile, VZoom) + VExt,
            VTileInfo.GetLoadDate
          );
        end;
        Inc(VTilesProcessed);
        if VTilesProcessed mod 100 = 0 then begin
          ProgressFormUpdateOnProgress(VTilesProcessed, VTilesToProcess);
        end;
      end;
    end;
  finally
    for I := 0 to Length(FZooms) - 1 do begin
      VTileIterators[I] := nil;
    end;
    VTileIterators := nil;
  end;
end;

end.
