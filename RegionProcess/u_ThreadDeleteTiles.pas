unit u_ThreadDeleteTiles;

interface

uses
  Windows,
  SysUtils,
  Classes,
  i_OperationNotifier,
  i_RegionProcessProgressInfo,
  i_VectorItemLonLat,
  i_VectorItemProjected,
  u_MapType,
  u_ThreadRegionProcessAbstract,
  u_ResStrings;

type
  TThreadDeleteTiles = class(TThreadRegionProcessAbstract)
  private
    FZoom: byte;
    FMapType: TMapType;
    FPolyProjected: IProjectedPolygon;
    DelBytes: boolean;
    DelBytesNum: integer;
  protected
    procedure ProcessRegion; override;
    procedure ProgressFormUpdateOnProgress(AProcessed, AToProcess, ADeleted: Int64);
  public
    constructor Create(
      ACancelNotifier: IOperationNotifier;
      AOperationID: Integer;
      AProgressInfo: IRegionProcessProgressInfo;
      APolyLL: ILonLatPolygon;
      AProjectedPolygon: IProjectedPolygon;
      Azoom: byte;
      Atypemap: TMapType;
      ADelByte: boolean;
      ADelBytesNum: integer
    );
  end;

implementation

uses
  i_TileIterator,
  u_TileIteratorByPolygon;

constructor TThreadDeleteTiles.Create(
  ACancelNotifier: IOperationNotifier;
  AOperationID: Integer;
  AProgressInfo: IRegionProcessProgressInfo;
  APolyLL: ILonLatPolygon;
  AProjectedPolygon: IProjectedPolygon;
  Azoom: byte;
  Atypemap: TMapType;
  ADelByte: boolean;
  ADelBytesNum: integer
);
begin
  inherited Create(
    ACancelNotifier,
    AOperationID,
    AProgressInfo,
    APolyLL
  );
  FPolyProjected := AProjectedPolygon;
  FZoom := Azoom;
  FMapType := Atypemap;
  DelBytes := ADelByte;
  DelBytesNum := ADelBytesNum;
end;

procedure TThreadDeleteTiles.ProcessRegion;
var
  VTile: TPoint;
  VTileIterator: ITileIterator;
  VDeletedCount: integer;
  VTilesToProcess: Int64;
  VTilesProcessed: Int64;
begin
  inherited;
  VTileIterator := TTileIteratorByPolygon.Create(FPolyProjected);
  try
    VTilesToProcess := VTileIterator.TilesTotal;
    ProgressInfo.Caption :=
      SAS_STR_Deleted + ' ' + inttostr(VTilesToProcess) + ' ' + SAS_STR_files + ' (x' + inttostr(FZoom + 1) + ')';
    VTilesProcessed := 0;
    VDeletedCount := 0;
    ProgressFormUpdateOnProgress(VTilesProcessed, VTilesToProcess, VDeletedCount);
    while VTileIterator.Next(VTile) do begin
      if CancelNotifier.IsOperationCanceled(OperationID) then begin
        exit;
      end;
      if (not DelBytes or (DelBytesNum = FMapType.TileSize(VTile, FZoom))) then begin
        if FMapType.DeleteTile(VTile, FZoom) then begin
          inc(VDeletedCount);
        end;
        ProgressFormUpdateOnProgress(VTilesProcessed, VTilesToProcess, VDeletedCount);
      end;
      inc(VTilesProcessed);
    end;
  finally
    VTileIterator := nil;
  end;
end;

procedure TThreadDeleteTiles.ProgressFormUpdateOnProgress(AProcessed, AToProcess, ADeleted: Int64);
begin
  ProgressInfo.Processed := AProcessed/AToProcess;
  ProgressInfo.SecondLine := SAS_STR_Processed + ' ' + inttostr(AProcessed);
  ProgressInfo.FirstLine := SAS_STR_AllDelete + ' ' + inttostr(ADeleted) + ' ' + SAS_STR_files;
end;

end.
