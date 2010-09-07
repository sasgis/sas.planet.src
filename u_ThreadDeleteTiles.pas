unit u_ThreadDeleteTiles;

interface

uses
  Windows,
  SysUtils,
  Classes,
  t_GeoTypes,
  UMapType,
  u_ThreadRegionProcessAbstract,
  UResStrings;

type
  TThreadDeleteTiles = class(TThreadRegionProcessAbstract)
  private
    FZoom: byte;
    FMapType: TMapType;
    FDeletedCount: integer;
    DelBytes: boolean;
    DelBytesNum: integer;
  protected
    procedure ProcessRegion; override;
    procedure ProgressFormUpdateOnProgress;
  public
    constructor Create(
      APolyLL: TExtendedPointArray;
      Azoom: byte;
      Atypemap: TMapType;
      ADelByte: boolean;
      ADelBytesNum: integer
    );
  end;

implementation

uses
  u_TileIteratorAbstract,
  u_TileIteratorStuped;

constructor TThreadDeleteTiles.Create(
  APolyLL: TExtendedPointArray;
  Azoom: byte;
  Atypemap: TMapType;
  ADelByte: boolean;
  ADelBytesNum: integer
);
begin
  inherited Create(APolyLL);
  FDeletedCount := 0;
  FZoom := Azoom;
  FMapType := Atypemap;
  DelBytes := ADelByte;
  DelBytesNum := ADelBytesNum;
end;

procedure TThreadDeleteTiles.ProcessRegion;
var
  VTile: TPoint;
  VTileIterator: TTileIteratorAbstract;
begin
  inherited;
  VTileIterator := TTileIteratorStuped.Create(FZoom, FPolygLL, FMapType.GeoConvert);
  try
    FTilesToProcess := VTileIterator.TilesTotal;
    ProgressFormUpdateCaption(
      '',
      SAS_STR_Deleted + ' ' + inttostr(FTilesToProcess) + ' ' + SAS_STR_files + ' (x' + inttostr(FZoom) + ')'
    );
    while VTileIterator.Next do begin
      if IsCancel then begin
        exit;
      end;
      VTile := VTileIterator.Current;
      if (not DelBytes or (DelBytesNum = FMapType.TileSize(VTile, FZoom - 1))) then begin
        if FMapType.DeleteTile(VTile, FZoom - 1) then begin
          inc(FDeletedCount);
        end;
        ProgressFormUpdateOnProgress;
      end;
      inc(FTilesProcessed);
    end;
  finally
    FreeAndNil(VTileIterator);
  end;
end;

procedure TThreadDeleteTiles.ProgressFormUpdateOnProgress;
begin
  ProgressFormUpdateProgressLine0AndLine1(
    round((FTilesProcessed / FTilesToProcess) * 100),
    SAS_STR_AllDelete + ' ' + inttostr(FDeletedCount) + ' ' + SAS_STR_files,
    SAS_STR_Processed + ' ' + inttostr(FTilesProcessed)
  );
end;

end.
