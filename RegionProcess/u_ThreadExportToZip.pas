unit u_ThreadExportToZip;

interface

uses
  Windows,
  SysUtils,
  Classes,
  KAZip,
  GR32,
  i_ITileFileNameGenerator,
  UMapType,
  UResStrings,
  t_GeoTypes,
  u_ThreadExportAbstract;

type
  TThreadExportToZip = class(TThreadExportAbstract)
  private
    FMapType: TMapType;
    FTileNameGen: ITileFileNameGenerator;

    FTargetFile: string;
    FZip: TKaZip;
  protected
    procedure ProcessRegion; override;
  public
    constructor Create(
      ATargetFile: string;
      APolygon: TArrayOfDoublePoint;
      Azoomarr: array of boolean;
      AMapType: TMapType;
      ATileNameGen: ITileFileNameGenerator
    );
    destructor Destroy; override;
  end;

implementation

uses
  Variants,
  i_ITileIterator,
  i_ITileInfoBasic,
  u_TileIteratorStuped,
  u_TileStorageAbstract;

constructor TThreadExportToZip.Create(
  ATargetFile: string;
  APolygon: TArrayOfDoublePoint;
  Azoomarr: array of boolean;
  AMapType: TMapType;
  ATileNameGen: ITileFileNameGenerator);
begin
  inherited Create(APolygon, Azoomarr);
  FTargetFile := ATargetFile;
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
begin
  inherited;
  FTilesToProcess := 0;
  SetLength(VTileIterators, Length(FZooms));
  for i := 0 to Length(FZooms) - 1 do begin
    VZoom := FZooms[i];
    VTileIterators[i] := TTileIteratorStuped.Create(VZoom, FPolygLL, FMapType.GeoConvert);
    FTilesToProcess := FTilesToProcess + VTileIterators[i].TilesTotal;
  end;
  try
    ProgressFormUpdateCaption(
      SAS_STR_ExportTiles,
      SAS_STR_AllSaves + ' ' + inttostr(FTilesToProcess) + ' ' + SAS_STR_Files
    );
    VMemStream := TMemoryStream.Create;
    try
      VTileStorage := FMapType.TileStorage;
      FZip.FileName := FTargetFile;
      FZip.CreateZip(FTargetFile);
      FZip.CompressionType := ctFast;
      FZip.Active := true;

      FTilesProcessed := 0;
      ProgressFormUpdateOnProgress;
      for i := 0 to Length(FZooms) - 1 do begin
        VZoom := FZooms[i];
          VExt := FMapType.TileStorage.TileFileExt;
          VPath := IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(FTargetFile) + FMapType.GetShortFolderName);
          VTileIterator := VTileIterators[i];
          while VTileIterator.Next(VTile) do begin
            if IsCancel then begin
              exit;
            end;
            VMemStream.Position := 0;
            VTileInfo := VTileStorage.GetTileInfo(VTile, VZoom, Unassigned);
            if VTileStorage.LoadTile(VTile, VZoom, Unassigned, VMemStream, VTileInfo) then begin
              VFileTime := VTileInfo.GetLoadDate;
              VMemStream.Position := 0;
              FZip.AddStream(
                FTileNameGen.GetTileFileName(VTile, VZoom)+ VTileStorage.GetTileFileExt,
                faArchive,
                VFileTime,
                VMemStream
              );
            end;
            inc(FTilesProcessed);
            if FTilesProcessed mod 100 = 0 then begin
              ProgressFormUpdateOnProgress;
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
  ProgressFormUpdateOnProgress;
  FTileNameGen := nil;
end;

end.
