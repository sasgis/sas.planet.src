unit u_ThreadExportToFileSystem;

interface

uses
  Windows,
  SysUtils,
  Classes,
  i_TileFileNameGenerator,
  i_VectorItemLonLat,
  u_MapType,
  u_ResStrings,
  t_GeoTypes,
  u_ThreadExportAbstract;

type
  TThreadExportToFileSystem = class(TThreadExportAbstract)
  private
    FMapTypeArr: array of TMapType;
    FTileNameGen: ITileFileNameGenerator;

    FIsMove: boolean;
    FIsReplace: boolean;
    FPathExport: string;
  protected
    procedure ProcessRegion; override;
  public
    constructor Create(
      APath: string;
      APolygon: ILonLatPolygonLine;
      Azoomarr: array of boolean;
      Atypemaparr: array of TMapType;
      Amove: boolean;
      Areplace: boolean;
      ATileNameGen: ITileFileNameGenerator
      );
  end;

implementation

uses
  i_CoordConverter,
  i_TileIterator,
  u_TileIteratorStuped;

constructor TThreadExportToFileSystem.Create(
  APath: string;
  APolygon: ILonLatPolygonLine;
  Azoomarr: array of boolean;
  Atypemaparr: array of TMapType;
  Amove, Areplace: boolean;
  ATileNameGen: ITileFileNameGenerator);
var
  i: integer;
begin
  inherited Create(APolygon, Azoomarr);
  FPathExport := APath;
  FIsMove := AMove;
  FTileNameGen := ATileNameGen;
  FIsReplace := AReplace;
  setlength(FMapTypeArr, length(Atypemaparr));
  for i := 0 to length(Atypemaparr) - 1 do begin
    FMapTypeArr[i] := Atypemaparr[i];
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
begin
  inherited;
  SetLength(VTileIterators, length(FMapTypeArr), Length(FZooms));
  FTilesToProcess := 0;
  for j := 0 to length(FMapTypeArr) - 1 do begin
    for i := 0 to Length(FZooms) - 1 do begin
      VZoom := FZooms[i];
      VGeoConvert := FMapTypeArr[j].GeoConvert;
      VTileIterators[j, i] := TTileIteratorStuped.Create(VZoom, FPolygLL, VGeoConvert);
      FTilesToProcess := FTilesToProcess + VTileIterators[j, i].TilesTotal;
    end;
  end;
  try
    ProgressFormUpdateCaption(
      SAS_STR_ExportTiles,
      SAS_STR_AllSaves + ' ' + inttostr(FTilesToProcess) + ' ' + SAS_STR_Files
      );
    FTilesProcessed := 0;
    ProgressFormUpdateOnProgress;
    for i := 0 to Length(FZooms) - 1 do begin
      VZoom := FZooms[i];
      for j := 0 to length(FMapTypeArr) - 1 do begin
        VMapType := FMapTypeArr[j];
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
          inc(FTilesProcessed);
          if FTilesProcessed mod 100 = 0 then begin
            ProgressFormUpdateOnProgress;
          end;
        end;
      end;
    end;
  finally
    for j := 0 to length(FMapTypeArr) - 1 do begin
      for i := 0 to Length(FZooms) - 1 do begin
        VTileIterators[j, i] := nil;
      end;
    end;
    VTileIterators := nil;
  end;
  ProgressFormUpdateOnProgress;
  FTileNameGen := nil;
end;

end.
