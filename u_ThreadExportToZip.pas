unit u_ThreadExportToZip;

interface

uses
  Windows,
  Forms,
  SysUtils,
  Classes,
  VCLZIp,
  GR32,
  i_ITileFileNameGenerator,
  UMapType,
  UGeoFun,
  UResStrings,
  t_GeoTypes,
  u_ThreadExportAbstract;

type
  TThreadExportToZip = class(TThreadExportAbstract)
  private
    FMapType: TMapType;
    FTileNameGen: ITileFileNameGenerator;

    FTargetFile: string;
    FZip: TVCLZip;
  protected
    procedure ProcessRegion; override;
    procedure Terminate; override;
  public
    constructor Create(
      ATargetFile: string;
      APolygon: TExtendedPointArray;
      Azoomarr: array of boolean;
      AMapType: TMapType;
      ATileNameGen: ITileFileNameGenerator
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_TileIteratorAbstract,
  u_TileIteratorStuped,
  u_GeoToStr;

procedure TThreadExportToZip.Terminate;
begin
  inherited;
  FZip.CancelTheOperation;
end;

constructor TThreadExportToZip.Create(
  ATargetFile: string;
  APolygon: TExtendedPointArray;
  Azoomarr: array of boolean;
  AMapType: TMapType;
  ATileNameGen: ITileFileNameGenerator);
begin
  inherited Create(APolygon, Azoomarr);
  FTargetFile := ATargetFile;
  FTileNameGen := ATileNameGen;
  FMapType := AMapType;
  FZip := TVCLZip.Create(nil);
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
  pathfrom: string;
  VExt: string;
  VPath: string;
  VTile: TPoint;
  VTileIterators: array of TTileIteratorAbstract;
  VTileIterator: TTileIteratorAbstract;
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
      SAS_STR_ExportTiles + ' ' + SAS_STR_CreateArhList,
      SAS_STR_AllSaves + ' ' + inttostr(FTilesToProcess) + ' ' + SAS_STR_Files
    );
    FZip.Recurse := False;
    FZip.StorePaths := true;
    FZip.PackLevel := 0; // Уровень сжатия
    FZip.ZipName := FTargetFile;
    FTilesProcessed := 0;
    ProgressFormUpdateOnProgress;
    for i := 0 to Length(FZooms) - 1 do begin
      VZoom := FZooms[i];
        VExt := FMapType.TileStorage.TileFileExt;
        VPath := IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(FTargetFile) + FMapType.GetShortFolderName);
        VTileIterator := VTileIterators[i];
        while VTileIterator.Next do begin
          if IsCancel then begin
            exit;
          end;
          VTile := VTileIterator.Current;
          if FMapType.TileExists(VTile, VZoom) then begin
//TODO: Разобраться и избавиться от путей. Нужно предусмотреть вариант, что тайлы хранятся не в файлах, а перед зипованием сохраняются в файлы.
            pathfrom := FMapType.GetTileFileName(VTile, VZoom);
            FZip.FilesList.Add(pathfrom);
          end;
          inc(FTilesProcessed);
          if FTilesProcessed mod 100 = 0 then begin
            ProgressFormUpdateOnProgress;
          end;
        end;
    end;
    ProgressFormUpdateCaption(
      SAS_STR_Pack + ' ' + FTargetFile,
      SAS_STR_AllSaves + ' ' + inttostr(FTilesToProcess) + ' ' + SAS_STR_Files
    );
    if FileExists(FZip.ZipName) then begin
      DeleteFile(FZip.ZipName);
    end;
    If FZip.Zip = 0 then begin
      Application.MessageBox(PChar(SAS_ERR_CreateArh), PChar(SAS_MSG_coution), 48);
    end;
  finally
    for i := 0 to Length(FZooms) - 1 do begin
      VTileIterators[i].Free;
    end;
    VTileIterators := nil;
  end;
  ProgressFormUpdateOnProgress;
  FTileNameGen := nil;
end;

end.
