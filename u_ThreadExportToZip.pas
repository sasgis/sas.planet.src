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
    FMapTypeArr: array of TMapType;
    FTileNameGen: ITileFileNameGenerator;

    FPathExport: string;
    FZip: TVCLZip;
  protected
    procedure ProcessRegion; override;
    procedure Terminate; override;
  public
    constructor Create(
      APath: string;
      APolygon: TExtendedPointArray;
      Azoomarr: array of boolean;
      Atypemaparr: array of TMapType;
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
  APath: string;
  APolygon: TExtendedPointArray;
  Azoomarr: array of boolean;
  Atypemaparr: array of TMapType;
  ATileNameGen: ITileFileNameGenerator);
var
  i: integer;
begin
  inherited Create(APolygon, Azoomarr);
  FPathExport := APath;
  FTileNameGen := ATileNameGen;
  setlength(FMapTypeArr, length(Atypemaparr));
  for i := 0 to length(Atypemaparr) - 1 do begin
    FMapTypeArr[i] := Atypemaparr[i];
  end;
  FZip := TVCLZip.Create(nil);
end;

function RetDate(inDate: TDateTime): string;
var
  xYear, xMonth, xDay: word;
begin
  DecodeDate(inDate, xYear, xMonth, xDay);
  Result := inttostr(xDay) + '.' + inttostr(xMonth) + '.' + inttostr(xYear);
end;

destructor TThreadExportToZip.Destroy;
begin
  inherited;
  FZip.free;
end;

procedure TThreadExportToZip.ProcessRegion;
var
  i, j: integer;
  VZoom: Byte;
  pathfrom, persl, perzoom, datestr: string;
  VExt: string;
  VPath: string;
  VTile: TPoint;
  VMapType: TMapType;
  VTileIterators: array of array of TTileIteratorAbstract;
  VTileIterator: TTileIteratorAbstract;
begin
  inherited;
    FTilesToProcess := 0;
    persl := '';
    datestr := RetDate(now);
    SetLength(VTileIterators, length(FMapTypeArr), Length(FZooms));
    for j := 0 to length(FMapTypeArr) - 1 do begin
      persl := persl + FMapTypeArr[j].GetShortFolderName + '_';
      perzoom := '';
      for i := 0 to Length(FZooms) - 1 do begin
        VZoom := FZooms[i];
        VTileIterators[j, i] := TTileIteratorStuped.Create(VZoom, FPolygLL, FMapTypeArr[j].GeoConvert);
        FTilesToProcess := FTilesToProcess + VTileIterators[j, i].TilesTotal;
          perzoom := perzoom + inttostr(VZoom + 1) + '_';
      end;
    end;
  try
    persl := copy(persl, 1, length(persl) - 1);
    perzoom := copy(perzoom, 1, length(perzoom) - 1);
    ProgressFormUpdateCaption(
      SAS_STR_ExportTiles + ' ' + SAS_STR_CreateArhList,
      SAS_STR_AllSaves + ' ' + inttostr(FTilesToProcess) + ' ' + SAS_STR_Files
    );
    FZip.Recurse := False;
    FZip.StorePaths := true;
    FZip.PackLevel := 0; // Уровень сжатия
    FZip.ZipName := FPathExport + 'SG-' + persl + '-' + perzoom + '-' + datestr + '.ZIP';
    FTilesProcessed := 0;
    ProgressFormUpdateOnProgress;
    for i := 0 to Length(FZooms) - 1 do begin
      VZoom := FZooms[i];
        for j := 0 to length(FMapTypeArr) - 1 do  begin
          VMapType := FMapTypeArr[j];
          VExt := VMapType.TileFileExt;
          VPath := IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(FPathExport) + VMapType.GetShortFolderName);
          VTileIterator := VTileIterators[j, i];
          while VTileIterator.Next do begin
            if IsCancel then begin
              exit;
            end;
            VTile := VTileIterator.Current;
            if VMapType.TileExists(VTile, VZoom) then begin
//TODO: Разобраться и избавиться от путей. Нужно предусмотреть вариант, что тайлы хранятся не в файлах, а перед зипованием сохраняются в файлы.
              pathfrom := VMapType.GetTileFileName(VTile, VZoom);
              FZip.FilesList.Add(pathfrom);
            end;
            inc(FTilesProcessed);
            if FTilesProcessed mod 100 = 0 then begin
              ProgressFormUpdateOnProgress;
            end;
          end;
        end;
    end;
    ProgressFormUpdateCaption(
      SAS_STR_Pack + ' ' + 'SG-' + persl + '-' + perzoom + '-' + datestr + '.ZIP',
      SAS_STR_AllSaves + ' ' + inttostr(FTilesToProcess) + ' ' + SAS_STR_Files
    );
    if FileExists(FZip.ZipName) then begin
      DeleteFile(FZip.ZipName);
    end;
    If FZip.Zip = 0 then begin
      Application.MessageBox(PChar(SAS_ERR_CreateArh), PChar(SAS_MSG_coution), 48);
    end;
  finally
    for j := 0 to length(FMapTypeArr) - 1 do begin
      for i := 0 to Length(FZooms) - 1 do begin
        VTileIterators[j, i].Free;
      end;
    end;
    VTileIterators := nil;
  end;
  ProgressFormUpdateOnProgress;
  FTileNameGen := nil;
end;

end.
