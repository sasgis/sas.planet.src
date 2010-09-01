unit UThreadExport;

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
  u_ExportThreadAbstract;

type
  TThreadExport = class(TExportThreadAbstract)
  private
    FMapTypeArr: array of TMapType;
    FTileNameGen: ITileFileNameGenerator;

    FIsMove: boolean;
    FIsZiped: boolean;
    FIsReplace: boolean;
    FPathExport: string;
    FZip: TVCLZip;
    Zippu: boolean;

    procedure CloseFProgress(Sender: TObject; var Action: TCloseAction);
  protected
    procedure ExportRegion; override;
  public
    constructor Create(
      APath: string;
      APolygon: TExtendedPointArray;
      Azoomarr: array of boolean;
      Atypemaparr: array of TMapType;
      Amove: boolean;
      Areplace: boolean;
      Aziped: boolean;
      ATileNameGen: ITileFileNameGenerator
      );
  end;

implementation

uses
  u_GeoToStr,
  unit1;

procedure TThreadExport.CloseFProgress(Sender: TObject; var Action: TCloseAction);
begin
  if Zippu then begin
    FZip.CancelTheOperation;
  end;
end;

constructor TThreadExport.Create(
  APath: string;
  APolygon: TExtendedPointArray;
  Azoomarr: array of boolean;
  Atypemaparr: array of TMapType;
  Amove, Areplace, Aziped: boolean;
  ATileNameGen: ITileFileNameGenerator);
var
  i: integer;
begin
  inherited Create(APolygon, Azoomarr);
  Zippu := false;
  FProgressForm.OnClose := CloseFProgress;
  FPathExport := APath;
  FIsMove := AMove;
  FTileNameGen := ATileNameGen;
  FIsZiped := Aziped;
  FIsReplace := AReplace;
  setlength(FMapTypeArr, length(Atypemaparr));
  for i := 1 to length(Atypemaparr) do begin
    FMapTypeArr[i - 1] := Atypemaparr[i - 1];
  end;
end;

function RetDate(inDate: TDateTime): string;
var
  xYear, xMonth, xDay: word;
begin
  DecodeDate(inDate, xYear, xMonth, xDay);
  Result := inttostr(xDay) + '.' + inttostr(xMonth) + '.' + inttostr(xYear);
end;

procedure TThreadExport.ExportRegion;
var
  p_x, p_y, i, j: integer;
  polyg: TPointArray;
  pathfrom, pathto, persl, perzoom, kti, datestr: string;
  max, min: TPoint;
  VExt: string;
  VPath: string;
  VMinLonLat, VMaxLonLat: TExtendedPoint;
  VTile: TPoint;
begin
    FTilesToProcess := 0;
    SetLength(polyg, length(FPolygLL));
    persl := '';
    kti := '';
    datestr := RetDate(now);
    for i := 0 to length(FMapTypeArr) - 1 do begin
      persl := persl + FMapTypeArr[i].GetShortFolderName + '_';
      perzoom := '';
      for j := 0 to 23 do begin
        if FZoomArr[j] then begin
          polyg := FMapTypeArr[i].GeoConvert.LonLatArray2PixelArray(FPolygLL, j);
          FTilesToProcess := FTilesToProcess + GetDwnlNum(min, max, Polyg, true);
          perzoom := perzoom + inttostr(j + 1) + '_';
          VMinLonLat := FMapTypeArr[i].GeoConvert.PixelPos2LonLat(min, j);
          VMaxLonLat := FMapTypeArr[i].GeoConvert.PixelPos2LonLat(min, j);
          kti := RoundEx(VMinLonLat.x, 4);
          kti := kti + '_' + RoundEx(VMinLonLat.y, 4);
          kti := kti + '_' + RoundEx(VMaxLonLat.x, 4);
          kti := kti + '_' + RoundEx(VMaxLonLat.y, 4);
        end;
      end;
    end;
    persl := copy(persl, 1, length(persl) - 1);
    perzoom := copy(perzoom, 1, length(perzoom) - 1);
    if FIsZiped then begin
      FShowOnFormLine0 := SAS_STR_ExportTiles + ' ' + SAS_STR_CreateArhList;
      Synchronize(UpdateProgressFormStr0);
      FZip := TVCLZip.Create(Fmain);
      Zippu := true;
      FZip.Recurse := False;
      FZip.StorePaths := true; // Путь не сохраняем
      FZip.PackLevel := 0; // Уровень сжатия
      FZip.ZipName := FPathExport + 'SG-' + persl + '-' + perzoom + '-' + kti + '-' + datestr + '.ZIP';
    end else begin
      FShowOnFormLine0 := SAS_STR_ExportTiles;
      Synchronize(UpdateProgressFormStr0);
    end;
    FShowFormCaption := SAS_STR_AllSaves + ' ' + inttostr(FTilesToProcess) + ' ' + SAS_STR_Files;
    Synchronize(UpdateProgressFormCaption);

    FShowOnFormLine1 := SAS_STR_Processed + ' 0%';
    Synchronize(UpdateProgressFormStr1);
    FTilesProcessed := 0;
    for i := 0 to 23 do //по масштабу
    begin
      if FZoomArr[i] then begin
        for j := 0 to length(FMapTypeArr) - 1 do //по типу
        begin
          polyg := FMapTypeArr[j].GeoConvert.LonLatArray2PixelArray(FPolygLL, i);
          VExt := FMapTypeArr[j].TileFileExt;
          VPath := IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(FPathExport) + FMapTypeArr[j].GetShortFolderName);
          GetDwnlNum(min, max, Polyg, false);
          p_x := min.x;
          while p_x < max.x do begin
            VTile.X := p_x shr 8;
            p_y := min.Y;
            while p_y < max.Y do begin
              VTile.Y := p_y shr 8;
              if not FProgressForm.Visible then begin
                exit;
              end;
              if not (RgnAndRgn(Polyg, p_x, p_y, false)) then begin
                inc(p_y, 256);
                CONTINUE;
              end;
              if FMapTypeArr[j].TileExists(VTile, i) then begin
                if FIsZiped then begin
//TODO: Разобраться и избавиться от путей. Нужно предусмотреть вариант, что тайлы хранятся не в файлах, а перед зипованием сохраняются в файлы.
                  pathfrom := FMapTypeArr[j].GetTileFileName(VTile, i);
                  FZip.FilesList.Add(pathfrom);
                end else begin
                  pathto := VPath + FTileNameGen.GetTileFileName(VTile, i) + VExt;
                  if FMapTypeArr[j].TileExportToFile(VTile, i, pathto, FIsReplace) then begin
                    if FIsMove then begin
                      FMapTypeArr[j].DeleteTile(VTile, i);
                    end;
                  end;
                end;
              end;
              inc(FTilesProcessed);
              if FTilesProcessed mod 100 = 0 then begin
                FProgressOnForm := round((FTilesProcessed / FTilesToProcess) * 100);
                Synchronize(UpdateProgressFormBar);
                FShowOnFormLine1 := SAS_STR_Processed + ' ' + inttostr(FProgressOnForm) + '%';
                Synchronize(UpdateProgressFormStr1);
              end;
              inc(p_y, 256);
            end;
            inc(p_x, 256);
          end;
        end;
      end;
    end;
    if FIsZiped then begin
      FShowOnFormLine0 := SAS_STR_Pack + ' ' + 'SG-' + persl + '-' + perzoom + '-' + kti + '-' + datestr + '.ZIP';
      Synchronize(UpdateProgressFormStr0);
      if FileExists(FZip.ZipName) then begin
        DeleteFile(FZip.ZipName);
      end;
      If FZip.Zip = 0 then begin
        Application.MessageBox(PChar(SAS_ERR_CreateArh), PChar(SAS_MSG_coution), 48);
      end;
      FZip.free;
      Zippu := false;
    end;
    FProgressOnForm := round((FTilesProcessed / FTilesToProcess) * 100);
    Synchronize(UpdateProgressFormBar);
    FShowOnFormLine1 := SAS_STR_Processed + ' ' + inttostr(FProgressOnForm) + '%';
    Synchronize(UpdateProgressFormStr1);
    FTileNameGen := nil;
end;

end.
