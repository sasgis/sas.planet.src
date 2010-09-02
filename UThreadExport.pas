unit UThreadExport;

interface

uses
  Windows,
  SysUtils,
  Classes,
  GR32,
  i_ITileFileNameGenerator,
  UMapType,
  UGeoFun,
  UResStrings,
  t_GeoTypes,
  u_ThreadExportAbstract;

type
  TThreadExport = class(TThreadExportAbstract)
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
      APolygon: TExtendedPointArray;
      Azoomarr: array of boolean;
      Atypemaparr: array of TMapType;
      Amove: boolean;
      Areplace: boolean;
      ATileNameGen: ITileFileNameGenerator
      );
  end;

implementation

uses
  i_ICoordConverter,
  u_GeoToStr;

constructor TThreadExport.Create(
  APath: string;
  APolygon: TExtendedPointArray;
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
  for i := 1 to length(Atypemaparr) do begin
    FMapTypeArr[i - 1] := Atypemaparr[i - 1];
  end;
end;

procedure TThreadExport.ProcessRegion;
var
  p_x, p_y, i, j: integer;
  polyg: TPointArray;
  pathto: string;
  VExt: string;
  VPath: string;
  VPixelRect: TRect;
  VTile: TPoint;
  VGeoConvert: ICoordConverter;
begin
    FTilesToProcess := 0;
    SetLength(polyg, length(FPolygLL));
    for i := 0 to length(FMapTypeArr) - 1 do begin
      for j := 0 to 23 do begin
        if FZoomArr[j] then begin
          polyg := FMapTypeArr[i].GeoConvert.LonLatArray2PixelArray(FPolygLL, j);
          FTilesToProcess := FTilesToProcess + GetDwnlNum(VPixelRect, Polyg, true);
        end;
      end;
    end;
    ProgressFormUpdateCaption(
      SAS_STR_ExportTiles,
      SAS_STR_AllSaves + ' ' + inttostr(FTilesToProcess) + ' ' + SAS_STR_Files
    );
    FTilesProcessed := 0;
    ProgressFormUpdateOnProgress;
    for i := 0 to 23 do //по масштабу
    begin
      if FZoomArr[i] then begin
        for j := 0 to length(FMapTypeArr) - 1 do //по типу
        begin
          VGeoConvert := FMapTypeArr[j].GeoConvert;
          polyg := VGeoConvert.LonLatArray2PixelArray(FPolygLL, i);
          VExt := FMapTypeArr[j].TileFileExt;
          VPath := IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(FPathExport) + FMapTypeArr[j].GetShortFolderName);
          GetDwnlNum(VPixelRect, Polyg, false);
          p_x := VPixelRect.Left;
          while p_x < VPixelRect.Right do begin
            VTile.X := p_x shr 8;
            p_y := VPixelRect.Top;
            while p_y < VPixelRect.Bottom do begin
              VTile.Y := p_y shr 8;
              if IsCancel then begin
                exit;
              end;
              if (RgnAndRgn(Polyg, p_x, p_y, false)) then begin
                if FMapTypeArr[j].TileExists(VTile, i) then begin
                  pathto := VPath + FTileNameGen.GetTileFileName(VTile, i) + VExt;
                  if FMapTypeArr[j].TileExportToFile(VTile, i, pathto, FIsReplace) then begin
                    if FIsMove then begin
                      FMapTypeArr[j].DeleteTile(VTile, i);
                    end;
                  end;
                end;
                inc(FTilesProcessed);
                if FTilesProcessed mod 100 = 0 then begin
                  ProgressFormUpdateOnProgress;
                end;
              end;
              inc(p_y, 256);
            end;
            inc(p_x, 256);
          end;
        end;
      end;
    end;
    ProgressFormUpdateOnProgress;
    FTileNameGen := nil;
end;

end.
