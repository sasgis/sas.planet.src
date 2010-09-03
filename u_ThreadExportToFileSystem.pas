unit u_ThreadExportToFileSystem;

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
  i_ICoordConverter;

constructor TThreadExportToFileSystem.Create(
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
  for i := 0 to length(Atypemaparr) - 1 do begin
    FMapTypeArr[i] := Atypemaparr[i];
  end;
end;

procedure TThreadExportToFileSystem.ProcessRegion;
var
  p_x, p_y, i, j: integer;
  VZoom: Byte;
  polyg: TPointArray;
  pathto: string;
  VExt: string;
  VPath: string;
  VPixelRect: TRect;
  VTile: TPoint;
  VMapType: TMapType;
  VGeoConvert: ICoordConverter;
begin
    FTilesToProcess := 0;
    for j := 0 to length(FMapTypeArr) - 1 do begin
      for i := 0 to Length(FZooms) - 1 do begin
        VZoom := FZooms[i];
        polyg := FMapTypeArr[j].GeoConvert.LonLatArray2PixelArray(FPolygLL, VZoom);
        FTilesToProcess := FTilesToProcess + GetDwnlNum(VPixelRect, Polyg, true);
      end;
    end;
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
          polyg := VGeoConvert.LonLatArray2PixelArray(FPolygLL, VZoom);
          VExt := VMapType.TileFileExt;
          VPath := IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(FPathExport) + VMapType.GetShortFolderName);
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
              inc(p_y, 256);
            end;
            inc(p_x, 256);
          end;
        end;
    end;
    ProgressFormUpdateOnProgress;
    FTileNameGen := nil;
end;

end.
