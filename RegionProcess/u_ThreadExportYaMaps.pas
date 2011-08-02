unit u_ThreadExportYaMaps;

interface

uses
  Windows,
  Types,
  SysUtils,
  Classes,
  GR32,
  u_MapType,
  u_ResStrings,
  u_YaMobileWrite,
  t_GeoTypes,
  u_ThreadExportAbstract;

type
  TThreadExportYaMaps = class(TThreadExportAbstract)
  private
    FMapTypeArr: array of TMapType;
    FIsReplace: boolean;
    FExportPath: string;
    csat, cmap: byte;
  protected
    procedure ProcessRegion; override;
  public
    constructor Create(
      APath: string;
      APolygon: TArrayOfDoublePoint;
      Azoomarr: array of boolean;
      Atypemaparr: array of TMapType;
      Areplace: boolean;
      Acsat: byte;
      Acmap: byte
    );
  end;

implementation

uses
  c_CoordConverter,
  i_CoordConverter,
  i_TileIterator,
  u_TileIteratorStuped,
  i_BitmapTileSaveLoad,
  u_BitmapTileVampyreSaver,
  u_GlobalState;

constructor TThreadExportYaMaps.Create(
  APath: string;
  APolygon: TArrayOfDoublePoint;
  Azoomarr: array of boolean;
  Atypemaparr: array of TMapType;
  Areplace: boolean;
  Acsat, Acmap: byte
);
var
  i: integer;
begin
  inherited Create(APolygon, Azoomarr);
  cSat := Acsat;
  cMap := Acmap;
  FExportPath := APath;
  FIsReplace := AReplace;
  setlength(FMapTypeArr, length(Atypemaparr));
  for i := 1 to length(Atypemaparr) do begin
    FMapTypeArr[i - 1] := Atypemaparr[i - 1];
  end;
end;

procedure TThreadExportYaMaps.ProcessRegion;
var
  i, j, xi, yi, hxyi, sizeim: integer;
  VZoom: Byte;
  bmp32, bmp322, bmp32crop: TCustomBitmap32;
  TileStream: TMemoryStream;
  tc: cardinal;
  VGeoConvert: ICoordConverter;
  JPGSaver, PNGSaver: IBitmapTileSaver;
  VTile: TPoint;
  VMapType: TMapType;
  VSaver: IBitmapTileSaver;
  Vmt: Byte;
  VTileIterators: array of ITileIterator;
begin
  inherited;
  if (FMapTypeArr[0] = nil) and (FMapTypeArr[1] = nil) and (FMapTypeArr[2] = nil) then begin
    exit;
  end;
  bmp32 := TCustomBitmap32.Create;
  bmp322 := TCustomBitmap32.Create;
  bmp32crop := TCustomBitmap32.Create;
  try
    hxyi := 1;
    sizeim := 128;
    JPGSaver := TVampyreBasicBitmapTileSaverJPG.create(cSat);
    PNGSaver := TVampyreBasicBitmapTileSaverPNGPalette.create(cMap);
    TileStream := TMemoryStream.Create;
    try
      bmp32.DrawMode := dmBlend;
      bmp322.DrawMode := dmBlend;
      bmp32crop.Width := sizeim;
      bmp32crop.Height := sizeim;
      VGeoConvert := GState.CoordConverterFactory.GetCoordConverterByCode(CYandexProjectionEPSG, CTileSplitQuadrate256x256);
      FTilesToProcess := 0;
      SetLength(VTileIterators,Length(FZooms));

      for i := 0 to Length(FZooms) - 1 do begin
        VZoom := FZooms[i];
        VTileIterators[i] := TTileIteratorStuped.Create(VZoom, FPolygLL, VGeoConvert);
        for j := 0 to 2 do begin
          if (FMapTypeArr[j] <> nil) and (not ((j = 0) and (FMapTypeArr[2] <> nil))) then begin
            FTilesToProcess := FTilesToProcess + VTileIterators[i].TilesTotal;
          end;
        end;
      end;
      try
        FTilesProcessed := 0;

        ProgressFormUpdateCaption(SAS_STR_ExportTiles, SAS_STR_AllSaves + ' ' + inttostr(FTilesToProcess) + ' ' + SAS_STR_files);
        ProgressFormUpdateOnProgress;

        tc := GetTickCount;
        for i := 0 to Length(FZooms) - 1 do begin
          VZoom := FZooms[i];
          while VTileIterators[i].Next(VTile) do begin
            if IsCancel then begin
              exit;
            end;
            for j := 0 to 2 do begin
              VMapType := FMapTypeArr[j];
              if (VMapType <> nil) and (not ((j = 0) and (FMapTypeArr[2] <> nil))) then begin
                bmp322.Clear;
                if (j = 2) and (FMapTypeArr[0] <> nil) then begin
                  FMapTypeArr[0].LoadTileUni(bmp322, VTile, VZoom, VGeoConvert, False, False, True);
                end;
                bmp32.Clear;
                if VMapType.LoadTileUni(bmp32, VTile, VZoom, VGeoConvert, False, False, True) then begin
                  if (j = 2) and (FMapTypeArr[0] <> nil) then begin
                    bmp322.Draw(0, 0, bmp32);
                    bmp32.Draw(0, 0, bmp322);
                  end;
                  if (j = 2) or (j = 0) then begin
                    VSaver := JPGSaver;
                    Vmt := 2;
                  end else begin
                    VSaver := PNGSaver;
                    Vmt := 1;
                  end;
                  for xi := 0 to hxyi do begin
                    for yi := 0 to hxyi do begin
                      bmp32crop.Clear;
                      bmp32crop.Draw(0, 0, bounds(sizeim * xi, sizeim * yi, sizeim, sizeim), bmp32);
                      TileStream.Clear;
                      VSaver.SaveToStream(bmp32crop, TileStream);
                      WriteTileToYaCache(VTile, VZoom, Vmt, (yi * 2) + xi, FExportPath, TileStream, FIsReplace);
                    end;
                  end;
                end;
                inc(FTilesProcessed);
                if (GetTickCount - tc > 1000) then begin
                  tc := GetTickCount;
                  ProgressFormUpdateOnProgress;
                end;
              end;
            end;
          end;
        end;
      finally
        for i := 0 to Length(FZooms)-1 do begin
          VTileIterators[i] := nil;
        end;
      end;
      ProgressFormUpdateOnProgress
    finally
      TileStream.Free;
    end;
  finally
    bmp32.Free;
    bmp322.Free;
    bmp32crop.Free;
  end;
end;

end.
