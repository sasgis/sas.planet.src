unit UThreadExportIPhone;

interface

uses
  Windows,
  Types,
  SysUtils,
  Classes,
  DISQLite3Database,
  DISQLite3Api,
  GR32,
  i_ICoordConverter,
  UMapType,
  UGeoFun,
  t_GeoTypes,
  u_ThreadExportAbstract;

type
  TThreadExportIPhone = class(TThreadExportAbstract)
  private
    FMapTypeArr: array of TMapType;
    FActiveMapIndex: integer;
    FNewFormat: Boolean;

    FIsReplace: boolean;
    FExportPath: string;
    FSQLite3Db: TDISQLite3Database;
    csat, cmap, chib: byte;

    procedure WritePListFile(AGeoConvert: ICoordConverter);
    function Write_Stream_to_Blob_Traditional(const AStream: TStream; Azoom, Ax, Ay, Aflags: integer): Int64;
  protected
    procedure ProcessRegion; override;
  public
    constructor Create(
      APath: string;
      APolygon: TExtendedPointArray;
      Azoomarr: array of boolean;
      Atypemaparr: array of TMapType;
      AActiveMapIndex: Integer;
      Areplace: boolean;
      ANewFormat: Boolean;
      Acsat: byte;
      Acmap: byte;
      Achib: byte
      );
  end;

implementation

uses
  u_GeoToStr,
  UResStrings,
  i_BitmapTileSaveLoad,
  u_BitmapTileVampyreSaver,
  u_BitmapTileJpegSaverIJL,
  u_CoordConverterMercatorOnSphere;

constructor TThreadExportIPhone.Create(
  APath: string;
  APolygon: TExtendedPointArray;
  Azoomarr: array of boolean;
  Atypemaparr: array of TMapType;
  AActiveMapIndex: Integer;
  Areplace: boolean;
  ANewFormat: Boolean;
  Acsat, Acmap, Achib: byte);
var
  i: integer;
begin
  inherited Create(APolygon, Azoomarr);
  cSat := Acsat;
  cMap := Acmap;
  cHib := Achib;
  FExportPath := IncludeTrailingPathDelimiter(APath);
  ForceDirectories(FExportPath);
  FNewFormat := ANewFormat;
  FIsReplace := AReplace;
  FActiveMapIndex := AActiveMapIndex;
  setlength(FMapTypeArr, length(Atypemaparr));
  for i := 1 to length(Atypemaparr) do begin
    FMapTypeArr[i - 1] := Atypemaparr[i - 1];
  end;
  if FActiveMapIndex >= Length(FMapTypeArr) then begin
    FActiveMapIndex := 0;
  end;
end;

function TThreadExportIPhone.Write_Stream_to_Blob_Traditional(const AStream: TStream; Azoom, Ax, Ay, Aflags: integer): Int64;
var
  l: Integer;
  p: Pointer;
  Stmt: TDISQLite3Statement;
begin
  Stmt := FSQLite3Db.Prepare('INSERT INTO Images (data,zoom,x,y,flags,length) VALUES (?,"' + inttostr(Azoom) + '","' + inttostr(Ax) + '","' + inttostr(Ay) + '","' + inttostr(AFlags) + '","' + inttostr(AStream.Size) + '")');
  try
    if AStream is TCustomMemoryStream then begin
      with AStream as TCustomMemoryStream do begin
        Stmt.Bind_Blob(1, Memory, Size, SQLITE_STATIC);
      end;
    end else begin
      l := AStream.Size;
      GetMem(p, l);
      AStream.Seek(0, soFromBeginning);
      AStream.Read(p^, l);
      Stmt.Bind_Blob(1, p, l, sqlite3_Destroy_Mem);
    end;
    Stmt.Step;
    Result := FSQLite3Db.LastInsertRowID;
  finally
    Stmt.Free;
  end;
end;

procedure TThreadExportIPhone.WritePListFile(AGeoConvert: ICoordConverter);
var
  PList: Text;
  VLLCenter: TExtendedPoint;
  VPolyg: TPointArray;
  max, min: TPoint;
  i: Integer;
begin
  i := 0;
  While not (FZoomArr[i]) do begin
    inc(i);
  end;
  VPolyg := AGeoConvert.LonLatArray2PixelArray(FPolygLL, i);
  GetMinMax(min, max, VPolyg, true);
  VLLCenter := AGeoConvert.PixelPos2LonLat(Point(min.x + (max.X - min.X) div 2, min.y + (max.y - min.y) div 2), i);
  AssignFile(Plist, FExportPath + 'com.apple.Maps.plist');
  Rewrite(PList);
  Writeln(PList, '<plist>');
  Writeln(PList, '<dict>');
  Writeln(PList, '<key>LastViewMode</key>');
  if FMapTypeArr[FActiveMapIndex] <> nil then begin
    Writeln(PList, '<integer>'+IntToStr(FActiveMapIndex)+'</integer>');
  end;
  Writeln(PList, '<key>LastViewedLatitude</key>');
  Writeln(PList, '<real>' + R2StrPoint(VLLCenter.y) + '</real>');
  Writeln(PList, '<key>LastViewedLongitude</key>');
  Writeln(PList, '<real>' + R2StrPoint(VLLCenter.x) + '</real>');
  Writeln(PList, '<key>LastViewedZoomScale</key>');
  Writeln(PList, '<real>' + inttostr(i + 1) + '</real>');
  Writeln(PList, '</dict>');
  Writeln(PList, '</plist>');
  CloseFile(PList);
end;

procedure TThreadExportIPhone.ProcessRegion;
var
  p_x, p_y: integer;
  VZoom, j, xi, yi, hxyi, sizeim: integer;
  VPolyg: TPointArray;
  max, min: TPoint;
  VTileStream: TMemoryStream;
  VGeoConvert: ICoordConverter;
  VTile: TPoint;
  VSavers: array of IBitmapTileSaver;
  VBitmaps: array of TCustomBitmap32;
  Vbmp32crop: TCustomBitmap32;
  VFlags: array of integer;
begin
  if (FMapTypeArr[0] = nil) and (FMapTypeArr[1] = nil) and (FMapTypeArr[2] = nil) then begin
    exit;
  end;
  VGeoConvert := TCoordConverterMercatorOnSphere.Create(6378137);

    WritePListFile(VGeoConvert);

    if FNewFormat then begin
      hxyi := 2;
      sizeim := 128;
    end else begin
      hxyi := 4;
      sizeim := 64;
    end;
    VTileStream := TMemoryStream.Create;

    SetLength(VBitmaps, 3);
    VBitmaps[0] := TCustomBitmap32.Create;
    VBitmaps[1] := TCustomBitmap32.Create;
    VBitmaps[2] := TCustomBitmap32.Create;
    VBitmaps[2].DrawMode := dmBlend;

    SetLength(VSavers, 3);
    VSavers[0] := TJpegBitmapTileSaverIJL.Create(cSat);
    VSavers[1] := TVampyreBasicBitmapTileSaverPNGRGB.Create(cMap);
    VSavers[2] := TJpegBitmapTileSaverIJL.Create(chib);

    SetLength(VFlags, 3);
    VFlags[0] := 3;
    VFlags[1] := 2;
    VFlags[2] := 6;

    Vbmp32crop := TCustomBitmap32.Create;
    Vbmp32crop.Width := sizeim;
    Vbmp32crop.Height := sizeim;
    try
      FTilesToProcess := 0;
      for VZoom := 0 to 23 do begin
        if FZoomArr[VZoom] then begin
          VPolyg := VGeoConvert.LonLatArray2PixelArray(FPolygLL, VZoom);
          FTilesToProcess := FTilesToProcess + GetDwnlNum(min, max, VPolyg, true);
        end;
      end;
      ProgressFormUpdateCaption(SAS_STR_ExportTiles, SAS_STR_AllSaves + ' ' + inttostr(FTilesToProcess) + ' ' + SAS_STR_files);
      FTilesProcessed := 0;
      ProgressFormUpdateOnProgress;

      sqlite3_initialize;
      FSQLite3Db := TDISQLite3Database.Create(nil);
      FSQLite3Db.DatabaseName := FExportPath + 'MapTiles.sqlitedb';
      if not (FileExists(FExportPath + 'MapTiles.sqlitedb')) then begin
        FIsReplace := true;
      end;
      If FIsReplace then begin
        If FileExists(FExportPath + 'MapTiles.sqlitedb') then begin
          DeleteFile(FExportPath + 'MapTiles.sqlitedb');
        end;
        FSQLite3Db.CreateDatabase;
        FSQLite3Db.Execute('CREATE TABLE version(version int)');
        FSQLite3Db.Execute('CREATE TABLE images(zoom int, x int, y int, flags int, length int, data blob);');
        FSQLite3Db.Execute('CREATE INDEX index1 on images (zoom,x,y,flags)');
      end else begin
        FSQLite3Db.Open;
      end;
      FSQLite3Db.Execute('PRAGMA locking_mode=EXCLUSIVE');
      FSQLite3Db.Execute('PRAGMA cache_size=100000');
      FSQLite3Db.Execute('PRAGMA synchronous=OFF');
      FSQLite3Db.Connected := true;
      If FIsReplace then begin
        if FNewFormat then begin
          FSQLite3Db.Execute('INSERT INTO version (version) VALUES ("5")');
        end else begin
          FSQLite3Db.Execute('INSERT INTO version (version) VALUES ("4")');
        end;
        FSQLite3Db.Execute('INSERT INTO version (version) VALUES ("0")');
      end;
      FSQLite3Db.Execute('BEGIN TRANSACTION');
      for VZoom := 0 to 23 do begin
        if FZoomArr[VZoom] then begin
          VPolyg := VGeoConvert.LonLatArray2PixelArray(FPolygLL, VZoom);
          GetDwnlNum(min, max, VPolyg, false);

          p_x := min.x;
          while p_x < max.x do begin
            VTile.X := p_x shr 8;
            p_y := min.Y;
            while p_y < max.Y do begin
              VTile.Y := p_y shr 8;
              if (IsCancel) or (not (RgnAndRgn(VPolyg, p_x, p_y, false))) then begin
                inc(p_y, 256);
                CONTINUE;
              end;
              for j := 0 to Length(FMapTypeArr) - 1 do begin
                if FMapTypeArr[j] <> nil then begin
                  if FMapTypeArr[j].LoadTileUni(VBitmaps[j], VTile, VZoom, False, VGeoConvert, False, true, true) then begin
                    if (j=2) and (FMapTypeArr[0] <> nil) then begin
                      VBitmaps[0].Draw(0, 0, VBitmaps[j]);
                      VBitmaps[j].Draw(0, 0, VBitmaps[0]);
                    end;
                    for xi := 0 to hxyi - 1 do begin
                      for yi := 0 to hxyi - 1 do begin
                        Vbmp32crop.Clear;
                        Vbmp32crop.Draw(0, 0, bounds(sizeim * xi, sizeim * yi, sizeim, sizeim), VBitmaps[j]);
                        VTileStream.Clear;
                        VSavers[j].SaveToStream(Vbmp32crop, VTileStream);
                        Write_Stream_to_Blob_Traditional(
                          VTileStream, VZoom+1,
                          VTile.X * hxyi + xi, VTile.Y * hxyi + yi,
                          VFlags[j]
                        );
                      end;
                    end;
                  end;
                  inc(FTilesProcessed);
                  if ((FTilesToProcess < 100) and (FTilesProcessed mod 5 = 0)) or
                    ((FTilesToProcess >= 100) and (FTilesProcessed mod 50 = 0)) then begin
                    ProgressFormUpdateOnProgress;
                  end;
                  if (FTilesProcessed mod 500 = 0) then begin
                    FSQLite3Db.Execute('COMMIT');
                    FSQLite3Db.Execute('BEGIN TRANSACTION');
                  end;
                end;
              end;
              inc(p_y, 256);
            end;
            inc(p_x, 256);
          end;
        end;
      end;
      FSQLite3Db.Execute('COMMIT');
      ProgressFormUpdateOnProgress;
    finally
      sqlite3_shutdown;
      FSQLite3Db.Free;
    end;
end;

end.
