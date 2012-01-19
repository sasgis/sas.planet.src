unit u_ThreadExportIPhone;

interface

uses
  Windows,
  Types,
  SysUtils,
  Classes,
  AlSqlite3Wrapper,
  GR32,
  i_CoordConverterFactory,
  i_CoordConverter,
  i_VectorItemLonLat,
  u_MapType,
  u_GeoFun,
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
    FSQLite3Lib: TALSqlite3Library;
    FSqlite3: PSQLite3;
    csat, cmap, chib: byte;
    FCoordConverterFactory: ICoordConverterFactory;
    procedure CheckSQLiteAPIError(AError: Boolean);
    procedure WritePListFile(AGeoConvert: ICoordConverter);
    procedure WriteTileToSQLite3(
      AXY: TPoint;
      AZoom: Integer;
      AMemStream: TMemoryStream;
      AFlags: Integer
    );
  protected
    procedure ProcessRegion; override;
  public
    constructor Create(
      ACoordConverterFactory: ICoordConverterFactory;
      APath: string;
      APolygon: ILonLatPolygonLine;
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
  c_CoordConverter,
  u_GeoToStr,
  u_ResStrings,
  i_TileIterator,
  u_TileIteratorStuped,
  i_BitmapTileSaveLoad,
  u_BitmapTileVampyreSaver;

constructor TThreadExportIPhone.Create(
  ACoordConverterFactory: ICoordConverterFactory;
  APath: string;
  APolygon: ILonLatPolygonLine;
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
  FCoordConverterFactory := ACoordConverterFactory;
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

procedure TThreadExportIPhone.WriteTileToSQLite3(
  AXY: TPoint;
  AZoom: Integer;
  AMemStream: TMemoryStream;
  AFlags: Integer
);
var
  s: string;
  stmt: PSQLite3Stmt;
begin
  s := 'INSERT INTO Images (data,zoom,x,y,flags,length) VALUES '+
       '(' +
           '?,' +
           '"' + IntToStr(AZoom) + '",' +
           '"' + IntToStr(AXY.X) + '",' +
           '"' + IntToStr(AXY.Y) + '",' +
           '"' + IntToStr(AFlags) + '",' +
           '"' + IntToStr(AMemStream.Size) + '"' +
       ')';
  CheckSQLiteAPIError(
    FSQLite3Lib.sqlite3_prepare_v2(
      FSqlite3,
      PChar(s),
      Length(s),
      stmt,
      nil
    ) <> SQLITE_OK
  );
  try
    AMemStream.Position := 0;
    CheckSQLiteAPIError(
      FSQLite3Lib.sqlite3_bind_blob(
        stmt,
        1,
        AMemStream.Memory,
        AMemStream.Size,
        SQLITE_STATIC
      ) <> SQLITE_OK
    );
    CheckSQLiteAPIError(not (FSQLite3Lib.sqlite3_step(stmt) in [SQLITE_DONE, SQLITE_ROW]));
  finally
    CheckSQLiteAPIError(FSQLite3Lib.sqlite3_finalize(stmt) <> SQLITE_OK);
  end;
end;

procedure TThreadExportIPhone.WritePListFile(AGeoConvert: ICoordConverter);
var
  PList: Text;
  VLLCenter: TDoublePoint;
  VPolyg: TArrayOfPoint;
  VRect: TRect;
  VZoom: Integer;
  VLen: Integer;
begin
  VZoom := FZooms[0];
  VLen := FPolygLL.Count;
  SetLength(VPolyg, VLen);
  AGeoConvert.LonLatArray2PixelArray(FPolygLL.Points, VLen, @VPolyg[0], VZoom);
  GetMinMax(VRect, @VPolyg[0], Length(VPolyg), true);
  VLLCenter := AGeoConvert.PixelPos2LonLat(Point(VRect.Left + (VRect.Right - VRect.Left) div 2, VRect.Top + (VRect.Bottom - VRect.Top) div 2), VZoom);
  AssignFile(Plist, FExportPath + 'com.apple.Maps.plist');
  Rewrite(PList);
  Writeln(PList, '<plist>');
  Writeln(PList, '<dict>');
  Writeln(PList, '<key>LastViewMode</key>');
  if FMapTypeArr[FActiveMapIndex] <> nil then begin
    Writeln(PList, '<integer>' + IntToStr(FActiveMapIndex) + '</integer>');
  end;
  Writeln(PList, '<key>LastViewedLatitude</key>');
  Writeln(PList, '<real>' + R2StrPoint(VLLCenter.y) + '</real>');
  Writeln(PList, '<key>LastViewedLongitude</key>');
  Writeln(PList, '<real>' + R2StrPoint(VLLCenter.x) + '</real>');
  Writeln(PList, '<key>LastViewedZoomScale</key>');
  Writeln(PList, '<real>' + inttostr(VZoom + 1) + '</real>');
  Writeln(PList, '</dict>');
  Writeln(PList, '</plist>');
  CloseFile(PList);
end;

procedure TThreadExportIPhone.ProcessRegion;

  procedure SQLiteWriteString(const AString: string);
  var
    stmt: PSQLite3Stmt;
  begin
    CheckSQLiteAPIError(
      FSQLite3Lib.sqlite3_prepare_v2(
        FSqlite3,
        PChar(AString),
        Length(AString),
        stmt,
        nil
      ) <> SQLITE_OK
    );
    try
      CheckSQLiteAPIError(not (FSQLite3Lib.sqlite3_step(stmt) in [SQLITE_DONE, SQLITE_ROW]));
    finally
      CheckSQLiteAPIError(FSQLite3Lib.sqlite3_finalize(stmt) <> SQLITE_OK);
    end;
  end;

var
  VZoom: byte;
  i, j, xi, yi, hxyi, sizeim: integer;
  VTileStream: TMemoryStream;
  VGeoConvert: ICoordConverter;
  VTile: TPoint;
  VSavers: array of IBitmapTileSaver;
  VBitmaps: array of TCustomBitmap32;
  Vbmp32crop: TCustomBitmap32;
  VFlags: array of integer;
  VTileIterators: array of ITileIterator;
  VTileIterator: ITileIterator;
  VDatabaseName: string;
begin
  inherited;
  if (FMapTypeArr[0] = nil) and (FMapTypeArr[1] = nil) and (FMapTypeArr[2] = nil) then begin
    exit;
  end;
  VGeoConvert := FCoordConverterFactory.GetCoordConverterByCode(CGoogleProjectionEPSG, CTileSplitQuadrate256x256);

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

  Vbmp32crop := TCustomBitmap32.Create;
  try
    SetLength(VSavers, 3);
    VSavers[0] := TVampyreBasicBitmapTileSaverJPG.Create(cSat);
    VSavers[1] := TVampyreBasicBitmapTileSaverPNGRGB.Create(cMap);
    VSavers[2] := TVampyreBasicBitmapTileSaverJPG.Create(chib);

    SetLength(VFlags, 3);
    VFlags[0] := 3;
    VFlags[1] := 2;
    VFlags[2] := 6;

    Vbmp32crop.Width := sizeim;
    Vbmp32crop.Height := sizeim;
    FTilesToProcess := 0;
    FTilesProcessed := 0;
    SetLength(VTileIterators, Length(FZooms));
    for i := 0 to Length(FZooms) - 1 do begin
      VZoom := FZooms[i];
      VTileIterators[i] := TTileIteratorStuped.Create(VZoom, FPolygLL, VGeoConvert);
      FTilesToProcess := FTilesToProcess + VTileIterators[i].TilesTotal;         
    end;
    try
      ProgressFormUpdateCaption(SAS_STR_ExportTiles, SAS_STR_AllSaves + ' ' + inttostr(FTilesToProcess) + ' ' + SAS_STR_files);
      ProgressFormUpdateOnProgress;

      FSqlite3 := nil;
      FSQLite3Lib := TALSqlite3Library.Create;
      try
        if FSQLite3Lib.Load then
        try
          VDatabaseName := FExportPath + 'MapTiles.sqlitedb';
          if not FileExists(VDatabaseName) then begin
            FIsReplace := True;
          end;
          If FIsReplace then begin

            If FileExists(VDatabaseName) then begin
              DeleteFile(VDatabaseName);
            end;

            try
              CheckSQLiteAPIError(
                FSQLite3Lib.sqlite3_open_v2(
                  PAnsiChar(VDatabaseName),
                  FSqlite3,
                  SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE,
                  nil
                ) <> SQLITE_OK
              );
            except
              if Assigned(FSqlite3) then begin
                FSQLite3Lib.sqlite3_close(FSqlite3);
              end;
              FSqlite3 := nil;
              raise;
            End;

            SQLiteWriteString('CREATE TABLE version(version int)');
            SQLiteWriteString('CREATE TABLE images(zoom int, x int, y int, flags int, length int, data blob);');
            SQLiteWriteString('CREATE INDEX index1 on images (zoom,x,y,flags)');
          end else begin

            try
              CheckSQLiteAPIError(
                FSQLite3Lib.sqlite3_open_v2(
                  PAnsiChar(VDatabaseName),
                  FSqlite3,
                  SQLITE_OPEN_READWRITE,
                  nil
                ) <> SQLITE_OK
              );
            except
              if Assigned(FSqlite3) then begin
                FSQLite3Lib.sqlite3_close(FSqlite3);
              end;
              FSqlite3 := nil;
              raise;
            End;

          end;

          SQLiteWriteString('PRAGMA locking_mode=EXCLUSIVE');
          SQLiteWriteString('PRAGMA cache_size=100000');
          SQLiteWriteString('PRAGMA synchronous=OFF');

          If FIsReplace then begin
            if FNewFormat then begin
              SQLiteWriteString('INSERT INTO version (version) VALUES ("5")');
            end else begin
              SQLiteWriteString('INSERT INTO version (version) VALUES ("4")');
            end;
           SQLiteWriteString('INSERT INTO version (version) VALUES ("0")');
          end;
          SQLiteWriteString('BEGIN TRANSACTION');
          try
            for i := 0 to Length(FZooms) - 1 do begin
              VZoom := FZooms[i];
              VTileIterator := VTileIterators[i];
              while VTileIterator.Next(VTile) do begin
                if CancelNotifier.IsOperationCanceled(OperationID) then begin
                  exit;
                end;
                for j := 0 to Length(FMapTypeArr) - 1 do begin
                  if FMapTypeArr[j] <> nil then begin
                    if FMapTypeArr[j].LoadTileUni(VBitmaps[j], VTile, VZoom, VGeoConvert, False, true, true) then begin
                      if (j = 2) and (FMapTypeArr[0] <> nil) then begin
                        VBitmaps[0].Draw(0, 0, VBitmaps[j]);
                        VBitmaps[j].Draw(0, 0, VBitmaps[0]);
                      end;
                      for xi := 0 to hxyi - 1 do begin
                        for yi := 0 to hxyi - 1 do begin
                          Vbmp32crop.Clear;
                          Vbmp32crop.Draw(0, 0, bounds(sizeim * xi, sizeim * yi, sizeim, sizeim), VBitmaps[j]);
                          VTileStream.Clear;
                          VSavers[j].SaveToStream(Vbmp32crop, VTileStream);
                          WriteTileToSQLite3(
                            Point(VTile.X * hxyi + xi, VTile.Y * hxyi + yi),
                            VZoom + 1,
                            VTileStream,
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
                      SQLiteWriteString('COMMIT TRANSACTION');
                      SQLiteWriteString('BEGIN TRANSACTION');
                    end;
                  end;
                end;
              end;
            end;
          finally
            SQLiteWriteString('COMMIT TRANSACTION');
          end;
          ProgressFormUpdateOnProgress;
        finally
          FSQLite3Lib.sqlite3_close(FSqlite3);
          FSqlite3 := nil;
          FSQLite3Lib.Unload;
        end;
      finally
        FreeAndNil(FSQLite3Lib);
      end;
    finally
      for i := 0 to Length(VTileIterators) - 1 do begin
        VTileIterators[i] := nil;
      end;
      VTileIterators := nil;
    end;
  finally
    VTileStream.Free;
    for i := 0 to Length(VBitmaps) - 1 do
      VBitmaps[i].Free;
    Vbmp32crop.Free;
  end;
end;

procedure TThreadExportIPhone.CheckSQLiteAPIError(AError: Boolean);
begin
  if AError then begin
    if Assigned(FSqlite3) then begin
      raise Exception.Create(
              FSQLite3Lib.sqlite3_errmsg(Fsqlite3) +
              ' ( error code: ' +
              IntToStr(FSQLite3Lib.sqlite3_errcode(Fsqlite3)) + ')'
            );
    end else begin
      raise Exception.Create('Sqlite3 error');
    end;
  end;
end;

end.
