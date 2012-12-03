unit u_ThreadExportIPhone;

interface

uses
  Windows,
  Types,
  SysUtils,
  Classes,
  AlSqlite3Wrapper,
  GR32,
  i_NotifierOperation,
  i_RegionProcessProgressInfo,
  i_BinaryData,
  i_BitmapTileSaveLoad,
  i_BitmapLayerProvider,
  i_CoordConverterFactory,
  i_LocalCoordConverterFactorySimpe,
  i_VectorItemsFactory,
  i_CoordConverter,
  i_VectorItemLonLat,
  i_BitmapTileSaveLoadFactory,
  u_MapType,
  u_GeoFun,
  t_GeoTypes,
  u_ThreadExportAbstract;

type
  TExportTaskIPhone = record
    FFlag: Integer;
    FSaver: IBitmapTileSaver;
    FImageProvider: IBitmapLayerProvider;
  end;

  TThreadExportIPhone = class(TThreadExportAbstract)
  private
    FTasks: array of TExportTaskIPhone;
    FActiveMapIndex: integer;
    FNewFormat: Boolean;

    FIsReplace: boolean;
    FExportPath: string;
    FSQLite3Lib: TALSqlite3Library;
    FSqlite3: PSQLite3;
    FCoordConverterFactory: ICoordConverterFactory;
    FLocalConverterFactory: ILocalCoordConverterFactorySimpe;
    FProjectionFactory: IProjectionInfoFactory;
    FVectorItmesFactory: IVectorItemsFactory;
    FBitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
    procedure CheckSQLiteAPIError(AError: Boolean);
    procedure WritePListFile(const AGeoConvert: ICoordConverter);
    procedure WriteTileToSQLite3(
      const AXY: TPoint;
      AZoom: Integer;
      const AData: IBinaryData;
      AFlags: Integer
    );
  protected
    procedure ProcessRegion; override;
  public
    constructor Create(
      const ACancelNotifier: INotifierOperation;
      AOperationID: Integer;
      const AProgressInfo: IRegionProcessProgressInfoInternal;
      const ACoordConverterFactory: ICoordConverterFactory;
      const ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
      const AProjectionFactory: IProjectionInfoFactory;
      const AVectorItmesFactory: IVectorItemsFactory;
      const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
      const APath: string;
      const APolygon: ILonLatPolygon;
      const Azoomarr: TByteDynArray;
      const Atypemaparr: array of TMapType;
      AActiveMapIndex: Integer;
      AReplace: boolean;
      ANewFormat: Boolean;
      Acsat: byte;
      Acmap: byte;
      Achib: byte
    );
    destructor Destroy; override;
  end;

implementation

uses
  c_CoordConverter,
  i_LocalCoordConverter,
  i_Bitmap32Static,
  u_GeoToStr,
  u_ResStrings,
  i_VectorItemProjected,
  i_TileIterator,
  u_Bitmap32Static,
  u_BitmapFunc,
  u_TileIteratorByPolygon,
  u_BitmapLayerProviderMapWithLayer;

constructor TThreadExportIPhone.Create(
  const ACancelNotifier: INotifierOperation;
  AOperationID: Integer;
  const AProgressInfo: IRegionProcessProgressInfoInternal;
  const ACoordConverterFactory: ICoordConverterFactory;
  const ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
  const AProjectionFactory: IProjectionInfoFactory;
  const AVectorItmesFactory: IVectorItemsFactory;
  const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
  const APath: string;
  const APolygon: ILonLatPolygon;
  const Azoomarr: TByteDynArray;
  const Atypemaparr: array of TMapType;
  AActiveMapIndex: Integer;
  AReplace: boolean;
  ANewFormat: Boolean;
  Acsat, Acmap, Achib: byte
);
var
  VTaskIndex: Integer;
begin
  inherited Create(
    ACancelNotifier,
    AOperationID,
    AProgressInfo,
    APolygon,
    Azoomarr,
    AnsiString(Self.ClassName)
  );
  FCoordConverterFactory := ACoordConverterFactory;
  FLocalConverterFactory := ALocalConverterFactory;
  FProjectionFactory := AProjectionFactory;
  FVectorItmesFactory := AVectorItmesFactory;
  FBitmapTileSaveLoadFactory := ABitmapTileSaveLoadFactory;
  FExportPath := IncludeTrailingPathDelimiter(APath);
  ForceDirectories(FExportPath);
  FNewFormat := ANewFormat;
  FIsReplace := AReplace;
  FActiveMapIndex := -1;

  if (length(Atypemaparr) <> 3) then begin
    raise Exception.Create('Not expected maps count');
  end;
  if (Atypemaparr[0] = nil) and
    (Atypemaparr[1] = nil) and
    (Atypemaparr[2] = nil) then begin
    raise Exception.Create('Maps are not selected');
  end;

  VTaskIndex := -1;
  if Atypemaparr[0] <> nil then begin
    Inc(VTaskIndex);
    SetLength(FTasks, VTaskIndex + 1);
    if AActiveMapIndex = 0 then begin
      FActiveMapIndex := VTaskIndex;
    end;
    FTasks[VTaskIndex].FFlag := 3;
    FTasks[VTaskIndex].FSaver := FBitmapTileSaveLoadFactory.CreateJpegSaver(Acsat);
    FTasks[VTaskIndex].FImageProvider :=
      TBitmapLayerProviderMapWithLayer.Create(
        Atypemaparr[0],
        nil,
        False,
        False
      );
  end;
  if Atypemaparr[1] <> nil then begin
    Inc(VTaskIndex);
    SetLength(FTasks, VTaskIndex + 1);
    if AActiveMapIndex = 1 then begin
      FActiveMapIndex := VTaskIndex;
    end;
    FTasks[VTaskIndex].FFlag := 2;
    FTasks[VTaskIndex].FSaver := FBitmapTileSaveLoadFactory.CreatePngSaver(i24bpp, Acmap);
    FTasks[VTaskIndex].FImageProvider :=
      TBitmapLayerProviderMapWithLayer.Create(
        Atypemaparr[1],
        nil,
        False,
        False
      );
  end;
  if Atypemaparr[2] <> nil then begin
    Inc(VTaskIndex);
    SetLength(FTasks, VTaskIndex + 1);
    if AActiveMapIndex = 2 then begin
      FActiveMapIndex := VTaskIndex;
    end;
    FTasks[VTaskIndex].FFlag := 6;
    FTasks[VTaskIndex].FSaver := FBitmapTileSaveLoadFactory.CreateJpegSaver(Achib);
    FTasks[VTaskIndex].FImageProvider :=
      TBitmapLayerProviderMapWithLayer.Create(
        Atypemaparr[0],
        Atypemaparr[2],
        False,
        False
      );
  end;

end;

destructor TThreadExportIPhone.Destroy;
var
  i: Integer;
begin
  for i := 0 to Length(FTasks) - 1 do begin
    FTasks[i].FSaver := nil;
    FTasks[i].FImageProvider := nil;
  end;
  inherited;
end;

procedure TThreadExportIPhone.WriteTileToSQLite3(
  const AXY: TPoint;
  AZoom: Integer;
  const AData: IBinaryData;
  AFlags: Integer
);
var
  s: AnsiString;
  stmt: PSQLite3Stmt;
begin
  s := 'INSERT INTO Images (data,zoom,x,y,flags,length) VALUES ' +
    '(' +
    '?,' +
    '"' + IntToStr(AZoom) + '",' +
    '"' + IntToStr(AXY.X) + '",' +
    '"' + IntToStr(AXY.Y) + '",' +
    '"' + IntToStr(AFlags) + '",' +
    '"' + IntToStr(AData.Size) + '"' +
    ')';
  CheckSQLiteAPIError(
    FSQLite3Lib.sqlite3_prepare_v2(
    FSqlite3,
    PAnsiChar(s),
    Length(s),
    stmt,
    nil
    ) <> SQLITE_OK
  );
  try
    CheckSQLiteAPIError(
      FSQLite3Lib.sqlite3_bind_blob(
      stmt,
      1,
      AData.Buffer,
      AData.Size,
      SQLITE_STATIC
      ) <> SQLITE_OK
    );
    CheckSQLiteAPIError(not (FSQLite3Lib.sqlite3_step(stmt) in [SQLITE_DONE, SQLITE_ROW]));
  finally
    CheckSQLiteAPIError(FSQLite3Lib.sqlite3_finalize(stmt) <> SQLITE_OK);
  end;
end;

procedure TThreadExportIPhone.WritePListFile(const AGeoConvert: ICoordConverter);
var
  PList: Text;
  VLLCenter: TDoublePoint;
  VZoom: Integer;
begin
  VZoom := FZooms[0];
  VLLCenter := AGeoConvert.PixelPosFloat2LonLat(RectCenter(AGeoConvert.LonLatRect2PixelRectFloat(PolygLL.Bounds.Rect, VZoom)), VZoom);
  AssignFile(Plist, FExportPath + 'com.apple.Maps.plist');
  Rewrite(PList);
  Writeln(PList, '<plist>');
  Writeln(PList, '<dict>');
  Writeln(PList, '<key>LastViewMode</key>');
  if FActiveMapIndex >= 0 then begin
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

  procedure SQLiteWriteString(const AString: AnsiString);
  var
    stmt: PSQLite3Stmt;
  begin
    CheckSQLiteAPIError(
      FSQLite3Lib.sqlite3_prepare_v2(
      FSqlite3,
      PAnsiChar(AString),
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
  VGeoConvert: ICoordConverter;
  VTile: TPoint;
  VBitmapTile: IBitmap32Static;
  Vbmp32crop: TCustomBitmap32;
  VStaticBitmapCrop: IBitmap32Static;
  VDataToSave: IBinaryData;
  VTileIterators: array of ITileIterator;
  VTileIterator: ITileIterator;
  VDatabaseName: string;
  VProjectedPolygon: IProjectedPolygon;
  VTilesToProcess: Int64;
  VTilesProcessed: Int64;
  VTileConverter: ILocalCoordConverter;
begin
  inherited;
  VGeoConvert := FCoordConverterFactory.GetCoordConverterByCode(CGoogleProjectionEPSG, CTileSplitQuadrate256x256);

  WritePListFile(VGeoConvert);

  if FNewFormat then begin
    hxyi := 2;
    sizeim := 128;
  end else begin
    hxyi := 4;
    sizeim := 64;
  end;

  Vbmp32crop := TCustomBitmap32.Create;
  try
    Vbmp32crop.Width := sizeim;
    Vbmp32crop.Height := sizeim;
    VTilesToProcess := 0;
    SetLength(VTileIterators, Length(FZooms));
    for i := 0 to Length(FZooms) - 1 do begin
      VZoom := FZooms[i];
      VProjectedPolygon :=
        FVectorItmesFactory.CreateProjectedPolygonByLonLatPolygon(
          FProjectionFactory.GetByConverterAndZoom(VGeoConvert, VZoom),
          PolygLL
        );
      VTileIterators[i] := TTileIteratorByPolygon.Create(VProjectedPolygon);
      VTilesToProcess := VTilesToProcess + VTileIterators[i].TilesTotal * Length(FTasks);
    end;
    try
      ProgressInfo.SetCaption(SAS_STR_ExportTiles);
      ProgressInfo.SetFirstLine(
        SAS_STR_AllSaves + ' ' + inttostr(VTilesToProcess) + ' ' + SAS_STR_Files
      );
      VTilesProcessed := 0;
      ProgressFormUpdateOnProgress(VTilesProcessed, VTilesToProcess);

      FSqlite3 := nil;
      FSQLite3Lib := TALSqlite3Library.Create;
      try
        if FSQLite3Lib.Load then begin
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
                  VTileConverter := FLocalConverterFactory.CreateForTile(VTile, VZoom, VGeoConvert);
                  for j := 0 to Length(FTasks) - 1 do begin
                    VBitmapTile :=
                      FTasks[j].FImageProvider.GetBitmapRect(
                        OperationID, CancelNotifier,
                        FLocalConverterFactory.CreateForTile(VTile, VZoom, VGeoConvert)
                      );
                    if VBitmapTile <> nil then begin
                      for xi := 0 to hxyi - 1 do begin
                        for yi := 0 to hxyi - 1 do begin
                          Vbmp32crop.Clear;
                          BlockTransfer(
                            Vbmp32crop,
                            0,
                            0,
                            VBitmapTile,
                            bounds(sizeim * xi, sizeim * yi, sizeim, sizeim),
                            dmOpaque
                          );
                          VStaticBitmapCrop := TBitmap32Static.CreateWithCopy(Vbmp32crop);
                          VDataToSave := FTasks[j].FSaver.Save(VStaticBitmapCrop);
                          WriteTileToSQLite3(
                            Point(VTile.X * hxyi + xi, VTile.Y * hxyi + yi),
                            VZoom + 1,
                            VDataToSave,
                            FTasks[j].FFlag
                          );
                        end;
                      end;
                    end;
                    inc(VTilesProcessed);
                    if ((VTilesToProcess < 100) and (VTilesProcessed mod 5 = 0)) or
                      ((VTilesToProcess >= 100) and (VTilesProcessed mod 50 = 0)) then begin
                      ProgressFormUpdateOnProgress(VTilesProcessed, VTilesToProcess);
                    end;
                    if (VTilesProcessed mod 500 = 0) then begin
                      SQLiteWriteString('COMMIT TRANSACTION');
                      SQLiteWriteString('BEGIN TRANSACTION');
                    end;
                  end;
                end;
              end;
            finally
              SQLiteWriteString('COMMIT TRANSACTION');
            end;
            ProgressFormUpdateOnProgress(VTilesProcessed, VTilesToProcess);
          finally
            FSQLite3Lib.sqlite3_close(FSqlite3);
            FSqlite3 := nil;
            FSQLite3Lib.Unload;
          end;
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
    Vbmp32crop.Free;
  end;
end;

procedure TThreadExportIPhone.CheckSQLiteAPIError(AError: Boolean);
begin
  if AError then begin
    if Assigned(FSqlite3) then begin
      raise Exception.Create(
        FSQLite3Lib.sqlite3_errmsg(FSqlite3) +
        ' ( error code: ' +
        IntToStr(FSQLite3Lib.sqlite3_errcode(FSqlite3)) + ')'
      );
    end else begin
      raise Exception.Create('Sqlite3 error');
    end;
  end;
end;

end.
