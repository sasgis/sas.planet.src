unit UThreadExportIPhone;

interface

uses
  Windows,
  Types,
  Forms,
  SysUtils,
  Classes,
  Graphics,
  DISQLite3Database,
  DISQLite3Api,
  PNGImage,
  JPEG,
  GR32,
  UMapType,
  UGeoFun,
  unit4,
  UResStrings,
  t_GeoTypes;

type
  TThreadExportIPhone = class(TThread)
  private
    FPolygLL: TExtendedPointArray;
    FZoomArr: array [0..23] of boolean;
    FMapTypeArr: array of TMapType;
    FActiveMapIndex: integer;
    FNewFormat: Boolean;

    FProgressForm: TFprogress2;
    FShowFormCaption: string;
    FShowOnFormLine0: string;
    FShowOnFormLine1: string;
    FProgressOnForm: integer;
    FMessageForShow: string;

    FIsReplace: boolean;
    FExportPath: string;
    FSQLite3Db: TDISQLite3Database;
    csat, cmap, chib: byte;

    procedure UpdateProgressFormBar;
    procedure UpdateProgressFormCaption;
    procedure UpdateProgressFormStr0;
    procedure UpdateProgressFormStr1;
    procedure UpdateProgressFormClose;
    procedure SynShowMessage;

    procedure export2iMaps();
    function Write_Stream_to_Blob_Traditional(const AStream: TStream; Azoom, Ax, Ay, Aflags: integer): Int64;
  protected
    procedure Execute; override;
  public
    constructor Create(
      APath: string;
      APolygon_: TExtendedPointArray;
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
  Dialogs,
  u_GeoToStr,
  i_ICoordConverter,
  u_GlobalState,
  i_BitmapTileSaveLoad,
  u_BitmapTileVampyreSaver,
  u_BitmapTileJpegSaverIJL,
  u_CoordConverterMercatorOnSphere;

constructor TThreadExportIPhone.Create(
  APath: string;
  APolygon_: TExtendedPointArray;
  Azoomarr: array of boolean;
  Atypemaparr: array of TMapType;
  AActiveMapIndex: Integer;
  Areplace: boolean;
  ANewFormat: Boolean;
  Acsat, Acmap, Achib: byte);
var
  i: integer;
begin
  inherited Create(false);
  Priority := tpLowest;
  FreeOnTerminate := true;
  Application.CreateForm(TFProgress2, FProgressForm);
  cSat := Acsat;
  cMap := Acmap;
  cHib := Achib;
  FProgressForm.ProgressBar1.Progress1 := 0;
  FProgressForm.ProgressBar1.Max := 100;
  FProgressForm.Visible := true;
  FExportPath := APath;
  FNewFormat := ANewFormat;
  FIsReplace := AReplace;
  setlength(FPolygLL, length(APolygon_));
  for i := 1 to length(APolygon_) do begin
    FPolygLL[i - 1] := Apolygon_[i - 1];
  end;
  for i := 0 to 23 do begin
    FZoomArr[i] := Azoomarr[i];
  end;
  FActiveMapIndex := AActiveMapIndex;
  setlength(FMapTypeArr, length(Atypemaparr));
  for i := 1 to length(Atypemaparr) do begin
    FMapTypeArr[i - 1] := Atypemaparr[i - 1];
  end;
  if FActiveMapIndex >= Length(FMapTypeArr) then begin
    FActiveMapIndex := 0;
  end;
end;


procedure TThreadExportIPhone.Execute;
begin
  export2iMaps;
  Synchronize(UpdateProgressFormClose);
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

procedure TThreadExportIPhone.export2iMaps;
var
  p_x, p_y: integer;
  i, j, xi, yi, hxyi, sizeim, cri, crj: integer;
  num_dwn, scachano, obrab, alpha: integer;
  polyg: TPointArray;
  max, min, p_h: TPoint;
  TileStream: TMemoryStream;
  PList: Text;
  LLCenter: TExtendedPoint;
  VGeoConvert: ICoordConverter;
  VMinLonLat, VMaxLonLat: TExtendedPoint;
  VTile: TPoint;
  VSavers: array of IBitmapTileSaver;
  VBitmaps: array of TCustomBitmap32;
  bmp32crop: TCustomBitmap32;
  VFlags: array of integer;
begin
  if (FMapTypeArr[0] = nil) and (FMapTypeArr[1] = nil) and (FMapTypeArr[2] = nil) then begin
    exit;
  end;
  VGeoConvert := TCoordConverterMercatorOnSphere.Create(6378137);
  try
    i := 0;
    While not (FZoomArr[i]) do begin
      inc(i);
    end;
    polyg := VGeoConvert.PoligonProject(i + 8, FPolygLL);
    GetMinMax(min, max, polyg, true);
    LLCenter := VGeoConvert.PixelPos2LonLat(Point(min.x + (max.X - min.X) div 2, min.y + (max.y - min.y) div 2), i);

    AssignFile(Plist, FExportPath + 'com.apple.Maps.plist');
    Rewrite(PList);
    Writeln(PList, '<plist>');
    Writeln(PList, '<dict>');
    Writeln(PList, '<key>LastViewMode</key>');
    if FMapTypeArr[FActiveMapIndex] <> nil then begin
      Writeln(PList, '<integer>'+IntToStr(FActiveMapIndex)+'</integer>');
    end;
    Writeln(PList, '<key>LastViewedLatitude</key>');
    Writeln(PList, '<real>' + R2StrPoint(LLCenter.y) + '</real>');
    Writeln(PList, '<key>LastViewedLongitude</key>');
    Writeln(PList, '<real>' + R2StrPoint(LLCenter.x) + '</real>');
    Writeln(PList, '<key>LastViewedZoomScale</key>');
    Writeln(PList, '<real>' + inttostr(i + 1) + '</real>');
    Writeln(PList, '</dict>');
    Writeln(PList, '</plist>');
    CloseFile(PList);

    if FNewFormat then begin
      hxyi := 2;
      sizeim := 128;
    end else begin
      hxyi := 4;
      sizeim := 64;
    end;
    TileStream := TMemoryStream.Create;

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

    bmp32crop := TCustomBitmap32.Create;
    bmp32crop.Width := sizeim;
    bmp32crop.Height := sizeim;
    try
      num_dwn := 0;
      for i := 0 to 23 do begin
        if FZoomArr[i] then begin
          polyg := VGeoConvert.PoligonProject(i + 8, FPolygLL);
          num_dwn := num_dwn + GetDwnlNum(min, max, Polyg, true);
        end;
      end;
      FShowOnFormLine0 := SAS_STR_ExportTiles;
      Synchronize(UpdateProgressFormStr0);

      FShowFormCaption := SAS_STR_AllSaves + ' ' + inttostr(num_dwn) + ' ' + SAS_STR_files;
      Synchronize(UpdateProgressFormCaption);

      FShowOnFormLine1 := SAS_STR_Processed + ' 0%';
      Synchronize(UpdateProgressFormStr1);

      obrab := 0;

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
      for i := 0 to 23 do begin
        if FZoomArr[i] then begin
          Polyg := VGeoConvert.PoligonProject(i + 8, FPolygLL);
          GetDwnlNum(min, max, Polyg, false);

          p_x := min.x;
          while p_x < max.x do begin
            VTile.X := p_x shr 8;
            p_y := min.Y;
            while p_y < max.Y do begin
              VTile.Y := p_y shr 8;
              if (FProgressForm.Visible = false) or (not (RgnAndRgn(Polyg, p_x, p_y, false))) then begin
                inc(p_y, 256);
                CONTINUE;
              end;
              for j := 0 to Length(FMapTypeArr) - 1 do begin
                if FMapTypeArr[j] <> nil then begin
                  if FMapTypeArr[j].LoadTileUni(VBitmaps[j], VTile, i, False, VGeoConvert, False, true, true) then begin
                    if (j=2) and (FMapTypeArr[0] <> nil) then begin
                      VBitmaps[0].Draw(0, 0, VBitmaps[j]);
                      VBitmaps[j].Draw(0, 0, VBitmaps[0]);
                    end;
                    for xi := 0 to hxyi - 1 do begin
                      for yi := 0 to hxyi - 1 do begin
                        bmp32crop.Clear;
                        bmp32crop.Draw(0, 0, bounds(sizeim * xi, sizeim * yi, sizeim, sizeim), VBitmaps[j]);
                        TileStream.Clear;
                        VSavers[j].SaveToStream(bmp32crop, TileStream);
                        Write_Stream_to_Blob_Traditional(
                          TileStream, i+1,
                          VTile.X * hxyi + xi, VTile.Y * hxyi + yi,
                          VFlags[j]
                        );
                      end;
                    end;
                  end;
                  inc(obrab);
                  if ((num_dwn < 100) and (obrab mod 5 = 0)) or
                    ((num_dwn >= 100) and (obrab mod 50 = 0)) then begin
                    FProgressOnForm := round((obrab / num_dwn) * 100);
                    Synchronize(UpdateProgressFormBar);
                    FShowOnFormLine1 := SAS_STR_Processed + ' ' + inttostr(FProgressOnForm) + '%';
                    Synchronize(UpdateProgressFormStr1);
                  end;
                  if (obrab mod 500 = 0) then begin
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
      FProgressOnForm := round((obrab / num_dwn) * 100);
      Synchronize(UpdateProgressFormBar);
      FShowOnFormLine1 := SAS_STR_Processed + ' ' + inttostr(FProgressOnForm) + '%';
      Synchronize(UpdateProgressFormStr1);
    finally
      sqlite3_shutdown;
      FSQLite3Db.Free;
    end;
  except
    on e: Exception do begin
      FMessageForShow := e.Message;
      Synchronize(SynShowMessage);
    end;
  end;
end;

procedure TThreadExportIPhone.SynShowMessage;
begin
  ShowMessage(FMessageForShow);
end;

procedure TThreadExportIPhone.UpdateProgressFormCaption;
begin
  FProgressForm.Caption := FShowFormCaption;
end;

procedure TThreadExportIPhone.UpdateProgressFormClose;
begin
  FProgressForm.Close;
end;

procedure TThreadExportIPhone.UpdateProgressFormStr0;
begin
  FProgressForm.MemoInfo.Lines[0] := FShowOnFormLine0;
end;

procedure TThreadExportIPhone.UpdateProgressFormStr1;
begin
  FProgressForm.MemoInfo.Lines[1] := FShowOnFormLine1;
end;

procedure TThreadExportIPhone.UpdateProgressFormBar;
begin
  FProgressForm.ProgressBar1.Progress1 := FProgressOnForm;
end;

end.
