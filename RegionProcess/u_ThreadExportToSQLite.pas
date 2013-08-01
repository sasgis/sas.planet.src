unit u_ThreadExportToSQLite;

interface

uses
  Windows,
  SysUtils,
  Classes,
  SQLite3Handler,
  i_TileInfoBasic,
  u_MapType,
  u_ThreadExportEnumerator;

type
  TThreadExportToSQLite = class(TThreadExportEnumerator)
  protected
    FSQLiteAvailable: Boolean;
  protected
    procedure CloseTargetStorage(const AEEData: PExportEnumeratorData); virtual;
  protected
    procedure InitializeExportEnumeratorData(out AEEData: PExportEnumeratorData); override;
    procedure UninitializeExportEnumeratorData(var AEEData: PExportEnumeratorData); override;
    procedure CreateTargetStorage(
      const ASourceMapType: TMapType;
      const AEEData: PExportEnumeratorData
    ); override;
  end;

  TThreadExportRMapsSQLite = class(TThreadExportToSQLite)
  private
    FInsertSQLText: AnsiString;
  protected
    procedure CloseTargetStorage(const AEEData: PExportEnumeratorData); override;
  protected
    procedure CreateTargetStorage(
      const ASourceMapType: TMapType;
      const AEEData: PExportEnumeratorData
    ); override;
    procedure SaveTileToTargetStorage(
      const ASourceMapType: TMapType;
      const AEEData: PExportEnumeratorData;
      const AEETile: PExportEnumeratorTile;
      const ATileInfo: ITileInfoWithData
    ); override;
  end;

implementation

uses
  ALFcnString,
  ALSqlite3Wrapper;

type
  PEEToSQLiteData = ^TEEToSQLiteData;
  TEEToSQLiteData = record
    Base: TExportEnumeratorData;
    SQLite3Db: TSQLite3DbHandler;
    CreateNewDB: Boolean;
  end;
  
{ TThreadExportToSQLite }

procedure TThreadExportToSQLite.CloseTargetStorage(const AEEData: PExportEnumeratorData);
begin
  if (AEEData<>nil) then
  with PEEToSQLiteData(AEEData)^ do
  if SQLite3Db.Opened then begin
    SQLite3Db.Commit;
    SQLite3Db.Close;
  end;
end;

procedure TThreadExportToSQLite.CreateTargetStorage(
  const ASourceMapType: TMapType;
  const AEEData: PExportEnumeratorData
);
begin
  // check library
  if (not FSQLiteAvailable) then
    raise ESQLite3SimpleError.Create('SQLite not available');

  // закрываем предыдущее (если есть)
  CloseTargetStorage(AEEData);

  // make sqlite database
  if FileExists(FExportPath) then begin
    // база уже есть - будем дописывать или грохнем
    if FForceDropTarget then begin
      DeleteFile(FExportPath);
      PEEToSQLiteData(AEEData)^.CreateNewDB := TRUE;
    end else begin
      PEEToSQLiteData(AEEData)^.CreateNewDB := FALSE;
    end;
  end else begin
    // базы ещё нет
    PEEToSQLiteData(AEEData)^.CreateNewDB := TRUE;
  end;

  with PEEToSQLiteData(AEEData)^ do begin
    // создаём новую или открываем существующую
    SQLite3Db.OpenW(FExportPath);
  end;
end;

procedure TThreadExportToSQLite.InitializeExportEnumeratorData(out AEEData: PExportEnumeratorData);
begin
  AEEData := HeapAlloc(GetProcessHeap, HEAP_ZERO_MEMORY, SizeOf(TEEToSQLiteData));
  FSQLiteAvailable := PEEToSQLiteData(AEEData)^.SQLite3Db.Init;
end;

procedure TThreadExportToSQLite.UninitializeExportEnumeratorData(var AEEData: PExportEnumeratorData);
begin
  if (nil=AEEData) then
    Exit;

  // закрываем базу
  CloseTargetStorage(AEEData);

  // убиваем объекты
  with PEEToSQLiteData(AEEData)^ do begin
    Base.Uninit;
  end;

  // освобождаем память
  HeapFree(GetProcessHeap, 0, AEEData);
  AEEData := nil;
end;

{ TThreadExportRMapsSQLite }

procedure TThreadExportRMapsSQLite.CloseTargetStorage(const AEEData: PExportEnumeratorData);
begin
  // перед закрытием надо обновить зумы
  if (AEEData<>nil) then
  with PEEToSQLiteData(AEEData)^ do
  if SQLite3Db.Opened then begin
    SQLite3Db.ExecSQL('UPDATE info SET minzoom = (SELECT DISTINCT z FROM tiles ORDER BY z ASC LIMIT 1)');
    SQLite3Db.ExecSQL('UPDATE info SET maxzoom = (SELECT DISTINCT z FROM tiles ORDER BY z DESC LIMIT 1)');
  end;
  
  // закрытие
  inherited;
end;

procedure TThreadExportRMapsSQLite.CreateTargetStorage(
  const ASourceMapType: TMapType;
  const AEEData: PExportEnumeratorData
);
begin
  // создаём или открываем БД
  inherited;

  // настраиваем текст SQL
  if FIsReplace then
    FInsertSQLText := 'REPLACE'
  else
    FInsertSQLText := 'IGNORE';

  FInsertSQLText := 'INSERT OR '+FInsertSQLText+' INTO tiles (x,y,z,s,image) VALUES (';

  // если новая - забацаем структуру
  with PEEToSQLiteData(AEEData)^ do begin
    if CreateNewDB then begin
      SQLite3Db.ExecSQL('CREATE TABLE IF NOT EXISTS tiles (x int, y int, z int, s int, image blob, PRIMARY KEY (x,y,z,s))');
      SQLite3Db.ExecSQL('CREATE TABLE IF NOT EXISTS info (maxzoom Int, minzoom Int)');
      SQLite3Db.ExecSQL('INSERT OR REPLACE INTO info (maxzoom, minzoom) VALUES (0,0)');
    end;

    SQLite3Db.SetExclusiveLockingMode;
    SQLite3Db.ExecSQL('PRAGMA synchronous=OFF');

    // открываем транзакцию для пущей скорости
    SQLite3Db.BeginTran;
  end;

(*
ещё есть
CREATE TABLE android_metadata (
  locale  text
);
с одной строкой
'en_US'
*)
end;

procedure TThreadExportRMapsSQLite.SaveTileToTargetStorage(
  const ASourceMapType: TMapType;
  const AEEData: PExportEnumeratorData;
  const AEETile: PExportEnumeratorTile;
  const ATileInfo: ITileInfoWithData
);
var
  VSQLText: AnsiString;
begin

  VSQLText := FInsertSQLText+
              ALIntToStr(AEETile^.Tile.X)+','+
              ALIntToStr(AEETile^.Tile.Y)+','+
              ALIntToStr(17-AEETile^.Zoom)+
              ',0,?)';

  PEEToSQLiteData(AEEData)^.SQLite3Db.ExecSQLWithBLOB(
    VSQLText,
    ATileInfo.TileData.Buffer,
    ATileInfo.TileData.Size
  );
end;

end.


