{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_TileStorageSQLiteFileConnectionMBTiles;

interface

uses
  Types,
  SysUtils,
  StrUtils,
  i_ContentTypeInfo,
  i_TileStorageSQLiteFileInfo,
  u_TileStorageSQLiteFileConnection;

type
  TTileStorageSQLiteFileConnectionMBTiles = class(TTileStorageSQLiteFileConnection)
  protected
    procedure CreateTables; override;
    procedure FetchMetadata; override;
  public
    constructor Create(
      const AIsReadOnly: Boolean;
      const AFileName: string;
      const AFileInfo: ITileStorageSQLiteFileInfo;
      const AMainContentType: IContentTypeInfoBasic
    );
  end;

implementation

uses
  libsqlite3,
  i_BinaryData,
  u_SQLite3Handler;

type
  TTileDataConnectionStatementMBTiles = class(TTileDataConnectionStatement)
  public
    function BindParams(X, Y, Z: Integer): Boolean; override;
    procedure GetResult(out ABlob: IBinaryData); override;
    constructor Create(const AIsInvertedY, AIsInvertedZ: Boolean);
  end;

  TTileInfoConnectionStatementMBTiles = class(TTileInfoConnectionStatement)
  public
    function BindParams(X, Y, Z: Integer): Boolean; override;
    procedure GetResult(out ABlobSize: Integer); override;
    constructor Create(const AIsInvertedY, AIsInvertedZ: Boolean);
  end;

  TRectInfoConnectionStatementMBTiles = class(TRectInfoConnectionStatement)
  public
    function BindParams(const ARect: TRect; Z: Integer): Boolean; override;
    procedure GetResult(out X, Y: Integer; out ASize: Integer); override;
    constructor Create(const AIsInvertedY, AIsInvertedZ: Boolean);
  end;

  TEnumTilesConnectionStatementMBTiles = class(TEnumTilesConnectionStatement)
  public
    procedure GetResult(out X, Y, Z: Integer; out ABlob: IBinaryData); override;
    constructor Create(const AIsInvertedY, AIsInvertedZ: Boolean);
  end;

  TInsertOrReplaceConnectionStatementMBTiles = class(TInsertOrReplaceConnectionStatement)
  public
    function BindParams(X, Y, Z: Integer; const ABlob: IBinaryData): Boolean; override;
    constructor Create(const AIsInvertedY, AIsInvertedZ: Boolean);
  end;

  TInsertOrIgnoreConnectionStatementMBTiles = class(TInsertOrIgnoreConnectionStatement)
  public
    function BindParams(X, Y, Z: Integer; const ABlob: IBinaryData): Boolean; override;
    constructor Create(const AIsInvertedY, AIsInvertedZ: Boolean);
  end;

  TDeleteTileConnectionStatementMBTiles = class(TDeleteTileConnectionStatement)
  public
    function BindParams(X, Y, Z: Integer): Boolean; override;
    constructor Create(const AIsInvertedY, AIsInvertedZ: Boolean);
  end;

function InvertY(Y, Z: Integer): Integer; inline; // same for Revert
begin
  Result := (1 shl Z) - Y - 1;
end;

{ TTileStorageSQLiteFileConnectionMBTiles }

constructor TTileStorageSQLiteFileConnectionMBTiles.Create(
  const AIsReadOnly: Boolean;
  const AFileName: string;
  const AFileInfo: ITileStorageSQLiteFileInfo;
  const AMainContentType: IContentTypeInfoBasic
);
var
  VValue: string;
  VIsInvertedY: Boolean;
  VIsInvertedZ: Boolean;
begin
  inherited Create(AIsReadOnly, AFileName, AFileInfo, AMainContentType);

  Assert(FFileInfo <> nil);

  if FFileInfo.TryGetMetadataValue('format', VValue) then begin
    if not SameText(FMainContentType.GetDefaultExt, '.' + VValue) then begin
      raise Exception.CreateFmt(
        'MBTiles: The detected tile format "%s" does not match the provided content type "%s"',
        [VValue, FMainContentType.GetContentType]
      );
    end;
  end else begin
    raise Exception.Create('MBTiles: "format" value is not specified!');
  end;

  if FFileInfo.TryGetMetadataValue('scheme', VValue) and SameText(VValue, 'xyz') then begin
    VIsInvertedY := False;
  end else begin
    VIsInvertedY := True;
  end;

  VIsInvertedZ := False;

  // read access
  FTileDataStmt := TTileDataConnectionStatementMBTiles.Create(VIsInvertedY, VIsInvertedZ);
  FTileInfoStmt := TTileInfoConnectionStatementMBTiles.Create(VIsInvertedY, VIsInvertedZ);
  FRectInfoStmt := TRectInfoConnectionStatementMBTiles.Create(VIsInvertedY, VIsInvertedZ);
  FEnumTilesStmt := TEnumTilesConnectionStatementMBTiles.Create(VIsInvertedY, VIsInvertedZ);

  // write access
  if not FIsReadOnly then begin
    FInsertOrReplaceStmt := TInsertOrReplaceConnectionStatementMBTiles.Create(VIsInvertedY, VIsInvertedZ);
    FInsertOrIgnoreStmt := TInsertOrIgnoreConnectionStatementMBTiles.Create(VIsInvertedY, VIsInvertedZ);
    FDeleteTileStmt := TDeleteTileConnectionStatementMBTiles.Create(VIsInvertedY, VIsInvertedZ);
  end;

  FEnabled :=
    FTileDataStmt.CheckPrepared(FSQLite3DB) and
    FTileInfoStmt.CheckPrepared(FSQLite3DB);
end;

procedure TTileStorageSQLiteFileConnectionMBTiles.CreateTables;
const
  CSqlTextCreate: array [0..3] of UTF8String = (
    // metadata
    'CREATE TABLE metadata (name text, value text)',
    'CREATE UNIQUE INDEX metadata_idx ON metadata (name)',
    // tiles
    'CREATE TABLE tiles (zoom_level integer, tile_column integer, tile_row integer, tile_data blob)',
    'CREATE INDEX tiles_idx on tiles (zoom_level, tile_column, tile_row)'
  );

  CSqlTextInsert: UTF8String = 'INSERT INTO metadata (name, value) VALUES (?,?)';

  procedure InsertMetadataKeyVal(const AKey, AVal: UTF8String; const AStmtData: TSQLite3StmtData);
  begin
    if AStmtData.BindText(1, PUTF8Char(AKey), Length(AKey)) and
       AStmtData.BindText(2, PUTF8Char(AVal), Length(AVal))
    then begin
      try
        if sqlite3_step(AStmtData.Stmt) <> SQLITE_DONE then begin
          FSQLite3DB.RaiseSQLite3Error;
        end;
      finally
        AStmtData.Reset;
      end;
    end else begin
      FSQLite3DB.RaiseSQLite3Error;
    end;
  end;

  function GetFormatStr: string;
  begin
    Result := Copy(FMainContentType.GetDefaultExt, 2);
    if Result = 'jpeg' then begin
      Result := 'jpg';
      Exit;
    end;
    if (Result <> 'png') and (Result <> 'jpg') and (Result <> 'webp') and (Result <> 'pbf') then begin
      Result := FMainContentType.GetContentType;
    end;
  end;

var
  I: Integer;
  VStmtData: TSQLite3StmtData;
begin
  // https://github.com/mapbox/mbtiles-spec/blob/master/1.3/spec.md

  // create tables and indexes
  for I := 0 to Length(CSqlTextCreate) - 1 do begin
    FSQLite3DB.ExecSql(CSqlTextCreate[I]);
  end;

  // initialize metadata
  if not FSQLite3Db.PrepareStatement(@VStmtData, CSqlTextInsert) then begin
    FSQLite3Db.RaiseSQLite3Error;
  end;

  InsertMetadataKeyVal('name', 'unnamed', VStmtData);
  InsertMetadataKeyVal('format', GetFormatStr, VStmtData);

  // todo
end;

procedure TTileStorageSQLiteFileConnectionMBTiles.FetchMetadata;
var
  VStmtData: TSQLite3StmtData;
  VName, VValue: string;
begin
  Assert(FFileInfo <> nil);

  if not FSQLite3Db.PrepareStatement(@VStmtData, 'SELECT name, value FROM metadata') then begin
    FSQLite3Db.RaiseSQLite3Error;
  end;

  try
    while sqlite3_step(VStmtData.Stmt) = SQLITE_ROW do begin
      VName := LowerCase(VStmtData.ColumnAsString(0));
      VValue := VStmtData.ColumnAsString(1);

      FFileInfo.AddOrSetMetadataValue(VName, VValue);
    end;
  finally
    VStmtData.Fin;
  end;
end;

{ TTileDataConnectionStatementMBTiles }

constructor TTileDataConnectionStatementMBTiles.Create(const AIsInvertedY, AIsInvertedZ: Boolean);
begin
  inherited;
  FText :=
    'SELECT tile_data FROM tiles WHERE zoom_level = ? AND tile_column = ? AND tile_row = ?';
end;

function TTileDataConnectionStatementMBTiles.BindParams(X, Y, Z: Integer): Boolean;
begin
  if FIsInvertedY then begin
    Y := InvertY(Y, Z);
  end;

  Result :=
    FStmt.BindInt(1, Z) and
    FStmt.BindInt(2, X) and
    FStmt.BindInt(3, Y);
end;

procedure TTileDataConnectionStatementMBTiles.GetResult(out ABlob: IBinaryData);
begin
  ABlob := BlobToBinaryData(0);
end;

{ TTileInfoConnectionStatementMBTiles }

constructor TTileInfoConnectionStatementMBTiles.Create(const AIsInvertedY, AIsInvertedZ: Boolean);
begin
  inherited;
  FText :=
    'SELECT length(tile_data) FROM tiles WHERE zoom_level = ? AND tile_column = ? AND tile_row = ?';
end;

function TTileInfoConnectionStatementMBTiles.BindParams(X, Y, Z: Integer): Boolean;
begin
  if FIsInvertedY then begin
    Y := InvertY(Y, Z);
  end;

  Result :=
    FStmt.BindInt(1, Z) and
    FStmt.BindInt(2, X) and
    FStmt.BindInt(3, Y);
end;

procedure TTileInfoConnectionStatementMBTiles.GetResult(out ABlobSize: Integer);
begin
  ABlobSize := FStmt.ColumnInt(0);
end;

{ TRectInfoConnectionStatementMBTiles }

constructor TRectInfoConnectionStatementMBTiles.Create(const AIsInvertedY, AIsInvertedZ: Boolean);
begin
  inherited;
  FText :=
    'SELECT tile_column, tile_row, length(tile_data) FROM tiles ' +
    'WHERE zoom_level = ? AND tile_column >= ? AND tile_column < ? AND ' +
    IfThen(AIsInvertedY, 'tile_row <= ? AND tile_row > ?', 'tile_row >= ? AND tile_row < ?');

  FZoom := -1;
end;

function TRectInfoConnectionStatementMBTiles.BindParams(const ARect: TRect; Z: Integer): Boolean;
var
  VTop, VBottom: Integer;
begin
  FZoom := Z; // used in GetResult

  VTop := ARect.Top;
  VBottom := ARect.Bottom;

  if FIsInvertedY then begin
    VTop := InvertY(VTop, Z);
    VBottom := InvertY(VBottom, Z);
  end;

  Result :=
    FStmt.BindInt(1, Z) and
    FStmt.BindInt(2, ARect.Left) and
    FStmt.BindInt(3, ARect.Right) and
    FStmt.BindInt(4, VTop) and
    FStmt.BindInt(5, VBottom);
end;

procedure TRectInfoConnectionStatementMBTiles.GetResult(out X, Y, ASize: Integer);
begin
  X := FStmt.ColumnInt(0);
  Y := FStmt.ColumnInt(1);
  ASize := FStmt.ColumnInt(2);

  if FIsInvertedY then begin
    Assert(FZoom >= 0);
    Y := InvertY(Y, FZoom);
  end;
end;

{ TEnumTilesConnectionStatementMBTiles }

constructor TEnumTilesConnectionStatementMBTiles.Create(const AIsInvertedY, AIsInvertedZ: Boolean);
begin
  inherited;
  FText := 'SELECT zoom_level, tile_column, tile_row, tile_data FROM tiles';
end;

procedure TEnumTilesConnectionStatementMBTiles.GetResult(out X, Y, Z: Integer; out ABlob: IBinaryData);
begin
  Z := FStmt.ColumnInt(0);
  X := FStmt.ColumnInt(1);
  Y := FStmt.ColumnInt(2);
  ABlob := BlobToBinaryData(3);

  if FIsInvertedY then begin
    Y := InvertY(Y, Z);
  end;
end;

{ TInsertOrReplaceConnectionStatementMBTiles }

constructor TInsertOrReplaceConnectionStatementMBTiles.Create(const AIsInvertedY, AIsInvertedZ: Boolean);
begin
  inherited;
  FText := 'INSERT OR REPLACE INTO tiles (zoom_level, tile_column, tile_row, tile_data) VALUES (?,?,?,?)';
end;

function TInsertOrReplaceConnectionStatementMBTiles.BindParams(X, Y, Z: Integer; const ABlob: IBinaryData): Boolean;
begin
  Assert(ABlob <> nil);

  if FIsInvertedY then begin
    Y := InvertY(Y, Z);
  end;

  Result :=
    FStmt.BindInt(1, Z) and
    FStmt.BindInt(2, X) and
    FStmt.BindInt(3, Y) and
    FStmt.BindBlob(4, ABlob.Buffer, ABlob.Size);
end;

{ TInsertOrIgnoreConnectionStatementMBTiles }

constructor TInsertOrIgnoreConnectionStatementMBTiles.Create(const AIsInvertedY, AIsInvertedZ: Boolean);
begin
  inherited;
  FText := 'INSERT OR IGNORE INTO tiles (zoom_level, tile_column, tile_row, tile_data) VALUES (?,?,?,?)';
end;

function TInsertOrIgnoreConnectionStatementMBTiles.BindParams(X, Y, Z: Integer; const ABlob: IBinaryData): Boolean;
begin
  Assert(ABlob <> nil);

  if FIsInvertedY then begin
    Y := InvertY(Y, Z);
  end;

  Result :=
    FStmt.BindInt(1, Z) and
    FStmt.BindInt(2, X) and
    FStmt.BindInt(3, Y) and
    FStmt.BindBlob(4, ABlob.Buffer, ABlob.Size);
end;

{ TDeleteTileConnectionStatementMBTiles }

constructor TDeleteTileConnectionStatementMBTiles.Create(const AIsInvertedY, AIsInvertedZ: Boolean);
begin
  inherited;
  FText := 'DELETE FROM tiles WHERE zoom_level = ? AND tile_column = ? AND tile_row = ?';
end;

function TDeleteTileConnectionStatementMBTiles.BindParams(X, Y, Z: Integer): Boolean;
begin
  if FIsInvertedY then begin
    Y := InvertY(Y, Z);
  end;

  Result :=
    FStmt.BindInt(1, Z) and
    FStmt.BindInt(2, X) and
    FStmt.BindInt(3, Y);
end;

end.
