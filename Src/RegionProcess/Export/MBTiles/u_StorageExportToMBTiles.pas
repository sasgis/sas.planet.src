{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
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

unit u_StorageExportToMBTiles;

interface

uses
  Types,
  Windows,
  Classes,
  SysUtils,
  libsqlite3,
  i_BinaryData,
  t_GeoTypes,
  u_SQLite3Handler;

const
  cKeyValSep = '=';

type
  TSQLiteStorageMBTilesBase = class
  private
    FName: string;
    FDescription: string;
    FAttribution: string;
    FImgType: string;
    FImgFormat: string;
    FScheme: string;
    FUseXYZScheme: Boolean;

    FExportPath: string;
    FExportFileName: string;

    FSQLite3DB: TSQLite3DbHandler;
    FSQLiteAvailable: Boolean;

    FFormatSettings: TFormatSettings;
  protected
    procedure InsertMetaKeyVal(const AKey, AValue: string);

    procedure WriteMetadata(const AKeyValList: TStringList);

    function KeyValToStr(
      const AKey, AValue: string;
      const ASep: Char = cKeyValSep
    ): string; inline;

    function GetBoundsStr(const ALonLatRect: TDoubleRect): string;

    function GetCenterStr(
      const ALonLatRect: TDoubleRect;
      const AMinZoom: Byte
    ): string;

    function GetTileXY(const ATile: TPoint; const AZoom: Byte): TPoint; inline;

    procedure OpenInternal(const ATablesDDL: array of AnsiString);
  public
    constructor Create(
      const AExportPath: string;
      const AExportFileName: string;
      const AName: string;
      const ADescription: string;
      const AAttribution: string;
      const AIsLayer: Boolean;
      const AImgFormat: string;
      const AUseXYZScheme: Boolean
    );

    procedure Add(
      const ATile: TPoint;
      const AZoom: Byte;
      const AData: IBinaryData
    ); virtual; abstract;

    procedure Open(
      const ALonLatRect: TDoubleRect;
      const AZooms: TByteDynArray
    ); virtual; abstract;

    procedure Close; virtual;

    procedure CommitAndBeginTran;
  end;

  TSQLiteStorageMBTilesBaseClass = class of TSQLiteStorageMBTilesBase;

  TSQLiteStorageMBTilesClassic = class(TSQLiteStorageMBTilesBase)
  private
    FInsertStmt: TSQLite3StmtData;
    FIsInsertStmtPrepared: Boolean;
  public
    procedure Add(
      const ATile: TPoint;
      const AZoom: Byte;
      const AData: IBinaryData
    ); override;

    procedure Open(
      const ALonLatRect: TDoubleRect;
      const AZooms: TByteDynArray
    ); override;

    procedure Close; override;
  end;

  TSQLiteStorageMBTilesTileMill = class(TSQLiteStorageMBTilesBase)
  private
    FInsertMapStmt: TSQLite3StmtData;
    FInsertImagesStmt: TSQLite3StmtData;
    FIsInsertStmtPrepared: Boolean;
  public
    procedure Add(
      const ATile: TPoint;
      const AZoom: Byte;
      const AData: IBinaryData
    ); override;

    procedure Open(
      const ALonLatRect: TDoubleRect;
      const AZooms: TByteDynArray
    ); override;

    procedure Close; override;
  end;

implementation

uses
  SynCrypto,
  SynCommons,
  u_AnsiStr,
  u_GeoFunc;

const
  cCRLF = #10;

const
  // metadata
  INSERT_METADATA_SQL = 'INSERT INTO metadata (name, value) VALUES (%s,%s)';

{ TSQLiteStorageMBTilesBase }

constructor TSQLiteStorageMBTilesBase.Create(
  const AExportPath: string;
  const AExportFileName: string;
  const AName: string;
  const ADescription: string;
  const AAttribution: string;
  const AIsLayer: Boolean;
  const AImgFormat: string;
  const AUseXYZScheme: Boolean
);
begin
  inherited Create;

  FExportPath := AExportPath;
  FExportFileName := AExportFileName;

  FName := AName;
  if FName = '' then begin
    FName := 'Unnamed map';
  end;

  FDescription := ADescription;
  if FDescription = '' then begin
    FDescription := 'Created by SAS.Planet';
  end;

  FAttribution := AAttribution;

  if AIsLayer then begin
    FImgType := 'overlay';
  end else begin
    FImgType := 'baselayer';
  end;

  FImgFormat := AImgFormat;

  FUseXYZScheme := AUseXYZScheme;

  if FUseXYZScheme then begin
    FScheme := 'xyz';
  end else begin
    FScheme := 'tms';
  end;

  FFormatSettings.DecimalSeparator := '.';

  FSQLiteAvailable := FSQLite3DB.Init;
end;

procedure TSQLiteStorageMBTilesBase.Close;
begin
  if FSQLite3DB.IsOpened then begin
    FSQLite3DB.CommitTransaction;
    FSQLite3DB.Close;
  end;
end;

procedure TSQLiteStorageMBTilesBase.CommitAndBeginTran;
begin
  if FSQLite3DB.IsOpened then begin
    FSQLite3Db.CommitTransaction;
    FSQLite3Db.BeginTransaction;
  end;
end;

function TSQLiteStorageMBTilesBase.KeyValToStr(
  const AKey, AValue: string;
  const ASep: Char
): string;
begin
  Result := AKey + ASep + AValue;
end;

procedure TSQLiteStorageMBTilesBase.InsertMetaKeyVal(const AKey, AValue: string);
begin
  FSQLite3DB.ExecSQL(
    FormatA(
      INSERT_METADATA_SQL,
      [ '''' + UTF8Encode(AKey) + '''', '''' + UTF8Encode(AValue) + '''']
    )
  );
end;

procedure TSQLiteStorageMBTilesBase.WriteMetadata(const AKeyValList: TStringList);
var
  I: Integer;
  VKey, VVal: string;
begin
  FSQLite3DB.BeginTransaction;
  try
    for I := 0 to AKeyValList.Count - 1 do begin
      VKey := AKeyValList.Names[I];
      VVal := AKeyValList.ValueFromIndex[I];
      InsertMetaKeyVal(VKey, VVal);
    end;
    FSQLite3DB.CommitTransaction;
  except
    //FSQLite3DB.Rollback; // journal_mode = OFF
    raise;
  end;
end;

function TSQLiteStorageMBTilesBase.GetBoundsStr(const ALonLatRect: TDoubleRect): string;
begin
  Result :=
    Format(
      '%.8f,%.8f,%.8f,%.8f',
      [ALonLatRect.Left, ALonLatRect.Bottom, ALonLatRect.Right, ALonLatRect.Top],
      FFormatSettings
    );
end;

function TSQLiteStorageMBTilesBase.GetCenterStr(
  const ALonLatRect: TDoubleRect;
  const AMinZoom: Byte
): string;
var
  VRectCenter: TDoublePoint;
begin
  VRectCenter := RectCenter(ALonLatRect);
  Result :=
    Format(
      '%.8f, %.8f, %d',
      [VRectCenter.X, VRectCenter.Y, AMinZoom],
      FFormatSettings
    );
end;

function TSQLiteStorageMBTilesBase.GetTileXY(const ATile: TPoint; const AZoom: Byte): TPoint;
begin
  Result.X := ATile.X;

  if FUseXYZScheme then begin
    Result.Y := ATile.Y;
  end else begin
    Result.Y := (1 shl AZoom) - ATile.Y - 1;
  end;
end;

procedure TSQLiteStorageMBTilesBase.OpenInternal(const ATablesDDL: array of AnsiString);
var
  I: Integer;
  VFileName: string;
begin
  if not FSQLiteAvailable then begin
    raise ESQLite3SimpleError.Create('The SQLite3 library is not available!');
  end;

  Close;

  VFileName := FExportPath + FExportFileName;

  if FileExists(VFileName) then begin
    if not DeleteFile(VFileName) then begin
      raise ESQLite3SimpleError.CreateFmt('Can''t delete database: %s', [VFileName]);
    end;
  end;

  // open db with r/w access, UTF-8 encoding, in multi-thread mode (one connection per thread)
  FSQLite3Db.Open(VFileName, SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE or SQLITE_OPEN_NOMUTEX);

  FSQLite3DB.ExecSQL('PRAGMA locking_mode = EXCLUSIVE');
  FSQLite3DB.ExecSQL('PRAGMA synchronous = OFF');
  FSQLite3DB.ExecSQL('PRAGMA journal_mode = OFF');
  FSQLite3DB.ExecSQL('PRAGMA temp_store = MEMORY');

  for I := Low(ATablesDDL) to High(ATablesDDL) do begin
    FSQLite3DB.ExecSQL(ATablesDDL[I]);
  end;
end;

{ TSQLiteStorageMBTilesClassic }

const
  CMBTilesDDL: array [0..2] of AnsiString = (
    // metadata
    'CREATE TABLE IF NOT EXISTS metadata (name text, value text)',
    'CREATE UNIQUE INDEX IF NOT EXISTS metadata_idx ON metadata (name)',
    // tiles
    'CREATE TABLE IF NOT EXISTS tiles (zoom_level integer, tile_column integer, tile_row integer, tile_data blob)'
  );

  INSERT_TILES_SQL = 'INSERT OR REPLACE INTO tiles (zoom_level, tile_column, tile_row, tile_data) VALUES (?,?,?,?)';

  CREATE_INDEX_SQL = 'CREATE INDEX IF NOT EXISTS tiles_idx on tiles (zoom_level, tile_column, tile_row)';

procedure TSQLiteStorageMBTilesClassic.Open(
  const ALonLatRect: TDoubleRect;
  const AZooms: Types.TByteDynArray
);
var
  VMetadata: TStringList;
begin
  OpenInternal(CMBTilesDDL);

  VMetadata := TStringList.Create;
  try
    VMetadata.NameValueSeparator := cKeyValSep;

    // base fields of MBTiles format
    // https://github.com/mapbox/mbtiles-spec/blob/master/1.3/spec.md

    // 1.0
    VMetadata.Add( KeyValToStr('name', FName) );
    VMetadata.Add( KeyValToStr('type', FImgType) );
    VMetadata.Add( KeyValToStr('version', '1.3') );
    VMetadata.Add( KeyValToStr('description', FDescription) );

    // 1.1
    VMetadata.Add( KeyValToStr('format', FImgFormat) );
    VMetadata.Add( KeyValToStr('bounds', GetBoundsStr(ALonLatRect)) );

    // 1.2
    VMetadata.Add( KeyValToStr('attribution', FAttribution) );

    // 1.3
    VMetadata.Add( KeyValToStr('minzoom', IntToStr(AZooms[Low(AZooms)])) );
    VMetadata.Add( KeyValToStr('maxzoom', IntToStr(AZooms[High(AZooms)])) );
    VMetadata.Add( KeyValToStr('center', GetCenterStr(ALonLatRect, AZooms[Low(AZooms)])) );

    // additional fields from TileJSON standart
    // https://github.com/mapbox/tilejson-spec/tree/master/2.1.0

    VMetadata.Add( KeyValToStr('scheme', FScheme) );

    WriteMetadata(VMetadata)
  finally
    VMetadata.Free;
  end;

  FIsInsertStmtPrepared := FSQLite3DB.PrepareStatement(@FInsertStmt, INSERT_TILES_SQL);
  if not FIsInsertStmtPrepared then begin
    FSQLite3DB.RaiseSQLite3Error;
  end;

  FSQLite3DB.BeginTransaction;
end;

procedure TSQLiteStorageMBTilesClassic.Close;
begin
  if not FSQLite3DB.IsOpened then begin
    Exit;
  end;

  if FIsInsertStmtPrepared then begin
    FInsertStmt.ClearBindings;
    FSQLite3DB.ClosePrepared(@FInsertStmt);
  end;

  FSQLite3DB.CommitTransaction;

  // create index at the very end to speed up insertion
  FSQLite3DB.ExecSql(CREATE_INDEX_SQL);

  FSQLite3DB.Close;
end;

procedure TSQLiteStorageMBTilesClassic.Add(
  const ATile: TPoint;
  const AZoom: Byte;
  const AData: IBinaryData
);
var
  VTile: TPoint;
  VBindResult: Boolean;
begin
  Assert(AData <> nil);
  Assert(FIsInsertStmtPrepared);

  VTile := Self.GetTileXY(ATile, AZoom);

  VBindResult :=
    FInsertStmt.BindInt(1, AZoom) and
    FInsertStmt.BindInt(2, VTile.X) and
    FInsertStmt.BindInt(3, VTile.Y) and
    FInsertStmt.BindBlob(4, AData.Buffer, AData.Size);

  if not VBindResult then begin
    FSQLite3DB.RaiseSQLite3Error;
  end;

  if not FSQLite3DB.ExecPrepared(@FInsertStmt) then begin
    FSQLite3DB.RaiseSQLite3Error;
  end;
end;

{ TSQLiteStorageMBTilesTileMill }

const
  cTileMillDDL: array [0..14] of AnsiString = (

    // grid_key
    'CREATE TABLE grid_key (grid_id TEXT, key_name TEXT);',
    'CREATE UNIQUE INDEX grid_key_lookup ON grid_key (grid_id, key_name);',

    // grid_utfgrid
    'CREATE TABLE grid_utfgrid (grid_id TEXT, grid_utfgrid BLOB);',
    'CREATE UNIQUE INDEX grid_utfgrid_lookup ON grid_utfgrid (grid_id);',

    // images
    'CREATE TABLE images (tile_data blob, tile_id text);',
    'CREATE UNIQUE INDEX images_id ON images (tile_id);',

    // keymap
    'CREATE TABLE keymap (key_name TEXT, key_json TEXT);',
    'CREATE UNIQUE INDEX keymap_lookup ON keymap (key_name);',

    // map
    'CREATE TABLE map ('                                              + cCRLF +
    '   zoom_level INTEGER,'                                          + cCRLF +
    '   tile_column INTEGER,'                                         + cCRLF +
    '   tile_row INTEGER,'                                            + cCRLF +
    '   tile_id TEXT,'                                                + cCRLF +
    '   grid_id TEXT'                                                 + cCRLF +
    ');',

    'CREATE UNIQUE INDEX map_index ON map (zoom_level, tile_column, tile_row);',

    // metadata
    'CREATE TABLE metadata (name text, value text);',
    'CREATE UNIQUE INDEX name ON metadata (name);',

    // grid_data
    'CREATE VIEW grid_data AS'                                        + cCRLF +
    '    SELECT'                                                      + cCRLF +
    '        map.zoom_level AS zoom_level,'                           + cCRLF +
    '        map.tile_column AS tile_column,'                         + cCRLF +
    '        map.tile_row AS tile_row,'                               + cCRLF +
    '        keymap.key_name AS key_name,'                            + cCRLF +
    '        keymap.key_json AS key_json'                             + cCRLF +
    '    FROM map'                                                    + cCRLF +
    '    JOIN grid_key ON map.grid_id = grid_key.grid_id'             + cCRLF +
    '    JOIN keymap ON grid_key.key_name = keymap.key_name;',

    // grids
    'CREATE VIEW grids AS'                                            + cCRLF +
    '    SELECT'                                                      + cCRLF +
    '        map.zoom_level AS zoom_level,'                           + cCRLF +
    '        map.tile_column AS tile_column,'                         + cCRLF +
    '        map.tile_row AS tile_row,'                               + cCRLF +
    '        grid_utfgrid.grid_utfgrid AS grid'                       + cCRLF +
    '    FROM map'                                                    + cCRLF +
    '    JOIN grid_utfgrid ON grid_utfgrid.grid_id = map.grid_id;',

    // tiles
    'CREATE VIEW tiles AS'                                            + cCRLF +
    '    SELECT'                                                      + cCRLF +
    '        map.zoom_level AS zoom_level,'                           + cCRLF +
    '        map.tile_column AS tile_column,'                         + cCRLF +
    '        map.tile_row AS tile_row,'                               + cCRLF +
    '        images.tile_data AS tile_data'                           + cCRLF +
    '    FROM map'                                                    + cCRLF +
    '    JOIN images ON images.tile_id = map.tile_id;'
  );

const
  INSERT_IMAGES_SQL = 'INSERT OR REPLACE INTO images (tile_id, tile_data) VALUES (?,?)';
  INSERT_MAP_SQL = 'INSERT OR REPLACE INTO map (zoom_level, tile_column, tile_row, tile_id) VALUES (?,?,?,?)';

procedure TSQLiteStorageMBTilesTileMill.Open(
  const ALonLatRect: TDoubleRect;
  const AZooms: Types.TByteDynArray
);
var
  VMetadata: TStringList;
begin
  OpenInternal(cTileMillDDL);

  VMetadata := TStringList.Create;
  try
    VMetadata.NameValueSeparator := cKeyValSep;

    VMetadata.Add( KeyValToStr('name', FName) );
    VMetadata.Add( KeyValToStr('type', FImgType) );
    VMetadata.Add( KeyValToStr('version', '1.0.0') );
    VMetadata.Add( KeyValToStr('description', FDescription) );
    VMetadata.Add( KeyValToStr('format', FImgFormat) );
    VMetadata.Add( KeyValToStr('bounds', GetBoundsStr(ALonLatRect)) );
    VMetadata.Add( KeyValToStr('attribution', FAttribution) );
    VMetadata.Add( KeyValToStr('scheme', FScheme) );
    VMetadata.Add( KeyValToStr('minzoom', IntToStr(AZooms[Low(AZooms)])) );
    VMetadata.Add( KeyValToStr('maxzoom', IntToStr(AZooms[High(AZooms)])) );
    VMetadata.Add( KeyValToStr('center', GetCenterStr(ALonLatRect, AZooms[Low(AZooms)])) );
    VMetadata.Add( KeyValToStr('template', '') );

    WriteMetadata(VMetadata)
  finally
    VMetadata.Free;
  end;

  FIsInsertStmtPrepared :=
    FSQLite3DB.PrepareStatement(@FInsertMapStmt, INSERT_MAP_SQL) and
    FSQLite3DB.PrepareStatement(@FInsertImagesStmt, INSERT_IMAGES_SQL);

  if not FIsInsertStmtPrepared then begin
    FSQLite3DB.RaiseSQLite3Error;
  end;

  FSQLite3DB.BeginTransaction;
end;

procedure TSQLiteStorageMBTilesTileMill.Close;
begin
  if FIsInsertStmtPrepared then begin
    FInsertMapStmt.ClearBindings;
    FInsertImagesStmt.ClearBindings;

    FSQLite3DB.ClosePrepared(@FInsertMapStmt);
    FSQLite3DB.ClosePrepared(@FInsertImagesStmt);
  end;

  inherited Close;
end;

procedure TSQLiteStorageMBTilesTileMill.Add(
  const ATile: TPoint;
  const AZoom: Byte;
  const AData: IBinaryData
);
var
  VTile: TPoint;
  VTileID: RawUTF8;
  VBindResult: Boolean;
begin
  Assert(AData <> nil);
  Assert(FIsInsertStmtPrepared);

  VTileID := MD5DigestToString(MD5Buf(AData.Buffer^, AData.Size));

  // insert data into 'images' table

  VBindResult :=
    FInsertImagesStmt.BindText(1, PAnsiChar(VTileID), Length(VTileID)) and
    FInsertImagesStmt.BindBlob(2, AData.Buffer, AData.Size);

  if not VBindResult then begin
    FSQLite3DB.RaiseSQLite3Error;
  end;

  if not FSQLite3DB.ExecPrepared(@FInsertImagesStmt) then begin
    FSQLite3DB.RaiseSQLite3Error;
  end;

  // insert data into 'map' table

  VTile := Self.GetTileXY(ATile, AZoom);

  VBindResult :=
    FInsertMapStmt.BindInt(1, AZoom) and
    FInsertMapStmt.BindInt(2, VTile.X) and
    FInsertMapStmt.BindInt(3, VTile.Y) and
    FInsertMapStmt.BindText(4, PAnsiChar(VTileID), Length(VTileID));

  if not VBindResult then begin
    FSQLite3DB.RaiseSQLite3Error;
  end;

  if not FSQLite3DB.ExecPrepared(@FInsertMapStmt) then begin
    FSQLite3DB.RaiseSQLite3Error;
  end;
end;

end.
