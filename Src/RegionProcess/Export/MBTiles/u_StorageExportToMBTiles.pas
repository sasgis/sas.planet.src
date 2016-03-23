{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2016, SAS.Planet development team.                      *}
{* This program is free software: you can redistribute it and/or modify       *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* This program is distributed in the hope that it will be useful,            *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *}
{*                                                                            *}
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

unit u_StorageExportToMBTiles;

interface

uses
  Types,
  Classes,
  SysUtils,
  SQLite3Handler,
  i_BinaryData,
  t_GeoTypes;

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
    ): string;

    function GetBoundsStr(const ALonLatRect: TDoubleRect): string;

    function GetCenterStr(
      const ALonLatRect: TDoubleRect;
      const AMinZoom: Byte
    ): string;

    procedure OpenInternal(const ATablesDDL: array of AnsiString);
  public
    procedure Init(
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

    procedure Close;
  end;

  TSQLiteStorageMBTilesClassic = class(TSQLiteStorageMBTilesBase)
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
  end;

  TSQLiteStorageMBTilesTileMill = class(TSQLiteStorageMBTilesBase)
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
  end;

implementation

uses
  ALString,
  ALSqlite3Wrapper,
  MD5,
  u_GeoFunc;

const
  cCRLF = #10;

const
  // metadata
  INSERT_METADATA_SQL = 'INSERT INTO metadata (name, value) VALUES (%s,%s)';

{ TSQLiteStorageMBTilesBase }

procedure TSQLiteStorageMBTilesBase.Init(
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
  if FSQLite3DB.Opened then begin
    FSQLite3DB.Commit;
    FSQLite3DB.Close;
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
    ALFormat(
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
  FSQLite3DB.BeginTran;
  try
    for I := 0 to AKeyValList.Count - 1 do begin
      VKey := AKeyValList.Names[I];
      VVal := AKeyValList.ValueFromIndex[I];
      InsertMetaKeyVal(VKey, VVal);
    end;
    FSQLite3DB.Commit;
  except
    FSQLite3DB.Rollback;
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

procedure TSQLiteStorageMBTilesBase.OpenInternal(const ATablesDDL: array of AnsiString);
var
  I: Integer;
  VFileName: string;
begin
  if not FSQLiteAvailable then begin
    raise ESQLite3SimpleError.Create('SQLite not available');
  end;

  Close;

  VFileName := FExportPath + FExportFileName;

  if FileExists(VFileName) then begin
    if not DeleteFile(VFileName) then begin
      raise ESQLite3SimpleError.CreateFmt('Can''t delete database: %s', [VFileName]);
    end;
  end;

  // open db with r/w access and UTF-8 encoding
  FSQLite3Db.Open(VFileName, SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE);

  FSQLite3DB.SetExclusiveLockingMode;
  FSQLite3DB.ExecSQL('PRAGMA synchronous=OFF');

  for I := Low(ATablesDDL) to High(ATablesDDL) do begin
    FSQLite3DB.ExecSQL(ATablesDDL[I]);
  end;
end;

{ TSQLiteStorageMBTilesClassic }

const
  cMBTilesDDL: array [0..3] of AnsiString = (
    // metadata
    'CREATE TABLE IF NOT EXISTS metadata (name text, value text)',
    'CREATE UNIQUE INDEX IF NOT EXISTS metadata_idx  ON metadata (name)',
    // tiles
    'CREATE TABLE IF NOT EXISTS tiles (zoom_level integer, tile_column integer, tile_row integer, tile_data blob)',
    'CREATE INDEX IF NOT EXISTS tiles_idx on tiles (zoom_level, tile_column, tile_row)'
  );

const
  INSERT_TILES_SQL = 'INSERT OR REPLACE INTO tiles (zoom_level, tile_column, tile_row, tile_data) VALUES (%d,%d,%d,?)';

procedure TSQLiteStorageMBTilesClassic.Open(
  const ALonLatRect: TDoubleRect;
  const AZooms: TByteDynArray
);
var
  VMetadata: TStringList;
begin
  OpenInternal(cMBTilesDDL);

  VMetadata := TStringList.Create;
  try
    VMetadata.NameValueSeparator := cKeyValSep;

    // base fields of MBTiles format
    // https://github.com/mapbox/mbtiles-spec/blob/master/1.2/spec.md

    // 1.0
    VMetadata.Add( KeyValToStr('name', FName) );
    VMetadata.Add( KeyValToStr('type', FImgType) );
    VMetadata.Add( KeyValToStr('version', '1.2') );
    VMetadata.Add( KeyValToStr('description', FDescription) );

    // 1.1
    VMetadata.Add( KeyValToStr('format', FImgFormat) );
    VMetadata.Add( KeyValToStr('bounds', GetBoundsStr(ALonLatRect)) );

    // 1.2
    VMetadata.Add( KeyValToStr('attribution', FAttribution) );

    // additional fiels from TileJSON standart
    // https://github.com/mapbox/tilejson-spec/tree/master/2.1.0

    VMetadata.Add( KeyValToStr('scheme', FScheme) );
    VMetadata.Add( KeyValToStr('minzoom', IntToStr(AZooms[Low(AZooms)])) );
    VMetadata.Add( KeyValToStr('maxzoom', IntToStr(AZooms[High(AZooms)])) );
    VMetadata.Add( KeyValToStr('center', GetCenterStr(ALonLatRect, AZooms[Low(AZooms)])) );

    WriteMetadata(VMetadata)
  finally
    VMetadata.Free;
  end;

  FSQLite3DB.BeginTran;
end;

procedure TSQLiteStorageMBTilesClassic.Add(
  const ATile: TPoint;
  const AZoom: Byte;
  const AData: IBinaryData
);
var
  X, Y: Integer;
begin
  Assert(AData <> nil);

  X := ATile.X;

  if FUseXYZScheme then begin
    Y := ATile.Y;
  end else begin
    Y := (1 shl AZoom) - ATile.Y - 1;
  end;

  FSQLite3DB.ExecSQLWithBLOB(
    ALFormat(INSERT_TILES_SQL, [AZoom, X, Y]),
    AData.Buffer,
    AData.Size
  );
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
  INSERT_IMAGES_SQL = 'INSERT OR REPLACE INTO images (tile_id, tile_data) VALUES (%s,?)';
  INSERT_MAP_SQL = 'INSERT OR REPLACE INTO map (zoom_level, tile_column, tile_row, tile_id) VALUES (%d,%d,%d,%s)';

procedure TSQLiteStorageMBTilesTileMill.Open(
  const ALonLatRect: TDoubleRect;
  const AZooms: TByteDynArray
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
    //VMetadata.Add( KeyValToStr('scheme', FScheme) );
    VMetadata.Add( KeyValToStr('minzoom', IntToStr(AZooms[Low(AZooms)])) );
    VMetadata.Add( KeyValToStr('maxzoom', IntToStr(AZooms[High(AZooms)])) );
    VMetadata.Add( KeyValToStr('center', GetCenterStr(ALonLatRect, AZooms[Low(AZooms)])) );
    VMetadata.Add( KeyValToStr('template', '') );

    WriteMetadata(VMetadata)
  finally
    VMetadata.Free;
  end;

  FSQLite3DB.BeginTran;
end;

procedure TSQLiteStorageMBTilesTileMill.Add(
  const ATile: TPoint;
  const AZoom: Byte;
  const AData: IBinaryData
);
var
  VMD5: string;
  VTileID: AnsiString;
begin
  VMD5 := MD5DigestToStr(MD5Buffer(AData.Buffer^, AData.Size));
  VTileID := AnsiString('''' + LowerCase(VMD5) + '''');

  // insert blob into 'tiles' table
  FSQLite3DB.ExecSQLWithBLOB(
    ALFormat(INSERT_IMAGES_SQL, [VTileID]),
    AData.Buffer,
    AData.Size
  );

  // insert coordinates into 'map' table
  FSQLite3DB.ExecSQL(
    ALFormat(INSERT_MAP_SQL, [AZoom, ATile.X, ATile.Y, VTileID])
  );
end;

end.
