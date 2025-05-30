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
  t_GeoTypes,
  i_ContentTypeInfo,
  i_ProjectionSet,
  i_TileStorageSQLiteFileInfo,
  u_SQLite3Handler,
  u_TileStorageSQLiteFileConnection;

type
  TMetadataConnectionStatementMBTiles = class(TConnectionStatement)
  private
    FSQLite3DB: TSQLite3DbHandler;
  public
    procedure ExecUpsert(const AName, AValue: UTF8String);
    constructor Create(const ASQLite3DB: TSQLite3DbHandler);
  end;

  TTileStorageSQLiteFileConnectionMBTiles = class(TTileStorageSQLiteFileConnection)
  private
    FBounds: TDoubleRect;
    FMinZoom: Integer;
    FMaxZoom: Integer;
    FMetadataStmt: TMetadataConnectionStatementMBTiles;
    FFormatSettings: TFormatSettings;
  protected
    procedure CreateTables; override;
    procedure FetchMetadata; override;
    procedure UpdateMetadata(const AXY: TPoint; const AZoom: Byte); override;
  public
    constructor Create(
      const AIsReadOnly: Boolean;
      const AFileName: string;
      const AFileInfo: ITileStorageSQLiteFileInfo;
      const AMainContentType: IContentTypeInfoBasic;
      const AProjectionSet: IProjectionSet
    );
    destructor Destroy; override;
  end;

implementation

uses
  IOUtils,
  libsqlite3,
  i_BinaryData,
  u_GeoFunc,
  u_GeoToStrFunc;

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
  const AMainContentType: IContentTypeInfoBasic;
  const AProjectionSet: IProjectionSet
);
var
  VValue: string;
  VBounds: TStringDynArray;
  VIsInvertedY: Boolean;
  VIsInvertedZ: Boolean;
begin
  inherited Create(AIsReadOnly, AFileName, AFileInfo, AMainContentType, AProjectionSet);

  FFormatSettings.DecimalSeparator := '.';

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

  if FFileInfo.TryGetMetadataValue('bounds', VValue) then begin
    VBounds := SplitString(VValue, ',');
    if Length(VBounds) = 4 then begin
      FBounds.Left   := StrPointToFloat(Trim(VBounds[0]));
      FBounds.Bottom := StrPointToFloat(Trim(VBounds[1]));
      FBounds.Right  := StrPointToFloat(Trim(VBounds[2]));
      FBounds.Top    := StrPointToFloat(Trim(VBounds[3]));
    end else begin
      raise Exception.CreateFmt('MBTiles: Invalid bounds value: "%s"', [VValue]);
    end;
  end else begin
    FBounds := DoubleRect(0, 0, 0, 0);
  end;

  if FFileInfo.TryGetMetadataValue('minzoom', VValue) then begin
    FMinZoom := StrToInt(VValue);
  end else begin
    FMinZoom := -1;
  end;

  if FFileInfo.TryGetMetadataValue('maxzoom', VValue) then begin
    FMaxZoom := StrToInt(VValue);
  end else begin
    FMaxZoom := -1;
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

    FMetadataStmt := TMetadataConnectionStatementMBTiles.Create(FSQLite3DB);
  end;

  FEnabled :=
    FTileDataStmt.CheckPrepared(FSQLite3DB) and
    FTileInfoStmt.CheckPrepared(FSQLite3DB);
end;

destructor TTileStorageSQLiteFileConnectionMBTiles.Destroy;
begin
  FreeAndNil(FMetadataStmt);
  inherited Destroy;
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
  VMetadataStmt: TMetadataConnectionStatementMBTiles;
begin
  // https://github.com/mapbox/mbtiles-spec/blob/master/1.3/spec.md

  // create tables and indexes
  for I := 0 to Length(CSqlTextCreate) - 1 do begin
    FSQLite3DB.ExecSql(CSqlTextCreate[I]);
  end;

  // initialize metadata
  VMetadataStmt := TMetadataConnectionStatementMBTiles.Create(FSQLite3DB);
  try
    VMetadataStmt.ExecUpsert('name', TPath.GetFileNameWithoutExtension(FFileInfo.FileName));
    VMetadataStmt.ExecUpsert('description', 'Created by SAS.Planet');
    VMetadataStmt.ExecUpsert('format', GetFormatStr);
    VMetadataStmt.ExecUpsert('scheme', 'tms');
    VMetadataStmt.ExecUpsert('crs', Format('EPSG:%d', [FProjectionSet.Zooms[0].ProjectionType.ProjectionEPSG]));
  finally
    VMetadataStmt.Free;
  end;
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

procedure TTileStorageSQLiteFileConnectionMBTiles.UpdateMetadata(const AXY: TPoint; const AZoom: Byte);

  function GetBoundsStr(const R: TDoubleRect): string;
  begin
    Result := Format('%.8f,%.8f,%.8f,%.8f', [R.Left, R.Bottom, R.Right, R.Top], FFormatSettings);
  end;

  function GetCenterStr(const ARect: TDoubleRect; const AMinZoom: Byte): string;
  var
    VCenter: TDoublePoint;
  begin
    VCenter := RectCenter(ARect);
    Result := Format('%.8f,%.8f,%d', [VCenter.X, VCenter.Y, AMinZoom], FFormatSettings);
  end;

  function UpdateBoundsWithRect(const R: TDoubleRect): Boolean;
  begin
    Result := False;

    if IsLonLatRectEmpty(FBounds) then begin
      FBounds := R;
      Result := True;
      Exit;
    end;

    if R.Left < FBounds.Left then begin
      FBounds.Left := R.Left;
      Result := True;
    end;

    if R.Right > FBounds.Right then begin
      FBounds.Right := R.Right;
      Result := True;
    end;

    if R.Top > FBounds.Top then begin
      FBounds.Top := R.Top;
      Result := True;
    end;

    if R.Bottom < FBounds.Bottom then begin
      FBounds.Bottom := R.Bottom;
      Result := True;
    end;
  end;

var
  VRect: TDoubleRect;
  VDoUpdateCenter: Boolean;
begin
  // this function executed inside sql transaction

  VDoUpdateCenter := False;

  if not FProjectionSet.CheckZoom(AZoom) then begin
    raise Exception.CreateFmt('MBTiles: Invalid zoom value: %d', [AZoom]);
  end;

  VRect := FProjectionSet.Zooms[AZoom].TilePos2LonLatRect(AXY);

  if (FMinZoom = -1) or (FMinZoom > AZoom) then begin
    FMinZoom := AZoom;
    VDoUpdateCenter := True;
    FMetadataStmt.ExecUpsert('minzoom', IntToStr(FMinZoom));
  end;

  if (FMaxZoom = -1) or (FMaxZoom < AZoom) then begin
    FMaxZoom := AZoom;
    FMetadataStmt.ExecUpsert('maxzoom', IntToStr(FMaxZoom));
  end;

  if UpdateBoundsWithRect(VRect) then begin
    VDoUpdateCenter := True;
    FMetadataStmt.ExecUpsert('bounds', GetBoundsStr(FBounds));
  end;

  if VDoUpdateCenter and not IsLonLatRectEmpty(FBounds) then begin
    FMetadataStmt.ExecUpsert('center', GetCenterStr(FBounds, FMinZoom));
  end;
end;

{ TMetadataConnectionStatementMBTiles }

constructor TMetadataConnectionStatementMBTiles.Create(const ASQLite3DB: TSQLite3DbHandler);
begin
  inherited Create(False, False);
  FSQLite3DB := ASQLite3DB;
  FText :=
    'INSERT INTO metadata (name, value) VALUES (?,?) ' +
    'ON CONFLICT (name) DO UPDATE SET value = excluded.value';
end;

procedure TMetadataConnectionStatementMBTiles.ExecUpsert(const AName, AValue: UTF8String);
begin
  if not Self.CheckPrepared(FSQLite3DB) then begin
    FSQLite3DB.RaiseSQLite3Error;
  end;

  if FStmt.BindText(1, PUTF8Char(AName), Length(AName)) and
     FStmt.BindText(2, PUTF8Char(AValue), Length(AValue))
  then begin
    try
      if sqlite3_step(FStmt.Stmt) <> SQLITE_DONE then begin
        FSQLite3DB.RaiseSQLite3Error;
      end;
    finally
      FStmt.Reset;
    end;
  end else begin
    FSQLite3DB.RaiseSQLite3Error;
  end;
end;

{ TTileDataConnectionStatementMBTiles }

constructor TTileDataConnectionStatementMBTiles.Create(const AIsInvertedY, AIsInvertedZ: Boolean);
begin
  inherited;
  FText := 'SELECT tile_data FROM tiles WHERE zoom_level = ? AND tile_column = ? AND tile_row = ?';
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
  FText := 'SELECT length(tile_data) FROM tiles WHERE zoom_level = ? AND tile_column = ? AND tile_row = ?';
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
