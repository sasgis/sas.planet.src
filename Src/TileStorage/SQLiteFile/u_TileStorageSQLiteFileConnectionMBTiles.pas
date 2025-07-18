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
  i_ContentTypeManager,
  i_ProjectionSet,
  i_TileStorageSQLiteFileInfo,
  u_SQLite3Handler,
  u_TileStorageSQLiteFileConnection;

type
  TMetadataConnectionStatementMBTiles = class(TConnectionStatement)
  private
    FSQLite3DB: TSQLite3DbHandler;
    FSelectStmt: TConnectionStatement;
  public
    function ExecSelect: TStringDynArray;
    procedure ExecUpsert(const AName, AValue: UTF8String);
    constructor Create(const ASQLite3DB: TSQLite3DbHandler);
    destructor Destroy; override;
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
      const AContentTypeManager: IContentTypeManager;
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

const
  CEmptyZoomValue = 255;

function InvertY(Y, Z: Integer): Integer; inline; // same for Revert
begin
  Result := (1 shl Z) - Y - 1;
end;

function StrToBounds(const AStr: string): TDoubleRect;
var
  VBoundsStr: TStringDynArray;
begin
  if AStr = '' then begin
    Result := DoubleRect(0, 0, 0, 0);
    Exit;
  end;

  VBoundsStr := SplitString(AStr, ',');

  if Length(VBoundsStr) = 4 then begin
    Result.Left   := StrPointToFloat(Trim(VBoundsStr[0]));
    Result.Bottom := StrPointToFloat(Trim(VBoundsStr[1]));
    Result.Right  := StrPointToFloat(Trim(VBoundsStr[2]));
    Result.Top    := StrPointToFloat(Trim(VBoundsStr[3]));
  end else begin
    raise Exception.CreateFmt('MBTiles: Invalid bounds value: "%s"', [AStr]);
  end;
end;

{ TTileStorageSQLiteFileConnectionMBTiles }

constructor TTileStorageSQLiteFileConnectionMBTiles.Create(
  const AIsReadOnly: Boolean;
  const AFileName: string;
  const AFileInfo: ITileStorageSQLiteFileInfo;
  const AMainContentType: IContentTypeInfoBasic;
  const AContentTypeManager: IContentTypeManager;
  const AProjectionSet: IProjectionSet
);
var
  VValue: string;
  VIsInvertedY: Boolean;
  VIsInvertedZ: Boolean;
begin
  inherited Create(AIsReadOnly, AFileName, AFileInfo, AMainContentType, AContentTypeManager, AProjectionSet);

  FFormatSettings.DecimalSeparator := '.';

  Assert(FFileInfo <> nil);

  if FFileInfo.TryGetMetadataValue('format', VValue) then begin
    if not SameText(FMainContentType.GetDefaultExt, '.' + VValue) then begin
      raise Exception.CreateFmt(
        'MBTiles: The detected tile format "%s" does not match the provided content type "%s"',
        [VValue, FMainContentType.GetContentType]
      );
    end;
  end;

  if FFileInfo.TryGetMetadataValue('bounds', VValue) then begin
    FBounds := StrToBounds(VValue);
  end else begin
    FBounds := DoubleRect(0, 0, 0, 0);
  end;

  if not (FFileInfo.TryGetMetadataValue('minzoom', VValue) and TryStrToInt(VValue, FMinZoom)) then begin
    FMinZoom := CEmptyZoomValue;
  end;

  if not (FFileInfo.TryGetMetadataValue('maxzoom', VValue) and TryStrToInt(VValue, FMaxZoom)) then begin
    FMaxZoom := CEmptyZoomValue;
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

  function GetCenterStr(const ARect: TDoubleRect; const ACenterZoom: Byte): string;
  var
    VCenter: TDoublePoint;
  begin
    VCenter := RectCenter(ARect);
    Result := Format('%.8f,%.8f,%d', [VCenter.X, VCenter.Y, ACenterZoom], FFormatSettings);
  end;

  function UpdateBoundsWithRect(var ABounds: TDoubleRect; const ARect: TDoubleRect): Boolean;
  begin
    Result := False;

    if IsLonLatRectEmpty(ARect) then begin
      Exit;
    end;

    if IsLonLatRectEmpty(ABounds) then begin
      ABounds := ARect;
      Result := True;
      Exit;
    end;

    if ARect.Left < ABounds.Left then begin
      ABounds.Left := ARect.Left;
      Result := True;
    end;

    if ARect.Right > ABounds.Right then begin
      ABounds.Right := ARect.Right;
      Result := True;
    end;

    if ARect.Top > ABounds.Top then begin
      ABounds.Top := ARect.Top;
      Result := True;
    end;

    if ARect.Bottom < ABounds.Bottom then begin
      ABounds.Bottom := ARect.Bottom;
      Result := True;
    end;
  end;

  procedure DoUpdateMetadata(const AName, AValue: string);
  begin
    FMetadataStmt.ExecUpsert(AName, AValue);
    FFileInfo.AddOrSetMetadataValue(AName, AValue);
  end;

var
  VStr: TStringDynArray;
  VRect: TDoubleRect;
  VValue: string;
  VCenterZoom: Integer;
  VMinZoom, VMaxZoom: Integer;
  VDoUpdate: Integer;
  VDoUpdateCenter: Boolean;
begin
  // this function executed inside sql transaction

  VDoUpdate := 0;
  VDoUpdateCenter := False;

  if not FProjectionSet.CheckZoom(AZoom) then begin
    raise Exception.CreateFmt('MBTiles: Invalid zoom value: %d', [AZoom]);
  end;

  if (FMinZoom = CEmptyZoomValue) or (FMinZoom > AZoom) then begin
    FMinZoom := AZoom;
    Inc(VDoUpdate);
  end;

  if (FMaxZoom = CEmptyZoomValue) or (FMaxZoom < AZoom) then begin
    FMaxZoom := AZoom;
    Inc(VDoUpdate);
  end;

  VRect := FProjectionSet.Zooms[AZoom].TilePos2LonLatRect(AXY);
  if UpdateBoundsWithRect(FBounds, VRect) then begin
    Inc(VDoUpdate);
  end;

  if VDoUpdate = 0 then begin
    Exit;
  end;

  VStr := FMetadataStmt.ExecSelect;

  // minzoom
  if not TryStrToInt(VStr[3], VMinZoom) then begin
    VMinZoom := CEmptyZoomValue;
  end;

  if (VMinZoom = CEmptyZoomValue) or (VMinZoom > FMinZoom) then begin
    DoUpdateMetadata('minzoom', IntToStr(FMinZoom));
    VDoUpdateCenter := True;
  end else
  if VMinZoom < FMinZoom then begin
    FMinZoom := VMinZoom;
  end;

  // maxzoom
  if not TryStrToInt(VStr[2], VMaxZoom) then begin
    VMaxZoom := CEmptyZoomValue;
  end;

  if (VMaxZoom = CEmptyZoomValue) or (VMaxZoom < FMaxZoom) then begin
    DoUpdateMetadata('maxzoom', IntToStr(FMaxZoom));
    VDoUpdateCenter := True;
  end else
  if VMaxZoom > FMaxZoom then begin
    FMaxZoom := VMaxZoom;
  end;

  // bounds
  VRect := StrToBounds(VStr[0]);
  if IsLonLatRectEmpty(VRect) then begin
    DoUpdateMetadata('bounds', GetBoundsStr(FBounds));
    VDoUpdateCenter := True;
  end else
  if UpdateBoundsWithRect(VRect, FBounds) then begin
    DoUpdateMetadata('bounds', GetBoundsStr(VRect));
    FBounds := VRect;
    VDoUpdateCenter := True;
  end;

  // center
  if VDoUpdateCenter then begin
    VCenterZoom := FMinZoom + ((FMaxZoom - FMinZoom) div 2);
    VValue := GetCenterStr(FBounds, VCenterZoom);
    if VValue <> VStr[1] then begin
      DoUpdateMetadata('center', VValue);
    end;
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

{ TMetadataConnectionStatementMBTiles }

constructor TMetadataConnectionStatementMBTiles.Create(const ASQLite3DB: TSQLite3DbHandler);
begin
  inherited Create(False, False);

  FSQLite3DB := ASQLite3DB;

  FSelectStmt := TConnectionStatement.Create(False, False);

  FSelectStmt.FText :=
    'SELECT name, value FROM metadata WHERE name in ("bounds", "center", "maxzoom", "minzoom")';

  FText :=
    'INSERT INTO metadata (name, value) VALUES (?,?) ' +
    'ON CONFLICT (name) DO UPDATE SET value = excluded.value';
end;

destructor TMetadataConnectionStatementMBTiles.Destroy;
begin
  FreeAndNil(FSelectStmt);
  inherited Destroy;
end;

function TMetadataConnectionStatementMBTiles.ExecSelect: TStringDynArray;

  function GetIndex(const AName: string): Integer;
  begin
    if AName = 'bounds'  then Result := 0 else
    if AName = 'center'  then Result := 1 else
    if AName = 'maxzoom' then Result := 2 else
    if AName = 'minzoom' then Result := 3 else
      raise Exception.CreateFmt('MBTiles: Invalid name parameter "%s"', [AName]);
  end;

var
  I: Integer;
  VName: string;
  VStmtData: PSQLite3StmtData;
begin
  if not FSelectStmt.CheckPrepared(FSQLite3DB) then begin
    FSQLite3DB.RaiseSQLite3Error;
  end;

  VStmtData := @FSelectStmt.FStmt;

  SetLength(Result, 4);
  try
    while sqlite3_step(VStmtData.Stmt) = SQLITE_ROW do begin
      VName := LowerCase(VStmtData.ColumnAsString(0));

      I := GetIndex(VName);
      Result[I] := VStmtData.ColumnAsString(1);
    end;
  finally
    VStmtData.Reset;
  end;
end;

procedure TMetadataConnectionStatementMBTiles.ExecUpsert(const AName, AValue: UTF8String);
begin
  if not Self.CheckPrepared(FSQLite3DB) then begin
    FSQLite3DB.RaiseSQLite3Error;
  end;

  if FStmt.BindText(1, AName) and FStmt.BindText(2, AValue) then begin
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

end.
