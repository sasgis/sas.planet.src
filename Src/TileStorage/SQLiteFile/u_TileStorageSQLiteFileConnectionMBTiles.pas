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
  SysUtils,
  StrUtils,
  i_ContentTypeInfo,
  i_TileStorageSQLiteFileInfo,
  u_TileStorageSQLiteFileConnection;

type
  TTileStorageSQLiteFileConnectionMBTiles = class(TTileStorageSQLiteFileConnection)
  private
    FIsXYZSchema: Boolean;
  protected
    function CoordToValue(const AZoom: Byte; const Y: Integer): Integer; override;
    function ValueToCoord(const AZoom: Byte; const Y: Integer): Integer; override;

    procedure FetchMetadata; override;
  public
    constructor Create(
      const AFileName: string;
      const AFileInfo: ITileStorageSQLiteFileInfo;
      const AMainContentType: IContentTypeInfoBasic
    );
  end;

implementation

uses
  libsqlite3,
  u_SQLite3Handler;

{ TTileStorageSQLiteFileConnectionMBTiles }

constructor TTileStorageSQLiteFileConnectionMBTiles.Create(
  const AFileName: string;
  const AFileInfo: ITileStorageSQLiteFileInfo;
  const AMainContentType: IContentTypeInfoBasic
);
var
  VValue: string;
begin
  inherited Create(AFileName, AFileInfo, AMainContentType);

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

  if FFileInfo.TryGetMetadataValue('scheme', VValue) then begin
    FIsXYZSchema := SameText(VValue, 'xyz');
  end;

  FTileDataStmt.Init(
    'SELECT tile_data FROM tiles WHERE zoom_level = ? AND tile_column = ? AND tile_row = ?;'
  );

  FTileInfoStmt.Init(
    'SELECT length(tile_data) FROM tiles WHERE zoom_level = ? AND tile_column = ? AND tile_row = ?;'
  );

  FRectInfoStmt.Init(
    'SELECT tile_column, tile_row, length(tile_data) FROM tiles ' +
    'WHERE zoom_level = ? AND tile_column >= ? AND tile_column < ? AND ' +
    IfThen(FIsXYZSchema, 'tile_row >= ? AND tile_row < ?', 'tile_row <= ? AND tile_row > ?') + ';'
  );

  FEnumTilesStmt.Init(
    'SELECT zoom_level, tile_column, tile_row, tile_data FROM tiles;'
  );

  FEnabled :=
    FTileDataStmt.CheckPrepared(FSQLite3DB) and
    FTileInfoStmt.CheckPrepared(FSQLite3DB);
end;

procedure TTileStorageSQLiteFileConnectionMBTiles.FetchMetadata;
var
  VStmtData: TSQLite3StmtData;
  VName, VValue: string;
begin
  Assert(FFileInfo <> nil);

  if not FSQLite3Db.PrepareStatement(@VStmtData, 'SELECT name, value FROM metadata;') then begin
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

function TTileStorageSQLiteFileConnectionMBTiles.CoordToValue(const AZoom: Byte; const Y: Integer): Integer;
begin
  if FIsXYZSchema then begin
    Result := Y;
  end else begin
    Result := (1 shl AZoom) - Y - 1;
  end;
end;

function TTileStorageSQLiteFileConnectionMBTiles.ValueToCoord(const AZoom: Byte; const Y: Integer): Integer;
begin
  if FIsXYZSchema then begin
    Result := Y;
  end else begin
    Result := (1 shl AZoom) - Y - 1;
  end;
end;

end.
