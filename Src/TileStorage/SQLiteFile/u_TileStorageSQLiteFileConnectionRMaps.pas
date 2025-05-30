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

unit u_TileStorageSQLiteFileConnectionRMaps;

interface

uses
  Types,
  SysUtils,
  StrUtils,
  t_TileStorageSQLiteFile,
  i_ContentTypeInfo,
  i_ProjectionSet,
  i_TileStorageSQLiteFileInfo,
  u_TileStorageSQLiteFileConnection;

type
  TTileStorageSQLiteFileConnectionRMaps = class(TTileStorageSQLiteFileConnection)
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
      const AProjectionSet: IProjectionSet;
      const AFormatId: TTileStorageSQLiteFileFormatId
    );
  end;

implementation

uses
  libsqlite3,
  i_BinaryData,
  u_SQLite3Handler;

type
  TTileDataConnectionStatementRMaps = class(TTileDataConnectionStatement)
  public
    function BindParams(X, Y, Z: Integer): Boolean; override;
    procedure GetResult(out ABlob: IBinaryData); override;
    constructor Create(const AIsInvertedY, AIsInvertedZ: Boolean);
  end;

  TTileInfoConnectionStatementRMaps = class(TTileInfoConnectionStatement)
  public
    function BindParams(X, Y, Z: Integer): Boolean; override;
    procedure GetResult(out ABlobSize: Integer); override;
    constructor Create(const AIsInvertedY, AIsInvertedZ: Boolean);
  end;

  TRectInfoConnectionStatementRMaps = class(TRectInfoConnectionStatement)
  public
    function BindParams(const ARect: TRect; Z: Integer): Boolean; override;
    procedure GetResult(out X, Y: Integer; out ASize: Integer); override;
    constructor Create(const AIsInvertedY, AIsInvertedZ: Boolean);
  end;

  TEnumTilesConnectionStatementRMaps = class(TEnumTilesConnectionStatement)
  public
    procedure GetResult(out X, Y, Z: Integer; out ABlob: IBinaryData); override;
    constructor Create(const AIsInvertedY, AIsInvertedZ: Boolean);
  end;

function InvertZ(Z: Integer): Integer; inline;
begin
  Result := 17 - Z;
end;

{ TTileStorageSQLiteFileConnectionRMaps }

constructor TTileStorageSQLiteFileConnectionRMaps.Create(
  const AIsReadOnly: Boolean;
  const AFileName: string;
  const AFileInfo: ITileStorageSQLiteFileInfo;
  const AMainContentType: IContentTypeInfoBasic;
  const AProjectionSet: IProjectionSet;
  const AFormatId: TTileStorageSQLiteFileFormatId
);
var
  VValue: string;
  VIsInvertedY: Boolean;
  VIsInvertedZ: Boolean;
begin
  inherited Create(AIsReadOnly, AFileName, AFileInfo, AMainContentType, AProjectionSet);

  Assert(FFileInfo <> nil);

  VIsInvertedY := False;

  case AFormatId of
    sfOsmAnd: begin
      VIsInvertedZ :=
        FFileInfo.TryGetMetadataValue('tilenumbering', VValue) and
        SameText(VValue, 'BigPlanet');
    end;

    sfLocus, sfRMaps: begin
      VIsInvertedZ := True;
    end;
  else
    raise Exception.CreateFmt('RMaps: Unsupported FormatID = %d', [Integer(AFormatId)]);
  end;

  // read access
  FTileDataStmt := TTileDataConnectionStatementRMaps.Create(VIsInvertedY, VIsInvertedZ);
  FTileInfoStmt := TTileInfoConnectionStatementRMaps.Create(VIsInvertedY, VIsInvertedZ);
  FRectInfoStmt := TRectInfoConnectionStatementRMaps.Create(VIsInvertedY, VIsInvertedZ);
  FEnumTilesStmt := TEnumTilesConnectionStatementRMaps.Create(VIsInvertedY, VIsInvertedZ);

  // write access
  FInsertOrReplaceStmt := nil;
  FInsertOrIgnoreStmt := nil;
  FDeleteTileStmt := nil;

  FEnabled :=
    FTileDataStmt.CheckPrepared(FSQLite3DB) and
    FTileInfoStmt.CheckPrepared(FSQLite3DB);
end;

procedure TTileStorageSQLiteFileConnectionRMaps.CreateTables;
begin
  raise Exception.Create('RMaps: Write access is not supported yet!');
end;

procedure TTileStorageSQLiteFileConnectionRMaps.FetchMetadata;
var
  I: Integer;
  VStmtData: TSQLite3StmtData;
  VName, VValue: string;
begin
  Assert(FFileInfo <> nil);

  if not FSQLite3Db.PrepareStatement(@VStmtData, 'SELECT * FROM info LIMIT 1') then begin
    FSQLite3Db.RaiseSQLite3Error;
  end;

  try
    if sqlite3_step(VStmtData.Stmt) = SQLITE_ROW then begin
      for I := 0 to VStmtData.ColumnCount - 1 do begin
        VName := UTF8ToString(VStmtData.ColumnName(I));

        if not VStmtData.IsNull(I) then begin
          VValue := VStmtData.ColumnAsString(I);
        end else begin
          VValue := '';
        end;

        FFileInfo.AddOrSetMetadataValue(LowerCase(VName), VValue);
      end;
    end;
  finally
    VStmtData.Fin;
  end;
end;

procedure TTileStorageSQLiteFileConnectionRMaps.UpdateMetadata(const AXY: TPoint; const AZoom: Byte);
begin
  raise Exception.Create('RMaps: Write access is not supported yet!');
end;

{ TTileDataConnectionStatementRMaps }

constructor TTileDataConnectionStatementRMaps.Create(const AIsInvertedY, AIsInvertedZ: Boolean);
begin
  inherited;
  FText := 'SELECT image FROM tiles WHERE x = ? AND y = ? AND z = ?';
end;

function TTileDataConnectionStatementRMaps.BindParams(X, Y, Z: Integer): Boolean;
begin
  if FIsInvertedZ then begin
    Z := InvertZ(Z);
  end;

  Result :=
    FStmt.BindInt(1, X) and
    FStmt.BindInt(2, Y) and
    FStmt.BindInt(3, Z);
end;

procedure TTileDataConnectionStatementRMaps.GetResult(out ABlob: IBinaryData);
begin
  ABlob := BlobToBinaryData(0);
end;

{ TTileInfoConnectionStatementRMaps }

constructor TTileInfoConnectionStatementRMaps.Create(const AIsInvertedY, AIsInvertedZ: Boolean);
begin
  inherited;
  FText := 'SELECT length(image) FROM tiles WHERE x = ? AND y = ? AND z = ?';
end;

function TTileInfoConnectionStatementRMaps.BindParams(X, Y, Z: Integer): Boolean;
begin
  if FIsInvertedZ then begin
    Z := InvertZ(Z);
  end;

  Result :=
    FStmt.BindInt(1, X) and
    FStmt.BindInt(2, Y) and
    FStmt.BindInt(3, Z);
end;

procedure TTileInfoConnectionStatementRMaps.GetResult(out ABlobSize: Integer);
begin
  ABlobSize := FStmt.ColumnInt(0);
end;

{ TRectInfoConnectionStatementRMaps }

constructor TRectInfoConnectionStatementRMaps.Create(const AIsInvertedY, AIsInvertedZ: Boolean);
begin
  inherited;
  FText :=
    'SELECT x, y, length(image) FROM tiles WHERE x >= ? AND x < ? AND ' +
    'y >= ? AND y < ? AND z = ?';
end;

function TRectInfoConnectionStatementRMaps.BindParams(const ARect: TRect; Z: Integer): Boolean;
begin
  if FIsInvertedZ then begin
    Z := InvertZ(Z);
  end;

  Result :=
    FStmt.BindInt(1, ARect.Left) and
    FStmt.BindInt(2, ARect.Right) and
    FStmt.BindInt(3, ARect.Top) and
    FStmt.BindInt(4, ARect.Bottom) and
    FStmt.BindInt(5, Z);
end;

procedure TRectInfoConnectionStatementRMaps.GetResult(out X, Y, ASize: Integer);
begin
  X := FStmt.ColumnInt(0);
  Y := FStmt.ColumnInt(1);
  ASize := FStmt.ColumnInt(2);
end;

{ TEnumTilesConnectionStatementRMaps }

constructor TEnumTilesConnectionStatementRMaps.Create(const AIsInvertedY, AIsInvertedZ: Boolean);
begin
  inherited;
  FText := 'SELECT x, y, z, image FROM tiles';
end;

procedure TEnumTilesConnectionStatementRMaps.GetResult(out X, Y, Z: Integer; out ABlob: IBinaryData);
begin
  X := FStmt.ColumnInt(0);
  Y := FStmt.ColumnInt(1);
  Z := FStmt.ColumnInt(2);
  ABlob := BlobToBinaryData(3);

  if FIsInvertedZ then begin
    Z := InvertZ(Z);
  end;
end;

end.
