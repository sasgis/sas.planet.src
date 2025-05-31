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
  t_GeoTypes,
  t_TileStorageSQLiteFile,
  i_ContentTypeInfo,
  i_ProjectionSet,
  i_TileStorageSQLiteFileInfo,
  u_SQLite3Handler,
  u_TileStorageSQLiteFileConnection;

type
  TZoomsArray = array of Integer;

  TMetadataConnectionStatementRMaps = class(TConnectionStatement)
  private
    FSQLite3DB: TSQLite3DbHandler;
    FSelectStmt: TConnectionStatement;
    FFormatId: TTileStorageSQLiteFileFormatId;
  public
    procedure ExecUpdate(
      var AMinZoom, AMaxZoom: Integer;
      var AZooms: TZoomsArray;
      const AFileInfo: ITileStorageSQLiteFileInfo
    );
    constructor Create(
      const AIsInvertedZ: Boolean;
      const ASQLite3DB: TSQLite3DbHandler;
      const AFormatId: TTileStorageSQLiteFileFormatId
    );
    destructor Destroy; override;
  end;

  TTileStorageSQLiteFileConnectionRMaps = class(TTileStorageSQLiteFileConnection)
  private
    FMinZoom: Integer;
    FMaxZoom: Integer;
    FZooms: TZoomsArray;
    FFormatId: TTileStorageSQLiteFileFormatId;
    FMetadataStmt: TMetadataConnectionStatementRMaps;
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
  i_BinaryData;

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

  TInsertOrReplaceConnectionStatementRMaps = class(TInsertOrReplaceConnectionStatement)
  public
    function BindParams(X, Y, Z: Integer; const ABlob: IBinaryData): Boolean; override;
    constructor Create(const AIsInvertedY, AIsInvertedZ: Boolean);
  end;

  TInsertOrIgnoreConnectionStatementRMaps = class(TInsertOrIgnoreConnectionStatement)
  public
    function BindParams(X, Y, Z: Integer; const ABlob: IBinaryData): Boolean; override;
    constructor Create(const AIsInvertedY, AIsInvertedZ: Boolean);
  end;

  TDeleteTileConnectionStatementRMaps = class(TDeleteTileConnectionStatement)
  public
    function BindParams(X, Y, Z: Integer): Boolean; override;
    constructor Create(const AIsInvertedY, AIsInvertedZ: Boolean);
  end;

function InvertZ(Z: Integer): Integer; inline;
begin
  Result := 17 - Z;
end;

const
  CEmptyZoomValue = 255;

// Locus min/max zoom is opposite to RMaps and OsmAnd
procedure SwapMinMaxZoom(var AMinZoom, AMaxZoom: Integer); inline;
var
  Z: Integer;
begin
  Z := AMinZoom;
  AMinZoom := AMaxZoom;
  AMaxZoom := Z;
end;

function UpdateZooms(const AZoom: Integer; var AZooms: TZoomsArray): Boolean;
var
  I: Integer;
  VIndex: Integer;
begin
  VIndex := Length(AZooms);

  for I := 0 to Length(AZooms) - 1 do begin
    if AZooms[I] = AZoom then begin
      Result := False;
      Exit;
    end else
    if AZooms[I] > AZoom then begin
      VIndex := I;
      Break;
    end;
  end;

  System.Insert(AZoom, AZooms, VIndex);
  Result := True;
end;

function StrToZooms(const AStr: string; const AIsInvertedZ: Boolean): TZoomsArray;
var
  I: Integer;
  VZooms: TStringDynArray;
begin
  VZooms := SplitString(AStr, ';');
  SetLength(Result, Length(VZooms));
  for I := 0 to Length(VZooms) - 1 do begin
    Result[I] := StrToInt(Trim(VZooms[I]));
    if AIsInvertedZ then begin
      Result[I] := InvertZ(Result[I]);
    end;
  end;
end;

function ZoomsToStr(const AZooms: TZoomsArray; const AIsInvertedZ: Boolean): string;
var
  I, Z: Integer;
begin
  Result := '';
  for I := 0 to Length(AZooms) - 1 do begin
    Z := AZooms[I];
    if AIsInvertedZ then begin
      Z := InvertZ(Z);
    end;
    Result := Result + IfThen(Result = '', '', ';') + IntToStr(Z);
  end;
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
  FFormatId := AFormatId;

  inherited Create(AIsReadOnly, AFileName, AFileInfo, AMainContentType, AProjectionSet);

  Assert(FFileInfo <> nil);

  VIsInvertedY := False;

  case FFormatId of
    sfOsmAnd: begin
      VIsInvertedZ :=
        FFileInfo.TryGetMetadataValue('tilenumbering', VValue) and
        SameText(VValue, 'BigPlanet');
    end;

    sfLocus, sfRMaps: begin
      VIsInvertedZ := True;
    end;
  else
    raise Exception.CreateFmt('RMaps: Unsupported FormatID = %d', [Integer(FFormatId)]);
  end;

  if not (FFileInfo.TryGetMetadataValue('minzoom', VValue) and TryStrToInt(VValue, FMinZoom)) then begin
    FMinZoom := CEmptyZoomValue;
  end;

  if not (FFileInfo.TryGetMetadataValue('maxzoom', VValue) and TryStrToInt(VValue, FMaxZoom)) then begin
    FMaxZoom := CEmptyZoomValue;
  end;

  if VIsInvertedZ then  begin
    if FMinZoom <> CEmptyZoomValue then begin
      FMinZoom := InvertZ(FMinZoom);
    end;
    if FMaxZoom <> CEmptyZoomValue then begin
      FMaxZoom := InvertZ(FMaxZoom);
    end;
  end;

  if FFormatId = sfLocus then begin
    SwapMinMaxZoom(FMinZoom, FMaxZoom);
    if FFileInfo.TryGetMetadataValue('zooms', VValue) then begin
      FZooms := StrToZooms(VValue, VIsInvertedZ);
    end;
  end;

  // read access
  FTileDataStmt := TTileDataConnectionStatementRMaps.Create(VIsInvertedY, VIsInvertedZ);
  FTileInfoStmt := TTileInfoConnectionStatementRMaps.Create(VIsInvertedY, VIsInvertedZ);
  FRectInfoStmt := TRectInfoConnectionStatementRMaps.Create(VIsInvertedY, VIsInvertedZ);
  FEnumTilesStmt := TEnumTilesConnectionStatementRMaps.Create(VIsInvertedY, VIsInvertedZ);

  // write access
  if not FIsReadOnly then begin
    FInsertOrReplaceStmt := TInsertOrReplaceConnectionStatementRMaps.Create(VIsInvertedY, VIsInvertedZ);
    FInsertOrIgnoreStmt := TInsertOrIgnoreConnectionStatementRMaps.Create(VIsInvertedY, VIsInvertedZ);
    FDeleteTileStmt := TDeleteTileConnectionStatementRMaps.Create(VIsInvertedY, VIsInvertedZ);

    FMetadataStmt := TMetadataConnectionStatementRMaps.Create(VIsInvertedZ, FSQLite3DB, FFormatId);
  end;

  FEnabled :=
    FTileDataStmt.CheckPrepared(FSQLite3DB) and
    FTileInfoStmt.CheckPrepared(FSQLite3DB);
end;

procedure TTileStorageSQLiteFileConnectionRMaps.CreateTables;
var
  VEpsg: Integer;
  VJson: string;
  VEllipsoid: string;
begin
  // https://osmand.net/docs/technical/osmand-file-formats/osmand-sqlite/

  VEpsg := FProjectionSet.Zooms[0].ProjectionType.ProjectionEPSG;
  VJson := Format('''{"epsg":%d,"format":"%s"}''', [VEpsg, FMainContentType.GetContentType]);

  case FFormatId of
    sfOsmAnd: begin
      if VEpsg = 3395 then begin
        VEllipsoid := '1';
      end else begin
        VEllipsoid := '0';
      end;
      FSQLite3DB.ExecSql(
        'CREATE TABLE info (minzoom int, maxzoom int, ellipsoid int, ' +
        'timecolumn text, expireminutes text, tilenumbering text, tilesize int, sasgis text)'
      );
      FSQLite3DB.ExecSql(
        'INSERT INTO info (ellipsoid, timecolumn, expireminutes, tilenumbering, tilesize, sasgis) ' +
        'VALUES (' + VEllipsoid + ', "no", 0, "BigPlanet", 256, ' + VJson + ')'
      );
    end;

    sfLocus: begin
      FSQLite3DB.ExecSql(
        'CREATE TABLE info (minzoom int, maxzoom int, center_x double, center_y double, ' +
        'zooms text, provider int, sasgis text)'
      );
      FSQLite3DB.ExecSql('INSERT INTO info (sasgis) VALUES (' + VJson + ')');
    end;

    sfRMaps: begin
      FSQLite3DB.ExecSql('CREATE TABLE info (minzoom int, maxzoom int, sasgis text)');
      FSQLite3DB.ExecSql('INSERT INTO info (sasgis) VALUES (' + VJson + ')');
    end;
  else
    raise Exception.CreateFmt('RMaps: Unsupported FormatID = %d', [Integer(FFormatId)]);
  end;

  FSQLite3DB.ExecSQL(
    'CREATE TABLE tiles (x int, y int, z int, s int, image blob, PRIMARY KEY (x,y,z,s))'
  );
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
var
  VDoUpdate: Boolean;
begin
  VDoUpdate := False;

  if (FMinZoom = CEmptyZoomValue) or (FMinZoom > AZoom) then begin
    FMinZoom := AZoom;
    VDoUpdate := True;
  end;

  if (FMaxZoom = CEmptyZoomValue) or (FMaxZoom < AZoom) then begin
    FMaxZoom := AZoom;
    VDoUpdate := True;
  end;

  if (FFormatId = sfLocus) and UpdateZooms(AZoom, FZooms) then begin
    VDoUpdate := True;
  end;

  if VDoUpdate then begin
    FMetadataStmt.ExecUpdate(FMinZoom, FMaxZoom, FZooms, FFileInfo);
  end;
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

{ TInsertOrReplaceConnectionStatementRMaps }

constructor TInsertOrReplaceConnectionStatementRMaps.Create(const AIsInvertedY, AIsInvertedZ: Boolean);
begin
  inherited;
  FText := 'INSERT OR REPLACE INTO tiles (x, y, z, s, image) VALUES (?,?,?,0,?)';
end;

function TInsertOrReplaceConnectionStatementRMaps.BindParams(X, Y, Z: Integer; const ABlob: IBinaryData): Boolean;
begin
  Assert(ABlob <> nil);

  if FIsInvertedZ then begin
    Z := InvertZ(Z);
  end;

  Result :=
    FStmt.BindInt(1, X) and
    FStmt.BindInt(2, Y) and
    FStmt.BindInt(3, Z) and
    FStmt.BindBlob(4, ABlob.Buffer, ABlob.Size);
end;

{ TInsertOrIgnoreConnectionStatementRMaps }

constructor TInsertOrIgnoreConnectionStatementRMaps.Create(const AIsInvertedY, AIsInvertedZ: Boolean);
begin
  inherited;
  FText := 'INSERT OR IGNORE INTO tiles (x, y, z, s, image) VALUES (?,?,?,0,?)';
end;

function TInsertOrIgnoreConnectionStatementRMaps.BindParams(X, Y, Z: Integer; const ABlob: IBinaryData): Boolean;
begin
  Assert(ABlob <> nil);

  if FIsInvertedZ then begin
    Z := InvertZ(Z);
  end;

  Result :=
    FStmt.BindInt(1, X) and
    FStmt.BindInt(2, Y) and
    FStmt.BindInt(3, Z) and
    FStmt.BindBlob(4, ABlob.Buffer, ABlob.Size);
end;

{ TDeleteTileConnectionStatementRMaps }

constructor TDeleteTileConnectionStatementRMaps.Create(const AIsInvertedY, AIsInvertedZ: Boolean);
begin
  inherited;
  FText := 'DELETE FROM tiles WHERE x = ? AND y = ? AND z = ?';
end;

function TDeleteTileConnectionStatementRMaps.BindParams(X, Y, Z: Integer): Boolean;
begin
  if FIsInvertedZ then begin
    Z := InvertZ(Z);
  end;

  Result :=
    FStmt.BindInt(1, X) and
    FStmt.BindInt(2, Y) and
    FStmt.BindInt(3, Z);
end;

{ TMetadataConnectionStatementRMaps }

constructor TMetadataConnectionStatementRMaps.Create(
  const AIsInvertedZ: Boolean;
  const ASQLite3DB: TSQLite3DbHandler;
  const AFormatId: TTileStorageSQLiteFileFormatId
);
begin
  inherited Create(False, AIsInvertedZ);

  FSQLite3DB := ASQLite3DB;
  FFormatId := AFormatId;

  FSelectStmt := TConnectionStatement.Create(False, False);

  if FFormatId = sfLocus then begin
    FText := 'UPDATE info SET minzoom = ?, maxzoom = ?, zooms = ?';
    FSelectStmt.FText := 'SELECT minzoom, maxzoom, zooms from info LIMIT 1';
  end else begin
    FText := 'UPDATE info SET minzoom = ?, maxzoom = ?';
    FSelectStmt.FText := 'SELECT minzoom, maxzoom from info LIMIT 1';
  end;
end;

destructor TMetadataConnectionStatementRMaps.Destroy;
begin
  FreeAndNil(FSelectStmt);
  inherited Destroy;
end;

procedure TMetadataConnectionStatementRMaps.ExecUpdate(
  var AMinZoom, AMaxZoom: Integer;
  var AZooms: TZoomsArray;
  const AFileInfo: ITileStorageSQLiteFileInfo
);

  function GetZoomValue(const AColNum: Integer): Integer;
  begin
    if not FSelectStmt.FStmt.IsNull(AColNum) then begin
      Result := FSelectStmt.FStmt.ColumnInt(AColNum);
      if FIsInvertedZ then begin
        Result := InvertZ(Result);
      end;
    end else begin
      Result := CEmptyZoomValue;
    end;
  end;

var
  I: Integer;
  VZooms: UTF8String;
  VZoomsStr: string;
  VZoomsArr: TZoomsArray;
  VZoomsMerged: string;
  VBindResult: Boolean;
  VMinZoom, VMaxZoom: Integer;
  VDoUpdate: Boolean;
begin
  VDoUpdate := False;

  if not Self.CheckPrepared(FSQLite3DB) or
     not FSelectStmt.CheckPrepared(FSQLite3DB)
  then begin
    FSQLite3DB.RaiseSQLite3Error;
  end;

  // read actual data from db
  try
    if sqlite3_step(FSelectStmt.FStmt.Stmt) = SQLITE_ROW then begin
      VMinZoom := GetZoomValue(0);
      VMaxZoom := GetZoomValue(1);

      if FFormatId = sfLocus then begin
        SwapMinMaxZoom(VMinZoom, VMaxZoom);
        VZoomsStr := FSelectStmt.FStmt.ColumnAsString(2);
        VZoomsArr := StrToZooms(VZoomsStr, FIsInvertedZ);
      end;
    end else begin
      VMinZoom := CEmptyZoomValue;
      VMaxZoom := CEmptyZoomValue;
      VZoomsStr := '';
      VZoomsArr := nil;
    end;
  finally
    FSelectStmt.FStmt.Reset;
  end;

  // compare and sync local values with values from db
  if (VMinZoom = CEmptyZoomValue) or (VMinZoom > AMinZoom) then begin
    VDoUpdate := True;
  end else
  if VMinZoom < AMinZoom then begin
    AMinZoom := VMinZoom;
  end;

  if (VMaxZoom = CEmptyZoomValue) or (VMaxZoom < AMaxZoom) then begin
    VDoUpdate := True;
  end else
  if VMaxZoom > AMaxZoom then begin
    AMaxZoom := VMaxZoom;
  end;

  if FFormatId = sfLocus then begin
    // merge zooms from db with local values
    for I := 0 to Length(VZoomsArr) - 1 do begin
      UpdateZooms(VZoomsArr[I], AZooms);
    end;
    VZoomsMerged := ZoomsToStr(AZooms, FIsInvertedZ);
    if VZoomsMerged <> VZoomsStr then begin
      VDoUpdate := True;
    end;
  end;

  if not VDoUpdate then begin
    // ok, other process/thread already update all data in db
    Exit;
  end;

  // do update db
  if FIsInvertedZ then begin
    VMinZoom := InvertZ(AMinZoom);
    VMaxZoom := InvertZ(AMaxZoom);
  end else begin
    VMinZoom := AMinZoom;
    VMaxZoom := AMaxZoom;
  end;

  if FFormatId = sfLocus then begin
    VZooms := UTF8Encode(VZoomsMerged);

    VBindResult :=
      FStmt.BindInt(1, VMaxZoom) and
      FStmt.BindInt(2, VMinZoom) and
      FStmt.BindText(3, PUTF8Char(VZooms), Length(VZooms));
  end else begin
    VBindResult :=
      FStmt.BindInt(1, VMinZoom) and
      FStmt.BindInt(2, VMaxZoom);
  end;

  if not VBindResult then begin
    FSQLite3DB.RaiseSQLite3Error;
  end;

  try
    if sqlite3_step(FStmt.Stmt) <> SQLITE_DONE then begin
      FSQLite3DB.RaiseSQLite3Error;
    end;
  finally
    FStmt.Reset;
  end;

  // sync local info with db state
  if FFormatId = sfLocus then begin
    AFileInfo.AddOrSetMetadataValue('minzoom', IntToStr(VMaxZoom));
    AFileInfo.AddOrSetMetadataValue('maxzoom', IntToStr(VMinZoom));
    AFileInfo.AddOrSetMetadataValue('zooms', VZoomsMerged);
  end else begin
    AFileInfo.AddOrSetMetadataValue('minzoom', IntToStr(VMinZoom));
    AFileInfo.AddOrSetMetadataValue('maxzoom', IntToStr(VMaxZoom));
  end;
end;

end.
