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

unit u_TileStorageSQLiteFileConnection;

interface

uses
  Types,
  SysUtils,
  SyncObjs,
  StrUtils,
  System.Generics.Collections,
  i_BinaryData,
  i_ContentTypeInfo,
  i_TileInfoBasic,
  i_TileStorage,
  i_NotifierOperation,
  u_SQLite3Handler;

type
  TTileStorageSQLiteFileConnection = class
  private
    type
      TStmtState = (ssNone, ssPrepared, ssError);
      TFetchNextState = (fnsNone, fnsNext, fnsDone, fnsError);

      TMetadataDictionary = TDictionary<string, string>;

      TConnectionStatement = record
        FState: TStmtState;
        FText: UTF8String;
        FStmt: TSQLite3StmtData;
      end;
      PConnectionStatement = ^TConnectionStatement;
  private
    FEnabled: Boolean;
    FSQLite3DB: TSQLite3DbHandler;
    FTileDataStmt: TConnectionStatement;
    FTileInfoStmt: TConnectionStatement;
    FRectInfoStmt: TConnectionStatement;
    FEnumTilesStmt: TConnectionStatement;
    FFetchNextState: TFetchNextState;
    FMainContentType: IContentTypeInfoBasic;
    FIsXYZSchema: Boolean;
  private
    function BlobToBinaryData(const AStmt: TSQLite3StmtData; const AColNum: Integer): IBinaryData;

    function CoordToValue(const AZoom: Byte; const Y: Integer): Integer; inline;
    function ValueToCoord(const AZoom: Byte; const Y: Integer): Integer; inline;

    procedure SetupStatement(
      const AStmt: PConnectionStatement;
      const ASqlText: UTF8String
    );

    function CheckStatement(
      const AStmt: PConnectionStatement
    ): Boolean;

    procedure FetchMetadataCallback(
      const AHandler: PSQLite3DbHandler;
      const ACallbackPtr: Pointer;
      const AStmtData: PSQLite3StmtData
    );
  public
    function FetchOne(
      const AXY: TPoint;
      const AZoom: Byte;
      const AMode: TGetTileInfoMode
    ): ITileInfoBasic;

    function FetchRectInfo(
      const ARect: TRect;
      const AZoom: Byte;
      const AOperationID: Integer;
      const ACancelNotifier: INotifierOperation
    ): ITileRectInfo;

    function FetchNext(
      var ATileInfo: TTileInfo
    ): Boolean;
  public
    constructor Create(
      const AFileName: string;
      const AMainContentType: IContentTypeInfoBasic
    );
    destructor Destroy; override;
  end;

implementation

uses
  libsqlite3,
  u_BinaryData,
  u_TileInfoBasic,
  u_TileRectInfoShort;

{ TTileStorageSQLiteFileConnection }

constructor TTileStorageSQLiteFileConnection.Create(
  const AFileName: string;
  const AMainContentType: IContentTypeInfoBasic
);
var
  VValue: string;
  VMetadata: TMetadataDictionary;
begin
  inherited Create;

  FMainContentType := AMainContentType;

  if not FileExists(AFileName) then begin
    raise Exception.CreateFmt('File not found: %s', [AFileName]);
  end;

  if not FSQLite3DB.Init then begin
    Exit;
  end;

  // open db
  FSQLite3Db.Open(
    'file:///' + AFileName + '?immutable=1',
    SQLITE_OPEN_READONLY or SQLITE_OPEN_URI or SQLITE_OPEN_NOMUTEX,
    False
  );

  // read metadata
  VMetadata := TMetadataDictionary.Create;
  try
    FSQLite3Db.OpenSql(
      'SELECT name, value FROM metadata', Self.FetchMetadataCallback, Pointer(VMetadata), True
    );

    if VMetadata.TryGetValue('format', VValue) then begin
      if not SameText(FMainContentType.GetDefaultExt, '.' + VValue) then begin
        raise Exception.CreateFmt(
          'The detected tile format "%s" does not match the provided content type "%s"',
          [VValue, FMainContentType.GetContentType]
        );
      end;
    end;

    if VMetadata.TryGetValue('scheme', VValue) then begin
      FIsXYZSchema := SameText(VValue, 'xyz');
    end;
  finally
    VMetadata.Free;
  end;

  // statements
  Self.SetupStatement(
    @FTileDataStmt,
    'SELECT tile_data FROM tiles WHERE zoom_level = ? AND tile_column = ? AND tile_row = ?;'
  );

  Self.SetupStatement(
    @FTileInfoStmt,
    'SELECT length(tile_data) FROM tiles WHERE zoom_level = ? AND tile_column = ? AND tile_row = ?;'
  );

  Self.SetupStatement(
    @FRectInfoStmt,
    'SELECT tile_column, tile_row, length(tile_data) FROM tiles ' +
    'WHERE zoom_level = ? AND tile_column >= ? AND tile_column < ? AND ' +
    IfThen(FIsXYZSchema, 'tile_row >= ? AND tile_row < ?', 'tile_row <= ? AND tile_row > ?') + ';'
  );

  Self.SetupStatement(
    @FEnumTilesStmt,
    'SELECT zoom_level, tile_column, tile_row, tile_data FROM tiles;'
  );

  if not CheckStatement(@FTileDataStmt) or not CheckStatement(@FTileInfoStmt) then begin
    Exit;
  end;

  FEnabled := True;
end;

destructor TTileStorageSQLiteFileConnection.Destroy;
begin
  if FEnabled and FSQLite3DB.IsOpened then begin
    FSQLite3DB.Close;
  end;
  inherited Destroy;
end;

procedure TTileStorageSQLiteFileConnection.FetchMetadataCallback(
  const AHandler: PSQLite3DbHandler;
  const ACallbackPtr: Pointer;
  const AStmtData: PSQLite3StmtData
);
var
  VName, VValue: string;
begin
  VName := LowerCase(AStmtData.ColumnAsString(0));
  VValue := AStmtData.ColumnAsString(1);

  TMetadataDictionary(ACallbackPtr).AddOrSetValue(VName, VValue);
end;

function TTileStorageSQLiteFileConnection.FetchOne(
  const AXY: TPoint;
  const AZoom: Byte;
  const AMode: TGetTileInfoMode
): ITileInfoBasic;
var
  VBindResult: Boolean;
  VBlobSize: Integer;
  VBinaryData: IBinaryData;
  VStmt: TSQLite3StmtData;
begin
  Result := nil;

  if not FEnabled then begin
    Exit;
  end;

  if AMode = gtimWithData then begin
    Assert(FTileDataStmt.FState = ssPrepared);
    VStmt := FTileDataStmt.FStmt;
  end else begin
    Assert(FTileInfoStmt.FState = ssPrepared);
    VStmt := FTileInfoStmt.FStmt;
  end;

  VBindResult :=
    VStmt.BindInt(1, AZoom) and
    VStmt.BindInt(2, AXY.X) and
    VStmt.BindInt(3, CoordToValue(AZoom, AXY.Y));

  if not VBindResult then begin
    FSQLite3DB.RaiseSQLite3Error;
  end;

  try
    if sqlite3_step(VStmt.Stmt) <> SQLITE_ROW then begin
      Exit;
    end;

    if AMode = gtimWithData then begin
      VBinaryData := BlobToBinaryData(VStmt, 0);
      Result := TTileInfoBasicExistsWithTile.Create(0, VBinaryData, nil, FMainContentType);
    end else begin
      VBlobSize := VStmt.ColumnInt(0);
      Result := TTileInfoBasicExists.Create(0, VBlobSize, nil, FMainContentType);
    end;
  finally
    VStmt.Reset;
  end;
end;

function TTileStorageSQLiteFileConnection.FetchRectInfo(
  const ARect: TRect;
  const AZoom: Byte;
  const AOperationID: Integer;
  const ACancelNotifier: INotifierOperation
): ITileRectInfo;
var
  VTile: TPoint;
  VBindResult: Boolean;
  VTop, VBottom: Integer;
  VCount: Integer;
  VTileSize: Integer;
  VItems: TArrayOfTileInfoShortInternal;
  VIndex: Integer;
  VStmt: TSQLite3StmtData;
begin
  Result := nil;

  if not FEnabled or ACancelNotifier.IsOperationCanceled(AOperationID) then begin
    Exit;
  end;

  if not CheckStatement(@FRectInfoStmt) then begin
    FEnabled := False;
    Exit;
  end;

  VStmt := FRectInfoStmt.FStmt;

  VTop := CoordToValue(AZoom, ARect.Top);
  VBottom := CoordToValue(AZoom, ARect.Bottom);

  VBindResult :=
    VStmt.BindInt(1, AZoom) and
    VStmt.BindInt(2, ARect.Left) and
    VStmt.BindInt(3, ARect.Right) and
    VStmt.BindInt(4, VTop) and
    VStmt.BindInt(5, VBottom);

  if not VBindResult then begin
    FSQLite3DB.RaiseSQLite3Error;
  end;

  VCount := 0;
  SetLength(VItems, (ARect.Right - ARect.Left) * (ARect.Bottom - ARect.Top));
  FillChar(VItems[0], SizeOf(TTileInfoShortInternal) * Length(VItems), 0);

  try
    while sqlite3_step(VStmt.Stmt) = SQLITE_ROW do begin
      Inc(VCount);

      if (VCount mod 1024 = 0) and ACancelNotifier.IsOperationCanceled(AOperationID) then begin
        Exit;
      end;

      VTile.X := VStmt.ColumnInt(0);
      VTile.Y := ValueToCoord(AZoom, VStmt.ColumnInt(1));
      VTileSize := VStmt.ColumnInt(2);

      VIndex := TTileRectInfoShort.TileInRectToIndex(VTile, ARect);
      Assert(VIndex >= 0);

      if VIndex >= 0 then begin
        VItems[VIndex].FInfoType := titExists;
        VItems[VIndex].FSize := VTileSize;
      end;
    end;
  finally
    VStmt.Reset;
  end;

  if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
    Exit;
  end;

  Result := TTileRectInfoShort.CreateWithOwn(ARect, AZoom, nil, FMainContentType, VItems);
end;

function TTileStorageSQLiteFileConnection.FetchNext(
  var ATileInfo: TTileInfo
): Boolean;
var
  VStepResult: Integer;
  VStmt: TSQLite3StmtData;
begin
  Result := False;

  if not FEnabled then begin
    Exit;
  end;

  if FFetchNextState = fnsNone then begin
    if CheckStatement(@FEnumTilesStmt) then begin
      FFetchNextState := fnsNext;
    end else begin
      FFetchNextState := fnsError;
      FEnabled := False;
      Exit;
    end;
  end;

  if FFetchNextState <> fnsNext then begin
    Exit;
  end;

  VStmt := FEnumTilesStmt.FStmt;
  try
    VStepResult := sqlite3_step(VStmt.Stmt);

    if VStepResult = SQLITE_ROW then begin
      ATileInfo.FZoom := VStmt.ColumnInt(0);
      ATileInfo.FTile.X := VStmt.ColumnInt(1);
      ATileInfo.FTile.Y := ValueToCoord(ATileInfo.FZoom, VStmt.ColumnInt(2));
      ATileInfo.FLoadDate := 0;
      ATileInfo.FVersionInfo := nil;
      ATileInfo.FData := BlobToBinaryData(VStmt, 3);
      ATileInfo.FContentType := FMainContentType;
      if ATileInfo.FData <> nil then begin
        ATileInfo.FSize := ATileInfo.FData.Size;
        ATileInfo.FInfoType := titExists;
      end else begin
        ATileInfo.FSize := 0;
        ATileInfo.FInfoType := titNotExists;
      end;
      Result := True;
    end else
    if VStepResult = SQLITE_DONE then begin
      FFetchNextState := fnsDone;
    end else begin
      FFetchNextState := fnsError;
    end;
  finally
    if FFetchNextState <> fnsNext then begin
      FEnumTilesStmt.FStmt.Reset;
    end;
  end;
end;

function TTileStorageSQLiteFileConnection.BlobToBinaryData(
  const AStmt: TSQLite3StmtData;
  const AColNum: Integer
): IBinaryData;
var
  VBlobSize: Integer;
  VBlobData: Pointer;
begin
  VBlobSize := AStmt.ColumnBlobSize(AColNum);
  VBlobData := AStmt.ColumnBlobData(AColNum);

  if (VBlobSize > 0) and (VBlobData <> nil) then begin
    Result := TBinaryData.Create(VBlobSize, VBlobData);
  end else begin
    Result := nil; // error
    Assert(False, Self.ClassName + ': Invalid BLOB data!');
  end;
end;

procedure TTileStorageSQLiteFileConnection.SetupStatement(
  const AStmt: PConnectionStatement;
  const ASqlText: UTF8String
);
begin
  AStmt.FState := ssNone;
  AStmt.FText := ASqlText;
end;

function TTileStorageSQLiteFileConnection.CheckStatement(
  const AStmt: PConnectionStatement
): Boolean;
begin
  if AStmt.FState = ssNone then begin
    if FSQLite3DB.PrepareStatement(@AStmt.FStmt, AStmt.FText) then begin
      AStmt.FState := ssPrepared;
    end else begin
      AStmt.FState := ssError;
      FSQLite3DB.RaiseSQLite3Error;
    end;
  end;
  Result := (AStmt.FState = ssPrepared);
end;

function TTileStorageSQLiteFileConnection.CoordToValue(const AZoom: Byte; const Y: Integer): Integer;
begin
  if FIsXYZSchema then begin
    Result := Y;
  end else begin
    Result := (1 shl AZoom) - Y - 1;
  end;
end;

function TTileStorageSQLiteFileConnection.ValueToCoord(const AZoom: Byte; const Y: Integer): Integer;
begin
  if FIsXYZSchema then begin
    Result := Y;
  end else begin
    Result := (1 shl AZoom) - Y - 1;
  end;
end;

end.
