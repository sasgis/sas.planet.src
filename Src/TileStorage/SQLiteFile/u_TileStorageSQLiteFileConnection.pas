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
  i_BinaryData,
  i_ContentTypeInfo,
  i_TileInfoBasic,
  i_TileStorage,
  i_NotifierOperation,
  i_TileStorageSQLiteFileInfo,
  u_SQLite3Handler;

type
  TConnectionStatement = class
  public
    type TStmtState = (ssNone, ssPrepared, ssError);
  public
    FState: TStmtState;
    FText: UTF8String;
    FStmt: TSQLite3StmtData;
    FIsInvertedY: Boolean;
    FIsInvertedZ: Boolean;
  public
    function BlobToBinaryData(const AColNum: Integer): IBinaryData;
    function CheckPrepared(const ASQLite3DB: TSQLite3DbHandler): Boolean;
  public
    constructor Create(const AIsInvertedY, AIsInvertedZ: Boolean);
    destructor Destroy; override;
  end;

  TTileDataConnectionStatement = class(TConnectionStatement)
  public
    function BindParams(X, Y, Z: Integer): Boolean; virtual; abstract;
    procedure GetResult(out ABlob: IBinaryData); virtual; abstract;
  end;

  TTileInfoConnectionStatement = class(TConnectionStatement)
  public
    function BindParams(X, Y, Z: Integer): Boolean; virtual; abstract;
    procedure GetResult(out ABlobSize: Integer); virtual; abstract;
  end;

  TRectInfoConnectionStatement = class(TConnectionStatement)
  protected
    FZoom: Integer; // may be necessary for GetResult
  public
    function BindParams(const ARect: TRect; Z: Integer): Boolean; virtual; abstract;
    procedure GetResult(out X, Y: Integer; out ASize: Integer); virtual; abstract;
  end;

  TEnumTilesConnectionStatement = class(TConnectionStatement)
  public
    procedure GetResult(out X, Y, Z: Integer; out ABlob: IBinaryData); virtual; abstract;
  end;

  TInsertOrReplaceConnectionStatement = class(TConnectionStatement)
  public
    function BindParams(X, Y, Z: Integer; const ABlob: IBinaryData): Boolean; virtual; abstract;
  end;

  TInsertOrIgnoreConnectionStatement = class(TConnectionStatement)
  public
    function BindParams(X, Y, Z: Integer; const ABlob: IBinaryData): Boolean; virtual; abstract;
  end;

  TDeleteTileConnectionStatement = class(TConnectionStatement)
  public
    function BindParams(X, Y, Z: Integer): Boolean; virtual; abstract;
  end;

  TTileStorageSQLiteFileConnection = class
  private
    type TFetchNextState = (fnsNone, fnsNext, fnsDone, fnsError);
  protected
    FEnabled: Boolean;
    FIsReadOnly: Boolean;
    FSQLite3DB: TSQLite3DbHandler;
    FTileDataStmt: TTileDataConnectionStatement;
    FTileInfoStmt: TTileInfoConnectionStatement;
    FRectInfoStmt: TRectInfoConnectionStatement;
    FEnumTilesStmt: TEnumTilesConnectionStatement;
    FFetchNextState: TFetchNextState;
    FInsertOrReplaceStmt: TInsertOrReplaceConnectionStatement;
    FInsertOrIgnoreStmt: TInsertOrIgnoreConnectionStatement;
    FDeleteTileStmt: TDeleteTileConnectionStatement;
    FMainContentType: IContentTypeInfoBasic;
    FFileInfo: ITileStorageSQLiteFileInfo;
    FFileDate: TDateTime;
  protected
    procedure CreateTables; virtual; abstract;
    procedure FetchMetadata; virtual; abstract;
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

    function Insert(
      const AXY: TPoint;
      const AZoom: Byte;
      const ALoadDate: TDateTime;
      const AData: IBinaryData;
      const AIsOverwrite: Boolean
    ): Boolean;

    function Delete(
      const AXY: TPoint;
      const AZoom: Byte
    ): Boolean;

    property FileInfo: ITileStorageSQLiteFileInfo read FFileInfo;
  public
    constructor Create(
      const AIsReadOnly: Boolean;
      const AFileName: string;
      const AFileInfo: ITileStorageSQLiteFileInfo;
      const AMainContentType: IContentTypeInfoBasic
    );
    destructor Destroy; override;
  end;

implementation

uses
  IOUtils,
  libsqlite3,
  u_BinaryData,
  u_TileInfoBasic,
  u_TileRectInfoShort,
  u_TileStorageSQLiteFileInfo;

{ TTileStorageSQLiteFileConnection }

constructor TTileStorageSQLiteFileConnection.Create(
  const AIsReadOnly: Boolean;
  const AFileName: string;
  const AFileInfo: ITileStorageSQLiteFileInfo;
  const AMainContentType: IContentTypeInfoBasic
);
var
  VNeedCreateTables: Boolean;
begin
  inherited Create;

  FIsReadOnly := AIsReadOnly;
  FFileInfo := AFileInfo;
  FMainContentType := AMainContentType;

  if not FSQLite3DB.Init then begin
    raise Exception.Create('SQLite3 library initialization error!');
  end;

  VNeedCreateTables := False;
  if FFileInfo = nil then begin
    // this is our first attempt to open db
    if not FileExists(AFileName) then begin
      if FIsReadOnly then begin
        raise Exception.CreateFmt('File not found: "%s"', [AFileName]);
      end else begin
        VNeedCreateTables := True;
        if not ForceDirectories(ExtractFileDir(AFileName)) then begin
          RaiseLastOSError;
        end;
      end;
    end;
  end;

  if FIsReadOnly then begin
    FSQLite3Db.Open(
      'file:///' + AFileName + '?immutable=1',
      SQLITE_OPEN_READONLY or SQLITE_OPEN_URI or SQLITE_OPEN_NOMUTEX,
      False
    );
  end else begin
    FSQLite3Db.Open(AFileName, SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE or SQLITE_OPEN_NOMUTEX, False);
    FSQLite3DB.ExecSQL('PRAGMA encoding = "UTF-8"');
    FSQLite3DB.ExecSQL('PRAGMA synchronous = NORMAL');
    FSQLite3DB.ExecSQL('PRAGMA journal_mode = WAL');
  end;

  if FFileInfo = nil then begin
    FFileDate := TFile.GetCreationTime(AFileName);
    FFileInfo := TTileStorageSQLiteFileInfo.Create(AFileName, FFileDate);

    if VNeedCreateTables then begin
      Self.CreateTables;
    end;

    Self.FetchMetadata;
  end else begin
    FFileDate := FFileInfo.FileDate;
  end;
end;

destructor TTileStorageSQLiteFileConnection.Destroy;
begin
  FreeAndNil(FTileDataStmt);
  FreeAndNil(FTileInfoStmt);
  FreeAndNil(FRectInfoStmt);
  FreeAndNil(FEnumTilesStmt);

  if FSQLite3DB.IsOpened then begin
    FSQLite3DB.Close;
  end;

  inherited Destroy;
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
  VStmtData: TSQLite3StmtData;
begin
  Result := nil;

  if not FEnabled then begin
    Exit;
  end;

  if AMode = gtimWithData then begin
    Assert(FTileDataStmt.FState = ssPrepared);
    VBindResult := FTileDataStmt.BindParams(AXY.X, AXY.Y, AZoom);
    VStmtData := FTileDataStmt.FStmt;
  end else begin
    Assert(FTileInfoStmt.FState = ssPrepared);
    VBindResult := FTileInfoStmt.BindParams(AXY.X, AXY.Y, AZoom);
    VStmtData := FTileInfoStmt.FStmt;
  end;

  if not VBindResult then begin
    FSQLite3DB.RaiseSQLite3Error;
  end;

  try
    if sqlite3_step(VStmtData.Stmt) <> SQLITE_ROW then begin
      Exit;
    end;

    if AMode = gtimWithData then begin
      FTileDataStmt.GetResult(VBinaryData);
      Assert(VBinaryData <> nil);
      Result := TTileInfoBasicExistsWithTile.Create(FFileDate, VBinaryData, nil, FMainContentType);
    end else begin
      FTileInfoStmt.GetResult(VBlobSize);
      Result := TTileInfoBasicExists.Create(FFileDate, VBlobSize, nil, FMainContentType);
    end;
  finally
    VStmtData.Reset;
  end;
end;

function TTileStorageSQLiteFileConnection.FetchRectInfo(
  const ARect: TRect;
  const AZoom: Byte;
  const AOperationID: Integer;
  const ACancelNotifier: INotifierOperation
): ITileRectInfo;
var
  X, Y: Integer;
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

  if not FRectInfoStmt.CheckPrepared(FSQLite3DB) then begin
    FEnabled := False;
    Exit;
  end;

  if not FRectInfoStmt.BindParams(ARect, AZoom) then begin
    FSQLite3DB.RaiseSQLite3Error;
  end;

  VCount := 0;
  SetLength(VItems, (ARect.Right - ARect.Left) * (ARect.Bottom - ARect.Top));
  FillChar(VItems[0], SizeOf(TTileInfoShortInternal) * Length(VItems), 0);

  VStmt := FRectInfoStmt.FStmt;
  try
    while sqlite3_step(VStmt.Stmt) = SQLITE_ROW do begin
      Inc(VCount);

      if (VCount mod 1024 = 0) and ACancelNotifier.IsOperationCanceled(AOperationID) then begin
        Exit;
      end;

      FRectInfoStmt.GetResult(X, Y, VTileSize);

      VIndex := TTileRectInfoShort.TileInRectToIndex(Point(X, Y), ARect);
      Assert(VIndex >= 0);

      if VIndex >= 0 then begin
        VItems[VIndex].FInfoType := titExists;
        VItems[VIndex].FSize := VTileSize;
        VItems[VIndex].FLoadDate := FFileDate;
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
  X, Y, Z: Integer;
  VStepResult: Integer;
  VBinaryData: IBinaryData;
  VStmt: TSQLite3StmtData;
begin
  Result := False;

  if not FEnabled then begin
    Exit;
  end;

  if FFetchNextState = fnsNone then begin
    if FEnumTilesStmt.CheckPrepared(FSQLite3DB) then begin
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

      FEnumTilesStmt.GetResult(X, Y, Z, VBinaryData);

      ATileInfo.FTile.X := X;
      ATileInfo.FTile.Y := Y;
      ATileInfo.FZoom := Z;
      ATileInfo.FLoadDate := FFileDate;
      ATileInfo.FVersionInfo := nil;
      ATileInfo.FData := VBinaryData;
      ATileInfo.FContentType := FMainContentType;
      if VBinaryData <> nil then begin
        ATileInfo.FSize := VBinaryData.Size;
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

function TTileStorageSQLiteFileConnection.Insert(
  const AXY: TPoint;
  const AZoom: Byte;
  const ALoadDate: TDateTime;
  const AData: IBinaryData;
  const AIsOverwrite: Boolean
): Boolean;
var
  VStmt: TSQLite3StmtData;
begin
  Result := False;

  if not FEnabled or (FInsertOrReplaceStmt = nil) or (FInsertOrIgnoreStmt = nil) then begin
    Exit;
  end;

  if AIsOverwrite then begin
    if not FInsertOrReplaceStmt.CheckPrepared(FSQLite3DB) then begin
      FEnabled := False;
      Exit;
    end;

    if not FInsertOrReplaceStmt.BindParams(AXY.X, AXY.Y, AZoom, AData) then begin
      FSQLite3DB.RaiseSQLite3Error;
    end;

    VStmt := FInsertOrReplaceStmt.FStmt;
  end else begin
    if not FInsertOrIgnoreStmt.CheckPrepared(FSQLite3DB) then begin
      FEnabled := False;
      Exit;
    end;

    if not FInsertOrIgnoreStmt.BindParams(AXY.X, AXY.Y, AZoom, AData) then begin
      FSQLite3DB.RaiseSQLite3Error;
    end;

    VStmt := FInsertOrIgnoreStmt.FStmt;
  end;

  try
    Result := (sqlite3_step(VStmt.Stmt) = SQLITE_DONE);
  finally
    VStmt.Reset;
  end;
end;

function TTileStorageSQLiteFileConnection.Delete(
  const AXY: TPoint;
  const AZoom: Byte
): Boolean;
begin
  Result := False;

  if not FEnabled or (FDeleteTileStmt = nil) then begin
    Exit;
  end;

  if not FDeleteTileStmt.CheckPrepared(FSQLite3DB) then begin
    FEnabled := False;
    Exit;
  end;

  if not FDeleteTileStmt.BindParams(AXY.X, AXY.Y, AZoom) then begin
    FSQLite3DB.RaiseSQLite3Error;
  end;

  try
    Result := (sqlite3_step(FDeleteTileStmt.FStmt.Stmt) = SQLITE_DONE);
  finally
    FDeleteTileStmt.FStmt.Reset;
  end;
end;

{ TConnectionStatement }

constructor TConnectionStatement.Create(const AIsInvertedY, AIsInvertedZ: Boolean);
begin
  inherited Create;
  FState := ssNone;
  FText := '';
  FIsInvertedY := AIsInvertedY;
  FIsInvertedZ := AIsInvertedZ;
end;

destructor TConnectionStatement.Destroy;
begin
  if FState = ssPrepared then begin
    FStmt.Fin;
    FState := ssNone;
  end;
  inherited Destroy;
end;

function TConnectionStatement.BlobToBinaryData(const AColNum: Integer): IBinaryData;
var
  VBlobSize: Integer;
  VBlobData: Pointer;
begin
  VBlobSize := FStmt.ColumnBlobSize(AColNum);
  VBlobData := FStmt.ColumnBlobData(AColNum);

  if (VBlobSize > 0) and (VBlobData <> nil) then begin
    Result := TBinaryData.Create(VBlobSize, VBlobData);
  end else begin
    Result := nil; // error
    Assert(False, Self.ClassName + ': Invalid BLOB data!');
  end;
end;

function TConnectionStatement.CheckPrepared(const ASQLite3DB: TSQLite3DbHandler): Boolean;
begin
  if FState = ssNone then begin
    Assert(FText <> '');
    if ASQLite3DB.PrepareStatement(@FStmt, FText) then begin
      FState := ssPrepared;
    end else begin
      FState := ssError;
      ASQLite3DB.RaiseSQLite3Error;
    end;
  end;
  Result := (FState = ssPrepared);
end;

end.
