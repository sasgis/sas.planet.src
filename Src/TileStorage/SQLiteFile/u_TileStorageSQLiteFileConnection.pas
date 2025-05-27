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
  TStmtState = (ssNone, ssPrepared, ssError);
  TFetchNextState = (fnsNone, fnsNext, fnsDone, fnsError);

  TConnectionStatement = record
    FState: TStmtState;
    FText: UTF8String;
    FStmt: TSQLite3StmtData;

    procedure Init(
      const ASqlText: UTF8String
    ); inline;

    function CheckPrepared(
      const ASQLite3DB: TSQLite3DbHandler
    ): Boolean;

    procedure Fin;
  end;

  TTileStorageSQLiteFileConnection = class
  protected
    FEnabled: Boolean;
    FSQLite3DB: TSQLite3DbHandler;
    FTileDataStmt: TConnectionStatement;
    FTileInfoStmt: TConnectionStatement;
    FRectInfoStmt: TConnectionStatement;
    FEnumTilesStmt: TConnectionStatement;
    FFetchNextState: TFetchNextState;
    FMainContentType: IContentTypeInfoBasic;
    FFileInfo: ITileStorageSQLiteFileInfo;
    FFileDate: TDateTime;
  protected
    function BlobToBinaryData(
      const AStmt: TSQLite3StmtData;
      const AColNum: Integer
    ): IBinaryData;

    function CoordToValue(const AZoom: Byte; const Y: Integer): Integer; virtual;
    function ValueToCoord(const AZoom: Byte; const Y: Integer): Integer; virtual;

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

    property FileInfo: ITileStorageSQLiteFileInfo read FFileInfo;
  public
    constructor Create(
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
  const AFileName: string;
  const AFileInfo: ITileStorageSQLiteFileInfo;
  const AMainContentType: IContentTypeInfoBasic
);
begin
  inherited Create;

  FFileInfo := AFileInfo;
  FMainContentType := AMainContentType;

  if not FSQLite3DB.Init then begin
    raise Exception.Create('SQLite3 library initialization error!');
  end;

  FSQLite3Db.Open(
    'file:///' + AFileName + '?immutable=1',
    SQLITE_OPEN_READONLY or SQLITE_OPEN_URI or SQLITE_OPEN_NOMUTEX,
    False
  );

  if FFileInfo = nil then begin
    FFileDate := TFile.GetCreationTime(AFileName);
    FFileInfo := TTileStorageSQLiteFileInfo.Create(AFileName, FFileDate);

    Self.FetchMetadata;
  end else begin
    FFileDate := FFileInfo.FileDate;
  end;
end;

destructor TTileStorageSQLiteFileConnection.Destroy;
begin
  FTileDataStmt.Fin;
  FTileInfoStmt.Fin;
  FRectInfoStmt.Fin;
  FEnumTilesStmt.Fin;

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
      Result := TTileInfoBasicExistsWithTile.Create(FFileDate, VBinaryData, nil, FMainContentType);
    end else begin
      VBlobSize := VStmt.ColumnInt(0);
      Result := TTileInfoBasicExists.Create(FFileDate, VBlobSize, nil, FMainContentType);
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

  if not FRectInfoStmt.CheckPrepared(FSQLite3DB) then begin
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
  VStepResult: Integer;
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
      ATileInfo.FZoom := VStmt.ColumnInt(0);
      ATileInfo.FTile.X := VStmt.ColumnInt(1);
      ATileInfo.FTile.Y := ValueToCoord(ATileInfo.FZoom, VStmt.ColumnInt(2));
      ATileInfo.FLoadDate := FFileDate;
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

function TTileStorageSQLiteFileConnection.CoordToValue(const AZoom: Byte; const Y: Integer): Integer;
begin
  Result := Y;
end;

function TTileStorageSQLiteFileConnection.ValueToCoord(const AZoom: Byte; const Y: Integer): Integer;
begin
  Result := Y;
end;

{ TConnectionStatement }

procedure TConnectionStatement.Init(const ASqlText: UTF8String);
begin
  FState := ssNone;
  FText := ASqlText;
end;

procedure TConnectionStatement.Fin;
begin
  if FState = ssPrepared then begin
    FStmt.Fin;
    FState := ssNone;
  end;
end;

function TConnectionStatement.CheckPrepared(const ASQLite3DB: TSQLite3DbHandler): Boolean;
begin
  if FState = ssNone then begin
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
