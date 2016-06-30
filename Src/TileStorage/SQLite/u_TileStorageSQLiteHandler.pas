unit u_TileStorageSQLiteHandler;

interface

uses
  Windows,
  Classes,
  SyncObjs,
  SQLite3Handler,
  t_TileStorageSQLite,
  t_NotifierOperationRec,
  t_TileStorageSQLiteHandler,
  i_BinaryData,
  i_ContentTypeInfo,
  i_InterfaceListSimple,
  i_MapVersionInfo,
  i_MapVersionListStatic,
  i_TileInfoBasic,
  i_TileStorageSQLiteHandler,
  i_TileStorageSQLiteHolder,
  u_BaseInterfacedObject;

type
  TTileStorageSQLiteHandler = class(TBaseInterfacedObject, ITileStorageSQLiteHandler)
  private
    FSQLite3DbHandler: TSQLite3DbHandler;
    FTileStorageSQLiteHolder: ITileStorageSQLiteHolder;
    FDBFilename: string;
    FSingleVersionOnly: IMapVersionInfo;
    FUseVersionFieldInDB: Boolean;
  protected
    FSQLite3DbModifyLock: TCriticalSection;
  protected
    procedure InternalCheckStructure; virtual; abstract;

    function GetSQL_DeleteTile(
      const ADeleteTileAllData: PDeleteTileAllData
    ): AnsiString; virtual; abstract;

    function GetSQL_TileRectInfo(
      const AUsePrevVersions: Boolean;
      const AEnumData: TTileInfoShortEnumData
    ): AnsiString; virtual; abstract;

    procedure CallbackSelectTileRectInfo(
      const AHandler: PSQLite3DbHandler;
      const ACallbackPtr: Pointer;
      const AStmtData: PSQLite3StmtData
    ); virtual; abstract;
  private
    procedure LogError(
      const ACmd: AnsiChar;
      const AMsg: String;
      const ARaiseError: Boolean = False
    );
    procedure ExecuteSQL(const ASQLStatement: AnsiString);
  private
    { ITileStorageSQLiteHandler }
    function Opened: Boolean;

    function GetTileInfo(
      const AOper: PNotifierOperationRec;
      const AXY: TPoint;
      const AZoom: Byte;
      const AVersionInfo: IMapVersionInfo;
      const AMode: TGetTileInfoModeSQLite;
      const AUsePrevVersions: Boolean;
      const AResult: PGetTileResult
    ): Boolean; virtual; abstract;

    function DeleteTile(
      const ADeleteTileAllData: PDeleteTileAllData
    ): Boolean;

    function SaveTile(
      const ASaveTileAllData: PSaveTileAllData
    ): Boolean; virtual; abstract;

    function GetListOfTileVersions(
      const AOper: PNotifierOperationRec;
      const AXY: TPoint;
      const AZoom: byte;
      const AVersionInfo: IMapVersionInfo
    ): IMapVersionListStatic; virtual; abstract;

    function GetTileRectInfo(
      const AOper: PNotifierOperationRec;
      const AUsePrevVersions: Boolean;
      const AEnumData: TTileInfoShortEnumData
    ): Boolean;
  public
    function GetSQLite3DbHandlerPtr: Pointer;
    property SQLite3DbHandlerPtr: Pointer read GetSQLite3DbHandlerPtr;
  public
    constructor Create(
      const ATileStorageSQLiteHolder: ITileStorageSQLiteHolder;
      const ADBFilename: string;
      const ASingleVersionOnly: IMapVersionInfo;
      const AUseVersionFieldInDB: Boolean
    );
    destructor Destroy; override;
  end;

  TTileStorageSQLiteHandlerComplex = class(TTileStorageSQLiteHandler)
  private
    FTBColInfo: TTBColInfo;
  private
    procedure CallbackSelectTileInfo(
      const AHandler: PSQLite3DbHandler;
      const ACallbackPtr: Pointer;
      const AStmtData: PSQLite3StmtData
    );
    procedure CallbackSelectVersions(
      const AHandler: PSQLite3DbHandler;
      const ACallbackPtr: Pointer;
      const AStmtData: PSQLite3StmtData
    );
    procedure CallbackSelectCols(
      const AHandler: PSQLite3DbHandler;
      const ACallbackPtr: Pointer;
      const AStmtData: PSQLite3StmtData
    );
  protected
    procedure InternalCheckStructure; override;

    procedure CallbackSelectTileRectInfo(
      const AHandler: PSQLite3DbHandler;
      const ACallbackPtr: Pointer;
      const AStmtData: PSQLite3StmtData
    ); override;

    function GetTileInfo(
      const AOper: PNotifierOperationRec;
      const AXY: TPoint;
      const AZoom: Byte;
      const AVersionInfo: IMapVersionInfo;
      const AMode: TGetTileInfoModeSQLite;
      const AUsePrevVersions: Boolean;
      const AResult: PGetTileResult
    ): Boolean; override;

    function SaveTile(
      const ASaveTileAllData: PSaveTileAllData
    ): Boolean; override;

    function GetListOfTileVersions(
      const AOper: PNotifierOperationRec;
      const AXY: TPoint;
      const AZoom: byte;
      const AVersionInfo: IMapVersionInfo
    ): IMapVersionListStatic; override;
  protected
    function GetSQL_DeleteTile(
      const ADeleteTileAllData: PDeleteTileAllData
    ): AnsiString; override;

    function GetSQL_TileRectInfo(
      const AUsePrevVersions: Boolean;
      const AEnumData: TTileInfoShortEnumData
    ): AnsiString; override;
  public
    function GetListOfVersions(const AOper: PNotifierOperationRec): IMapVersionListStatic;
    function GetTBColInfoPtr: Pointer;
  end;

implementation

uses
  SysUtils,
  DateUtils,
  ALString,
  AlSqlite3Wrapper,
  CRC32,
  c_TileStorageSQLite,
  u_InterfaceListSimple,
  u_MapVersionListStatic,
  u_TileInfoBasic,
  u_TileRectInfoShort,
  u_TileStorageSQLiteFunc;

const
  cReplaceOrIgnore: array [Boolean] of AnsiString = ('REPLACE', 'IGNORE');

type
  PGetTileRectInfoCancellable = ^TGetTileRectInfoCancellable;
  TGetTileRectInfoCancellable = record
    AEnumDataPtr: PTileInfoShortEnumData;
    AOperationPtr: PNotifierOperationRec;
    ACancelled: Boolean;
  end;

type
  TSelectTileVersions = record
    ListOfVersions: IInterfaceListSimple;
    OperationPtr: PNotifierOperationRec;
    Cancelled: Boolean;
    StoreEmptyVersion: Boolean;
  public
    procedure Init;
    procedure Uninit;
    function GetMapVersionInfoStatic: IMapVersionListStatic;
  end;
  PSelectTileVersions = ^TSelectTileVersions;

{ TSelectTileVersions }

procedure TSelectTileVersions.Init;
begin
  Cancelled := False;
  OperationPtr := nil;
  ListOfVersions := TInterfaceListSimple.Create;
  StoreEmptyVersion := False;
end;

procedure TSelectTileVersions.Uninit;
begin
  OperationPtr := nil;
end;

function TSelectTileVersions.GetMapVersionInfoStatic: IMapVersionListStatic;
begin
  if not Cancelled and (ListOfVersions.Count > 0) then begin
    Result := TMapVersionListStatic.Create(ListOfVersions.MakeStaticCopy, True);
  end else begin
    Result := nil;
  end;
end;

function GetOrderByVersion(
  const ATBColInfoModeV: TVersionColMode;
  const AIsOrderDESC: Boolean
): AnsiString; inline;
var
  VOrder: AnsiString;
begin
  if AIsOrderDESC then begin
    VOrder := ' DESC';
  end else begin
    VOrder := ' ASC';
  end;
  case ATBColInfoModeV of
    vcm_Int: begin
      // версия в БД целочисленная
      Result := 'v' + VOrder;
    end;
    vcm_Text: begin
      // версия в БД текстовая
      Result := 'v COLLATE ' + cLogicalSortingCollation + VOrder;
    end;
  else
    begin
      Assert(False, IntToStr(Ord(ATBColInfoModeV)));
    end;
  end;
end;

function GetOrderByVersion_DESC(
  const ATBColInfoModeV: TVersionColMode
): AnsiString; inline;
begin
  Result := GetOrderByVersion(ATBColInfoModeV, True);
end;

function GetOrderByVersion_ASC(
  const ATBColInfoModeV: TVersionColMode
): AnsiString; inline;
begin
  Result := GetOrderByVersion(ATBColInfoModeV, False);
end;

function VersionFieldIsEqualOrMax(
  const ARequestedVersionIsInt: Boolean;
  const ATBColInfoModeV: TVersionColMode;
  const ARequestedVersionToDB: AnsiString;
  const AXY: TPoint
): AnsiString;
var
  VVer, VMax: AnsiString;
begin
  if (ATBColInfoModeV = vcm_Int) and not ARequestedVersionIsInt then begin
    // версия в БД целочисленная, но дали текстовую
    VVer := 'cast(v as TEXT)';
  end else begin
    VVer := 'v';
  end;

  VMax :=
    '(SELECT v FROM t' +
    ' WHERE x=' + ALIntToStr(AXY.X) +
    ' AND y=' + ALIntToStr(AXY.Y) +
    ' ORDER BY ' + GetOrderByVersion_DESC(ATBColInfoModeV) + ' LIMIT 1)';

  Result := VVer + ' IN (' + ARequestedVersionToDB + ',' + VMax + ')';
end;

function GetCheckPrevVersionBySizeSQLText(
  const AXY: TPoint;
  const ATileSize: Integer
): AnsiString;
var
  s: AnsiString;
begin
  s :=
    'SELECT s FROM t WHERE ' +
    'x=' + ALIntToStr(AXY.X) + ' AND ' +
    'y=' + ALIntToStr(AXY.Y) + ' AND ' +
    's=' + ALIntToStr(ATileSize);

  // insert or replace where size <>
  Result := ALIntToStr(ATileSize) + '<>' + 'COALESCE((' + s + '),-1)';
end;

function GetCheckPrevVersionByCRC32SQLText(
  const AXY: TPoint;
  const ATileCRC32: Cardinal
): AnsiString;
var
  h: AnsiString;
begin
  h :=
    'SELECT h FROM t WHERE ' +
    'x=' + ALIntToStr(AXY.X) + ' AND ' +
    'y=' + ALIntToStr(AXY.Y) + ' AND ' +
    'h=' + ALIntToStr(ATileCRC32);

  // insert or replace where crc32 <>
  Result := ALIntToStr(ATileCRC32) + '<>' + 'COALESCE((' + h + '),-1)';
end;

{ TTileStorageSQLiteHandler }

constructor TTileStorageSQLiteHandler.Create(
  const ATileStorageSQLiteHolder: ITileStorageSQLiteHolder;
  const ADBFilename: string;
  const ASingleVersionOnly: IMapVersionInfo;
  const AUseVersionFieldInDB: Boolean
);
var
  VOpenFlags: Integer;
begin
  Assert(ATileStorageSQLiteHolder <> nil);
  inherited Create;
  FTileStorageSQLiteHolder := ATileStorageSQLiteHolder;
  FDBFilename := ADBFilename;
  FSingleVersionOnly := ASingleVersionOnly;
  FUseVersionFieldInDB := AUseVersionFieldInDB;
  FSQLite3DbModifyLock := TCriticalSection.Create;

  // open database and prepare to work
  try
    if FSQLite3DbHandler.Init then begin
      // open existing or create new
      VOpenFlags := SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE;
      FSQLite3DbHandler.Open(FDBFilename, VOpenFlags, True);
      // apply session params
      FTileStorageSQLiteHolder.ExecMakeSession(ExecuteSQL);
      // make or check structure
      InternalCheckStructure;
      // apply working params
      FTileStorageSQLiteHolder.ExecEstablished(ExecuteSQL);
      // register BUSY-handler
      FSQLite3DbHandler.SetBusyTryCount(3);
    end;
  except
    on E: Exception do begin
      // no sqlite or another error
      LogError(c_Log_Init, FSQLite3DbHandler.LibVersionInfo);
      FSQLite3DbHandler.Close;
      LogError(c_Log_Init, E.Message);
    end
  end;
end;

function TTileStorageSQLiteHandler.DeleteTile(
  const ADeleteTileAllData: PDeleteTileAllData
): Boolean;
var
  VRowsAffected: Integer;
begin
  Result := False;
  VRowsAffected := 0;
  try
    FSQLite3DbModifyLock.Acquire;
    try
      FSQLite3DbHandler.ExecSQL(
        GetSQL_DeleteTile(ADeleteTileAllData),
        @VRowsAffected
      );
    finally
      FSQLite3DbModifyLock.Release;
    end;
    Result := (VRowsAffected > 0);
  except
    on E: Exception do
      LogError(c_Log_Delete, E.Message);
  end;
end;

destructor TTileStorageSQLiteHandler.Destroy;
begin
  FSQLite3DbHandler.Close;
  FSQLite3DbModifyLock.Free;
  FTileStorageSQLiteHolder := nil;
  inherited Destroy;
end;

procedure TTileStorageSQLiteHandler.ExecuteSQL(const ASQLStatement: AnsiString);
begin
  FSQLite3DbHandler.ExecSQL(ASQLStatement);
end;

function TTileStorageSQLiteHandler.GetSQLite3DbHandlerPtr: Pointer;
begin
  Result := @FSQLite3DbHandler;
end;

function TTileStorageSQLiteHandler.GetTileRectInfo(
  const AOper: PNotifierOperationRec;
  const AUsePrevVersions: Boolean;
  const AEnumData: TTileInfoShortEnumData
): Boolean;
var
  VData: TGetTileRectInfoCancellable;
begin
  VData.AEnumDataPtr := @AEnumData;
  VData.AOperationPtr := AOper;
  VData.ACancelled := False;
  try
    // call
    FSQLite3DbHandler.OpenSQL(
      GetSQL_TileRectInfo(AUsePrevVersions, AEnumData),
      CallbackSelectTileRectInfo,
      @VData,
      True
    );
  except
    on E: Exception do
      LogError(c_Log_GetMap, E.Message);
  end;
  Result := not VData.ACancelled;
end;

procedure TTileStorageSQLiteHandler.LogError(
  const ACmd: AnsiChar;
  const AMsg: String;
  const ARaiseError: Boolean
);
begin
  FTileStorageSQLiteHolder.LogError(ACmd, AMsg, ARaiseError);
end;

function TTileStorageSQLiteHandler.Opened: Boolean;
begin
  Result := FSQLite3DbHandler.Opened;
end;

{ TTileStorageSQLiteHandlerComplex }

procedure TTileStorageSQLiteHandlerComplex.CallbackSelectCols(
  const AHandler: PSQLite3DbHandler;
  const ACallbackPtr: Pointer;
  const AStmtData: PSQLite3StmtData
);
var
  I: Integer;
  VName: PAnsiChar;
  VFound: Byte;
begin
  VFound := 3;

  with PTBColInfo(ACallbackPtr)^ do begin
    ColCount := AStmtData^.ColumnCount;
    for I := 0 to ColCount - 1 do begin
      VName := AStmtData^.ColumnName(I);
      if StrIComp(VName, 'v') = 0 then begin
        VName := AStmtData^.ColumnDeclType(I);
        if StrIComp(VName, 'TEXT') = 0 then begin
          ModeV := vcm_Text;
        end else begin
          ModeV := vcm_Int;
        end;
        Dec(VFound);
      end else if StrIComp(VName, 'c') = 0 then begin
        HasC := True;
        Dec(VFound);
      end else if StrIComp(VName, 'h') = 0 then begin
        HasH := True;
        Dec(VFound);
      end;
      if VFound = 0 then begin
        Break;
      end;
    end;
  end;

  AStmtData^.Cancelled := True;
end;

procedure TTileStorageSQLiteHandlerComplex.CallbackSelectTileInfo(
  const AHandler: PSQLite3DbHandler;
  const ACallbackPtr: Pointer;
  const AStmtData: PSQLite3StmtData
);
var
  VOriginalTileSize: Integer;
  VBlobSize: Integer;
  VTemp: Int64;
  VDateTime: TDateTime;
  VColType: Integer;
  VVersionStr: String;
  VContentType: AnsiString;
  VBinaryData: IBinaryData;
begin
  // s,d[,v][,c][,b]
  // здесь читаем только один тайл
  AStmtData^.Cancelled := True;

  with PSelectTileInfoComplex(ACallbackPtr)^.TileResult^ do begin
    // размер и дату тащим даже без запроса пользователя
    GExtraMode := GExtraMode + [gtiiSize, gtiiLoadDate];
  end;

  // original tile size (in bytes)
  VOriginalTileSize := AStmtData^.ColumnInt(0);

  // time (in unix seconds)
  VTemp := AStmtData^.ColumnInt64(1);
  VDateTime := UnixToDateTime(VTemp);

  // version
  if FTBColInfo.ModeV <> vcm_None then begin
    // get version as field 2
    VColType := AStmtData^.ColumnType(2);
    case VColType of
      SQLITE_NULL: begin
        // null value
        with PSelectTileInfoComplex(ACallbackPtr)^ do begin
          if RequestedVersionIsInt and (RequestedVersionAsInt = cDefaultVersionAsIntValue) then begin
            // use source version
          end else begin
            // make new empty version
            RequestedVersionInfo := FTileStorageSQLiteHolder.GetVersionInfo(cDefaultVersionAsStrValue);
          end;
        end;
      end;
      SQLITE_INTEGER: begin
        // version as integer
        VTemp := AStmtData^.ColumnInt64(2);
        if VTemp = cDefaultVersionAsIntValue then begin
          VVersionStr := cDefaultVersionAsStrValue;
        end else begin
          VVersionStr := IntToStr(VTemp);
        end;
        with PSelectTileInfoComplex(ACallbackPtr)^ do begin
          if RequestedVersionIsInt then begin
            // check given version was the same
            if RequestedVersionAsInt <> VTemp then begin
              // make new version
              RequestedVersionInfo := FTileStorageSQLiteHolder.GetVersionInfo(VVersionStr);
            end;
          end else begin
            // make new version
            RequestedVersionInfo := FTileStorageSQLiteHolder.GetVersionInfo(VVersionStr);
          end;
        end;
      end;
    else
      begin
        // SQLITE_FLOAT, SQLITE_BLOB, SQLITE_TEXT
        VVersionStr := AStmtData^.ColumnAsString(2);
        with PSelectTileInfoComplex(ACallbackPtr)^ do begin
          if (RequestedVersionInfo = nil) or not SameText(RequestedVersionInfo.StoreString, VVersionStr) then begin
            // make new version
            RequestedVersionInfo := FTileStorageSQLiteHolder.GetVersionInfo(VVersionStr);
          end;
        end;
      end;
    end;
  end;

  if VOriginalTileSize <= 0 then begin
    // TNE
    PSelectTileInfoComplex(ACallbackPtr)^.TileResult^.GTileInfo :=
      TTileInfoBasicTNE.Create(
        VDateTime,
        PSelectTileInfoComplex(ACallbackPtr)^.RequestedVersionInfo
      );
    Exit;
  end;

  // content-type
  if FTBColInfo.HasC then begin
    // get content_type (FieldIndex = 2 + Ord(FTBColInfo.HasV)
    VContentType := AStmtData^.ColumnAsAnsiString(2 + Ord(FTBColInfo.ModeV <> vcm_None));
  end else begin
    // use default content_type
    VContentType := '';
  end;

  with PSelectTileInfoComplex(ACallbackPtr)^.TileResult^ do begin
    // тип тайла тут всегда тащим
    GExtraMode := GExtraMode + [gtiiContentType];
  end;

  // treat as tile
  if gtiiBody in PSelectTileInfoComplex(ACallbackPtr)^.SelectMode then begin
    // get tile with body
    VColType := 2 + Ord(FTBColInfo.ModeV <> vcm_None) + Ord(FTBColInfo.HasC);
    VBlobSize := AStmtData^.ColumnBlobSize(VColType);
    if VBlobSize <= 0 then begin
      // TNE ?!
      PSelectTileInfoComplex(ACallbackPtr)^.TileResult^.GTileInfo :=
        TTileInfoBasicTNE.Create(
          VDateTime,
          PSelectTileInfoComplex(ACallbackPtr)^.RequestedVersionInfo
        );
    end else begin
      // has body
      VBinaryData :=
        CreateTileBinaryData(
          VOriginalTileSize,
          VBlobSize,
          AStmtData^.ColumnBlobData(VColType)
        );
      PSelectTileInfoComplex(ACallbackPtr)^.TileResult^.GTileInfo :=
        TTileInfoBasicExistsWithTile.Create(
          VDateTime,
          VBinaryData,
          PSelectTileInfoComplex(ACallbackPtr)^.RequestedVersionInfo,
          FTileStorageSQLiteHolder.GetContentTypeInfo(VContentType)
        );
    end;
  end else begin
    // no need tile body
    PSelectTileInfoComplex(ACallbackPtr)^.TileResult^.GTileInfo :=
      TTileInfoBasicExists.Create(
        VDateTime,
        VOriginalTileSize,
        PSelectTileInfoComplex(ACallbackPtr)^.RequestedVersionInfo,
        FTileStorageSQLiteHolder.GetContentTypeInfo(VContentType)
      );
  end;
end;

procedure TTileStorageSQLiteHandlerComplex.CallbackSelectTileRectInfo(
  const AHandler: PSQLite3DbHandler;
  const ACallbackPtr: Pointer;
  const AStmtData: PSQLite3StmtData
);
var
  VIndex: Integer;
  VXY: TPoint;
  VDataPtr: PGetTileRectInfoCancellable;
  VPtr: PTileInfoShortEnumData;
begin
  VDataPtr := PGetTileRectInfoCancellable(ACallbackPtr);

  // check
  with VDataPtr^ do begin
    if AOperationPtr^.IsOperationCancelled then begin
      AStmtData^.Cancelled := True;
      ACancelled := True;
      Exit;
    end;
  end;

  // x,y,d,s
  VXY.X := AStmtData^.ColumnInt(0);
  VXY.Y := AStmtData^.ColumnInt(1);

  VPtr := VDataPtr^.AEnumDataPtr;

  // get index in array
  VIndex := TTileRectInfoShort.TileInRectToIndex(VXY, VPtr^.DestRect);

  // apply values
  with VPtr^.RectItems[VIndex] do begin
    FLoadDate := UnixToDateTime(AStmtData^.ColumnInt64(2));
    FSize := AStmtData^.ColumnInt(3);
    if FSize > 0 then begin
      FInfoType := titExists;
    end else begin
      FInfoType := titTneExists;
    end;
  end;
end;

procedure TTileStorageSQLiteHandlerComplex.CallbackSelectVersions(
  const AHandler: PSQLite3DbHandler;
  const ACallbackPtr: Pointer;
  const AStmtData: PSQLite3StmtData
);
var
  VData: PSelectTileVersions;
  VColType: Integer;
  VTemp: Int64;
  VVersionStr: string;
  VVersionInfo: IMapVersionInfo;
begin
  VData := PSelectTileVersions(ACallbackPtr);

  // check
  with VData^ do begin
    if OperationPtr^.IsOperationCancelled then begin
      AStmtData^.Cancelled := True;
      Cancelled := True;
      Exit;
    end;
  end;

  // make version
  VColType := AStmtData^.ColumnType(0);
  case VColType of
    SQLITE_NULL: begin
      // NULL - use empty string
      VVersionStr := cDefaultVersionAsStrValue;
    end;
    SQLITE_INTEGER: begin
      // Int64
      VTemp := AStmtData^.ColumnInt64(0);
      if VTemp = cDefaultVersionAsIntValue then begin
        VVersionStr := cDefaultVersionAsStrValue;
      end else begin
        VVersionStr := IntToStr(VTemp);
      end;
    end;
  else
    begin
      // SQLITE_FLOAT, SQLITE_BLOB, SQLITE_TEXT
      VVersionStr := AStmtData^.ColumnAsString(0);
    end;
  end;

  if VData^.StoreEmptyVersion or (VVersionStr <> cDefaultVersionAsStrValue) then begin
    VVersionInfo := FTileStorageSQLiteHolder.GetVersionInfo(VVersionStr);
  end else begin
    VVersionInfo := nil;
  end;

  // add it to list
  if VVersionInfo <> nil then begin
    VData^.ListOfVersions.Add(VVersionInfo);
  end;
end;

function TTileStorageSQLiteHandlerComplex.GetListOfTileVersions(
  const AOper: PNotifierOperationRec;
  const AXY: TPoint;
  const AZoom: byte;
  const AVersionInfo: IMapVersionInfo
): IMapVersionListStatic;
var
  VSelectTileVersions: TSelectTileVersions;
  VSQLText: AnsiString;
begin
  if not FUseVersionFieldInDB or (FTBColInfo.ModeV = vcm_None) then begin
    // no versions
    Result := nil;
    Exit;
  end;

  VSQLText :=
    'SELECT v FROM t' +
    ' WHERE x=' + ALIntToStr(AXY.X) +
    ' AND y=' + ALIntToStr(AXY.Y) +
    ' ORDER BY ' + GetOrderByVersion_DESC(FTBColInfo.ModeV);

  VSelectTileVersions.Init;
  try
    VSelectTileVersions.OperationPtr := AOper;
    VSelectTileVersions.StoreEmptyVersion := False;
    try
      FSQLite3DbHandler.OpenSQL(
        VSQLText,
        CallbackSelectVersions,
        @VSelectTileVersions,
        True
      );
      // return versions
      Result := VSelectTileVersions.GetMapVersionInfoStatic;
    except
      on E: Exception do
        LogError(c_Log_GetVer, E.Message);
    end;
  finally
    VSelectTileVersions.Uninit;
  end;
end;

function TTileStorageSQLiteHandlerComplex.GetListOfVersions(
  const AOper: PNotifierOperationRec
): IMapVersionListStatic;
var
  VSelectTileVersions: TSelectTileVersions;
  VSQLText: AnsiString;
begin
  if not FUseVersionFieldInDB or (FTBColInfo.ModeV = vcm_None) then begin
    // no versions
    Result := nil;
    Exit;
  end;

  VSQLText := 'SELECT DISTINCT v FROM t';

  VSelectTileVersions.Init;
  try
    VSelectTileVersions.OperationPtr := AOper;
    VSelectTileVersions.StoreEmptyVersion := True;
    try
      FSQLite3DbHandler.OpenSQL(
        VSQLText,
        CallbackSelectVersions,
        @VSelectTileVersions,
        True
      );
      // return versions
      Result := VSelectTileVersions.GetMapVersionInfoStatic;
    except
      on E: Exception do
        LogError(c_Log_GetVer, E.Message);
    end;
  finally
    VSelectTileVersions.Uninit;
  end;
end;

function TTileStorageSQLiteHandlerComplex.GetSQL_DeleteTile(
  const ADeleteTileAllData: PDeleteTileAllData
): AnsiString;
var
  VInfo: TSelectTileInfoComplex;
begin
  with ADeleteTileAllData^ do begin
    Result := 'DELETE FROM t WHERE x=' + ALIntToStr(DXY.X) + ' AND y=' + ALIntToStr(DXY.Y);

    if FTBColInfo.ModeV <> vcm_None then begin
      // table has version field
      ParseSQLiteDBVersion(
        FUseVersionFieldInDB,
        FTBColInfo.ModeV,
        DVersionInfo,
        VInfo
      );

      Result := Result + ' AND ' +
        VersionFieldIsEqual(
          VInfo.RequestedVersionIsInt,
          FTBColInfo.ModeV,
          VInfo.RequestedVersionToDB
        );
    end;
  end;
end;

function TTileStorageSQLiteHandlerComplex.GetSQL_TileRectInfo(
  const AUsePrevVersions: Boolean;
  const AEnumData: TTileInfoShortEnumData
): AnsiString;
var
  VSelect: AnsiString;
  VComplex: TSelectTileInfoComplex;
begin
  VSelect := 'SELECT x,y,d,s';
  Result := ' FROM t WHERE x';
  with AEnumData do begin
    // x
    if DestRect.Left = DestRect.Right - 1 then begin
      Result := Result + '=' + ALIntToStr(DestRect.Left);
    end else begin
      Result := Result +
        ' between ' + ALIntToStr(DestRect.Left) +
        ' and ' + ALIntToStr(DestRect.Right - 1);
    end;

    // y
    Result := Result + ' AND y';
    if DestRect.Top = DestRect.Bottom - 1 then begin
      Result := Result + '=' + ALIntToStr(DestRect.Top);
    end else begin
      Result := Result +
        ' between ' + ALIntToStr(DestRect.Top) +
        ' and ' + ALIntToStr(DestRect.Bottom - 1);
    end;
  end;

  // add version
  if FTBColInfo.ModeV <> vcm_None then begin
    // version
    VSelect := VSelect + ',v';

    ParseSQLiteDBVersion(
      FUseVersionFieldInDB,
      FTBColInfo.ModeV,
      AEnumData.RectVersionInfo,
      VComplex
    );

    // use given version
    if not AUsePrevVersions then begin
      // use ONLY given version
      Result := Result +
        ' AND ' +
        VersionFieldIsEqual(
          VComplex.RequestedVersionIsInt,
          FTBColInfo.ModeV,
          VComplex.RequestedVersionToDB
        );
    end else begin
      // use last version
      Result := Result + ' ORDER BY ' + GetOrderByVersion_DESC(FTBColInfo.ModeV);
    end;
  end;

  Result := VSelect + Result;
end;

function TTileStorageSQLiteHandlerComplex.GetTBColInfoPtr: Pointer;
begin
  Result := @FTBColInfo;
end;

function TTileStorageSQLiteHandlerComplex.GetTileInfo(
  const AOper: PNotifierOperationRec;
  const AXY: TPoint;
  const AZoom: Byte;
  const AVersionInfo: IMapVersionInfo;
  const AMode: TGetTileInfoModeSQLite;
  const AUsePrevVersions: Boolean;
  const AResult: PGetTileResult
): Boolean;
var
  VSelectTileInfo: TSelectTileInfoComplex;
  VSQLText: AnsiString;
  VSQLWhere: AnsiString;
  VSQLOrder: AnsiString;
begin
  Result := False;

  // select single tile
  VSQLText := 'SELECT s,d';
  VSQLWhere := 'WHERE x=' + ALIntToStr(AXY.X) + ' AND y=' + ALIntToStr(AXY.Y);
  VSQLOrder := '';

  if FTBColInfo.ModeV <> vcm_None then begin
    // select version
    VSQLText := VSQLText + ',v';

    ParseSQLiteDBVersion(
      FUseVersionFieldInDB,
      FTBColInfo.ModeV,
      AVersionInfo,
      VSelectTileInfo
    );
    VSelectTileInfo.RequestedVersionInfo := AVersionInfo;

    // use given version
    if not AUsePrevVersions then begin
      // use ONLY given version
      VSQLWhere := VSQLWhere +
        ' AND ' +
        VersionFieldIsEqual(
          VSelectTileInfo.RequestedVersionIsInt,
          FTBColInfo.ModeV,
          VSelectTileInfo.RequestedVersionToDB
        );
    end else if VSelectTileInfo.RequestedVersionIsSet then begin
      // use given or last version
      VSQLWhere := VSQLWhere +
        ' AND ' +
        VersionFieldIsEqualOrMax(
          VSelectTileInfo.RequestedVersionIsInt,
          FTBColInfo.ModeV,
          VSelectTileInfo.RequestedVersionToDB,
          AXY
        );
      VSQLOrder := ' ORDER BY ' + GetOrderByVersion_ASC(FTBColInfo.ModeV);
    end else begin
      // no given version - just use last version
      VSQLOrder := ' ORDER BY ' + GetOrderByVersion_DESC(FTBColInfo.ModeV);
    end;
  end;

  if FTBColInfo.HasC then begin
    // select contenttype
    VSQLText := VSQLText + ',c';
  end;

  if gtiiBody in AMode then begin
    // with body
    VSQLText := VSQLText + ',b';
  end;

  VSelectTileInfo.SelectMode := AMode;
  VSelectTileInfo.TileResult := AResult;

  // make full select
  VSQLText := VSQLText + ' FROM t ' + VSQLWhere + VSQLOrder + ' LIMIT 1';

  try
    FSQLite3DbHandler.OpenSQL(
      VSQLText,
      CallbackSelectTileInfo,
      @VSelectTileInfo,
      True
    );
    Result := Assigned(AResult^.GTileInfo);
  except
    on E: Exception do
      LogError(c_Log_Select, E.Message);
  end;
end;

procedure TTileStorageSQLiteHandlerComplex.InternalCheckStructure;
const
  cSelectFieldTypesSQL = 'SELECT * FROM t WHERE 0=1';
begin
  FillChar(FTBColInfo, SizeOf(FTBColInfo), 0);

  // check field count in main table
  FSQLite3DbHandler.DeclareSQL(
    cSelectFieldTypesSQL,
    CallbackSelectCols,
    @FTBColInfo,
    False
  );

  if FTBColInfo.ColCount = 0 then begin
    // no table - create it
    FTileStorageSQLiteHolder.ExecForNewTable(ExecuteSQL);

    // check again
    FSQLite3DbHandler.DeclareSQL(
      cSelectFieldTypesSQL,
      CallbackSelectCols,
      @FTBColInfo,
      False
    );
  end;

  with FTBColInfo do begin
    if (ColCount < 5) or (ColCount > 8) then begin
      // invalid struct
      LogError(c_Log_Init, 'Invalid column count');
    end;
  end;
end;

function TTileStorageSQLiteHandlerComplex.SaveTile(
  const ASaveTileAllData: PSaveTileAllData
): Boolean;
var
  VIsTne: Boolean;
  VInfoComplex: TSelectTileInfoComplex;
  VOriginalTileBody: Pointer;
  VOriginalTileSize: Integer;
  VOriginalTileCRC32: Cardinal;
  VRowsAffected: Integer;
  VKeepExisting: Boolean;
  VSQLText: AnsiString;
  VSQLInsert: AnsiString;
  VSQLValues: AnsiString;
  VSQLAfter: AnsiString;

  procedure _BuildSqlText;
  begin
    if Length(VSQLAfter) > 0 then begin
      // insert .. select
      VSQLText := 'SELECT ' + VSQLValues + VSQLAfter;
    end else begin
      // insert .. values
      VSQLText := 'VALUES (' + VSQLValues + ')';
    end;
    VSQLText :=
      'INSERT OR ' + cReplaceOrIgnore[VKeepExisting] +
      ' INTO t (' + VSQLInsert + ') ' + VSQLText;
  end;

begin
  VSQLAfter := '';

  with ASaveTileAllData^ do begin
    if SData <> nil then begin
      VOriginalTileBody := SData.Buffer;
      VOriginalTileSize := SData.Size;
    end else begin
      VOriginalTileBody := nil;
      VOriginalTileSize := 0;
    end;

    VIsTne := (VOriginalTileSize = 0) or (VOriginalTileBody = nil);

    // x,y - tile coordinates
    // d for datetime - as unix seconds
    VSQLInsert := 'x,y,d';

    VSQLValues :=
      ALIntToStr(SXY.X) + ',' +
      ALIntToStr(SXY.Y) + ',' +
      ALIntToStr(DateTimeToUnix(SLoadDate));

    // crc32
    if FTBColInfo.HasH then begin
      VOriginalTileCRC32 := CRC32Buf(VOriginalTileBody, VOriginalTileSize);
      VSQLInsert := VSQLInsert + ',h';
      VSQLValues := VSQLValues + ',' + ALIntToStr(VOriginalTileCRC32);
    end else begin
      VOriginalTileCRC32 := 0;
    end;

    // version
    if FTBColInfo.ModeV <> vcm_None then begin
      // add version
      VSQLInsert := VSQLInsert + ',v';
      ParseSQLiteDBVersion(
        FUseVersionFieldInDB,
        FTBColInfo.ModeV,
        SVersionInfo,
        VInfoComplex
      );
      VSQLValues := VSQLValues + ',' + VInfoComplex.RequestedVersionToDB;

      // check prev version with same crc32/size
      if not VIsTne and (stfSkipIfSameAsPrev in SSaveTileFlags) then begin
        if FTBColInfo.HasH then begin
          VSQLAfter := ' WHERE ' + GetCheckPrevVersionByCRC32SQLText(SXY, VOriginalTileCRC32);
        end else begin
          VSQLAfter := ' WHERE ' + GetCheckPrevVersionBySizeSQLText(SXY, VOriginalTileSize);
        end;
      end;
    end;

    VKeepExisting := (stfKeepExisting in SSaveTileFlags);
  end;

  Result := False;
  VRowsAffected := 0;

  if not VIsTne then begin
    // content-type
    if FTBColInfo.HasC then begin
      VSQLInsert := VSQLInsert + ',c';
      VSQLValues := VSQLValues + ',' +
        FTileStorageSQLiteHolder.GetContentTypeToDB(ASaveTileAllData^.SContentType);
    end;

    // other fields (s for size and b for body)
    VSQLInsert := VSQLInsert + ',s,b';
    VSQLValues := VSQLValues + ',' + ALIntToStr(VOriginalTileSize) + ',?';

    // execute for TILE
    try
      _BuildSqlText;

      FSQLite3DbModifyLock.Acquire;
      try
        FSQLite3DbHandler.ExecSQLWithBLOB(
          VSqlText,
          VOriginalTileBody,
          VOriginalTileSize,
          @VRowsAffected
        );
      finally
        FSQLite3DbModifyLock.Release;
      end;

      // check if inserted/updated or ignored
      Result := (VRowsAffected > 0);
    except
      on E: Exception do
        LogError(c_Log_Replace, E.Message);
    end;
  end else begin
    // TNE

    // other fields (s for size and b for body)
    VSQLInsert := VSQLInsert + ',s,b';
    VSQLValues := VSQLValues + ',0,NULL';

    // execute for TNE
    try
      _BuildSqlText;

      FSQLite3DbModifyLock.Acquire;
      try
        FSQLite3DbHandler.ExecSQL(
          VSqlText,
          @VRowsAffected
        );
      finally
        FSQLite3DbModifyLock.Release;
      end;

      // check if inserted/updated or ignored
      Result := (VRowsAffected > 0);
    except
      on E: Exception do
        LogError(c_Log_Replace, E.Message);
    end;
  end;
end;

end.
