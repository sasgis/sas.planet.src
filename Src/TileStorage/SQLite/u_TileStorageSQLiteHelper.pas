unit u_TileStorageSQLiteHelper;

interface

uses
  Windows,
  Types,
  SysUtils,
  Classes,
  t_TileStorageSQLite,
  t_NotifierOperationRec,
  i_MapVersionInfo,
  i_MapVersionRequest,
  i_MapVersionListStatic,
  i_TileFileNameGenerator,
  i_TileStorage,
  i_TileStorageSQLiteHelper,
  i_TileStorageSQLiteHandler,
  i_TileStorageSQLiteHolder,
  u_TileStorageSQLiteList,
  u_BaseInterfacedObject;

type
  TTileStorageSQLiteHelper = class(TBaseInterfacedObject, ITileStorageSQLiteHelper)
  private
    FStoragePath: string;
    FDBSingleList: TSQLiteSingleList; // databases
    FTileStorageSQLiteHolder: ITileStorageSQLiteHolder;
    FFileNameGenerator: ITileFileNameGenerator;
    FUseVersion: Boolean;
    FShutdown: Boolean;
    function InternalHandlerFactory(
      const AOper: PNotifierOperationRec;
      const AXY: TPoint;
      const AZoom: Byte;
      const AVersionInfo: IMapVersionInfo;
      const AForceMakeDB: Boolean;
      var ADBNotFound: Boolean
    ): ITileStorageSQLiteHandler;
  private
    { ITileStorageSQLiteHelper }
    procedure Sync;

    function GetTileInfo(
      const AOper: PNotifierOperationRec;
      const AXY: TPoint;
      const AZoom: Byte;
      const AVersionInfo: IMapVersionInfo;
      const AMode: TGetTileInfoMode;
      const AUseOtherVersions: Boolean;
      const AResult: PGetTileResult
    ): Boolean;

    // load tile rect info
    function GetTileRectInfo(
      const AOper: PNotifierOperationRec;
      const AUseOtherVersions: Boolean;
      const AEnumData: TTileInfoShortEnumData
    ): Boolean;

    // delete tile
    function DeleteTile(
      const AOper: PNotifierOperationRec;
      const ADeleteTileAllData: PDeleteTileAllData
    ): Boolean;

    // save tile or tne to storage
    function SaveTile(
      const AOper: PNotifierOperationRec;
      const ASaveTileAllData: PSaveTileAllData
    ): Boolean;

    // get list of versions
    function GetListOfTileVersions(
      const AOper: PNotifierOperationRec;
      const AXY: TPoint;
      const AZoom: byte;
      const AVersion: IMapVersionRequest
    ): IMapVersionListStatic;
  public
    constructor Create(
      const AStoragePath: string;
      const ATileStorageSQLiteHolder: ITileStorageSQLiteHolder;
      const AFileNameGenerator: ITileFileNameGenerator;
      const AUseVersion: Boolean
    );
    destructor Destroy; override;
  end;

implementation

uses
  c_TileStorageSQLite,
  u_TileStorageSQLiteHandler;

function TileInfoModeToSQLiteMode(
  const AMode: TGetTileInfoMode
): TGetTileInfoModeSQLite;
begin
  if AMode = gtimWithData then begin
    Result := [gtiiLoadDate, gtiiSize, gtiiBody, gtiiContentType];
  end else begin
    // gtimWithoutData, gtimAsIs
    Result := [gtiiLoadDate, gtiiSize, gtiiContentType];
  end;
end;

{ TTileStorageSQLiteHelper }

constructor TTileStorageSQLiteHelper.Create(
  const AStoragePath: string;
  const ATileStorageSQLiteHolder: ITileStorageSQLiteHolder;
  const AFileNameGenerator: ITileFileNameGenerator;
  const AUseVersion: Boolean
);
begin
  inherited Create;
  FStoragePath := IncludeTrailingPathDelimiter(AStoragePath);
  FTileStorageSQLiteHolder := ATileStorageSQLiteHolder;
  FFileNameGenerator := AFileNameGenerator;
  FUseVersion := AUseVersion;
  FShutdown := False;
  FDBSingleList.Init(InternalHandlerFactory);
end;

destructor TTileStorageSQLiteHelper.Destroy;
begin
  FShutdown := True;
  FDBSingleList.Uninit;
  FTileStorageSQLiteHolder := nil;
  FFileNameGenerator := nil;
  inherited Destroy;
end;

function TTileStorageSQLiteHelper.InternalHandlerFactory(
  const AOper: PNotifierOperationRec;
  const AXY: TPoint;
  const AZoom: Byte;
  const AVersionInfo: IMapVersionInfo;
  const AForceMakeDB: Boolean;
  var ADBNotFound: Boolean
): ITileStorageSQLiteHandler;
var
  VDBPath: String;
begin
  // get path
  VDBPath :=
    FStoragePath +
    FFileNameGenerator.GetTileFileName(AXY, AZoom) +
    cSQLiteDBFileExt;

  ADBNotFound := not FileExists(VDBPath);
  if ADBNotFound then begin
    // no database
    if not AForceMakeDB then begin
      Result := nil;
      Exit;
    end;
    // forced
    ForceDirectories(ExtractFilePath(VDBPath));
  end;

  if AOper^.IsOperationCancelled then begin
    Result := nil;
    Exit;
  end;

  // make new database or open existing
  Result :=
    TTileStorageSQLiteHandlerComplex.Create(
      FTileStorageSQLiteHolder,
      VDBPath,
      AVersionInfo,
      FUseVersion
    );

  // check if opened
  if not Result.Opened then begin
    Result := nil;
  end;
end;

function TTileStorageSQLiteHelper.DeleteTile(
  const AOper: PNotifierOperationRec;
  const ADeleteTileAllData: PDeleteTileAllData
): Boolean;
var
  VHandler: ITileStorageSQLiteHandler;
begin
  if FShutdown then begin
    Result := False;
    Exit;
  end;

  // get database by xyz
  with ADeleteTileAllData^ do begin
    VHandler :=
      FDBSingleList.GetHandler(
        AOper,
        DZoom,
        DXY,
        DVersionInfo,
        False
      );
  end;

  if AOper^.IsOperationCancelled then begin
    Result := False;
    Exit;
  end;

  // check if database exists
  if VHandler = nil then begin
    // no database - no tile
    Result := True;
  end else begin
    // delete tile
    Result := VHandler.DeleteTile(ADeleteTileAllData);
  end;
end;

function TTileStorageSQLiteHelper.GetListOfTileVersions(
  const AOper: PNotifierOperationRec;
  const AXY: TPoint;
  const AZoom: byte;
  const AVersion: IMapVersionRequest
): IMapVersionListStatic;
var
  VHandler: ITileStorageSQLiteHandler;
  VVersionInfo: IMapVersionInfo;
begin
  if FShutdown or not FUseVersion then begin
    Result := nil;
    Exit;
  end;

  if Assigned(AVersion) then begin
    VVersionInfo := AVersion.BaseVersion;
  end else begin
    VVersionInfo := nil;
  end;

  // get database by xyz
  VHandler :=
    FDBSingleList.GetHandler(
      AOper,
      AZoom,
      AXY,
      VVersionInfo,
      False
    );

  // check if database exists
  if (VHandler = nil) or AOper^.IsOperationCancelled then begin
    // no database - no versions
    Result := nil;
  end else begin
    // delete tile
    Result :=
      VHandler.GetListOfTileVersions(
        AOper,
        AXY,
        AZoom,
        VVersionInfo
      );
  end;
end;

function TTileStorageSQLiteHelper.GetTileInfo(
  const AOper: PNotifierOperationRec;
  const AXY: TPoint;
  const AZoom: Byte;
  const AVersionInfo: IMapVersionInfo;
  const AMode: TGetTileInfoMode;
  const AUseOtherVersions: Boolean;
  const AResult: PGetTileResult
): Boolean;
var
  VHandler: ITileStorageSQLiteHandler;
begin
  if FShutdown then begin
    Result := False;
    Exit;
  end;

  // get database by xyz
  VHandler :=
    FDBSingleList.GetHandler(
      AOper,
      AZoom,
      AXY,
      AVersionInfo,
      False
    );

  // check if database exists
  if VHandler = nil then begin
    // no database - no tile
    Result := False;
  end else begin
    // obtain tile or tne or info only
    Result :=
      VHandler.GetTileInfo(
        AOper,
        AXY,
        AZoom,
        AVersionInfo,
        TileInfoModeToSQLiteMode(AMode),
        AUseOtherVersions,
        AResult
      );
  end;
end;

function TTileStorageSQLiteHelper.GetTileRectInfo(
  const AOper: PNotifierOperationRec;
  const AUseOtherVersions: Boolean;
  const AEnumData: TTileInfoShortEnumData
): Boolean;
var
  I, J: Integer;
  VXY: TPoint;
  VDBRect: TRect;
  VResult: Boolean;
  VHandler: ITileStorageSQLiteHandler;
begin
  Result := False;

  if FShutdown then begin
    Exit;
  end;

  // check zoom
  if AEnumData.DestZoom <= c_Max_Single_Zoom then begin
    // single database request without shiftings
    VHandler :=
      FDBSingleList.GetHandler(
        AOper,
        AEnumData.DestZoom,
        FDBSingleList.Zero,
        AEnumData.RectVersionInfo,
        False
      );
    if VHandler <> nil then begin
      // obtain information from single database
      Result :=
        VHandler.GetTileRectInfo(
          AOper,
          AUseOtherVersions,
          AEnumData
        );
    end;
  end else begin
    // covered databases
    VDBRect.Top := AEnumData.DestRect.Top shr 8;
    VDBRect.Left := AEnumData.DestRect.Left shr 8;
    VDBRect.Bottom := (AEnumData.DestRect.Bottom - 1) shr 8;
    VDBRect.Right := (AEnumData.DestRect.Right - 1) shr 8;
    // iterate by shifted values
    for I := VDBRect.Left to VDBRect.Right do begin
      if FShutdown or AOper^.IsOperationCancelled then begin
        Result := False;
        Exit;
      end;
      for J := VDBRect.Top to VDBRect.Bottom do begin
        // check
        if FShutdown or AOper^.IsOperationCancelled then begin
          Result := False;
          Exit;
        end;
        // restore value
        VXY.X := I shl 8;
        VXY.Y := J shl 8;
        // get handler
        VHandler :=
          FDBSingleList.GetHandler(
            AOper,
            AEnumData.DestZoom,
            VXY,
            AEnumData.RectVersionInfo,
            False
          );
        if VHandler <> nil then begin
          // obtain information from covered database
          VResult :=
            VHandler.GetTileRectInfo(
              AOper,
              AUseOtherVersions,
              AEnumData
            );
          Result := Result or VResult;
        end;
      end;
    end;
  end;
end;

function TTileStorageSQLiteHelper.SaveTile(
  const AOper: PNotifierOperationRec;
  const ASaveTileAllData: PSaveTileAllData
): Boolean;
var
  VHandler: ITileStorageSQLiteHandler;
begin
  if FShutdown then begin
    Result := False;
    Exit;
  end;

  // get database by xyz
  with ASaveTileAllData^ do begin
    VHandler :=
      FDBSingleList.GetHandler(
        AOper,
        SZoom,
        SXY,
        SVersionInfo,
        True
      );
  end;

  if AOper^.IsOperationCancelled then begin
    Result := False;
    Exit;
  end;

  // check if database exists
  if VHandler = nil then begin
    // no database - error
    Result := False;
  end else begin
    // save tile
    Result := VHandler.SaveTile(ASaveTileAllData);
  end;
end;

procedure TTileStorageSQLiteHelper.Sync;
begin
  FDBSingleList.Sync;
end;

end.
