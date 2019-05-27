unit u_TileStorageSQLite;

interface

uses
  Types,
  Windows,
  SysUtils,
  i_BinaryData,
  i_ContentTypeInfo,
  i_TileInfoBasic,
  i_ContentTypeManager,
  i_ProjectionSet,
  i_NotifierOperation,
  i_TileStorageAbilities,
  i_TileStorage,
  i_MapVersionRequestConfig,
  i_MapVersionRequest,
  i_MapVersionListStatic,
  i_MapVersionFactory,
  i_MapVersionInfo,
  i_NotifierTime,
  i_NotifierTilePyramidUpdate,
  i_ListenerTime,
  i_TileFileNameGenerator,
  i_TileInfoBasicMemCache,
  i_TileStorageSQLiteHelper,
  i_TileStorageSQLiteHolder,
  u_TileStorageAbstract;

type
  TTileStorageSQLite = class(TTileStorageAbstract)
  private
    FIsVersioned: Boolean;
    FGCNotifier: INotifierTime;
    FSyncCallListner: IListenerTimeWithUsedFlag;
    FTileInfoMemCache: ITileInfoBasicMemCache;
    FTileNotExistsTileInfo: ITileInfoBasic;
    FTileFileNameGenerator: ITileFileNameGenerator;
    FMainContentType: IContentTypeInfoBasic;
    FContentTypeManager: IContentTypeManager;
    FMapVersionRequestConfig: IMapVersionRequestConfig;
    FTileStorageSQLiteHelper: ITileStorageSQLiteHelper;
    FTileStorageSQLiteHolder: ITileStorageSQLiteHolder;
  private
    procedure OnSyncCall;
    function GetTileInfoInternal(
      const AXY: TPoint;
      const AZoom: Byte;
      const AVersionInfo: IMapVersionInfo;
      const AMode: TGetTileInfoMode;
      const AShowOtherVersions: Boolean
    ): ITileInfoBasic;
  protected
    function GetTileFileName(
      const AXY: TPoint;
      const AZoom: Byte;
      const AVersionInfo: IMapVersionInfo
    ): string; override;

    function GetTileInfo(
      const AXY: TPoint;
      const AZoom: Byte;
      const AVersionInfo: IMapVersionInfo;
      const AMode: TGetTileInfoMode
    ): ITileInfoBasic; override;

    function GetTileInfoEx(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersionInfo: IMapVersionRequest;
      const AMode: TGetTileInfoMode
    ): ITileInfoBasic; override;

    function GetTileRectInfo(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ARect: TRect;
      const AZoom: Byte;
      const AVersionInfo: IMapVersionRequest
    ): ITileRectInfo; override;

    function DeleteTile(
      const AXY: TPoint;
      const AZoom: Byte;
      const AVersionInfo: IMapVersionInfo
    ): Boolean; override;

    function SaveTile(
      const AXY: TPoint;
      const AZoom: Byte;
      const AVersionInfo: IMapVersionInfo;
      const ALoadDate: TDateTime;
      const AContentType: IContentTypeInfoBasic;
      const AData: IBinaryData;
      const AIsOverwrite: Boolean
    ): Boolean; override;

    function GetListOfTileVersions(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersionInfo: IMapVersionRequest
    ): IMapVersionListStatic; override;

    function ScanTiles(
      const AIgnoreTNE: Boolean;
      const AIgnoreMultiVersionTiles: Boolean
    ): IEnumTileInfo; override;
  public
    constructor Create(
      const ATileInfoMemCache: ITileInfoBasicMemCache;
      const AMainContentType: IContentTypeInfoBasic;
      const AStorageTypeAbilities: ITileStorageTypeAbilities;
      const AStorageForceAbilities: ITileStorageAbilities;
      const AMapVersionFactory: IMapVersionFactory;
      const AProjectionSet: IProjectionSet;
      const ATileNotifier: INotifierTilePyramidUpdateInternal;
      const AStoragePath: string;
      const AGCNotifier: INotifierTime;
      const AContentTypeManager: IContentTypeManager;
      const AIsVersioned: Boolean
    );
    destructor Destroy; override;
  end;

implementation

uses
  Classes,
  c_TileStorageSQLite,
  t_TileStorageSQLite,
  t_NotifierOperationRec,
  i_FileNameIterator,
  i_TileFileNameParser,
  u_ListenerTime,
  u_TileRectInfoShort,
  u_FileNameIteratorFolderWithSubfolders,
  u_FoldersIteratorRecursiveByLevels,
  u_FileNameIteratorInFolderByMaskList,
  u_TileInfoBasic,
  u_TileFileNameSQLite,
  u_TileStorageSQLiteHelper,
  u_TileStorageSQLiteHolder,
  u_EnumTileInfoBySQLite;

{ TTileStorageSQLite }

constructor TTileStorageSQLite.Create(
  const ATileInfoMemCache: ITileInfoBasicMemCache;
  const AMainContentType: IContentTypeInfoBasic;
  const AStorageTypeAbilities: ITileStorageTypeAbilities;
  const AStorageForceAbilities: ITileStorageAbilities;
  const AMapVersionFactory: IMapVersionFactory;
  const AProjectionSet: IProjectionSet;
  const ATileNotifier: INotifierTilePyramidUpdateInternal;
  const AStoragePath: string;
  const AGCNotifier: INotifierTime;
  const AContentTypeManager: IContentTypeManager;
  const AIsVersioned: Boolean
);
begin
  inherited Create(
    AStorageTypeAbilities,
    AStorageForceAbilities,
    AMapVersionFactory,
    AProjectionSet,
    ATileNotifier,
    AStoragePath
  );

  FMainContentType := AMainContentType;
  FTileInfoMemCache := ATileInfoMemCache;
  FContentTypeManager := AContentTypeManager;
  FGCNotifier := AGCNotifier;

  FMapVersionRequestConfig := nil;
  FTileNotExistsTileInfo := TTileInfoBasicNotExists.Create(0, nil);
  FTileFileNameGenerator := TTileFileNameSQLite.Create as ITileFileNameGenerator;

  FIsVersioned := AIsVersioned;

  FTileStorageSQLiteHolder :=
    TTileStorageSQLiteHolder.Create(
      Self.StoragePath,
      FContentTypeManager,
      FMainContentType,
      Self.MapVersionFactory,
      True
    );

  FTileStorageSQLiteHelper :=
    TTileStorageSQLiteHelper.Create(
      Self.StoragePath,
      FTileStorageSQLiteHolder,
      FTileFileNameGenerator,
      FIsVersioned
    );

  FSyncCallListner :=
    TListenerTTLCheck.Create(
      Self.OnSyncCall,
      cStorageSyncInterval
    );

  if FGCNotifier <> nil then begin
    FGCNotifier.Add(FSyncCallListner);
  end;
end;

destructor TTileStorageSQLite.Destroy;
begin
  if Assigned(FGCNotifier) and Assigned(FSyncCallListner) then begin
    FGCNotifier.Remove(FSyncCallListner);
    FGCNotifier := nil;
  end;
  FSyncCallListner := nil;
  FTileStorageSQLiteHolder := nil;
  FTileStorageSQLiteHelper := nil;
  inherited Destroy;
end;

procedure TTileStorageSQLite.OnSyncCall;
begin
  FTileStorageSQLiteHelper.Sync;
end;

function TTileStorageSQLite.GetTileFileName(
  const AXY: TPoint;
  const AZoom: Byte;
  const AVersionInfo: IMapVersionInfo
): string;
begin
  Result :=
    StoragePath +
    FTileFileNameGenerator.GetTileFileName(AXY, AZoom) +
    cSQLiteDBFileExt;
end;

function TTileStorageSQLite.GetListOfTileVersions(
  const AXY: TPoint;
  const AZoom: byte;
  const AVersionInfo: IMapVersionRequest
): IMapVersionListStatic;
begin
  Result := nil;
  if StorageStateInternal.ReadAccess then begin
    Result :=
      FTileStorageSQLiteHelper.GetListOfTileVersions(
        nil,
        AXY,
        AZoom,
        AVersionInfo
      );
  end;
end;

function TTileStorageSQLite.GetTileInfoInternal(
  const AXY: TPoint;
  const AZoom: Byte;
  const AVersionInfo: IMapVersionInfo;
  const AMode: TGetTileInfoMode;
  const AShowOtherVersions: Boolean
): ITileInfoBasic;
var
  VTileResult: TGetTileResult;
  VObtained: Boolean;
begin
  if Assigned(FTileInfoMemCache) then begin
    Result := FTileInfoMemCache.Get(AXY, AZoom, AVersionInfo, AMode, True);
    if Result <> nil then begin
      Exit;
    end;
  end;

  if StorageStateInternal.ReadAccess then begin
    FillChar(VTileResult, SizeOf(VTileResult), 0);
    VObtained :=
      FTileStorageSQLiteHelper.GetTileInfo(
        nil,
        AXY,
        AZoom,
        AVersionInfo,
        AMode,
        AShowOtherVersions,
        @VTileResult
      );
    if VObtained then begin
      Result := VTileResult.GTileInfo;
    end else begin
      if AVersionInfo = nil then begin
        Result := FTileNotExistsTileInfo;
      end else begin
        Result := TTileInfoBasicNotExists.Create(0, AVersionInfo);
      end;
    end;
  end else begin
    Result := FTileNotExistsTileInfo;
  end;
  if Assigned(FTileInfoMemCache) then begin
    FTileInfoMemCache.Add(AXY, AZoom, AVersionInfo, Result);
  end;
end;

function TTileStorageSQLite.GetTileInfo(
  const AXY: TPoint;
  const AZoom: Byte;
  const AVersionInfo: IMapVersionInfo;
  const AMode: TGetTileInfoMode
): ITileInfoBasic;
var
  VShowOtherVersions: Boolean;
begin
  VShowOtherVersions := not (Assigned(AVersionInfo) and (AVersionInfo.StoreString <> ''));
  Result := GetTileInfoInternal(AXY, AZoom, AVersionInfo, AMode, VShowOtherVersions);
end;

function TTileStorageSQLite.GetTileInfoEx(
  const AXY: TPoint;
  const AZoom: Byte;
  const AVersionInfo: IMapVersionRequest;
  const AMode: TGetTileInfoMode
): ITileInfoBasic;
var
  VVersionInfo: IMapVersionInfo;
  VShowOtherVersions: Boolean;
begin
  if Assigned(AVersionInfo) then begin
    VVersionInfo := AVersionInfo.BaseVersion;
    VShowOtherVersions := AVersionInfo.ShowOtherVersions;
  end else begin
    VVersionInfo := nil;
    VShowOtherVersions := True;
  end;
  Result := GetTileInfoInternal(AXY, AZoom, VVersionInfo, AMode, VShowOtherVersions);
end;

function TTileStorageSQLite.GetTileRectInfo(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ARect: TRect;
  const AZoom: Byte;
  const AVersionInfo: IMapVersionRequest
): ITileRectInfo;
var
  VOper: TNotifierOperationRec;
  VTileInfoShortEnumData: TTileInfoShortEnumData;
  VVersionInfo: IMapVersionInfo;
  VShowOtherVersions: Boolean;
begin
  Result := nil;

  if Assigned(AVersionInfo) then begin
    VVersionInfo := AVersionInfo.BaseVersion;
    VShowOtherVersions := AVersionInfo.ShowOtherVersions;
  end else begin
    VVersionInfo := nil;
    VShowOtherVersions := True;
  end;

  if StorageStateInternal.ReadAccess then begin
    with VTileInfoShortEnumData do begin
      try
        // check
        DestRect := ARect;
        DestZoom := AZoom;

        Self.ProjectionSet.Zooms[DestZoom].ValidateTileRect(DestRect);

        RectCount.X := DestRect.Right - DestRect.Left;
        RectCount.Y := DestRect.Bottom - DestRect.Top;

        if (RectCount.X > 0) and (RectCount.Y > 0) and
          (RectCount.X <= 2048) and (RectCount.Y <= 2048) then begin

          SetLength(RectItems, RectCount.X * RectCount.Y);

          RectVersionInfo := VVersionInfo;

          VOper.OperationID := AOperationID;
          VOper.CancelNotifier := ACancelNotifier;

          FTileStorageSQLiteHelper.GetTileRectInfo(
            @VOper,
            VShowOtherVersions,
            VTileInfoShortEnumData
          );

          if VOper.IsOperationCancelled then begin
            // cancelled
            Result := nil
          end else begin
            Result :=
              TTileRectInfoShort.CreateWithOwn(
                DestRect,
                DestZoom,
                VVersionInfo,
                FMainContentType,
                RectItems
              );
          end;
          RectItems := nil;
        end;
      finally
        RectVersionInfo := nil;
      end;
    end;
  end;
end;

function TTileStorageSQLite.SaveTile(
  const AXY: TPoint;
  const AZoom: Byte;
  const AVersionInfo: IMapVersionInfo;
  const ALoadDate: TDateTime;
  const AContentType: IContentTypeInfoBasic;
  const AData: IBinaryData;
  const AIsOverwrite: Boolean
): Boolean;
var
  VSaveData: TSaveTileAllData;
  VTileInfo: ITileInfoBasic;
begin
  Result := False;
  if StorageStateInternal.AddAccess then begin
    // prepare data
    with VSaveData do begin
      SXY := AXY;
      SZoom := AZoom;
      SVersionInfo := AVersionInfo;
      SLoadDate := ALoadDate;
      SContentType := AContentType;
      SData := AData;
      SSaveTileFlags := [stfSkipIfSameAsPrev];
      if not AIsOverwrite then begin
        Include(SSaveTileFlags, stfKeepExisting);
      end;
    end;

    // save to storage
    Result := FTileStorageSQLiteHelper.SaveTile(nil, @VSaveData);

    // save to memory
    if Result then begin
      if Assigned(FTileInfoMemCache) then begin
        // make for cache
        if AData = nil then begin
          VTileInfo := TTileInfoBasicTNE.Create(ALoadDate, AVersionInfo);
        end else begin
          VTileInfo :=
            TTileInfoBasicExistsWithTile.Create(
              ALoadDate,
              AData,
              AVersionInfo,
              FMainContentType
            );
        end;
        // save to mem
        FTileInfoMemCache.Add(
          AXY,
          AZoom,
          AVersionInfo,
          VTileInfo
        );
      end;
      FSyncCallListner.CheckUseTimeUpdated;
      NotifyTileUpdate(AXY, AZoom, AVersionInfo);
    end;
  end;
end;

function TTileStorageSQLite.DeleteTile(
  const AXY: TPoint;
  const AZoom: byte;
  const AVersionInfo: IMapVersionInfo
): Boolean;
var
  VDelData: TDeleteTileAllData;
begin
  Result := False;
  if StorageStateInternal.DeleteAccess then begin
    try
      // prepare data
      with VDelData do begin
        DXY := AXY;
        DZoom := AZoom;
        DVersionInfo := AVersionInfo;
      end;

      // delete tile or tne
      Result := FTileStorageSQLiteHelper.DeleteTile(nil, @VDelData);
    except
      Result := False;
    end;

    if Result then begin
      if Assigned(FTileInfoMemCache) then begin
        FTileInfoMemCache.Remove(AXY, AZoom, AVersionInfo);
      end;
      FSyncCallListner.CheckUseTimeUpdated;
      NotifyTileUpdate(AXY, AZoom, AVersionInfo);
    end;
  end;
end;

function TTileStorageSQLite.ScanTiles(
  const AIgnoreTNE: Boolean;
  const AIgnoreMultiVersionTiles: Boolean
): IEnumTileInfo;
const
  cMaxFolderDepth = 10;
var
  VProcessFileMasks: TStringList;
  VFilesIterator: IFileNameIterator;
  VFoldersIteratorFactory: IFileNameIteratorFactory;
  VFilesInFolderIteratorFactory: IFileNameIteratorFactory;
  VBaseIteratorPath: String;
  VFileNameParser: ITileFileNameParser;
begin
  if not StorageStateInternal.ScanAccess then begin
    Result := nil;
    Exit;
  end;
  VProcessFileMasks := TStringList.Create;
  try
    VProcessFileMasks.Add('*' + cSQLiteDBFileExt);

    VFoldersIteratorFactory :=
      TFoldersIteratorRecursiveByLevelsFactory.Create(cMaxFolderDepth);

    VFilesInFolderIteratorFactory :=
      TFileNameIteratorInFolderByMaskListFactory.Create(VProcessFileMasks, True);

    VBaseIteratorPath := StoragePath;

    VFilesIterator :=
      TFileNameIteratorFolderWithSubfolders.Create(
        StoragePath,
        '',
        VFoldersIteratorFactory,
        VFilesInFolderIteratorFactory
      );

    VFileNameParser := TTileFileNameSQLite.Create as ITileFileNameParser;

    Result :=
      TEnumTileInfoBySQLite.Create(
        StoragePath,
        VFilesIterator,
        VFileNameParser,
        FTileStorageSQLiteHolder,
        FIsVersioned,
        AIgnoreTNE,
        AIgnoreMultiVersionTiles
      );
  finally
    VProcessFileMasks.Free;
  end;
end;

end.
