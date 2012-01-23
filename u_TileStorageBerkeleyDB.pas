{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
{* This program is free software: you can redistribute it and/or modify       *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* This program is distributed in the hope that it will be useful,            *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *}
{*                                                                            *}
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_TileStorageBerkeleyDB;

interface

uses
  Types,
  Classes,
  SysUtils,
  i_SimpleTileStorageConfig,
  i_MapVersionInfo,
  i_ContentTypeInfo,
  i_TileInfoBasic,
  i_ContentTypeManager,
  i_TTLCheckNotifier,
  i_TTLCheckListener,
  u_BerkeleyDB,
  u_BerkeleyDBEnv,
  u_BerkeleyDBPool,
  u_TileStorageBerkeleyDBHelper,
  u_GlobalCahceConfig,
  u_MapTypeCacheConfig,
  u_TileStorageAbstract;

type
  TTileStorageBerkeleyDB = class(TTileStorageAbstract)
  private
    FBDBPool: TBerkeleyDBPool;
    FBDBHelper: TTileStorageBerkeleyDBHelper;
    FCacheConfig: TMapTypeCacheConfigBerkeleyDB;
    FMainContentType: IContentTypeInfoBasic;
    FContentTypeManager: IContentTypeManager;
    FTileNotExistsTileInfo: ITileInfoBasic;
    FGCList: ITTLCheckNotifier;
    FTTLListener: ITTLCheckListener;
    procedure CreateDirIfNotExists(APath: string);
    function GetTileInfoByBDBData(ABDBData: PBDBData): ITileInfoBasic;
    procedure Sync(Sender: TObject);
  public
    constructor Create(
      AGCList: ITTLCheckNotifier;
      AConfig: ISimpleTileStorageConfig;
      AGlobalCacheConfig: TGlobalCahceConfig;
      AContentTypeManager: IContentTypeManager
    );
    destructor Destroy; override;

    function GetMainContentType: IContentTypeInfoBasic; override;
    function GetAllowDifferentContentTypes: Boolean; override;

    function GetCacheConfig: TMapTypeCacheConfigAbstract; override;

    function GetTileFileName(
      AXY: TPoint;
      Azoom: byte;
      AVersionInfo: IMapVersionInfo
    ): string; override;

    function GetTileInfo(
      AXY: TPoint;
      Azoom: byte;
      AVersionInfo: IMapVersionInfo
    ): ITileInfoBasic; override;

    function LoadTile(
      AXY: TPoint;
      Azoom: byte;
      AVersionInfo: IMapVersionInfo;
      AStream: TStream;
      out ATileInfo: ITileInfoBasic
    ): Boolean; override;

    function DeleteTile(
      AXY: TPoint;
      Azoom: byte;
      AVersionInfo: IMapVersionInfo
    ): Boolean; override;

    function DeleteTNE(
      AXY: TPoint;
      Azoom: byte;
      AVersionInfo: IMapVersionInfo
    ): Boolean; override;

    procedure SaveTile(
      AXY: TPoint;
      Azoom: byte;
      AVersionInfo: IMapVersionInfo;
      AStream: TStream
    ); override;

    procedure SaveTNE(
      AXY: TPoint;
      Azoom: byte;
      AVersionInfo: IMapVersionInfo
    ); override;

  end;

implementation

uses
  Variants,
  t_CommonTypes,
  u_ContentTypeInfo,
  u_MapVersionInfo,
  u_TTLCheckListener,
  u_TileFileNameBDB,
  u_TileStorageTypeAbilities,
  u_TileInfoBasic;

{ TTileStorageBerkeleyDB }

constructor TTileStorageBerkeleyDB.Create(
  AGCList: ITTLCheckNotifier;
  AConfig: ISimpleTileStorageConfig;
  AGlobalCacheConfig: TGlobalCahceConfig;
  AContentTypeManager: IContentTypeManager
);
const
  CBDBSync = 300000; // 5 min
  CBDBSyncCheckInterval = 60000; // 60 sec
begin
  inherited Create(TTileStorageTypeAbilitiesBerkeleyDB.Create, AConfig);
  FGCList := AGCList;
  FTileNotExistsTileInfo := TTileInfoBasicNotExists.Create(0, nil);
  FCacheConfig := TMapTypeCacheConfigBerkeleyDB.Create(
    AConfig,
    TTileFileNameBDB.Create,
    AGlobalCacheConfig
  );
  FContentTypeManager := AContentTypeManager;
  FMainContentType := FContentTypeManager.GetInfoByExt(Config.TileFileExt);
  FBDBHelper := TTileStorageBerkeleyDBHelper.Create(
    IncludeTrailingPathDelimiter(
      AGlobalCacheConfig.CacheGlobalPath +
      AGlobalCacheConfig.BDBCachepath +
      AConfig.NameInCache
    ),
    AConfig.CoordConverter.Datum.EPSG
  );
  FBDBPool := FBDBHelper.Pool;
  FTTLListener := TTTLCheckListener.Create(
    Self.Sync,
    CBDBSync,
    CBDBSyncCheckInterval
  );
  FGCList.Add(FTTLListener);
end;

destructor TTileStorageBerkeleyDB.Destroy;
begin
  FGCList.Remove(FTTLListener);
  FTTLListener := nil;
  FBDBPool := nil;
  FreeAndNil(FBDBHelper);
  FMainContentType := nil;
  FContentTypeManager := nil;
  FreeAndNil(FCacheConfig);
  FTileNotExistsTileInfo := nil;
  FGCList := nil;
  inherited;
end;

procedure TTileStorageBerkeleyDB.Sync(Sender: TObject);
begin
  if Assigned(FBDBPool) then begin
    FBDBPool.Sync;
  end;
end;

procedure TTileStorageBerkeleyDB.CreateDirIfNotExists(APath: string);
var
  i: integer;
begin
  i := LastDelimiter(PathDelim, Apath);
  Apath := copy(Apath, 1, i);
  if not(DirectoryExists(Apath)) then begin
    ForceDirectories(Apath);
  end;
end;

function TTileStorageBerkeleyDB.DeleteTile(
  AXY: TPoint;
  AZoom: Byte;
  AVersionInfo: IMapVersionInfo
): Boolean;
var
  VPath: string;
  VBDB: TBerkeleyDB;
  VKey: TBDBKey;
begin
  Result := False;
  if StorageStateStatic.DeleteAccess <> asDisabled then begin
    try
      VPath := FCacheConfig.GetTileFileName(AXY, AZoom);
      if FileExists(VPath) then begin
        VBDB := FBDBPool.Acquire(VPath);
        try
          if Assigned(VBDB) then begin
            VKey := PointToKey(AXY);
            Result := VBDB.Del(@VKey, SizeOf(TBDBKey));
          end;
        finally
          FBDBPool.Release(VBDB);
        end;
      end;
      if not Result then begin
        Result := DeleteTNE(AXY, Azoom, AVersionInfo);
      end;
    except
      Result := False;
    end;
    if Result then begin
      NotifierByZoomInternal[Azoom].TileUpdateNotify(AXY);
    end;
  end;
end;

function TTileStorageBerkeleyDB.DeleteTNE(
  AXY: TPoint;
  Azoom: byte;
  AVersionInfo: IMapVersionInfo
): Boolean;
var
  VPath: string;
  VBDB: TBerkeleyDB;
  VKey: TBDBKey;
begin
  Result := False;
  if StorageStateStatic.DeleteAccess <> asDisabled then begin
    try
      VPath := FCacheConfig.GetTileFileName(AXY, AZoom);
      VPath := ChangeFileExt(VPath, '.tne');
      if FileExists(VPath) then begin
        VBDB := FBDBPool.Acquire(VPath);
        try
          if Assigned(VBDB) then begin
            VKey := PointToKey(AXY);
            Result := VBDB.Del(@VKey, SizeOf(TBDBKey));
          end;
        finally
          FBDBPool.Release(VBDB);
        end;
      end;
    except
      Result := False;
    end;
  end;
end;

function TTileStorageBerkeleyDB.GetAllowDifferentContentTypes: Boolean;
begin
  Result := True;
end;

function TTileStorageBerkeleyDB.GetCacheConfig: TMapTypeCacheConfigAbstract;
begin
  Result := FCacheConfig;
end;

function TTileStorageBerkeleyDB.GetMainContentType: IContentTypeInfoBasic;
begin
  Result := FMainContentType;
end;

function TTileStorageBerkeleyDB.GetTileFileName(
  AXY: TPoint;
  Azoom: byte;
  AVersionInfo: IMapVersionInfo
): string;
begin
  Result := FCacheConfig.GetTileFileName(AXY, Azoom) + PathDelim +
    'x' + IntToStr(AXY.X) + PathDelim + 'y' + IntToStr(AXY.Y) +
    FMainContentType.GetDefaultExt;
end;

function TTileStorageBerkeleyDB.GetTileInfo(
  AXY: TPoint;
  Azoom: byte;
  AVersionInfo: IMapVersionInfo
): ITileInfoBasic;
var
  VPath: string;
  VBDB: TBerkeleyDB;
  VKey: TBDBKey;
  VData: TBDBData;
  VRawData: Pointer;
  VRawDataSize: Cardinal;
begin
  Result := FTileNotExistsTileInfo;
  if StorageStateStatic.ReadAccess <> asDisabled then begin
    VPath := FCacheConfig.GetTileFileName(AXY, Azoom);
    if not FileExists(VPath) then begin
      Result := TTileInfoBasicNotExists.Create(0, AVersionInfo);
    end else begin
      VBDB := FBDBPool.Acquire(VPath);
      try
        if Assigned(VBDB) then begin
          VKey := PointToKey(AXY);
          if VBDB.Exists(@VKey, SizeOf(TBDBKey)) then begin
            VRawData := nil;
            VRawDataSize := 0;
            if VBDB.Read(@VKey, SizeOf(TBDBKey), VRawData, VRawDataSize) then begin
              if (VRawData <> nil) and (VRawDataSize > 0) then
              try
                if PRawDataToPBDBData(VRawData, VRawDataSize, @VData) then begin
                  Result := GetTileInfoByBDBData(@VData);
                end;
              finally
                FreeMem(VRawData, VRawDataSize);
              end;
            end;
          end else begin
            Result := TTileInfoBasicNotExists.Create(0, AVersionInfo);
          end;
        end;
      finally
        FBDBPool.Release(VBDB);
      end;
    end;
  end;
end;

function TTileStorageBerkeleyDB.GetTileInfoByBDBData(ABDBData: PBDBData): ITileInfoBasic;
begin
  if ABDBData <> nil then begin
    Result := TTileInfoBasicExists.Create(
      ABDBData.TileDate,
      ABDBData.TileSize,
      TMapVersionInfo.Create(WideString(ABDBData.TileVer)),
      FContentTypeManager.GetInfo(WideString(ABDBData.TileMIME))
    );
  end else begin
    Result := TTileInfoBasicNotExists.Create(0, nil);
  end;
end;

function TTileStorageBerkeleyDB.LoadTile(
  AXY: TPoint;
  AZoom: Byte;
  AVersionInfo: IMapVersionInfo;
  AStream: TStream;
  out ATileInfo: ITileInfoBasic
): Boolean;
var
  VPath: String;
  VBDB: TBerkeleyDB;
  VKey: TBDBKey;
  VData: TBDBData;
  VRawData: Pointer;
  VRawDataSize: Cardinal;
begin
  Result := False;
  ATileInfo := nil;
  AStream.Size := 0;
  if StorageStateStatic.ReadAccess <> asDisabled then begin
    VPath := FCacheConfig.GetTileFileName(AXY, AZoom);
    if FileExists(VPath) then begin
      VBDB := FBDBPool.Acquire(VPath);
      try
        if Assigned(VBDB) then begin
          VKey := PointToKey(AXY);
          if VBDB.Read(@VKey, SizeOf(TBDBKey), VRawData, VRawDataSize) then begin
            if (VRawData <> nil) and (VRawDataSize > 0) then
            try
              if PRawDataToPBDBData(VRawData, VRawDataSize, @VData) then begin
                ATileInfo := GetTileInfoByBDBData(@VData);
                if ATileInfo.GetIsExists then begin
                  AStream.Position := 0;
                  Result := AStream.Write(VData.TileBody^, VData.TileSize) = Integer(VData.TileSize);
                  AStream.Position := 0;
                end;
              end;
            finally
              FreeMem(VRawData);
            end;
          end;
        end;
      finally
        FBDBPool.Release(VBDB);
      end;
    end;
  end;
end;

procedure TTileStorageBerkeleyDB.SaveTile(
  AXY: TPoint;
  Azoom: byte;
  AVersionInfo: IMapVersionInfo;
  AStream: TStream
);
var
  VPath: String;
  VMemStream: TMemoryStream;
  VBDB: TBerkeleyDB;
  VKey: TBDBKey;
  VData: TBDBData;
begin
  if StorageStateStatic.WriteAccess <> asDisabled then begin
    VPath := FCacheConfig.GetTileFileName(AXY, Azoom);
    CreateDirIfNotExists(VPath);
    VBDB := FBDBPool.Acquire(VPath);
    try
      if Assigned(VBDB) then begin
        VKey := PointToKey(AXY);
        VMemStream := TMemoryStream.Create;
        try
          VData.TileSize := AStream.Size;
          VData.TileDate := Now;
          VData.TileVer := PWideChar(VarToWideStrDef(AVersionInfo.Version, ''));
          VData.TileMIME := PWideChar(FMainContentType.GetContentType);

          AStream.Position := 0;
          if AStream is TMemoryStream then begin
            VData.TileBody := TMemoryStream(AStream).Memory;
            PBDBDataToMemStream(@VData, VMemStream);
          end else begin
            GetMem(VData.TileBody, VData.TileSize);
            try
              AStream.Read(VData.TileBody^, VData.TileSize);
              PBDBDataToMemStream(@VData, VMemStream);
            finally
              FreeMem(VData.TileBody);
            end;
          end;
          VMemStream.Position := 0;
          VBDB.Write(@VKey, SizeOf(TBDBKey), VMemStream.Memory, VMemStream.Size);
        finally
          VMemStream.Free;
        end;
      end;
    finally
      FBDBPool.Release(VBDB);
    end;
    NotifierByZoomInternal[Azoom].TileUpdateNotify(AXY);
  end;
end;

procedure TTileStorageBerkeleyDB.SaveTNE(
  AXY: TPoint;
  Azoom: byte;
  AVersionInfo: IMapVersionInfo
);
var
  VPath: String;
  VMemStream: TMemoryStream;
  VBDB: TBerkeleyDB;
  VKey: TBDBKey;
  VData: TBDBData;
begin
  if StorageStateStatic.WriteAccess <> asDisabled then begin
    VPath := FCacheConfig.GetTileFileName(AXY, Azoom);
    VPath := ChangeFileExt(VPath, '.tne');
    CreateDirIfNotExists(VPath);
    VBDB := FBDBPool.Acquire(VPath);
    try
      if Assigned(VBDB) then begin
        VKey := PointToKey(AXY);
        VMemStream := TMemoryStream.Create;
        try
          VData.TileSize := 0;
          VData.TileDate := Now;
          VData.TileVer := PWideChar(VarToWideStrDef(AVersionInfo.Version, ''));
          VData.TileMIME := PWideChar(FMainContentType.GetContentType);
          VData.TileBody := nil;
          PBDBDataToMemStream(@VData, VMemStream);
          VMemStream.Position := 0;
          VBDB.Write(@VKey, SizeOf(TBDBKey), VMemStream.Memory, VMemStream.Size);
        finally
          VMemStream.Free;
        end;
      end;
    finally
      FBDBPool.Release(VBDB);
    end;
  end;
end;

end.
