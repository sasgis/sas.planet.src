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

unit u_TileStorageDBMS;

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
  u_DBMS_provider,
  u_GlobalCahceConfig,
  u_MapTypeCacheConfig,
  u_TileStorageAbstract,
  t_ETS_Result,
  t_ETS_Tiles,
  u_ETS;

type
  TTileStorageDBMS = class(TTileStorageAbstract)
  private
    FExtStorage: TETS_Host_Provider_Basic;
    FExtLink: TETS_Host_Link;
    //FBDBPool: TDBMSPool;
    FCacheConfig: TMapTypeCacheConfigDBMS;
    FMainContentType: IContentTypeInfoBasic;
    FTileNotExistsTileInfo: ITileInfoBasic;
    FGCList: ITTLCheckNotifier;
    FTTLListener: ITTLCheckListener;
    FAutoExecDDL: Boolean;
    procedure Sync(Sender: TObject);
    procedure InternalCreateStorageLink;
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
  u_MapVersionFactorySimpleString,
  u_TTLCheckListener,
  u_TileStorageTypeAbilities,
  u_TileInfoBasic;

{ TTileStorageDBMS }

constructor TTileStorageDBMS.Create(
  AGCList: ITTLCheckNotifier;
  AConfig: ISimpleTileStorageConfig;
  AGlobalCacheConfig: TGlobalCahceConfig;
  AContentTypeManager: IContentTypeManager
);
const
  CBDBSync = 300000; // 5 min
  CBDBSyncCheckInterval = 60000; // 60 sec
begin
  inherited Create(
    TTileStorageTypeAbilitiesDBMS.Create,
    TMapVersionFactorySimpleString.Create,
    AConfig
  );

  FAutoExecDDL:=FALSE;
  
  FGCList := AGCList;

  FTileNotExistsTileInfo := TTileInfoBasicNotExists.Create(0, nil);

  FCacheConfig := TMapTypeCacheConfigDBMS.Create(
    AConfig,
    //TTileFileNameBDB.Create,
    AGlobalCacheConfig
  );

  FMainContentType := AContentTypeManager.GetInfoByExt(Config.TileFileExt);

  // create storage provider
  FExtStorage:=Create_Tile_Storage_EXE(r_DBMS_Provider_Query_Info, FCacheConfig.GlobalStorageIdentifier);
  // do not create connection to storage at startup
  FExtLink:=nil;

  //FBDBPool := TDBMSPool.Create;
  FTTLListener := TTTLCheckListener.Create(Self.Sync, CBDBSync, CBDBSyncCheckInterval);
  FGCList.Add(FTTLListener);
end;

destructor TTileStorageDBMS.Destroy;
begin
  FGCList.Remove(FTTLListener);
  FTTLListener := nil;
  FGCList := nil;
  FreeAndNil(FCacheConfig);
  //FreeAndNil(FBDBPool);
  FreeAndNil(FExtLink);
  FreeAndNil(FExtStorage);
  FTileNotExistsTileInfo:=nil;
  inherited;
end;

procedure TTileStorageDBMS.Sync(Sender: TObject);
begin
  if (nil<>FExtLink) then
    FExtLink.Sync(Self);
end;

function TTileStorageDBMS.DeleteTile(
  AXY: TPoint;
  AZoom: Byte;
  AVersionInfo: IMapVersionInfo
): Boolean;
var
  Vtid: TTILE_ID_XYZ;
begin
{
  Delete single tile (by XYZ and Version) from table:
  a) no version defined (nil=AVersionInfo) - kill last version only (allow repeat for all versions)
  b) if special version defined - kill only given version
  Also kill TNE with same version (if exists)
}
  Result := False;
  if StorageStateStatic.DeleteAccess <> asDisabled then begin
    InternalCreateStorageLink;
    if (nil<>FExtLink) then
    if (FExtLink.Connected) then begin
      try
        // make tile_id struct
        Vtid.x:=AXY.X;
        Vtid.y:=AXY.Y;
        Vtid.z:=AZoom;
        // execute
        Result:=(ETSR_OK=FExtLink.Delete_Tile_TNE(@Vtid, AVersionInfo, (ETS_DELETE_TILE or ETS_DELETE_TNE)));
        // if no table or other DDL errors - treat as no tile
      except
        Result := False;
      end;
      if Result then begin
        NotifyTileUpdate(AXY, Azoom, AVersionInfo);
      end;
    end;
  end;
end;

function TTileStorageDBMS.DeleteTNE(
  AXY: TPoint;
  Azoom: byte;
  AVersionInfo: IMapVersionInfo
): Boolean;
var
  Vtid: TTILE_ID_XYZ;
begin
  Result := False;
  if StorageStateStatic.DeleteAccess <> asDisabled then begin
    InternalCreateStorageLink;
    if (nil<>FExtLink) then
    if (FExtLink.Connected) then begin
      try
        // make tile_id struct
        Vtid.x:=AXY.X;
        Vtid.y:=AXY.Y;
        Vtid.z:=AZoom;
        // execute
        Result:=(ETSR_OK=FExtLink.Delete_Tile_TNE(@Vtid, AVersionInfo, ETS_DELETE_TNE));
        // if no table or other DDL errors - treat as no tile
      except
        Result := False;
      end;
    end;
  end;
end;

function TTileStorageDBMS.GetAllowDifferentContentTypes: Boolean;
begin
  Result := (nil<>FExtLink) and (FExtLink.Underlaying) and (FExtLink.ContentTypeMixed);
end;

function TTileStorageDBMS.GetCacheConfig: TMapTypeCacheConfigAbstract;
begin
  Result := FCacheConfig;
end;

function TTileStorageDBMS.GetMainContentType: IContentTypeInfoBasic;
begin
  Result := FMainContentType;
end;

function TTileStorageDBMS.GetTileFileName(
  AXY: TPoint;
  Azoom: byte;
  AVersionInfo: IMapVersionInfo
): string;
var VVer: String;
begin
  // TODO: add AVersionInfo to underlaying call GetTileFileName
  Result := FCacheConfig.GetTileFileName(AXY, Azoom);
  if Assigned(AVersionInfo) then begin
    VVer:=AVersionInfo.StoreString;
    if (0<=Length(VVer)) then
      Result:=Result +'['+VVer+']';
  end;
end;

function TTileStorageDBMS.GetTileInfo(
  AXY: TPoint;
  Azoom: byte;
  AVersionInfo: IMapVersionInfo
): ITileInfoBasic;
var
  Vtid: TTILE_ID_XYZ;
  VResult: LongInt;
begin
  Result := nil;
  if StorageStateStatic.ReadAccess <> asDisabled then begin
    InternalCreateStorageLink;
    if (nil<>FExtLink) then
    if (FExtLink.Connected) then begin
      try
        // make tile_id struct
        Vtid.x:=AXY.X;
        Vtid.y:=AXY.Y;
        Vtid.z:=AZoom;
        // execute
        VResult:=FExtLink.Query_Tile(@Vtid, AVersionInfo, Result);
        // if no table or other DDL errors - treat as no tile
        if (ETSR_OK<>VResult) then
          SysUtils.Abort;
      except
        Result:=nil;
      end;
    end;
  end;
end;

procedure TTileStorageDBMS.InternalCreateStorageLink;
var t: TETS_SOURCE_STORAGE_OPTIONS;
begin
  if (nil=FExtLink) then begin
    Init_TETS_SOURCE_STORAGE_OPTIONS(@t);
    FExtLink:=FExtStorage.CreateNewLink(FCacheConfig.ServiceName, '', '', @t);
  end;
end;

function TTileStorageDBMS.LoadTile(
  AXY: TPoint;
  AZoom: Byte;
  AVersionInfo: IMapVersionInfo;
  AStream: TStream;
  out ATileInfo: ITileInfoBasic
): Boolean;
var
  Vtid: TTILE_ID_XYZ;
  VResult: LongInt;
begin
  Result := False;
  ATileInfo := nil;
  AStream.Size := 0;
  if StorageStateStatic.ReadAccess <> asDisabled then begin
    InternalCreateStorageLink;
    if (nil<>FExtLink) then
    if (FExtLink.Connected) then begin
      try
        // make tile_id struct
        Vtid.x:=AXY.X;
        Vtid.y:=AXY.Y;
        Vtid.z:=AZoom;
        // execute
        VResult:=FExtLink.Select_Tile(@Vtid, AVersionInfo, ATileInfo, AStream);
        if (ETSR_OK<>VResult) then
          SysUtils.Abort // if no table or other DDL errors - treat as no tile
        else
          Result:=TRUE;
      except
        AStream.Size := 0;
        ATileInfo:=nil;
        Result:=FALSE;
      end;
    end;
  end;
end;

procedure TTileStorageDBMS.SaveTile(
  AXY: TPoint;
  Azoom: byte;
  AVersionInfo: IMapVersionInfo;
  AStream: TStream
);
var
  Vtid: TTILE_ID_XYZ;
  VTileBuffer: Pointer;
  VTileSize: LongWord;
  VMemStream: TMemoryStream;
  VResult: LongInt;
begin
  if Assigned(AStream) then
  if StorageStateStatic.WriteAccess <> asDisabled then begin
    InternalCreateStorageLink;
    if (nil<>FExtLink) then
    if (FExtLink.Connected) then begin
      VMemStream:=nil;
      try
        // make tile_id struct
        Vtid.x:=AXY.X;
        Vtid.y:=AXY.Y;
        Vtid.z:=AZoom;

        // create buffer (or use it from TMemoryStream)
        if (AStream is TCustomMemoryStream) then begin
          VTileBuffer:=TCustomMemoryStream(AStream).Memory;
          VTileSize:=TCustomMemoryStream(AStream).Size;
        end else begin
          // another stream - convert to memory
          AStream.Position:=0;
          VMemStream:=TMemoryStream.Create;
          VMemStream.CopyFrom(AStream, AStream.Size);
          VTileBuffer:=VMemStream.Memory;
          VTileSize:=VMemStream.Size;
        end;

        // execute
        // TODO: get date (UTC) from caller
        VResult:=FExtLink.Insert_Tile(@Vtid, AVersionInfo, VTileBuffer, VTileSize, nil);
        if ((ETSR_ERROR_NEED_DDL_MAPS=VResult) or (ETSR_ERROR_NEED_DDL_TILES=VResult)) and FAutoExecDDL then begin
          // autorun DDL and then repeat
          VResult:=FExtLink.Execute_DDL(@Vtid, AVersionInfo);
          if (ETSR_OK=VResult) then begin
            // DDL ok
            {VResult:=}FExtLink.Insert_Tile(@Vtid, AVersionInfo, VTileBuffer, VTileSize, nil);
            // TODO: check result
          end else begin
            // DDL failed
            // TODO: check result
          end;
        end else begin
          // general error or no autoexec DDL
          // TODO: check result
        end;
        
      finally
        FreeAndNil(VMemStream);
      end;

      NotifyTileUpdate(AXY, Azoom, AVersionInfo);
    end;
  end;
end;

procedure TTileStorageDBMS.SaveTNE(
  AXY: TPoint;
  Azoom: byte;
  AVersionInfo: IMapVersionInfo
);
var
  Vtid: TTILE_ID_XYZ;
begin
  if StorageStateStatic.WriteAccess <> asDisabled then begin
    InternalCreateStorageLink;
    if (nil<>FExtLink) then
    if (FExtLink.Connected) then begin
      try
        // make tile_id struct
        Vtid.x:=AXY.X;
        Vtid.y:=AXY.Y;
        Vtid.z:=AZoom;
        // execute
        FExtLink.Insert_TNE(@Vtid, AVersionInfo);
        // TODO: worth to check result?
      except
      end;
    end;
  end;
end;

end.
