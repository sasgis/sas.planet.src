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
  u_TileStorageBerkeleyDBHelper,
  u_GlobalCahceConfig,
  u_MapTypeCacheConfig,
  u_TileStorageAbstract;

type
  TTileStorageBerkeleyDB = class(TTileStorageAbstract)
  private
    FHelper: TTileStorageBerkeleyDBHelper;
    FCacheConfig: TMapTypeCacheConfigBerkeleyDB;
    FMainContentType: IContentTypeInfoBasic;
    FContentTypeManager: IContentTypeManager;
    FTileNotExistsTileInfo: ITileInfoBasic;
    FGCList: ITTLCheckNotifier;
    FTTLListener: ITTLCheckListener;
    procedure OnMapSettingsEdit(Sender: TObject);
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
  u_MapVersionFactorySimpleString,
  u_TTLCheckListener,
  u_TileFileNameBDB,
  u_TileStorageBerkeleyDBRecParser,
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
  inherited Create(
    TTileStorageTypeAbilitiesBerkeleyDB.Create,
    TMapVersionFactorySimpleString.Create,
    AConfig
  );
  FTileNotExistsTileInfo := TTileInfoBasicNotExists.Create(0, nil);
  FCacheConfig := TMapTypeCacheConfigBerkeleyDB.Create(
    AConfig,
    TTileFileNameBDB.Create,
    AGlobalCacheConfig,
    Self.OnMapSettingsEdit
  );
  FContentTypeManager := AContentTypeManager;
  FMainContentType := FContentTypeManager.GetInfoByExt(Config.TileFileExt);
  FHelper := TTileStorageBerkeleyDBHelper.Create(
    FCacheConfig.BasePath,
    AConfig.CoordConverter.Datum.EPSG
  );
  FTTLListener := TTTLCheckListener.Create(
    FHelper.Sync,
    CBDBSync,
    CBDBSyncCheckInterval
  );
  FGCList := AGCList;
  FGCList.Add(FTTLListener);
end;

destructor TTileStorageBerkeleyDB.Destroy;
begin
  if Assigned(FGCList) then begin
    FGCList.Remove(FTTLListener);
    FGCList := nil;
  end;
  FTTLListener := nil;
  FreeAndNil(FHelper);
  FMainContentType := nil;
  FContentTypeManager := nil;
  FreeAndNil(FCacheConfig);
  FTileNotExistsTileInfo := nil;
  inherited;
end;

procedure TTileStorageBerkeleyDB.OnMapSettingsEdit(Sender: TObject);
var
  VCacheConfig: TMapTypeCacheConfigBerkeleyDB;
begin
  if Assigned(FHelper) then begin
    if Sender is TMapTypeCacheConfigBerkeleyDB then begin
      VCacheConfig := Sender as TMapTypeCacheConfigBerkeleyDB;
      if Assigned(VCacheConfig) then begin
        FHelper.ChangeRootPath(VCacheConfig.BasePath);
      end;
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
  AZoom: byte;
  AVersionInfo: IMapVersionInfo
): ITileInfoBasic;
var
  VPath: string;
  VResult: Boolean;
  VData: TBDBData;
  VStream: TMemoryStream;
begin
  Result := FTileNotExistsTileInfo;
  if StorageStateStatic.ReadAccess <> asDisabled then begin

    VPath := FCacheConfig.GetTileFileName(AXY, AZoom);

    VResult := False;

    if FileExists(VPath) then begin
      VStream := TMemoryStream.Create;
      try
        VResult := FHelper.LoadTile(
          VPath,
          AXY,
          AZoom,
          AVersionInfo,
          VStream,
          VData
        );
        if VResult then begin
          VStream.Position := 0;
          Result := TTileInfoBasicExistsWithTile.Create(
            VData.TileDate,
            VStream.Memory,
            VData.TileSize,
            MapVersionFactory.CreateByStoreString(WideString(VData.TileVer)),
            FContentTypeManager.GetInfo(WideString(VData.TileMIME))
          );
        end;
      finally
        VStream.Free;
      end;
    end;

    if not VResult then begin
      VPath := ChangeFileExt(VPath, '.tne');
      if FileExists(VPath) then begin
        VResult := FHelper.IsTNEFound(
          VPath,
          AXY,
          AZoom,
          AVersionInfo,
          VData
        );
        if VResult then begin
          Result := TTileInfoBasicTNE.Create(VData.TileDate, AVersionInfo);
        end;
      end;
    end;

    if not VResult then begin
      Result := TTileInfoBasicNotExists.Create(0, AVersionInfo);
    end;
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
  VTile: Pointer;
  VSize: Integer;
begin
  Result := False;
  ATileInfo := FTileNotExistsTileInfo;
  AStream.Size := 0;
  if StorageStateStatic.ReadAccess <> asDisabled then begin
    ATileInfo := GetTileInfo(AXY, AZoom, AVersionInfo);
    if ATileInfo.IsExists then begin
      VTile := ATileInfo.Tile;
      VSize := ATileInfo.Size;
      if (VTile <> nil) and (VSize > 0) then begin
        AStream.Position := 0;
        Result := AStream.Write(VTile^, VSize) = VSize;
        AStream.Position := 0;
      end;
    end;
  end;
end;

procedure TTileStorageBerkeleyDB.SaveTile(
  AXY: TPoint;
  AZoom: byte;
  AVersionInfo: IMapVersionInfo;
  AStream: TStream
);
var
  VPath: string;
  VResult: Boolean;
begin
  if StorageStateStatic.WriteAccess <> asDisabled then begin
    VPath := FCacheConfig.GetTileFileName(AXY, AZoom);
    if FHelper.CreateDirIfNotExists(VPath) then begin
      VResult := FHelper.SaveTile(
        VPath,
        AXY,
        AZoom,
        Now,
        AVersionInfo,
        PWideChar(FMainContentType.GetContentType),
        AStream
      );
      if VResult then begin
        NotifierByZoomInternal[Azoom].TileUpdateNotify(AXY);
      end;
    end;
  end;
end;

procedure TTileStorageBerkeleyDB.SaveTNE(
  AXY: TPoint;
  AZoom: Byte;
  AVersionInfo: IMapVersionInfo
);
var
  VPath: String;
  VResult: Boolean;
begin
  if StorageStateStatic.WriteAccess <> asDisabled then begin
    VPath := FCacheConfig.GetTileFileName(AXY, AZoom);
    VPath := ChangeFileExt(VPath, '.tne');
    if FHelper.CreateDirIfNotExists(VPath) then begin
      VResult := FHelper.SaveTile(
        VPath,
        AXY,
        AZoom,
        Now,
        AVersionInfo,
        PWideChar(FMainContentType.GetContentType),
        nil
      );
      if VResult then begin
        NotifierByZoomInternal[AZoom].TileUpdateNotify(AXY);
      end;
    end;
  end;
end;

function TTileStorageBerkeleyDB.DeleteTile(
  AXY: TPoint;
  AZoom: Byte;
  AVersionInfo: IMapVersionInfo
): Boolean;
var
  VPath: string;
begin
  Result := False;
  if StorageStateStatic.DeleteAccess <> asDisabled then begin
    try
      VPath := FCacheConfig.GetTileFileName(AXY, AZoom);
      if FileExists(VPath) then begin
        Result := FHelper.DeleteTile(
          VPath,
          AXY,
          AZoom,
          AVersionInfo
        );
      end;
      if not Result then begin
        Result := DeleteTNE(AXY, AZoom, AVersionInfo);
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
begin
  Result := False;
  if StorageStateStatic.DeleteAccess <> asDisabled then begin
    try
      VPath := FCacheConfig.GetTileFileName(AXY, AZoom);
      VPath := ChangeFileExt(VPath, '.tne');
      if FileExists(VPath) then begin
        Result := FHelper.DeleteTile(
          VPath,
          AXY,
          AZoom,
          AVersionInfo
        );
      end;
    except
      Result := False;
    end;
  end;
end;

end.
