{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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

unit u_GlobalCacheConfig;

interface

uses
  i_Notifier,
  i_PathConfig,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider;

type
  TGlobalCacheConfig = class
  private
    FCacheGlobalPath: IPathConfig;

    //Способ храения кэша по-умолчанию.
    FDefCache: byte;

    //Пути к кэшам разных типов
    FNewCPath: IPathConfig;
    FOldCPath: IPathConfig;
    FESCPath: IPathConfig;
    FGMTilesPath: IPathConfig;
    FGECachePath: IPathConfig;
    FGCCachePath: IPathConfig;
    FBDBCachePath: IPathConfig;
    FDBMSCachePath: IPathConfig;

    FCacheChangeNotifier: INotifier;
    FCacheChangeNotifierInternal: INotifierInternal;
    procedure SetDefCache(const Value: byte);
  public
    constructor Create(
      const ACacheGlobalPath: IPathConfig
    );
    destructor Destroy; override;

    procedure LoadConfig(const AConfigProvider: IConfigDataProvider);
    procedure SaveConfig(const AConfigProvider: IConfigDataWriteProvider);

    //Способ храения кэша по-умолчанию.
    property DefCache: byte read FDefCache write SetDefCache;

    //Пути к кэшам разных типов
    property NewCPath: IPathConfig read FNewCPath;
    property OldCPath: IPathConfig read FOldCPath;
    property ESCPath: IPathConfig read FESCPath;
    property GMTilesPath: IPathConfig read FGMTilesPath;
    property GECachePath: IPathConfig read FGECachePath;
    property GCCachePath: IPathConfig read FGCCachePath;
    property BDBCachePath: IPathConfig read FBDBCachePath;
    property DBMSCachePath: IPathConfig read FDBMSCachePath;

    property CacheChangeNotifier: INotifier read FCacheChangeNotifier;
  end;

implementation

uses
  c_CacheTypeCodes,
  u_PathConfig,
  u_Notifier;

{ TGlobalCacheConfig }

constructor TGlobalCacheConfig.Create(
  const ACacheGlobalPath: IPathConfig
);
begin
  inherited Create;
  FCacheGlobalPath := ACacheGlobalPath;
  FDefCache := c_File_Cache_Id_SAS;
  FCacheChangeNotifierInternal := TNotifierBase.Create;
  FCacheChangeNotifier := FCacheChangeNotifierInternal;

  FOldCPath      := TPathConfig.Create('GMVC',      c_File_Cache_Default_GMV,  FCacheGlobalPath);
  FNewCPath      := TPathConfig.Create('SASC',      c_File_Cache_Default_SAS,  FCacheGlobalPath);
  FESCPath       := TPathConfig.Create('ESC',       c_File_Cache_Default_ES,   FCacheGlobalPath);
  FGMTilesPath   := TPathConfig.Create('GMTiles',   c_File_Cache_Default_GM,   FCacheGlobalPath);
  FGECachePath   := TPathConfig.Create('GECache',   c_File_Cache_Default_GE,   FCacheGlobalPath);
  FGCCachePath   := TPathConfig.Create('GCCache',   c_File_Cache_Default_GC,   FCacheGlobalPath);
  FBDBCachePath  := TPathConfig.Create('BDBCache',  c_File_Cache_Default_BDB,  FCacheGlobalPath);
  FDBMSCachePath := TPathConfig.Create('DBMSCache', c_File_Cache_Default_DBMS, FCacheGlobalPath);
end;

destructor TGlobalCacheConfig.Destroy;
begin
  FCacheChangeNotifier := nil;
  inherited;
end;

procedure TGlobalCacheConfig.LoadConfig(const AConfigProvider: IConfigDataProvider);
var
  VViewConfig: IConfigDataProvider;
  VPathConfig: IConfigDataProvider;
begin
  VViewConfig := AConfigProvider.GetSubItem('VIEW');
  if VViewConfig <> nil then begin
    DefCache := VViewConfig.ReadInteger('DefCache', FDefCache);
  end;

  VPathConfig := AConfigProvider.GetSubItem('PATHtoCACHE');
  if VPathConfig <> nil then begin
    OldCPath.ReadConfig(VPathConfig);
    NewCPath.ReadConfig(VPathConfig);
    ESCPath.ReadConfig(VPathConfig);
    GMTilesPath.ReadConfig(VPathConfig);
    GECachePath.ReadConfig(VPathConfig);
    GCCachePath.ReadConfig(VPathConfig);
    BDBCachePath.ReadConfig(VPathConfig);
    DBMSCachePath.ReadConfig(VPathConfig);
  end;
end;

procedure TGlobalCacheConfig.SaveConfig(
  const AConfigProvider: IConfigDataWriteProvider
);
var
  VViewConfig: IConfigDataWriteProvider;
  VPathConfig: IConfigDataWriteProvider;
begin
  VViewConfig := AConfigProvider.GetOrCreateSubItem('VIEW');
  VPathConfig := AConfigProvider.GetOrCreateSubItem('PATHtoCACHE');
  VViewConfig.WriteInteger('DefCache', FDefCache);

  OldCPath.WriteConfig(VPathConfig);
  NewCPath.WriteConfig(VPathConfig);
  ESCPath.WriteConfig(VPathConfig);
  GMTilesPath.WriteConfig(VPathConfig);
  GECachePath.WriteConfig(VPathConfig);
  GCCachePath.WriteConfig(VPathConfig);
  BDBCachePath.WriteConfig(VPathConfig);
  DBMSCachePath.WriteConfig(VPathConfig);
end;

procedure TGlobalCacheConfig.SetDefCache(const Value: byte);
begin
  if Value in [c_File_Cache_Id_GMV,
    c_File_Cache_Id_SAS,
    c_File_Cache_Id_ES,
    c_File_Cache_Id_GM,
    c_File_Cache_Id_GM_Aux,
    c_File_Cache_Id_DBMS,
    c_File_Cache_Id_BDB] then begin
    if FDefCache <> Value then begin
      FDefCache := Value;
      FCacheChangeNotifierInternal.Notify(nil);
    end;
  end;
end;

end.
