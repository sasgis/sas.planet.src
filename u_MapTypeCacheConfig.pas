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

unit u_MapTypeCacheConfig;

interface

uses
  Types,
  SysUtils,
  i_Notifier,
  i_Listener,
  i_SimpleTileStorageConfig,
  i_TileFileNameGeneratorsList,
  u_GlobalCahceConfig,
  u_ETS_Path,
  i_TileFileNameGenerator;

type
  TOnAfterMapSettingsEdit = procedure(Sender: TObject) of object;

  TMapTypeCacheConfigAbstract = class
  private
    FConfig: ISimpleTileStorageConfig;

    FGlobalCacheConfig: TGlobalCahceConfig;
    FGlobalSettingsListener: IListener;
    procedure OnSettingsEdit; virtual; abstract;
  protected
    FEffectiveCacheType: Byte;
    FBasePath: String;
    FFileNameGenerator: ITileFileNameGenerator;

    FConfigChangeNotifier: INotifier;
  public
    constructor Create(
      const AConfig: ISimpleTileStorageConfig;
      AGlobalCacheConfig: TGlobalCahceConfig
    );
    destructor Destroy; override;
    function GetTileFileName(
      const AXY: TPoint;
      const AZoom: byte
    ): string; virtual;

    property ConfigChangeNotifier: INotifier read FConfigChangeNotifier;
    property BasePath: string read FBasePath;
  end;

  TMapTypeCacheConfig = class(TMapTypeCacheConfigAbstract)
  private
    FTileNameGeneratorList: ITileFileNameGeneratorsList;
    procedure OnSettingsEdit; override;
  public
    constructor Create(
      const AConfig: ISimpleTileStorageConfig;
      AGlobalCacheConfig: TGlobalCahceConfig;
      const ATileNameGeneratorList: ITileFileNameGeneratorsList
    );
  end;

  TMapTypeCacheConfigDLL = class(TMapTypeCacheConfigAbstract)
  private
    FCS: IReadWriteSync;
    FNameInCache: String;
    FOnSettingsEdit: TOnAfterMapSettingsEdit;
  protected
    function GetGlobalCacheParameter: String; virtual; abstract;
    procedure OnSettingsEdit; override;
  public
    constructor Create(
      const AConfig: ISimpleTileStorageConfig;
      AGlobalCacheConfig: TGlobalCahceConfig;
      AOnSettingsEdit: TOnAfterMapSettingsEdit
    );
    destructor Destroy; override;

    function GetNameInCache: string;
  end;

  TMapTypeCacheConfigGC = class(TMapTypeCacheConfigDLL)
  protected
    function GetGlobalCacheParameter: String; override;
  end;

  TMapTypeCacheConfigGE = class(TMapTypeCacheConfigDLL)
  protected
    function GetGlobalCacheParameter: String; override;
  end;

  TMapTypeCacheConfigBerkeleyDB = class(TMapTypeCacheConfigAbstract)
  private
    FCS: IReadWriteSync;
  protected
    FOnSettingsEdit: TOnAfterMapSettingsEdit;
    procedure OnSettingsEdit; override;
  public
    constructor Create(
      const AConfig: ISimpleTileStorageConfig;
      const AFileNameGenerator: ITileFileNameGenerator;
      AGlobalCacheConfig: TGlobalCahceConfig;
      AOnSettingsEdit: TOnAfterMapSettingsEdit
    );
    destructor Destroy; override;
    function GetTileFileName(
      const AXY: TPoint;
      const AZoom: byte
    ): string; override;
    property BasePath: string read FBasePath;
  end;

  TMapTypeCacheConfigDBMS = class(TMapTypeCacheConfigAbstract)
  private
    FGlobalStorageIdentifier: String;
    FServiceName: String;
    FOnSettingsEdit: TOnAfterMapSettingsEdit;
    procedure OnSettingsEdit; override;
  public
    constructor Create(
      const AConfig: ISimpleTileStorageConfig;
      AGlobalCacheConfig: TGlobalCahceConfig;
      AOnSettingsEdit: TOnAfterMapSettingsEdit
    );

    property ServiceName: String read FServiceName;
    property GlobalStorageIdentifier: String read FGlobalStorageIdentifier;
  end;


implementation

uses
  Windows,
  u_Synchronizer,
  ShLwApi,
  c_CacheTypeCodes,
  u_Notifier,
  u_ListenerByEvent;

{ TMapTypeCacheConfigAbstract }

constructor TMapTypeCacheConfigAbstract.Create(
  const AConfig: ISimpleTileStorageConfig;
  AGlobalCacheConfig: TGlobalCahceConfig
);
begin
  inherited Create;
  FConfig := AConfig;
  FGlobalCacheConfig := AGlobalCacheConfig;
  FConfigChangeNotifier := TNotifierBase.Create;

  FGlobalSettingsListener := TNotifyNoMmgEventListener.Create(Self.OnSettingsEdit);
  FGlobalCacheConfig.CacheChangeNotifier.Add(FGlobalSettingsListener);
  FConfig.ChangeNotifier.Add(FGlobalSettingsListener);
end;

destructor TMapTypeCacheConfigAbstract.Destroy;
begin
  FConfig.ChangeNotifier.Remove(FGlobalSettingsListener);
  FGlobalCacheConfig.CacheChangeNotifier.Remove(FGlobalSettingsListener);
  FGlobalSettingsListener := nil;

  FConfigChangeNotifier := nil;
  inherited;
end;

function TMapTypeCacheConfigAbstract.GetTileFileName(
  const AXY: TPoint;
  const AZoom: byte
): string;
begin
  Result := FBasePath + FFileNameGenerator.GetTileFileName(AXY, AZoom) + FConfig.GetStatic.TileFileExt;
end;

{ TMapTypeCacheConfig }

constructor TMapTypeCacheConfig.Create(
  const AConfig: ISimpleTileStorageConfig;
  AGlobalCacheConfig: TGlobalCahceConfig;
  const ATileNameGeneratorList: ITileFileNameGeneratorsList
);
begin
  inherited Create(AConfig, AGlobalCacheConfig);
  FTileNameGeneratorList := ATileNameGeneratorList;
  OnSettingsEdit;
end;

procedure TMapTypeCacheConfig.OnSettingsEdit;
var
  VCacheType: Byte;
  VBasePath: string;
  VConfig: ISimpleTileStorageConfigStatic;
begin
  VConfig := FConfig.GetStatic;
  VCacheType := VConfig.CacheTypeCode;
  if VCacheType = c_File_Cache_Id_DEFAULT then begin
    VCacheType := FGlobalCacheConfig.DefCache;
  end;
  FEffectiveCacheType := VCacheType;
  FFileNameGenerator := FTileNameGeneratorList.GetGenerator(FEffectiveCacheType);

  if (c_File_Cache_Id_DBMS = FEffectiveCacheType) then begin
    // very special
    FBasePath := ETS_TilePath_Single(FGlobalCacheConfig.DBMSCachePath, VConfig.NameInCache);
    Exit;
  end;


  VBasePath := VConfig.NameInCache;
  //TODO: С этим бардаком нужно что-то будет сделать
  if (length(VBasePath) < 2) or ((VBasePath[2] <> '\') and (system.pos(':', VBasePath) = 0)) then begin
    case FEffectiveCacheType of
      c_File_Cache_Id_GMV: begin
        if FGlobalCacheConfig.OldCPath <> '' then begin
          VBasePath := IncludeTrailingPathDelimiter(FGlobalCacheConfig.OldCPath) + VBasePath;
        end;
      end;
      c_File_Cache_Id_SAS: begin
        if FGlobalCacheConfig.NewCPath <> '' then begin
          VBasePath := IncludeTrailingPathDelimiter(FGlobalCacheConfig.NewCPath) + VBasePath;
        end;
      end;
      c_File_Cache_Id_ES: begin
        if FGlobalCacheConfig.ESCPath <> '' then begin
          VBasePath := IncludeTrailingPathDelimiter(FGlobalCacheConfig.ESCPath) + VBasePath;
        end;
      end;
      c_File_Cache_Id_GM, c_File_Cache_Id_GM_Aux, c_File_Cache_Id_GM_Bing: begin
        if FGlobalCacheConfig.GMTilesPath <> '' then begin
          VBasePath := IncludeTrailingPathDelimiter(FGlobalCacheConfig.GMTilesPath) + VBasePath;
        end;
      end;
      c_File_Cache_Id_GE: begin
        if FGlobalCacheConfig.GECachePath <> '' then begin
          VBasePath := IncludeTrailingPathDelimiter(FGlobalCacheConfig.GECachePath) + VBasePath;
        end;
      end;
      c_File_Cache_Id_BDB: begin
        if FGlobalCacheConfig.BDBCachePath <> '' then begin
          VBasePath := IncludeTrailingPathDelimiter(FGlobalCacheConfig.BDBCachePath) + VBasePath;
        end;
      end;
      c_File_Cache_Id_GC: begin
        if FGlobalCacheConfig.GCCachePath <> '' then begin
          VBasePath := IncludeTrailingPathDelimiter(FGlobalCacheConfig.GCCachePath) + VBasePath;
        end;
      end;
    end;
  end;
  //TODO: С этим бардаком нужно что-то будет сделать
  if (length(VBasePath) < 2) or ((VBasePath[2] <> '\') and (system.pos(':', VBasePath) = 0)) then begin
    VBasePath := IncludeTrailingPathDelimiter(FGlobalCacheConfig.CacheGlobalPath) + VBasePath;
  end;
  VBasePath := IncludeTrailingPathDelimiter(VBasePath);
  FBasePath := VBasePath;
end;

{ TMapTypeCacheConfigDLL }

constructor TMapTypeCacheConfigDLL.Create(
  const AConfig: ISimpleTileStorageConfig;
  AGlobalCacheConfig: TGlobalCahceConfig;
  AOnSettingsEdit: TOnAfterMapSettingsEdit
);
begin
  inherited Create(AConfig, AGlobalCacheConfig);
  FCS := MakeSyncRW_Sym(Self, TRUE);
  FOnSettingsEdit := AOnSettingsEdit;
  OnSettingsEdit;
end;

procedure TMapTypeCacheConfigDLL.OnSettingsEdit;
var
  VBasePath: string;
begin
  FCS.BeginWrite;
  try
    // current GE cache path
    FNameInCache := FConfig.GetStatic.NameInCache;
    if (Length(FNameInCache) > 0) then begin
      if (FNameInCache[Length(FNameInCache)] <> PathDelim) then begin
        FNameInCache := FNameInCache + PathDelim;
      end;
    end;

    // global GE cache path
    VBasePath := GetGlobalCacheParameter;
    //TODO: С этим бардаком нужно что-то будет сделать
    if (length(VBasePath) < 2) or ((VBasePath[2] <> '\') and (system.pos(':', VBasePath) = 0)) then begin
      VBasePath := IncludeTrailingPathDelimiter(FGlobalCacheConfig.CacheGlobalPath) + VBasePath;
    end;
    VBasePath := IncludeTrailingPathDelimiter(VBasePath);
    FBasePath := VBasePath;
  finally
    FCS.EndWrite;
  end;

  if Assigned(FOnSettingsEdit) then begin
    FOnSettingsEdit(Self);
  end;
end;

destructor TMapTypeCacheConfigDLL.Destroy;
begin
  FCS := nil;
  inherited Destroy;
end;

function TMapTypeCacheConfigDLL.GetNameInCache: string;
begin
  FCS.BeginRead;
  try
    Result := FNameInCache;
    if (0 = Length(Result)) then begin
      Result := FBasePath;
    end;
  finally
    FCS.EndRead;
  end;
end;

{ TMapTypeCacheConfigBerkeleyDB }

constructor TMapTypeCacheConfigBerkeleyDB.Create(
  const AConfig: ISimpleTileStorageConfig;
  const AFileNameGenerator: ITileFileNameGenerator;
  AGlobalCacheConfig: TGlobalCahceConfig;
  AOnSettingsEdit: TOnAfterMapSettingsEdit
);
begin
  inherited Create(AConfig, AGlobalCacheConfig);
  FCS := MakeSyncRW_Sym(Self, TRUE);
  FFileNameGenerator := AFileNameGenerator;
  FOnSettingsEdit := AOnSettingsEdit;
  OnSettingsEdit;
end;

destructor TMapTypeCacheConfigBerkeleyDB.Destroy;
begin
  FCS := nil;
  inherited Destroy;
end;

procedure TMapTypeCacheConfigBerkeleyDB.OnSettingsEdit;

  function RelativeToAbsolutePath(const ABasePath, ARelativePath: string): string;
  begin
    SetLength(Result, MAX_PATH);
    PathCombine(@Result[1], PChar(ABasePath), PChar(ARelativePath));
    SetLength(Result, StrLen(@Result[1]));
  end;

var
  VBasePath: string;
  VCachePath: string;
begin
  FCS.BeginWrite;
  try
    VBasePath := FConfig.GetStatic.NameInCache;
    if PathIsRelative(PAnsiChar(VBasePath)) then begin
      if FGlobalCacheConfig.BDBCachePath <> '' then begin
        VCachePath := IncludeTrailingPathDelimiter(FGlobalCacheConfig.BDBCachePath);
      end else begin
        VCachePath := '';
      end;
      if PathIsRelative(PAnsiChar(VCachePath)) then begin
        VCachePath := RelativeToAbsolutePath(
          IncludeTrailingPathDelimiter(FGlobalCacheConfig.CacheGlobalPath),
          VCachePath
        );
      end;
      VBasePath := RelativeToAbsolutePath(VCachePath, VBasePath);
    end;
    FBasePath := IncludeTrailingPathDelimiter(VBasePath);
  finally
    FCS.EndWrite;
  end;

  if Assigned(FOnSettingsEdit) then begin
    FOnSettingsEdit(Self);
  end;
end;

function TMapTypeCacheConfigBerkeleyDB.GetTileFileName(
  const AXY: TPoint;
  const AZoom: Byte
): string;
begin
  FCS.BeginRead;
  try
    Result := FBasePath + FFileNameGenerator.GetTileFileName(AXY, AZoom) + '.sdb';
  finally
    FCS.EndRead;
  end;
end;

{ TMapTypeCacheConfigDBMS }

constructor TMapTypeCacheConfigDBMS.Create(
  const AConfig: ISimpleTileStorageConfig;
  AGlobalCacheConfig: TGlobalCahceConfig;
  AOnSettingsEdit: TOnAfterMapSettingsEdit
);
begin
  inherited Create(AConfig, AGlobalCacheConfig);
  FOnSettingsEdit := AOnSettingsEdit;
  OnSettingsEdit;
end;

procedure TMapTypeCacheConfigDBMS.OnSettingsEdit;
begin
  // no need to sync - cache at tilestorage and read only because of FOnSettingsEdit ticks
  FGlobalStorageIdentifier := FGlobalCacheConfig.DBMSCachepath;
  FServiceName := FConfig.GetStatic.NameInCache;
  FBasePath := ETS_TilePath_Single(FGlobalStorageIdentifier, FServiceName);
  // notify
  if Assigned(FOnSettingsEdit) then begin
    FOnSettingsEdit(Self);
  end;
end;

{ TMapTypeCacheConfigGC }

function TMapTypeCacheConfigGC.GetGlobalCacheParameter: String;
begin
  Result := FGlobalCacheConfig.GCCachePath;
end;

{ TMapTypeCacheConfigGE }

function TMapTypeCacheConfigGE.GetGlobalCacheParameter: String;
begin
  Result := FGlobalCacheConfig.GECachePath;
end;

end.
