{******************************************************************************}
{* SAS.Планета (SAS.Planet)                                                   *}
{* Copyright (C) 2007-2011, авторы программы SAS.Планета (SAS.Planet).        *}
{* Это программа является свободным программным обеспечением. Вы можете       *}
{* распространять и/или модифицировать её согласно условиям Стандартной       *}
{* Общественной Лицензии GNU, опубликованной Фондом Свободного Программного   *}
{* Обеспечения, версии 3. Эта программа распространяется в надежде, что она   *}
{* будет полезной, но БЕЗ ВСЯКИХ ГАРАНТИЙ, в том числе подразумеваемых        *}
{* гарантий ТОВАРНОГО СОСТОЯНИЯ ПРИ ПРОДАЖЕ и ГОДНОСТИ ДЛЯ ОПРЕДЕЛЁННОГО      *}
{* ПРИМЕНЕНИЯ. Смотрите Стандартную Общественную Лицензию GNU версии 3, для   *}
{* получения дополнительной информации. Вы должны были получить копию         *}
{* Стандартной Общественной Лицензии GNU вместе с программой. В случае её     *}
{* отсутствия, посмотрите http://www.gnu.org/licenses/.                       *}
{*                                                                            *}
{* http://sasgis.ru/sasplanet                                                 *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_MapTypeCacheConfig;

interface

uses
  Types,
  i_JclNotify,
  i_SimpleTileStorageConfig,
  i_TileFileNameGeneratorsList,
  u_GlobalCahceConfig,
  i_TileFileNameGenerator;

type
  TMapTypeCacheConfigAbstract = class
  private
    FConfig: ISimpleTileStorageConfig;

    FGlobalCacheConfig: TGlobalCahceConfig;
    FGlobalSettingsListener: IJclListener;
    procedure OnSettingsEdit(Sender: TObject); virtual; abstract;
  protected
    FEffectiveCacheType: Byte;
    FBasePath: String;
    FFileNameGenerator: ITileFileNameGenerator;

    FConfigChangeNotifier: IJclNotifier;
  public
    constructor Create(
      AConfig: ISimpleTileStorageConfig;
      AGlobalCacheConfig: TGlobalCahceConfig
    );
    destructor Destroy; override;
    function GetTileFileName(AXY: TPoint; Azoom: byte): string;

    property ConfigChangeNotifier: IJclNotifier read FConfigChangeNotifier;
  end;

  TMapTypeCacheConfig = class(TMapTypeCacheConfigAbstract)
  private
    FTileNameGeneratorList: ITileFileNameGeneratorsList;
    procedure OnSettingsEdit(Sender: TObject); override;
  public
    constructor Create(
      AConfig: ISimpleTileStorageConfig;
      AGlobalCacheConfig: TGlobalCahceConfig;
      ATileNameGeneratorList: ITileFileNameGeneratorsList
    );
  end;

  TMapTypeCacheConfigGE = class(TMapTypeCacheConfigAbstract)
  protected
    procedure OnSettingsEdit(Sender: TObject); override;
  public
    constructor Create(
      AConfig: ISimpleTileStorageConfig;
      AGlobalCacheConfig: TGlobalCahceConfig
    );
    function GetIndexFileName: string;
    function GetDataFileName: string;
    function GetNameInCache: string;
  end;

implementation

uses
  SysUtils,
  u_JclNotify,
  u_NotifyEventListener;

{ TMapTypeCacheConfigAbstract }

constructor TMapTypeCacheConfigAbstract.Create(
  AConfig: ISimpleTileStorageConfig;
  AGlobalCacheConfig: TGlobalCahceConfig
);
begin
  FConfig := AConfig;
  FGlobalCacheConfig := AGlobalCacheConfig;
  FConfigChangeNotifier := TJclBaseNotifier.Create;

  FGlobalSettingsListener := TNotifyEventListener.Create(Self.OnSettingsEdit);
  FGlobalCacheConfig.CacheChangeNotifier.Add(FGlobalSettingsListener);
end;

destructor TMapTypeCacheConfigAbstract.Destroy;
begin
  FGlobalCacheConfig.CacheChangeNotifier.Remove(FGlobalSettingsListener);
  FGlobalSettingsListener := nil;

  FConfigChangeNotifier := nil;
  inherited;
end;

function TMapTypeCacheConfigAbstract.GetTileFileName(AXY: TPoint; Azoom: byte): string;
begin
  Result := FBasePath + FFileNameGenerator.GetTileFileName(AXY, Azoom) + FConfig.GetStatic.TileFileExt;
end;

{ TMapTypeCacheConfig }

constructor TMapTypeCacheConfig.Create(
  AConfig: ISimpleTileStorageConfig;
  AGlobalCacheConfig: TGlobalCahceConfig;
  ATileNameGeneratorList: ITileFileNameGeneratorsList
);
begin
  inherited Create(AConfig, AGlobalCacheConfig);
  FTileNameGeneratorList := ATileNameGeneratorList;
  OnSettingsEdit(nil);
end;

procedure TMapTypeCacheConfig.OnSettingsEdit(Sender: TObject);
var
  VCacheType: Byte;
  VBasePath: string;
  VConfig: ISimpleTileStorageConfigStatic;
begin
  VConfig := FConfig.GetStatic;
  VCacheType := VConfig.CacheTypeCode;
  if VCacheType = 0 then begin
    VCacheType := FGlobalCacheConfig.DefCache;
  end;
  FEffectiveCacheType := VCacheType;
  FFileNameGenerator := FTileNameGeneratorList.GetGenerator(FEffectiveCacheType);

  VBasePath := VConfig.NameInCache;
  //TODO: С этим бардаком нужно что-то будет сделать
  if (length(VBasePath) < 2) or ((VBasePath[2] <> '\') and (system.pos(':', VBasePath) = 0)) then begin
    case FEffectiveCacheType of
      1: begin
        VBasePath:=IncludeTrailingPathDelimiter(FGlobalCacheConfig.OldCpath) + VBasePath;
      end;
      2: begin
        VBasePath:=IncludeTrailingPathDelimiter(FGlobalCacheConfig.NewCpath)+VBasePath;
      end;
      3: begin
        VBasePath:=IncludeTrailingPathDelimiter(FGlobalCacheConfig.ESCpath)+VBasePath;
      end;
      4,41: begin
        VBasePath:=IncludeTrailingPathDelimiter(FGlobalCacheConfig.GMTilespath)+VBasePath;
      end;
      5: begin
        VBasePath:=IncludeTrailingPathDelimiter(FGlobalCacheConfig.GECachepath)+VBasePath;
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

{ TMapTypeCacheConfigGE }

constructor TMapTypeCacheConfigGE.Create(
  AConfig: ISimpleTileStorageConfig;
  AGlobalCacheConfig: TGlobalCahceConfig
);
begin
  inherited Create(AConfig, AGlobalCacheConfig);
  OnSettingsEdit(nil);
end;

procedure TMapTypeCacheConfigGE.OnSettingsEdit(Sender: TObject);
var
  VBasePath: string;
begin
  VBasePath:=FGlobalCacheConfig.GECachepath;
  //TODO: С этим бардаком нужно что-то будет сделать
  if (length(VBasePath) < 2) or ((VBasePath[2] <> '\') and (system.pos(':', VBasePath) = 0)) then begin
    VBasePath := IncludeTrailingPathDelimiter(FGlobalCacheConfig.CacheGlobalPath) + VBasePath;
  end;
  VBasePath := IncludeTrailingPathDelimiter(VBasePath);
  FBasePath := VBasePath;
end;

function TMapTypeCacheConfigGE.GetDataFileName: string;
begin
  Result := FBasePath + 'dbCache.dat';
end;

function TMapTypeCacheConfigGE.GetIndexFileName: string;
begin
  Result := FBasePath + 'dbCache.dat.index';
end;

function TMapTypeCacheConfigGE.GetNameInCache: string;
begin
  Result := FConfig.GetStatic.NameInCache;
end;

end.
