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

unit u_GlobalCahceConfig;

interface

uses
  i_Notifier,
  i_PathConfig,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider;

type
  TGlobalCahceConfig = class
  private
    FCacheGlobalPath: IPathConfig;

    //Способ храения кэша по-умолчанию.
    FDefCache: byte;

    //Пути к кэшам разных типов
    FNewCPath: string;
    FOldCPath: string;
    FESCPath: string;
    FGMTilesPath: string;
    FGECachePath: string;
    FGCCachePath: string;
    FBDBCachePath: string;
    FDBMSCachePath: string;

    FCacheChangeNotifier: INotifier;
    FCacheChangeNotifierInternal: INotifierInternal;
    procedure SetDefCache(const Value: byte);
    procedure SetESCPath(const Value: string);
    procedure SetGECachePath(const Value: string);
    procedure SetBDBCachePath(const Value: string);
    procedure SetDBMSCachePath(const Value: string);
    procedure SetGMTilesPath(const Value: string);
    procedure SetNewCPath(const Value: string);
    procedure SetOldCPath(const Value: string);
    function GetCacheGlobalPath: string;
    procedure SetGCCachePath(const Value: string);
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
    property NewCPath: string read FNewCPath write SetNewCPath;
    property OldCPath: string read FOldCPath write SetOldCPath;
    property ESCPath: string read FESCPath write SetESCPath;
    property GMTilesPath: string read FGMTilesPath write SetGMTilesPath;
    property GECachePath: string read FGECachePath write SetGECachePath;
    property GCCachePath: string read FGCCachePath write SetGCCachePath;
    property BDBCachePath: string read FBDBCachePath write SetBDBCachePath;
    property DBMSCachePath: string read FDBMSCachePath write SetDBMSCachePath;

    property CacheGlobalPath: string read GetCacheGlobalPath;
    property CacheChangeNotifier: INotifier read FCacheChangeNotifier;
  end;

const
  c_File_Cache_Id_DEFAULT = 0; // subst only
  c_File_Cache_Id_GMV = 1;  // old
  c_File_Cache_Id_SAS = 2;  // new
  c_File_Cache_Id_ES = 3;
  c_File_Cache_Id_GM = 4;
  c_File_Cache_Id_GM_Aux = 41; // auxillary
  c_File_Cache_Id_GM_Bing = 42; // "Bing Maps (Virtual Earth) Tiles"
  c_File_Cache_Id_GE = 5;  // GE cache direct access
  c_File_Cache_Id_BDB = 6;
  c_File_Cache_Id_DBMS = 7;
  c_File_Cache_Id_GC = 8;  // GeoCacher.LOCAL direct access

implementation

uses
  SysUtils,
  u_Notifier;

{ TGlobalCahceConfig }

constructor TGlobalCahceConfig.Create(
  const ACacheGlobalPath: IPathConfig
);
begin
  inherited Create;
  FCacheGlobalPath := ACacheGlobalPath;
  FDefCache := c_File_Cache_Id_SAS;
  FCacheChangeNotifierInternal := TNotifierBase.Create;
  FCacheChangeNotifier := FCacheChangeNotifierInternal;
  FOldCpath := 'cache_old' + PathDelim;
  FNewCpath := 'cache' + PathDelim;
  FESCPath := 'cache_ES' + PathDelim;
  FGMTilesPath := 'cache_gmt' + PathDelim;
  FGECachePath := 'cache_GE' + PathDelim;
  FGCCachePath := 'cache_GC' + PathDelim;
  FBDBCachePath := 'cache_db' + PathDelim;
  FDBMSCachePath := 'cache_sasgis' + PathDelim + 'cache_sasgis'; // it is global DBMS identifier: SERVER\DATABASE
end;

destructor TGlobalCahceConfig.Destroy;
begin
  FCacheChangeNotifier := nil;
  inherited;
end;

function TGlobalCahceConfig.GetCacheGlobalPath: string;
begin
  Result := FCacheGlobalPath.FullPath;
end;

procedure TGlobalCahceConfig.LoadConfig(const AConfigProvider: IConfigDataProvider);
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
    OldCpath := VPathConfig.ReadString('GMVC', OldCpath);
    NewCpath := VPathConfig.ReadString('SASC', NewCpath);
    ESCPath := VPathConfig.ReadString('ESC', ESCPath);
    GMTilesPath := VPathConfig.ReadString('GMTiles', GMTilesPath);
    GECachePath := VPathConfig.ReadString('GECache', GECachePath);
    GCCachePath := VPathConfig.ReadString('GCCache', GCCachePath);
    BDBCachePath := VPathConfig.ReadString('BDBCache', BDBCachePath);
    DBMSCachePath := VPathConfig.ReadString('DBMSCache', DBMSCachePath);
  end;
end;

procedure TGlobalCahceConfig.SaveConfig(
  const AConfigProvider: IConfigDataWriteProvider
);
var
  VViewConfig: IConfigDataWriteProvider;
  VPathConfig: IConfigDataWriteProvider;
begin
  VViewConfig := AConfigProvider.GetOrCreateSubItem('VIEW');
  VPathConfig := AConfigProvider.GetOrCreateSubItem('PATHtoCACHE');
  VViewConfig.WriteInteger('DefCache', FDefCache);

  VPathConfig.WriteString('GMVC', OldCpath);
  VPathConfig.WriteString('SASC', NewCpath);
  VPathConfig.WriteString('ESC', ESCPath);
  VPathConfig.WriteString('GMTiles', GMTilesPath);
  VPathConfig.WriteString('GECache', GECachePath);
  VPathConfig.WriteString('GCCache', GCCachePath);
  VPathConfig.WriteString('BDBCache', BDBCachePath);
  VPathConfig.WriteString('DBMSCache', DBMSCachePath);
end;

procedure TGlobalCahceConfig.SetDBMSCachePath(const Value: string);
begin
  if FDBMSCachePath <> Value then begin
    FDBMSCachePath := Value;
    FCacheChangeNotifierInternal.Notify(nil);
  end;
end;

procedure TGlobalCahceConfig.SetDefCache(const Value: byte);
begin
  if Value in [c_File_Cache_Id_GMV,
    c_File_Cache_Id_SAS,
    c_File_Cache_Id_ES,
    c_File_Cache_Id_GM,
    c_File_Cache_Id_GM_Aux,
    c_File_Cache_Id_BDB] then begin
    if FDefCache <> Value then begin
      FDefCache := Value;
      FCacheChangeNotifierInternal.Notify(nil);
    end;
  end;
end;

procedure TGlobalCahceConfig.SetESCPath(const Value: string);
begin
  if FESCPath <> Value then begin
    FESCPath := Value;
    FCacheChangeNotifierInternal.Notify(nil);
  end;
end;

procedure TGlobalCahceConfig.SetGCCachePath(const Value: string);
begin
  if FGCCachePath <> Value then begin
    FGCCachePath := Value;
    FCacheChangeNotifierInternal.Notify(nil);
  end;
end;

procedure TGlobalCahceConfig.SetGECachePath(const Value: string);
begin
  if FGECachePath <> Value then begin
    FGECachePath := Value;
    FCacheChangeNotifierInternal.Notify(nil);
  end;
end;

procedure TGlobalCahceConfig.SetBDBCachePath(const Value: string);
begin
  if FBDBCachePath <> Value then begin
    FBDBCachePath := Value;
    FCacheChangeNotifierInternal.Notify(nil);
  end;
end;

procedure TGlobalCahceConfig.SetGMTilesPath(const Value: string);
begin
  if FGMTilesPath <> Value then begin
    FGMTilesPath := Value;
    FCacheChangeNotifierInternal.Notify(nil);
  end;
end;

procedure TGlobalCahceConfig.SetNewCPath(const Value: string);
begin
  if FNewCPath <> Value then begin
    FNewCPath := Value;
    FCacheChangeNotifierInternal.Notify(nil);
  end;
end;

procedure TGlobalCahceConfig.SetOldCPath(const Value: string);
begin
  if FOldCPath <> Value then begin
    FOldCPath := Value;
    FCacheChangeNotifierInternal.Notify(nil);
  end;
end;

end.
