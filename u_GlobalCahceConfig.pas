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
  i_JclNotify,
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
    FESCpath: string;
    FGMTilespath: string;
    FGECachepath: string;
    FGCCachepath: string;
    FBDBCachepath: string;
    FDBMSCachepath: string;

    FCacheChangeNotifier: IJclNotifier;
    procedure SetDefCache(const Value: byte);
    procedure SetESCpath(const Value: string);
    procedure SetGECachepath(const Value: string);
    procedure SetBDBCachepath(const Value: string);
    procedure SetDBMSCachepath(const Value: string);
    procedure SetGMTilespath(const Value: string);
    procedure SetNewCPath(const Value: string);
    procedure SetOldCPath(const Value: string);
    function GetCacheGlobalPath: string;
    procedure SetGCCachepath(const Value: string);
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
    property ESCpath: string read FESCpath write SetESCpath;
    property GMTilespath: string read FGMTilespath write SetGMTilespath;
    property GECachepath: string read FGECachepath write SetGECachepath;
    property GCCachepath: string read FGCCachepath write SetGCCachepath;
    property BDBCachepath: string read FBDBCachepath write SetBDBCachepath;
    property DBMSCachepath: string read FDBMSCachepath write SetDBMSCachepath;

    property  CacheGlobalPath: string read GetCacheGlobalPath;
    property CacheChangeNotifier: IJclNotifier read FCacheChangeNotifier;
  end;

const
  c_File_Cache_Id_DEFAULT = 0; // subst only
  c_File_Cache_Id_GMV    = 1;  // old
  c_File_Cache_Id_SAS    = 2;  // new
  c_File_Cache_Id_ES     = 3;
  c_File_Cache_Id_GM     = 4;
  c_File_Cache_Id_GM_Aux = 41; // auxillary
  c_File_Cache_Id_GE     = 5;  // GE cache direct access
  c_File_Cache_Id_BDB    = 6;
  c_File_Cache_Id_DBMS   = 7;
  c_File_Cache_Id_GC     = 8;  // GeoCacher.LOCAL direct access

implementation

uses
  SysUtils,
  u_JclNotify;

{ TGlobalCahceConfig }

constructor TGlobalCahceConfig.Create(
 const  ACacheGlobalPath: IPathConfig
);
begin
  inherited Create;
  FCacheGlobalPath := ACacheGlobalPath;
  FDefCache := c_File_Cache_Id_SAS;
  FCacheChangeNotifier := TJclBaseNotifier.Create;
  FOldCpath := 'cache_old' + PathDelim;
  FNewCpath := 'cache' + PathDelim;
  FESCpath := 'cache_ES' + PathDelim;
  FGMTilesPath := 'cache_gmt' + PathDelim;
  FGECachePath := 'cache_GE' + PathDelim;
  FGCCachePath := 'cache_GC' + PathDelim;
  FBDBCachePath := 'cache_db' + PathDelim;
  FDBMSCachepath := 'cache_sasgis' + PathDelim + 'cache_sasgis'; // it is global DBMS identifier: SERVER\DATABASE
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
    ESCpath := VPathConfig.ReadString('ESC', ESCpath);
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
  VPathConfig.WriteString('ESC', ESCpath);
  VPathConfig.WriteString('GMTiles', GMTilesPath);
  VPathConfig.WriteString('GECache', GECachePath);
  VPathConfig.WriteString('GCCache', GCCachePath);
  VPathConfig.WriteString('BDBCache', BDBCachePath);
  VPathConfig.WriteString('DBMSCache', DBMSCachePath);
end;

procedure TGlobalCahceConfig.SetDBMSCachepath(const Value: string);
begin
  if FDBMSCachepath <> Value then begin
    FDBMSCachepath := Value;
    FCacheChangeNotifier.Notify(nil);
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
      FCacheChangeNotifier.Notify(nil);
    end;
  end;
end;

procedure TGlobalCahceConfig.SetESCpath(const Value: string);
begin
  if FESCpath <> Value then begin
    FESCpath := Value;
    FCacheChangeNotifier.Notify(nil);
  end;
end;

procedure TGlobalCahceConfig.SetGCCachepath(const Value: string);
begin
  if FGCCachepath <> Value then begin
    FGCCachepath := Value;
    FCacheChangeNotifier.Notify(nil);
  end;
end;

procedure TGlobalCahceConfig.SetGECachepath(const Value: string);
begin
  if FGECachepath <> Value then begin
    FGECachepath := Value;
    FCacheChangeNotifier.Notify(nil);
  end;
end;

procedure TGlobalCahceConfig.SetBDBCachepath(const Value: string);
begin
  if FBDBCachepath <> Value then begin
    FBDBCachepath := Value;
    FCacheChangeNotifier.Notify(nil);
  end;
end;

procedure TGlobalCahceConfig.SetGMTilespath(const Value: string);
begin
  if FGMTilespath <> Value then begin
    FGMTilespath := Value;
    FCacheChangeNotifier.Notify(nil);
  end;
end;

procedure TGlobalCahceConfig.SetNewCPath(const Value: string);
begin
  if FNewCPath <> Value then begin
    FNewCPath := Value;
    FCacheChangeNotifier.Notify(nil);
  end;
end;

procedure TGlobalCahceConfig.SetOldCPath(const Value: string);
begin
  if FOldCPath <> Value then begin
    FOldCPath := Value;
    FCacheChangeNotifier.Notify(nil);
  end;
end;

end.
