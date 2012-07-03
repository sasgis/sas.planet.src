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

unit u_MemTileCache;

interface

uses
  Types,
  Classes,
  SysUtils,
  i_Notifier, i_Listener,
  i_MainMemCacheConfig,
  i_Bitmap32Static,
  i_MapVersionInfo,
  i_VectorDataItemSimple,
  i_TTLCheckListener,
  i_TTLCheckNotifier,
  i_TileObjCache,
  i_CoordConverter,
  u_TileStorageAbstract;

type
  TMemTileCacheBase = class(TInterfacedObject)
  private
    FConfig: IMainMemCacheConfig;
    FGCList: INotifierTTLCheck;
    FConfigListener: IListener;
    FTileStorage: TTileStorageAbstract;
    FCoordConverter: ICoordConverter;
    FStorageChangeListener: IListener;
    FTTLListener: ITTLCheckListener;

    FCacheList: TStringList;
    FSync: IReadWriteSync;
    procedure OnChangeConfig;
    procedure OnTileStorageChange(const AMsg: IInterface);
    function GetMemCacheKey(
      const AXY: TPoint;
      const Azoom: byte;
      const AMapVersionInfo: IMapVersionInfo
    ): string;
    procedure OnTTLTrim(Sender: TObject);
  protected
    procedure ItemFree(AIndex: Integer);
    procedure IncUpdateCounter;
  protected
    procedure Clear;
    procedure DeleteTileFromCache(
      const AXY: TPoint;
      const AZoom: Byte;
      const AMapVersionInfo: IMapVersionInfo
    );
    procedure AddObjectToCache(
      const AObj: IInterface;
      const AXY: TPoint;
      const AZoom: Byte;
      const AMapVersionInfo: IMapVersionInfo
    );
    function TryLoadObjectFromCache(
      const AXY: TPoint;
      const AZoom: Byte;
      const AMapVersionInfo: IMapVersionInfo
    ): IInterface;
  public
    constructor Create(
      const AGCList: INotifierTTLCheck;
      const ATileStorage: TTileStorageAbstract;
      const ACoordConverter: ICoordConverter;
      const AConfig: IMainMemCacheConfig
    );
    destructor Destroy; override;
  end;

  TMemTileCacheVector = class(TMemTileCacheBase, ITileObjCacheVector)
  protected
    procedure AddTileToCache(
      const AObj: IVectorDataItemList;
      const AXY: TPoint;
      const AZoom: Byte;
      const AMapVersionInfo: IMapVersionInfo
    );
    function TryLoadTileFromCache(
      const AXY: TPoint;
      const AZoom: Byte;
      const AMapVersionInfo: IMapVersionInfo
    ): IVectorDataItemList;
  end;

  TMemTileCacheBitmap = class(TMemTileCacheBase, ITileObjCacheBitmap)
  protected
    procedure AddTileToCache(
      const AObj: IBitmap32Static;
      const AXY: TPoint;
      const AZoom: Byte;
      const AMapVersionInfo: IMapVersionInfo
    );
    function TryLoadTileFromCache(
      const AXY: TPoint;
      const AZoom: Byte;
      const AMapVersionInfo: IMapVersionInfo
    ): IBitmap32Static;
  end;

implementation

uses
  i_TileKey,
  i_TileRectUpdateNotifier,
  u_TTLCheckListener,
  u_Synchronizer,
  u_NotifyEventListener;

{ TTileCacheBase }

constructor TMemTileCacheBase.Create(
  const AGCList: INotifierTTLCheck;
  const ATileStorage: TTileStorageAbstract;
  const ACoordConverter: ICoordConverter;
  const AConfig: IMainMemCacheConfig
);
var
  i: Integer;
  VNotifier: INotifierTileRectUpdate;
begin
  inherited Create;
  FConfig := AConfig;
  FGCList := AGCList;
  FConfigListener := TNotifyNoMmgEventListener.Create(Self.OnChangeConfig);
  FConfig.GetChangeNotifier.Add(FConfigListener);

  if ATileStorage <> nil then begin
    FTileStorage := ATileStorage;
    FCoordConverter := ACoordConverter;
    FStorageChangeListener := TNotifyEventListener.Create(Self.OnTileStorageChange);
    for i := FCoordConverter.MinZoom to FCoordConverter.MaxZoom do begin
      VNotifier := FTileStorage.NotifierByZoom[i];
      if VNotifier <> nil then begin
        VNotifier.Add(FStorageChangeListener, FCoordConverter.TileRectAtZoom(i));
      end;
    end;
  end;

  FCacheList := TStringList.Create;
  FCacheList.Capacity := FConfig.MaxSize;
  FSync := MakeSyncRW_Big(Self, False);
  FTTLListener := TTTLCheckListener.Create(Self.OnTTLTrim, 40000, 1000);
  FGCList.Add(FTTLListener);
end;

destructor TMemTileCacheBase.Destroy;
var
  i: Integer;
  VNotifier: INotifierTileRectUpdate;
begin
  FConfig.GetChangeNotifier.Remove(FConfigListener);
  FConfigListener := nil;
  FConfig := nil;

  FGCList.Remove(FTTLListener);
  FTTLListener := nil;
  FGCList := nil;

  for i := FCoordConverter.MinZoom to FCoordConverter.MaxZoom do begin
    VNotifier := FTileStorage.NotifierByZoom[i];
    if VNotifier <> nil then begin
      VNotifier.Remove(FStorageChangeListener);
    end;
  end;
  FStorageChangeListener := nil;
  FCoordConverter := nil;
  FTileStorage := nil;

  Clear;
  FreeAndNil(FCacheList);
  inherited;
end;

procedure TMemTileCacheBase.AddObjectToCache(
  const AObj: IInterface;
  const AXY: TPoint;
  const AZoom: Byte;
  const AMapVersionInfo: IMapVersionInfo
);
var
  VKey: string;
  i: integer;
begin
  VKey := GetMemCacheKey(AXY, AZoom, AMapVersionInfo);
  FSync.BeginWrite;
  try
    i := FCacheList.IndexOf(VKey);
    if (i < 0) and (FCacheList.Capacity > 0) then begin
      if (FCacheList.Count >= FCacheList.Capacity) and (FCacheList.Count > 0) then begin
        ItemFree(0);
        FCacheList.Delete(0);
      end;
      AObj._AddRef;
      FCacheList.AddObject(VKey, Pointer(AObj));
    end;
  finally
    FSync.EndWrite;
  end;
end;

procedure TMemTileCacheBase.Clear;
var
  i: integer;
begin
  FSync.BeginWrite;
  try
    for i := FCacheList.Count - 1 downto 0 do begin
      ItemFree(i);
      FCacheList.Delete(i);
    end;
  finally
    FSync.EndWrite;
  end;
end;

procedure TMemTileCacheBase.DeleteTileFromCache(
  const AXY: TPoint;
  const AZoom: Byte;
  const AMapVersionInfo: IMapVersionInfo
);
var
  i: Integer;
  VKey: string;
begin
  VKey := GetMemCacheKey(AXY, AZoom, AMapVersionInfo);
  FSync.BeginWrite;
  try
    i := FCacheList.IndexOf(VKey);
    if i >= 0 then begin
      ItemFree(i);
      FCacheList.Delete(i);
    end;
  finally
    FSync.EndWrite;
  end;
end;

function TMemTileCacheBase.GetMemCacheKey(
  const AXY: TPoint;
  const Azoom: byte;
  const AMapVersionInfo: IMapVersionInfo
): string;
var
  VVer: String;
begin
  Result := inttostr(Azoom) + '_' + inttostr(AXY.X) + '_' + inttostr(AXY.Y);
  if Assigned(AMapVersionInfo) then begin
    VVer := AMapVersionInfo.StoreString;
    if (0 < Length(VVer)) then begin
      Result := Result + '_' + VVer;
    end;
  end;
end;

procedure TMemTileCacheBase.IncUpdateCounter;
begin
  FTTLListener.UpdateUseTime;
end;

procedure TMemTileCacheBase.ItemFree(AIndex: Integer);
begin
  IInterface(Pointer(FCacheList.Objects[AIndex]))._Release;
end;

procedure TMemTileCacheBase.OnChangeConfig;
var
  VNewSize: Integer;
  i: Integer;
begin
  VNewSize := FConfig.MaxSize;
  FSync.BeginWrite;
  try
    if VNewSize <> FCacheList.Capacity then begin
      if VNewSize < FCacheList.Count then begin
        for i := 0 to (FCacheList.Count - VNewSize) - 1 do begin
          ItemFree(0);
          FCacheList.Delete(0);
        end;
      end;
      FCacheList.Capacity := VNewSize;
    end;
  finally
    FSync.EndWrite;
  end;
end;

procedure TMemTileCacheBase.OnTileStorageChange(const AMsg: IInterface);
var
  VTileKey: ITileKey;
begin
  if Supports(AMsg, ITileKey, VTileKey) then begin
    DeleteTileFromCache(VTileKey.Tile, VTileKey.Zoom, VTileKey.VersionInfo);
  end;
end;

procedure TMemTileCacheBase.OnTTLTrim(Sender: TObject);
begin
  Clear;
end;

function TMemTileCacheBase.TryLoadObjectFromCache(
  const AXY: TPoint;
  const AZoom: Byte;
  const AMapVersionInfo: IMapVersionInfo
): IInterface;
var
  i: integer;
  VKey: string;
begin
  Result := nil;
  VKey := GetMemCacheKey(AXY, AZoom, AMapVersionInfo);
  FSync.BeginRead;
  try
    i := FCacheList.IndexOf(VKey);
    if i >= 0 then begin
      Result := IInterface(Pointer(FCacheList.Objects[i]));
    end;
  finally
    FSync.EndRead;
  end;
  if Result <> nil then begin
    IncUpdateCounter;
  end;
end;

{ TMemTileCacheVector }

procedure TMemTileCacheVector.AddTileToCache(
  const AObj: IVectorDataItemList;
  const AXY: TPoint;
  const AZoom: Byte;
  const AMapVersionInfo: IMapVersionInfo
);
var
  VKey: string;
  i: integer;
begin
  VKey := GetMemCacheKey(AXY, AZoom, AMapVersionInfo);
  FSync.BeginWrite;
  try
    i := FCacheList.IndexOf(VKey);
    if (i < 0) and (FCacheList.Capacity > 0) then begin
      if (FCacheList.Count >= FCacheList.Capacity) and (FCacheList.Count > 0) then begin
        ItemFree(0);
        FCacheList.Delete(0);
      end;
      AObj._AddRef;
      FCacheList.AddObject(VKey, Pointer(AObj));
    end;
  finally
    FSync.EndWrite;
  end;
end;

function TMemTileCacheVector.TryLoadTileFromCache(
  const AXY: TPoint;
  const AZoom: Byte;
  const AMapVersionInfo: IMapVersionInfo
): IVectorDataItemList;
var
  VObj: IInterface;
begin
  VObj := TryLoadObjectFromCache(AXY, AZoom, AMapVersionInfo);
  if not Supports(VObj, IVectorDataItemList, Result) then begin
    Result := nil;
  end;
end;

{ TMemTileCacheBitmap }

procedure TMemTileCacheBitmap.AddTileToCache(
  const AObj: IBitmap32Static;
  const AXY: TPoint;
  const AZoom: Byte;
  const AMapVersionInfo: IMapVersionInfo
);
begin
  AddObjectToCache(AObj, AXY, AZoom, AMapVersionInfo);
end;

function TMemTileCacheBitmap.TryLoadTileFromCache(
  const AXY: TPoint;
  const AZoom: Byte;
  const AMapVersionInfo: IMapVersionInfo
): IBitmap32Static;
var
  VObj: IInterface;
begin
  VObj := TryLoadObjectFromCache(AXY, AZoom, AMapVersionInfo);
  if not Supports(VObj, IBitmap32Static, Result) then begin
    Result := nil;
  end;
end;

end.


