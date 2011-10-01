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

unit u_MemFileCache;

interface

uses
  Windows,
  SysUtils,
  Classes,
  GR32,
  i_JclNotify,
  i_InternalPerformanceCounter,
  i_MemObjCache,
  i_MainMemCacheConfig,
  i_VectorDataItemSimple;

type
  TMemFileCacheBase = class(TInterfacedObject)
  private
    FConfig: IMainMemCacheConfig;
    FConfigListener: IJclListener;
    FCacheList: TStringList;
    FSync: TMultiReadExclusiveWriteSynchronizer;
    FTryLoadCounter: IInternalPerformanceCounter;
    FAddCounter: IInternalPerformanceCounter;
    FDeleteCounter: IInternalPerformanceCounter;
    FClearCounter: IInternalPerformanceCounter;
    procedure OnChangeConfig(Sender: TObject);
    procedure ItemFree(AIndex: Integer); virtual; abstract;
  protected
    procedure Clear;
    procedure DeleteFileFromCache(path: string);
  public
    constructor Create(
      AConfig: IMainMemCacheConfig;
      APerfCounterList: IInternalPerformanceCounterList
    );
    destructor Destroy; override;
  end;

  TMemFileCacheVector = class(TMemFileCacheBase, IMemObjCacheVector)
  private
    procedure ItemFree(AIndex: Integer); override;
  protected
    procedure AddTileToCache(btm: IVectorDataItemList; APath: string);
    function TryLoadFileFromCache(var btm: IVectorDataItemList; APath: string): boolean;
  end;

  TMemFileCacheBitmap = class(TMemFileCacheBase, IMemObjCacheBitmap)
  private
    procedure ItemFree(AIndex: Integer); override;
  protected
    procedure AddTileToCache(btm: TCustomBitmap32; APath: string);
    function TryLoadFileFromCache(btm: TCustomBitmap32; APath: string): boolean;
  end;


implementation

uses
  u_NotifyEventListener;

{ TMemFileCacheBase }

constructor TMemFileCacheBase.Create(
  AConfig: IMainMemCacheConfig;
  APerfCounterList: IInternalPerformanceCounterList
);
begin
  FConfig := AConfig;
  FConfigListener := TNotifyEventListener.Create(Self.OnChangeConfig);
  FConfig.GetChangeNotifier.Add(FConfigListener);

  FCacheList := TStringList.Create;
  FCacheList.Capacity := FConfig.MaxSize;
  FSync := TMultiReadExclusiveWriteSynchronizer.Create;

  FTryLoadCounter := APerfCounterList.CreateAndAddNewCounter('TryLoad');
  FAddCounter := APerfCounterList.CreateAndAddNewCounter('Add');
  FDeleteCounter := APerfCounterList.CreateAndAddNewCounter('Delete');
  FClearCounter := APerfCounterList.CreateAndAddNewCounter('Clear');
end;

destructor TMemFileCacheBase.Destroy;
begin
  FConfig.GetChangeNotifier.Remove(FConfigListener);
  FConfigListener := nil;
  FConfig := nil;

  Clear;
  FreeAndNil(FSync);
  FreeAndNil(FCacheList);
  inherited;
end;

procedure TMemFileCacheBase.Clear;
var
  i: integer;
  VCounterContext: TInternalPerformanceCounterContext;
begin
  VCounterContext := FClearCounter.StartOperation;
  try
    FSync.BeginWrite;
    try
      for i := 0 to FCacheList.Count - 1 do begin
        ItemFree(i);
      end;
      FCacheList.Clear;
    finally
      FSync.EndWrite;
    end;
  finally
    FClearCounter.FinishOperation(VCounterContext);
  end;
end;

procedure TMemFileCacheBase.DeleteFileFromCache(path: string);
var
  i: Integer;
  VCounterContext: TInternalPerformanceCounterContext;
begin
  VCounterContext := FDeleteCounter.StartOperation;
  try
    FSync.BeginWrite;
    try
      i := FCacheList.IndexOf(AnsiUpperCase(Path));
      if i >= 0 then begin
        ItemFree(i);
        FCacheList.Delete(i);
      end;
    finally
      FSync.EndWrite;
    end;
  finally
    FDeleteCounter.FinishOperation(VCounterContext);
  end;
end;

procedure TMemFileCacheBase.OnChangeConfig(Sender: TObject);
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

{ TMemFileCache }

procedure TMemFileCacheVector.AddTileToCache(btm: IVectorDataItemList; APath: string);
var
  VPath: string;
  i: integer;
  VCounterContext: TInternalPerformanceCounterContext;
begin
  VCounterContext := FAddCounter.StartOperation;
  try
    VPath := AnsiUpperCase(APath);
    FSync.BeginWrite;
    try
      i := FCacheList.IndexOf(APath);
      if (i < 0)and(FCacheList.Capacity>0) then begin
        if (FCacheList.Count >= FCacheList.Capacity)and(FCacheList.Count>0) then begin
          ItemFree(0);
          FCacheList.Delete(0);
        end;
        btm._AddRef;
        FCacheList.AddObject(APath, Pointer(btm));
      end;
    finally
      FSync.EndWrite;
    end;
  finally
    FAddCounter.FinishOperation(VCounterContext);
  end;
end;

procedure TMemFileCacheVector.ItemFree(AIndex: Integer);
begin
  IInterface(Pointer(FCacheList.Objects[AIndex]))._Release;
end;

function TMemFileCacheVector.TryLoadFileFromCache(var btm: IVectorDataItemList;
  APath: string): boolean;
var
  i: integer;
  VPath: string;
  VCounterContext: TInternalPerformanceCounterContext;
begin
  Result := false;
  VCounterContext := FTryLoadCounter.StartOperation;
  try
    VPath := AnsiUpperCase(APath);
    FSync.BeginRead;
    try
      i := FCacheList.IndexOf(VPath);
      if i >= 0 then begin
        btm := IVectorDataItemList(Pointer(FCacheList.Objects[i]));
        result := true;
      end;
    finally
      FSync.EndRead;
    end;
  finally
    FTryLoadCounter.FinishOperation(VCounterContext);
  end;
end;

{ TMemFileCacheBitmap }

procedure TMemFileCacheBitmap.AddTileToCache(btm: TCustomBitmap32;
  APath: string);
var
  btmcache: TCustomBitmap32;
  VPath: string;
  i: integer;
  VCounterContext: TInternalPerformanceCounterContext;
begin
  VCounterContext := FAddCounter.StartOperation;
  try
    VPath := AnsiUpperCase(APath);
    FSync.BeginWrite;
    try
      i := FCacheList.IndexOf(APath);
      if (i < 0)and(FCacheList.Capacity>0) then begin
        if (FCacheList.Count >= FCacheList.Capacity)and(FCacheList.Count>0) then begin
          ItemFree(0);
          FCacheList.Delete(0);
        end;
        btmcache := TCustomBitmap32.Create;
        btmcache.Assign(btm);
        FCacheList.AddObject(APath, btmcache);
      end;
    finally
      FSync.EndWrite;
    end;
  finally
    FAddCounter.FinishOperation(VCounterContext);
  end;
end;

procedure TMemFileCacheBitmap.ItemFree(AIndex: Integer);
begin
  FCacheList.Objects[AIndex].Free;
end;

function TMemFileCacheBitmap.TryLoadFileFromCache(btm: TCustomBitmap32;
  APath: string): boolean;
var
  i: integer;
  VPath: string;
  VCounterContext: TInternalPerformanceCounterContext;
begin
  Result := false;
  VCounterContext := FTryLoadCounter.StartOperation;
  try
    VPath := AnsiUpperCase(APath);
    FSync.BeginRead;
    try
      i := FCacheList.IndexOf(VPath);
      if i >= 0 then begin
        btm.Assign(TCustomBitmap32(FCacheList.Objects[i]));
        result := true;
      end;
    finally
      FSync.EndRead;
    end;
  finally
    FTryLoadCounter.FinishOperation(VCounterContext);
  end;
end;

end.
