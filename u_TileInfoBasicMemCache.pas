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

unit u_TileInfoBasicMemCache;

interface

uses
  Windows,
  Classes,
  SysUtils,
  i_MapVersionInfo,
  i_TileInfoBasic,
  i_TileInfoBasicMemCache,
  i_NotifierTime,
  i_ListenerTime,
  i_InternalPerformanceCounter,
  u_BaseInterfacedObject;

type
  TClearByTTLStrategy = (
    csByOldest   = 0, // удалять ВСЕ тайлы из кэша, если истёк TTL у самого СТАРОГО тайла
    csByYoungest = 1, // удалять ВСЕ тайлы из кэша, если истёк TTL у самого МОЛОДОГО тайла
    csOneByOne   = 2  // удалять только те тайлы, у которых истёк TTL
  );

  TTileInfoBasicMemCache = class(TBaseInterfacedObject, ITileInfoBasicMemCache)
  private
    type
      TTileInfoCacheRec = record
        TileTTL: Cardinal;
        TileXY: TPoint;
        TileZoom: Byte;
        TileVersionInfo: IMapVersionInfo;
        TileInfoBasic: ITileInfoBasic;
        IsEmptyCacheRec: Boolean;
      end;
      PTileInfoCacheRec = ^TTileInfoCacheRec;
  private
    FList: TList;
    FCapacity: Integer;
    FTTL: Cardinal;
    FCS: IReadWriteSync;
    FClearStrategy: TClearByTTLStrategy;
    FTTLCheckNotifier: INotifierTime;
    FTTLCheckListener: IListenerTimeWithUsedFlag;
    FAddCounter: IInternalPerformanceCounter;
    FRemoveCounter: IInternalPerformanceCounter;
    FGetCounter: IInternalPerformanceCounter;
    FHitCounter: IInternalPerformanceCounter;
    FMissCounter: IInternalPerformanceCounter;
    FClearByTTLCounter: IInternalPerformanceCounter;
    procedure MakeItClean(const ATileRec: PTileInfoCacheRec); inline;
    procedure ClearByTTL;
  private
    { ITileInfoBasicMemCache }
    procedure Add(
      const AXY: TPoint;
      const AZoom: Byte;
      const AVersionInfo: IMapVersionInfo;
      const ATileInfoBasic: ITileInfoBasic
    );
    procedure Remove(
      const AXY: TPoint;
      const AZoom: Byte
    );
    function Get(
      const AXY: TPoint;
      const AZoom: Byte;
      const AUpdateTTL: Boolean
    ): ITileInfoBasic;
    procedure Clear;
  public
    constructor Create(
      const ACapacity: Integer;
      const ATTL: Cardinal;
      const AClearStrategy: Integer;
      const ATTLCheckNotifier: INotifierTime;
      const APerfCounterList: IInternalPerformanceCounterList
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_ListenerTime,
  u_Synchronizer;

type
  ETileInfoBasicMemCache = class(Exception);

{ TTileInfoBasicMemCache }

constructor TTileInfoBasicMemCache.Create(
  const ACapacity: Integer;
  const ATTL: Cardinal;
  const AClearStrategy: Integer;
  const ATTLCheckNotifier: INotifierTime;
  const APerfCounterList: IInternalPerformanceCounterList
);
begin
  inherited Create;
  FCS := MakeSyncRW_Var(Self, False);
  FList := TList.Create;
  FCapacity := ACapacity;
  FTTL := ATTL;
  FClearStrategy := TClearByTTLStrategy(AClearStrategy);

  FAddCounter := APerfCounterList.CreateAndAddNewCounter('Add');
  FRemoveCounter := APerfCounterList.CreateAndAddNewCounter('Remove');
  FGetCounter := APerfCounterList.CreateAndAddNewCounter('Get/Total');
  FHitCounter := APerfCounterList.CreateAndAddNewCounter('Get/Hit');
  FMissCounter := APerfCounterList.CreateAndAddNewCounter('Get/Miss');
  FClearByTTLCounter := APerfCounterList.CreateAndAddNewCounter('ClearByTTL');

  FTTLCheckNotifier := ATTLCheckNotifier;
  if Assigned(FTTLCheckNotifier) then begin
    FTTLCheckListener := TListenerTTLCheck.Create(ClearByTTL, FTTL);
    FTTLCheckNotifier.Add(FTTLCheckListener);
  end;
end;

destructor TTileInfoBasicMemCache.Destroy;
begin
  if Assigned(FTTLCheckNotifier) then begin
    FTTLCheckNotifier.Remove(FTTLCheckListener);
    FTTLCheckNotifier := nil;
  end;
  Self.Clear;
  FreeAndNil(FList);
  FCS := nil;
  inherited Destroy;
end;

procedure TTileInfoBasicMemCache.Add(
  const AXY: TPoint;
  const AZoom: Byte;
  const AVersionInfo: IMapVersionInfo;
  const ATileInfoBasic: ITileInfoBasic
);
const
  cUndefItemValue = -1;
var
  I: Integer;
  VTile: PTileInfoCacheRec;
  VOldestItem: Integer;
  VEmptyItem: Integer;
  VMinTTL: Cardinal;
  VCounterContext: TInternalPerformanceCounterContext;
begin
  VCounterContext := FAddCounter.StartOperation;
  try
    FCS.BeginWrite;
    try
      FTTLCheckListener.CheckUseTimeUpdated;
      VOldestItem := cUndefItemValue;
      VEmptyItem := cUndefItemValue;
      VMinTTL := $FFFFFFFF;
      for I := 0 to FList.Count - 1 do begin
        VTile := PTileInfoCacheRec(FList.Items[I]);
        if (VTile.TileXY.X = AXY.X) and
           (VTile.TileXY.Y = AXY.Y) and
           (VTile.TileZoom = AZoom) and
           (not VTile.IsEmptyCacheRec)
        then begin
          VTile.TileTTL := GetTickCount + FTTL;
          VTile.TileVersionInfo := AVersionInfo;
          VTile.TileInfoBasic := ATileInfoBasic;
          Exit; // OK - found and update tile info rec
        end else begin
          if VTile.IsEmptyCacheRec then begin
            VEmptyItem := I;
          end else if (VTile.TileTTL < VMinTTL) then begin
            VOldestItem := I;
            VMinTTL := VTile.TileTTL;
          end;
        end;
      end;
      if (VEmptyItem <> cUndefItemValue) then begin
        VTile := PTileInfoCacheRec(FList.Items[VEmptyItem]);
      end else if (FCapacity > FList.Count) then begin
        New(VTile);
        FList.Add(VTile);
      end else if (VOldestItem <> cUndefItemValue) then begin
        VTile := PTileInfoCacheRec(FList.Items[VOldestItem]);
        MakeItClean(VTile);
      end else begin
        raise ETileInfoBasicMemCache.Create('Can''t add TileInfo to MemCache!');
      end;
      VTile.TileTTL := GetTickCount + FTTL;
      VTile.TileXY := AXY;
      VTile.TileZoom := AZoom;
      VTile.TileVersionInfo := AVersionInfo;
      VTile.TileInfoBasic := ATileInfoBasic;
      VTile.IsEmptyCacheRec := False;
    finally
      FCS.EndWrite;
    end;
  finally
    FAddCounter.FinishOperation(VCounterContext);
  end;
end;

procedure TTileInfoBasicMemCache.Remove(
  const AXY: TPoint;
  const AZoom: Byte
);
var
  I: Integer;
  VTile: PTileInfoCacheRec;
  VCounterContext: TInternalPerformanceCounterContext;
begin
  VCounterContext := FRemoveCounter.StartOperation;
  try
    FCS.BeginWrite;
    try
      for I := 0 to FList.Count - 1 do begin
        VTile := PTileInfoCacheRec(FList.Items[I]);
        if (VTile.TileXY.X = AXY.X) and
           (VTile.TileXY.Y = AXY.Y) and
           (VTile.TileZoom = AZoom)
        then begin
          MakeItClean(VTile);
          Break;
        end;
      end;
      FTTLCheckListener.CheckUseTimeUpdated;
    finally
      FCS.EndWrite;
    end;
  finally
    FRemoveCounter.FinishOperation(VCounterContext);
  end;
end;

function TTileInfoBasicMemCache.Get(
  const AXY: TPoint;
  const AZoom: Byte;
  const AUpdateTTL: Boolean
): ITileInfoBasic;
var
  I: Integer;
  VTile: PTileInfoCacheRec;
  VCounterContext: TInternalPerformanceCounterContext;
begin
  VCounterContext := FGetCounter.StartOperation;
  try
    FCS.BeginWrite;
    try
      Result := nil;
      for I := 0 to FList.Count - 1 do begin
        VTile := PTileInfoCacheRec(FList.Items[I]);
        if (VTile.TileXY.X = AXY.X) and
           (VTile.TileXY.Y = AXY.Y) and
           (VTile.TileZoom = AZoom) and
           (not VTile.IsEmptyCacheRec)
        then begin
          if (VTile.TileTTL < GetTickCount) then begin
            if FClearStrategy = csOneByOne then begin
              MakeItClean(VTile);
            end;
          end else begin
            if AUpdateTTL then begin
              VTile.TileTTL := GetTickCount + FTTL;
            end;
            Result := VTile.TileInfoBasic;
          end;
          Break;
        end;
      end;
    finally
      FCS.EndWrite;
    end;
  finally
    FGetCounter.FinishOperation(VCounterContext);
  end;
  if Assigned(Result) then begin
    VCounterContext := FHitCounter.StartOperation;
    FHitCounter.FinishOperation(VCounterContext);
  end else begin
    VCounterContext := FMissCounter.StartOperation;
    FMissCounter.FinishOperation(VCounterContext);
  end;
end;

procedure TTileInfoBasicMemCache.Clear;
var
  I: Integer;
  VTile: PTileInfoCacheRec;
begin
  FCS.BeginWrite;
  try
    for I := 0 to FList.Count - 1 do begin
      VTile := PTileInfoCacheRec(FList.Items[I]);
      MakeItClean(VTile);
      Dispose(VTile);
    end;
    FList.Clear;
  finally
    FCS.EndWrite;
  end;
end;

procedure TTileInfoBasicMemCache.ClearByTTL;
var
  I: Integer;
  VTile: PTileInfoCacheRec;
  VMinTTL: Cardinal;
  VMaxTTL: Cardinal;
  VCleanerCalled: Boolean;
  VCounterContext: TInternalPerformanceCounterContext;
begin
  FCS.BeginWrite;
  try
    VCleanerCalled := False;
    if FClearStrategy in [csByOldest, csByYoungest] then begin
      VMinTTL := $FFFFFFFF;
      VMaxTTL := 0;
      for I := 0 to FList.Count - 1 do begin
        VTile := PTileInfoCacheRec(FList.Items[I]);
        if not VTile.IsEmptyCacheRec then begin
          if VTile.TileTTL < VMinTTL then begin
            VMinTTL := VTile.TileTTL; // oldest item
          end;
          if VTile.TileTTL > VMaxTTL then begin
            VMaxTTL := VTile.TileTTL; // youngest item
          end;
        end;
      end;
      if ((FClearStrategy = csByOldest) and (VMinTTL < GetTickCount)) or
         ((FClearStrategy = csByYoungest) and (VMaxTTL < GetTickCount)) then
      begin // clear all records
        VCleanerCalled := True;
        VCounterContext := FClearByTTLCounter.StartOperation;
        try
          for I := 0 to FList.Count - 1 do begin
            MakeItClean(PTileInfoCacheRec(FList.Items[I]));
          end;
        finally
          FClearByTTLCounter.FinishOperation(VCounterContext);
        end;
      end;
    end else begin // csOneByOne
      for I := 0 to FList.Count - 1 do begin
        VTile := PTileInfoCacheRec(FList.Items[I]);
        if not VTile.IsEmptyCacheRec and (VTile.TileTTL < GetTickCount) then begin
          VCleanerCalled := True;
          MakeItClean(VTile);
        end;
      end;
      if VCleanerCalled then begin
        VCounterContext := FClearByTTLCounter.StartOperation;
        FClearByTTLCounter.FinishOperation(VCounterContext);
      end;
    end;
    if not VCleanerCalled then begin
      FTTLCheckListener.CheckUseTimeUpdated;
    end;
  finally
    FCS.EndWrite;
  end;
end;

procedure TTileInfoBasicMemCache.MakeItClean(const ATileRec: PTileInfoCacheRec);
begin
  if (ATileRec <> nil) then begin
    ATileRec.TileTTL := 0;
    ATileRec.TileXY := Point(MaxInt, MaxInt);
    ATileRec.TileZoom := 255;
    ATileRec.TileVersionInfo := nil;
    ATileRec.TileInfoBasic := nil;
    ATileRec.IsEmptyCacheRec := True;
  end;
end;

end.
