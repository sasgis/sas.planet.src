{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_TileInfoBasicMemCache;

interface

uses
  Types,
  Windows,
  Classes,
  SysUtils,
  i_MapVersionInfo,
  i_TileInfoBasic,
  i_TileInfoBasicMemCache,
  i_Notifier,
  i_NotifierTime,
  i_ListenerTime,
  i_TileStorage,
  i_InternalPerformanceCounter,
  u_BaseInterfacedObject;

type
  TClearByTTLStrategy = (
    csByOldest   = 0, // delete ALL tiles from the cache if the TTL of the OLDEST tile has expired
    csByYoungest = 1, // remove ALL tiles from the cache if the TTL of the YOUNGEST tile has expired
    csOneByOne   = 2, // delete only those tiles whose TTL has expired
    csNoOne      = 3  // do not delete any tile by TTL (disable TTL cleaner)
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
    FClearByTTLNotifier: INotifierInternal;
    FClearByTTLCounter: IInternalPerformanceCounter;
    FOnTileInfoUpdateNotify: TOnTileInfoUpdateNotify;
    procedure MakeItClean(const ATileRec: PTileInfoCacheRec); inline;
    procedure DoTileInfoUpdateNotify(const ATileRec: PTileInfoCacheRec); inline;
    procedure ClearByTTL;
  private
    { ITileInfoBasicMemCache }
    procedure Add(
      const AXY: TPoint;
      const AZoom: Byte;
      const AVersionInfo: IMapVersionInfo;
      const ATileInfoBasic: ITileInfoBasic
    );

    function Remove(
      const AXY: TPoint;
      const AZoom: Byte;
      const AVersionInfo: IMapVersionInfo
    ): Boolean;

    function Get(
      const AXY: TPoint;
      const AZoom: Byte;
      const AVersionInfo: IMapVersionInfo;
      const AMode: TGetTileInfoMode;
      const AUpdateTTL: Boolean
    ): ITileInfoBasic;

    procedure Clear;

    function GetClearByTTLNotifier: INotifier;

    function GetOnTileInfoUpdate: TOnTileInfoUpdateNotify;
    procedure SetOnTileInfoUpdate(const AValue: TOnTileInfoUpdateNotify);

    function GetEnum(
      const AIgnoreTNE: Boolean;
      const AIgnoreMultiVersionTiles: Boolean
    ): IEnumTileInfo;
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
  u_Notifier,
  u_ListenerTime,
  u_Synchronizer;

type
  ETileInfoBasicMemCache = class(Exception);

  TMemCacheEnum = class(TBaseInterfacedObject, IEnumTileInfo)
  private
    type
      PCacheRec = TTileInfoBasicMemCache.PTileInfoCacheRec;
  private
    FItems: array of TTileInfo;
    FNextIndex: Integer;
  private
    { IEnumTileInfo }
    function Next(var ATileInfo: TTileInfo): Boolean;
  public
    constructor Create(
      const AItems: TList;
      const AIgnoreTNE: Boolean;
      const AIgnoreMultiVersionTiles: Boolean
    );
  end;

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
  FCS := GSync.SyncVariable.Make(Self.ClassName);
  FList := TList.Create;
  FCapacity := ACapacity;
  FTTL := ATTL;
  FClearStrategy := TClearByTTLStrategy(AClearStrategy);
  FOnTileInfoUpdateNotify := nil;

  FAddCounter := APerfCounterList.CreateAndAddNewCounter('Add');
  FRemoveCounter := APerfCounterList.CreateAndAddNewCounter('Remove');
  FGetCounter := APerfCounterList.CreateAndAddNewCounter('Get/Total');
  FHitCounter := APerfCounterList.CreateAndAddNewCounter('Get/Hit');
  FMissCounter := APerfCounterList.CreateAndAddNewCounter('Get/Miss');
  FClearByTTLCounter := APerfCounterList.CreateAndAddNewCounter('ClearByTTL');

  FClearByTTLNotifier := TNotifierBase.Create(GSync.SyncStd.Make(Self.ClassName));

  FTTLCheckNotifier := ATTLCheckNotifier;
  if Assigned(FTTLCheckNotifier) then begin
    FTTLCheckListener := TListenerTTLCheck.Create(ClearByTTL, FTTL);
    FTTLCheckNotifier.Add(FTTLCheckListener);
  end;
end;

destructor TTileInfoBasicMemCache.Destroy;
begin
  if Assigned(FTTLCheckNotifier) and Assigned(FTTLCheckListener) then begin
    FTTLCheckNotifier.Remove(FTTLCheckListener);
    FTTLCheckNotifier := nil;
  end;
  if Assigned(FCS) then begin
    Self.Clear;
  end;
  FreeAndNil(FList);
  FCS := nil;
  FClearByTTLNotifier := nil;
  inherited;
end;

procedure TTileInfoBasicMemCache.DoTileInfoUpdateNotify(const ATileRec: PTileInfoCacheRec);
begin
  if Assigned(FOnTileInfoUpdateNotify) then begin
    FOnTileInfoUpdateNotify(ATileRec.TileXY, ATileRec.TileZoom, ATileRec.TileVersionInfo);
  end;
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
  VVersion: string;
  VTileVersion: string;
  VTile: PTileInfoCacheRec;
  VOldestItem: Integer;
  VEmptyItem: Integer;
  VMinTTL: Cardinal;
  VTickCount: Cardinal;
  VCounterContext: TInternalPerformanceCounterContext;
begin
  FCS.BeginWrite;
  try
    VCounterContext := FAddCounter.StartOperation;
    try
      FTTLCheckListener.CheckUseTimeUpdated;
      VOldestItem := cUndefItemValue;
      VEmptyItem := cUndefItemValue;
      VMinTTL := $FFFFFFFF;
      VTickCount := GetTickCount;
      if Assigned(AVersionInfo) then begin
        VVersion := AVersionInfo.StoreString;
      end else begin
        VVersion := '';
      end;
      for I := 0 to FList.Count - 1 do begin
        VTile := PTileInfoCacheRec(FList.Items[I]);
        if Assigned(VTile.TileVersionInfo) then begin
          VTileVersion := VTile.TileVersionInfo.StoreString;
        end else begin
          VTileVersion := '';
        end;
        if not VTile.IsEmptyCacheRec and
           (VTile.TileXY.X = AXY.X) and
           (VTile.TileXY.Y = AXY.Y) and
           (VTile.TileZoom = AZoom) and
           SameStr(VTileVersion, VVersion)
        then begin
          VTile.TileTTL := VTickCount + FTTL;
          VTile.TileVersionInfo := AVersionInfo;
          VTile.TileInfoBasic := ATileInfoBasic;
          DoTileInfoUpdateNotify(VTile);
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
      VTile.TileTTL := VTickCount + FTTL;
      VTile.TileXY := AXY;
      VTile.TileZoom := AZoom;
      VTile.TileVersionInfo := AVersionInfo;
      VTile.TileInfoBasic := ATileInfoBasic;
      VTile.IsEmptyCacheRec := False;
      DoTileInfoUpdateNotify(VTile);
    finally
      FAddCounter.FinishOperation(VCounterContext);
    end;
  finally
    FCS.EndWrite;
  end;
end;

function TTileInfoBasicMemCache.Remove(
  const AXY: TPoint;
  const AZoom: Byte;
  const AVersionInfo: IMapVersionInfo
): Boolean;
var
  I: Integer;
  VVersion: string;
  VTileVersion: string;
  VTile: PTileInfoCacheRec;
  VCounterContext: TInternalPerformanceCounterContext;
begin
  Result := False;
  FCS.BeginWrite;
  try
    VCounterContext := FRemoveCounter.StartOperation;
    try
      if Assigned(AVersionInfo) then begin
        VVersion := AVersionInfo.StoreString;
      end else begin
        VVersion := '';
      end;
      for I := 0 to FList.Count - 1 do begin
        VTile := PTileInfoCacheRec(FList.Items[I]);
        if VTile.IsEmptyCacheRec then begin
          Continue;
        end;
        if Assigned(VTile.TileVersionInfo) then begin
          VTileVersion := VTile.TileVersionInfo.StoreString;
        end else begin
          VTileVersion := '';
        end;
        if (VTile.TileXY.X = AXY.X) and
           (VTile.TileXY.Y = AXY.Y) and
           (VTile.TileZoom = AZoom) and
           SameStr(VTileVersion, VVersion)
        then begin
          MakeItClean(VTile);
          Result := True;
          Break;
        end;
      end;
      FTTLCheckListener.CheckUseTimeUpdated;
    finally
      FRemoveCounter.FinishOperation(VCounterContext);
    end;
  finally
    FCS.EndWrite;
  end;
end;

function TTileInfoBasicMemCache.Get(
  const AXY: TPoint;
  const AZoom: Byte;
  const AVersionInfo: IMapVersionInfo;
  const AMode: TGetTileInfoMode;
  const AUpdateTTL: Boolean
): ITileInfoBasic;
var
  I: Integer;
  VVersion: string;
  VTileVersion: string;
  VTickCount: Cardinal;
  VTile: PTileInfoCacheRec;
  VCounterContext: TInternalPerformanceCounterContext;
begin
  Result := nil;
  FCS.BeginWrite;
  try
    VCounterContext := FGetCounter.StartOperation;
    try
      VTickCount := GetTickCount;
      if Assigned(AVersionInfo) then begin
        VVersion := AVersionInfo.StoreString;
      end else begin
        VVersion := '';
      end;
      for I := 0 to FList.Count - 1 do begin
        VTile := PTileInfoCacheRec(FList.Items[I]);
        if VTile.IsEmptyCacheRec then begin
          Continue;
        end;
        if Assigned(VTile.TileVersionInfo) then begin
          VTileVersion := VTile.TileVersionInfo.StoreString;
        end else begin
          VTileVersion := '';
        end;
        if (VTile.TileXY.X = AXY.X) and
           (VTile.TileXY.Y = AXY.Y) and
           (VTile.TileZoom = AZoom) and
           SameStr(VTileVersion, VVersion)
        then begin
          if VTile.TileTTL > VTickCount then begin
            if AUpdateTTL then begin
              VTile.TileTTL := VTickCount + FTTL;
            end;
            if AMode = gtimWithData then begin
              if not Supports(VTile.TileInfoBasic, ITileInfoWithData, Result) then begin
                Result := nil;
              end;
            end else begin // gtimWithoutData, gtimAsIs
              Result := VTile.TileInfoBasic;
            end;
          end else if FClearStrategy = csOneByOne then begin
            MakeItClean(VTile);
          end;
          Break;
        end;
      end;
    finally
      FGetCounter.FinishOperation(VCounterContext);
    end;
  finally
    FCS.EndWrite;
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
  VTickCount: Cardinal;
  VCleanerCalled: Boolean;
  VCounterContext: TInternalPerformanceCounterContext;
begin
  if FClearStrategy = csNoOne then begin
    Exit;
  end;
  FCS.BeginWrite;
  try
    VCounterContext := FClearByTTLCounter.StartOperation;
    try
      VCleanerCalled := False;
      VTickCount := GetTickCount;
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
        if ((FClearStrategy = csByOldest) and (VMinTTL < VTickCount)) or
           ((FClearStrategy = csByYoungest) and (VMaxTTL < VTickCount)) then
        begin // clear all records
          VCleanerCalled := True;
          for I := 0 to FList.Count - 1 do begin
            MakeItClean(PTileInfoCacheRec(FList.Items[I]));
          end;
         end;
      end else begin // csOneByOne
        for I := 0 to FList.Count - 1 do begin
          VTile := PTileInfoCacheRec(FList.Items[I]);
          if not VTile.IsEmptyCacheRec and (VTile.TileTTL < VTickCount) then begin
            VCleanerCalled := True;
            MakeItClean(VTile);
          end;
        end;
      end;
      if not VCleanerCalled then begin
        FTTLCheckListener.CheckUseTimeUpdated;
      end;
    finally
      FClearByTTLCounter.FinishOperation(VCounterContext);
      FClearByTTLNotifier.Notify(nil);
    end;
  finally
    FCS.EndWrite;
  end;
end;

procedure TTileInfoBasicMemCache.MakeItClean(const ATileRec: PTileInfoCacheRec);
begin
  if (ATileRec <> nil) and not ATileRec.IsEmptyCacheRec then begin
    DoTileInfoUpdateNotify(ATileRec);

    ATileRec.TileTTL := 0;
    ATileRec.TileXY := Point(MaxInt, MaxInt);
    ATileRec.TileZoom := 255;
    ATileRec.TileVersionInfo := nil;
    ATileRec.TileInfoBasic := nil;
    ATileRec.IsEmptyCacheRec := True;
  end;
end;

function TTileInfoBasicMemCache.GetOnTileInfoUpdate: TOnTileInfoUpdateNotify;
begin
  FCS.BeginRead;
  try
    Result := FOnTileInfoUpdateNotify;
  finally
    FCS.EndRead;
  end;
end;

procedure TTileInfoBasicMemCache.SetOnTileInfoUpdate(const AValue: TOnTileInfoUpdateNotify);
begin
  FCS.BeginWrite;
  try
    FOnTileInfoUpdateNotify := AValue;
  finally
    FCS.EndWrite;
  end;
end;

function TTileInfoBasicMemCache.GetClearByTTLNotifier: INotifier;
begin
  Result := FClearByTTLNotifier as INotifier;
end;

function TTileInfoBasicMemCache.GetEnum(
  const AIgnoreTNE: Boolean;
  const AIgnoreMultiVersionTiles: Boolean
): IEnumTileInfo;
begin
  Result := TMemCacheEnum.Create(FList, AIgnoreTNE, AIgnoreMultiVersionTiles);
end;

{ TMemCacheEnum }

constructor TMemCacheEnum.Create(
  const AItems: TList;
  const AIgnoreTNE: Boolean;
  const AIgnoreMultiVersionTiles: Boolean
);
var
  I, J: Integer;
  VItem: PTileInfo;
  VTile: PCacheRec;
  VInfo: ITileInfoWithData;
begin
  inherited Create;

  J := 0;
  SetLength(FItems, AItems.Count);

  for I := 0 to AItems.Count - 1 do begin

    VTile := AItems.Items[I];
    if
      (VTile = nil) or
      VTile.IsEmptyCacheRec or
      (VTile.TileInfoBasic = nil) or
      (AIgnoreTNE and VTile.TileInfoBasic.IsExistsTNE)
    then begin
      Continue;
    end;

    VItem := @FItems[J];

    VItem.FTile := VTile.TileXY;
    VItem.FZoom := VTile.TileZoom;
    VItem.FLoadDate := VTile.TileInfoBasic.LoadDate;
    VItem.FVersionInfo := VTile.TileVersionInfo;
    VItem.FContentType := VTile.TileInfoBasic.ContentType;
    VItem.FSize := VTile.TileInfoBasic.Size;

    VItem.FInfoType := titUnknown;
    if Supports(VTile.TileInfoBasic, ITileInfoWithData, VInfo) then begin
      VItem.FData := VInfo.TileData;
      VItem.FInfoType := titExists;
    end else begin
      VItem.FData := nil;
      if VTile.TileInfoBasic.IsExistsTNE then begin
        VItem.FInfoType := titTneExists;
      end
    end;

    if VItem.FInfoType in [titExists, titTneExists] then begin
      Inc(J);
    end;
  end;

  SetLength(FItems, J);
  FNextIndex := 0;
end;

function TMemCacheEnum.Next(var ATileInfo: TTileInfo): Boolean;
begin
  Result := FNextIndex < Length(FItems);
  if Result then begin
    ATileInfo := FItems[FNextIndex];
    Inc(FNextIndex);
  end;
end;

end.
