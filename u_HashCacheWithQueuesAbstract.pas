{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
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
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

unit u_HashCacheWithQueuesAbstract;

interface

uses
  SysUtils,
  t_Hash,
  u_BaseInterfacedObject;

type
  TQueueType = (qtEmpty = 0, qtMulti = 1, qtFirstIn = 2, qtFirstOut = 3, qtUnknown = 4);

  TItemIndex = Word;
  THashIndex = Integer;
  TArrayOfHashIndex = array of THashIndex;

  PCacheItem = ^TCacheItem;

  TCacheItem = record
    Key: THashValue;
    Value: IInterface;
    NextIndex: TItemIndex;
    PrevIndex: TItemIndex;
    CollisionNextIndex: TItemIndex;
    QueueType: TQueueType;
  end;

  PCacheItemArray = ^TCacheItemArray;
  TCacheItemArray = array [0..0] of TCacheItem;

  TArrayOfCacheItem = array of TCacheItem;

  TItemsArray = class
  private
    FItemsCount: Integer;
    FItems: TArrayOfCacheItem;
  public
    property ItemsCount: Integer read FItemsCount;
    function GetItemByIndex(AIndex: TItemIndex): PCacheItem;
  public
    constructor Create(AItemsCount: Integer);
  end;

  TStack = class
  private
    FItems: TItemsArray;
    FItemsCount: Integer;
    FCount: Integer;
    FHeadIndex: TItemIndex;
  public
    property Count: Integer read FCount;

    procedure Push(
      AIndex: TItemIndex;
      AItem: PCacheItem
    );
    function Pop(out AIndex: TItemIndex): PCacheItem;
  public
    constructor Create(
      AItems: TItemsArray
    );
  end;

  TQueue = class
  private
    FQueueType: TQueueType;
    FItems: TItemsArray;
    FItemsCount: Integer;

    FCount: Integer;
    FHeadIndex: TItemIndex;
    FTailIndex: TItemIndex;
  public
    property Count: Integer read FCount;
    procedure MoveItemToHead(
      AIndex: TItemIndex;
      AItem: PCacheItem
    );
    procedure PushItemToHead(
      AIndex: TItemIndex;
      AItem: PCacheItem
    );
    function PopItemFromTail(out AIndex: TItemIndex): PCacheItem;
    procedure ExcludeItem(
      AIndex: TItemIndex;
      AItem: PCacheItem
    );
  public
    constructor Create(
      AQueueType: TQueueType;
      AItems: TItemsArray
    );
  end;

  THashTable = class
  private
    FHash: TArrayOfHashIndex;
    FHashSize: Integer;
    FItems: TItemsArray;
    FItemsCount: Integer;
  public
    function GetItem(
      AHashIndex: THashIndex;
      const AKey: THashValue;
      out AIndex: TItemIndex
    ): PCacheItem;
    procedure RemoveItem(
      AHashIndex: THashIndex;
      AIndex: TItemIndex;
      AItem: PCacheItem
    );
    procedure AddItem(
      AHashIndex: THashIndex;
      AIndex: TItemIndex;
      AItem: PCacheItem
    );
  public
    constructor Create(
      AHashSize: Integer;
      AItems: TItemsArray
    );
  end;

  THashCacheWithQueuesAbstract = class(TBaseInterfacedObject)
  private
    FCS: IReadWriteSync;
    FItems: TItemsArray;
    FItemsCount: Integer;
    FHash: THashTable;
    FHashSize: Integer;
    FHashMask: THashValue;

    // Стек для неиспользуемых элементов кэша
    FFreeItems: TStack;

    // Очередь для долгоживущих, часто используемых объектов
    FQueueMultiMaxCount: Integer;
    FQueueMulti: TQueue;

    // В эту очередь объект попадает первый раз встретившийся объект
    FQueueFirstInMaxCount: Integer;
    FQueueFirstIn: TQueue;

    // Сюда объекты вытесняются из очереди QueueFirstIn
    // Оособенность этой очереди, что сами объекты в ней уже не хранятся
    // и их придется создавать заново если понадобятся. Из этой очереди
    // они попадают в разряд долгоживущих и помещаются в очередь QueueMulti
    FQueueFirstOutMaxCount: Integer;
    FQueueFirstOut: TQueue;
    procedure MoveItemFromFirstInToFirstOut;
    procedure FreeItemFromQueueMulti;
  protected
    function CreateByKey(
      const AKey: THashValue;
      AData: Pointer
    ): IInterface; virtual; abstract;
    function GetIndexByKey(const AKey: THashValue): THashIndex; virtual;
    function GetOrCreateItem(
      const AKey: THashValue;
      AData: Pointer
    ): IInterface;
    procedure DeleteItem(const AKey: THashValue);
    procedure Clear;
  public
    constructor Create(
      AHashSizeInBit: Byte;
      AFirstUseCount: Integer;
      AMultiUseCount: Integer;
      AFirstOutCount: Integer
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_Synchronizer;

{ TItemsArray }

constructor TItemsArray.Create(AItemsCount: Integer);
begin
  inherited Create;
  FItemsCount := AItemsCount;
  SetLength(FItems, FItemsCount);
end;

function TItemsArray.GetItemByIndex(AIndex: TItemIndex): PCacheItem;
begin
  if AIndex < FItemsCount then begin
    Result := @FItems[AIndex];
  end else begin
    Result := nil;
  end;
end;

{ TStack }

constructor TStack.Create(AItems: TItemsArray);
begin
  inherited Create;
  FItems := AItems;
  FItemsCount := FItems.ItemsCount;
  FCount := 0;
  FHeadIndex := FItemsCount;
end;

function TStack.Pop(out AIndex: TItemIndex): PCacheItem;
begin
  if FCount > 0 then begin
    AIndex := FHeadIndex;
    Assert(AIndex < FItemsCount);
    Result := FItems.GetItemByIndex(AIndex);
    Assert(Result <> nil);
    FHeadIndex := Result.CollisionNextIndex;
    Dec(FCount);
    Result.CollisionNextIndex := FItemsCount;
  end else begin
    Result := nil;
  end;
end;

procedure TStack.Push(
  AIndex: TItemIndex;
  AItem: PCacheItem
);
begin
  Assert(AIndex < FItemsCount);
  Assert(AItem <> nil);
  Assert(FItems.GetItemByIndex(AIndex) = AItem);
  AItem.Key := 0;
  AItem.Value := nil;
  AItem.QueueType := qtEmpty;
  AItem.NextIndex := FItemsCount;
  AItem.PrevIndex := FItemsCount;
  AItem.CollisionNextIndex := FHeadIndex;
  FHeadIndex := AIndex;
  Inc(FCount);
end;

{ TQueue }

constructor TQueue.Create(
  AQueueType: TQueueType;
  AItems: TItemsArray
);
begin
  inherited Create;
  FQueueType := AQueueType;
  FItems := AItems;
  FItemsCount := FItems.FItemsCount;
  FCount := 0;
  FHeadIndex := FItemsCount;
  FTailIndex := FItemsCount;
end;

procedure TQueue.ExcludeItem(
  AIndex: TItemIndex;
  AItem: PCacheItem
);
var
  VPrevItem: PCacheItem;
  VPrevIndex: TItemIndex;
  VNextItem: PCacheItem;
  VNextIndex: TItemIndex;
begin
  Assert(AIndex < FItemsCount);
  Assert(AItem <> nil);
  Assert(FItems.GetItemByIndex(AIndex) = AItem);
  Assert(AItem.QueueType = FQueueType);
  Assert(FCount > 0);
  VPrevIndex := AItem.PrevIndex;
  if AItem.PrevIndex < FItemsCount then begin
    Assert(FHeadIndex <> AIndex);
    VPrevItem := FItems.GetItemByIndex(VPrevIndex);
    Assert(VPrevItem <> nil);
    Assert(VPrevItem.QueueType = FQueueType);
    Assert(VPrevItem.NextIndex = AIndex);
    if VPrevItem <> nil then begin
      VNextIndex := AItem.NextIndex;
      if VNextIndex < FItemsCount then begin
        VNextItem := FItems.GetItemByIndex(VNextIndex);
        Assert(VNextItem <> nil);
        Assert(VNextItem.QueueType = FQueueType);
        Assert(VNextItem.PrevIndex = AIndex);
        if VNextItem <> nil then begin
          VPrevItem.NextIndex := VNextIndex;
          VNextItem.PrevIndex := VPrevIndex;
          Dec(FCount);
          AItem.PrevIndex := FItemsCount;
          AItem.NextIndex := FItemsCount;
          AItem.QueueType := qtUnknown;
        end;
      end else begin
        Assert(FTailIndex = AIndex);
        VPrevItem.NextIndex := FItemsCount;
        FTailIndex := VPrevIndex;
        Dec(FCount);
        AItem.PrevIndex := FItemsCount;
        AItem.NextIndex := FItemsCount;
        AItem.QueueType := qtUnknown;
      end;
    end;
  end else begin
    Assert(FHeadIndex = AIndex);
    VNextIndex := AItem.NextIndex;
    if VNextIndex < FItemsCount then begin
      VNextItem := FItems.GetItemByIndex(VNextIndex);
      Assert(VNextItem <> nil);
      Assert(VNextItem.QueueType = FQueueType);
      Assert(VNextItem.PrevIndex = AIndex);
      if VNextItem <> nil then begin
        FHeadIndex := VNextIndex;
        VNextItem.PrevIndex := VPrevIndex;
        Dec(FCount);
        AItem.PrevIndex := FItemsCount;
        AItem.NextIndex := FItemsCount;
        AItem.QueueType := qtUnknown;
      end;
    end else begin
      Assert(FTailIndex = AIndex);
      Assert(FCount = 1);
      FTailIndex := FItemsCount;
      FHeadIndex := FItemsCount;
      Dec(FCount);
      AItem.PrevIndex := FItemsCount;
      AItem.NextIndex := FItemsCount;
      AItem.QueueType := qtUnknown;
    end;
  end;
end;

procedure TQueue.MoveItemToHead(
  AIndex: TItemIndex;
  AItem: PCacheItem
);
var
  VPrevItem: PCacheItem;
  VPrevIndex: TItemIndex;
  VNextItem: PCacheItem;
  VNextIndex: TItemIndex;
begin
  Assert(AIndex < FItemsCount);
  Assert(AItem <> nil);
  Assert(FItems.GetItemByIndex(AIndex) = AItem);
  Assert(AItem.QueueType = FQueueType);
  Assert(FCount > 0);
  VPrevIndex := AItem.PrevIndex;
  if AItem.PrevIndex < FItemsCount then begin
    Assert(FHeadIndex <> AIndex);
    VPrevItem := FItems.GetItemByIndex(VPrevIndex);
    Assert(VPrevItem <> nil);
    Assert(VPrevItem.QueueType = FQueueType);
    Assert(VPrevItem.NextIndex = AIndex);
    VNextIndex := AItem.NextIndex;
    if VNextIndex < FItemsCount then begin
      VNextItem := FItems.GetItemByIndex(VNextIndex);
      Assert(VNextItem <> nil);
      Assert(VNextItem.QueueType = FQueueType);
      Assert(VNextItem.PrevIndex = AIndex);
      VPrevItem.NextIndex := VNextIndex;
      VNextItem.PrevIndex := VPrevIndex;
    end else begin
      Assert(FTailIndex = AIndex);
      VPrevItem.NextIndex := FItemsCount;
      FTailIndex := VPrevIndex;
    end;
    VNextIndex := FHeadIndex;
    VNextItem := FItems.GetItemByIndex(VNextIndex);
    AItem.PrevIndex := FItemsCount;
    AItem.NextIndex := VNextIndex;
    FHeadIndex := AIndex;
    VNextItem.PrevIndex := AIndex;
  end else begin
    Assert(FHeadIndex = AIndex);
  end;
end;

function TQueue.PopItemFromTail(out AIndex: TItemIndex): PCacheItem;
var
  VPrevItem: PCacheItem;
  VPrevIndex: TItemIndex;
begin
  AIndex := FItemsCount;
  Result := nil;
  Assert(FCount > 0);
  if FCount > 0 then begin
    Assert(FTailIndex < FItemsCount);
    AIndex := FTailIndex;
    Result := FItems.GetItemByIndex(AIndex);
    Assert(Result <> nil);
    Assert(Result.QueueType = FQueueType);
    if Result <> nil then begin
      VPrevIndex := Result.PrevIndex;
      if VPrevIndex < FItemsCount then begin
        VPrevItem := FItems.GetItemByIndex(VPrevIndex);
        Assert(VPrevItem <> nil);
        Assert(VPrevItem.QueueType = FQueueType);
        Assert(VPrevItem.NextIndex = AIndex);
        VPrevItem.NextIndex := FItemsCount;
        FTailIndex := VPrevIndex;
        Dec(FCount);
      end else begin
        Assert(FCount = 1);
        Assert(FHeadIndex = FTailIndex);
        FTailIndex := FItemsCount;
        FHeadIndex := FItemsCount;
        Dec(FCount);
      end;
      Result.NextIndex := FItemsCount;
      Result.PrevIndex := FItemsCount;
      Result.QueueType := qtUnknown;
    end else begin
      AIndex := FItemsCount;
    end;
  end;
end;

procedure TQueue.PushItemToHead(
  AIndex: TItemIndex;
  AItem: PCacheItem
);
var
  VNextItem: PCacheItem;
  VNextIndex: TItemIndex;
begin
  Assert(AIndex < FItemsCount);
  Assert(AItem <> nil);
  Assert(FItems.GetItemByIndex(AIndex) = AItem);
  Assert(AItem.QueueType in [qtEmpty, qtUnknown]);
  VNextIndex := FHeadIndex;
  if VNextIndex < FItemsCount then begin
    Assert(FCount > 0);
    VNextItem := FItems.GetItemByIndex(VNextIndex);
    Assert(VNextItem <> nil);
    Assert(VNextItem.PrevIndex >= FItemsCount);
    VNextItem.PrevIndex := AIndex;
    AItem.NextIndex := VNextIndex;
    AItem.PrevIndex := FItemsCount;
    AItem.QueueType := FQueueType;
    FHeadIndex := AIndex;
    Inc(FCount);
  end else begin
    Assert(FCount = 0);
    Assert(FTailIndex >= FItemsCount);
    AItem.NextIndex := FItemsCount;
    AItem.PrevIndex := FItemsCount;
    AItem.QueueType := FQueueType;
    FTailIndex := AIndex;
    FHeadIndex := AIndex;
    Inc(FCount);
  end;
end;

{ THashTable }

constructor THashTable.Create(
  AHashSize: Integer;
  AItems: TItemsArray
);
var
  i: Integer;
begin
  inherited Create;
  FHashSize := AHashSize;
  SetLength(FHash, FHashSize);
  FItems := AItems;
  FItemsCount := FItems.ItemsCount;

  for i := 0 to FHashSize - 1 do begin
    FHash[i] := FItemsCount;
  end;
end;

procedure THashTable.AddItem(
  AHashIndex: THashIndex;
  AIndex: TItemIndex;
  AItem: PCacheItem
);
var
  VIndex: TItemIndex;
begin
  Assert(AIndex < FItemsCount);
  Assert(AItem <> nil);
  Assert(FItems.GetItemByIndex(AIndex) = AItem);
  Assert(AHashIndex >= 0);
  Assert(AHashIndex < FHashSize);
  Assert(AItem.CollisionNextIndex = FItemsCount);
  VIndex := FHash[AHashIndex];
  Assert(VIndex <> AIndex);
  FHash[AHashIndex] := AIndex;
  AItem.CollisionNextIndex := VIndex;
end;

function THashTable.GetItem(
  AHashIndex: THashIndex;
  const AKey: THashValue;
  out AIndex: TItemIndex
): PCacheItem;
begin
  Assert(AHashIndex >= 0);
  Assert(AHashIndex < FHashSize);
  Result := nil;
  AIndex := FHash[AHashIndex];
  while AIndex < FItemsCount do begin
    Result := FItems.GetItemByIndex(AIndex);
    Assert(Result <> nil);
    if Result <> nil then begin
      Assert(Result.QueueType in [qtMulti, qtFirstIn, qtFirstOut]);
      if Result.Key = AKey then begin
        Break;
      end else begin
        Assert(AIndex <> Result.CollisionNextIndex);
        AIndex := Result.CollisionNextIndex;
        Result := nil;
      end;
    end else begin
      AIndex := FItemsCount;
    end;
  end;
end;

procedure THashTable.RemoveItem(
  AHashIndex: THashIndex;
  AIndex: TItemIndex;
  AItem: PCacheItem
);
var
  VIndex: TItemIndex;
  VPrevItem: PCacheItem;
begin
  Assert(AIndex < FItemsCount);
  Assert(AItem <> nil);
  Assert(FItems.GetItemByIndex(AIndex) = AItem);
  Assert(AItem.QueueType = qtUnknown);
  Assert(AHashIndex >= 0);
  Assert(AHashIndex < FHashSize);
  VIndex := FHash[AHashIndex];

  if VIndex = AIndex then begin
    FHash[AHashIndex] := AItem.CollisionNextIndex;
    AItem.CollisionNextIndex := FItemsCount;
  end else begin
    while VIndex < FItemsCount do begin
      VPrevItem := FItems.GetItemByIndex(VIndex);
      Assert(VPrevItem <> nil);
      if VPrevItem <> nil then begin
        VIndex := VPrevItem.CollisionNextIndex;
        if VIndex = AIndex then begin
          VPrevItem.CollisionNextIndex := AItem.CollisionNextIndex;
          AItem.CollisionNextIndex := FItemsCount;
          Break;
        end else begin
          VIndex := VPrevItem.CollisionNextIndex;
        end;
      end else begin
        VIndex := FItemsCount;
      end;
    end;
  end;
end;

{ THashCacheWithQueuesAbstract }

constructor THashCacheWithQueuesAbstract.Create(
  AHashSizeInBit: Byte;
  AFirstUseCount: Integer;
  AMultiUseCount: Integer;
  AFirstOutCount: Integer
);
var
  VIndex: TItemIndex;
  VHashSizeInBit: Byte;
begin
  inherited Create;
  Assert(AHashSizeInBit >= 6);
  Assert(AHashSizeInBit <= 30);
  Assert(AMultiUseCount > 0);
  Assert(AMultiUseCount < High(TItemIndex));

  Assert(AFirstUseCount >= 0);
  Assert(AFirstUseCount < High(TItemIndex));

  Assert(AFirstOutCount >= 0);
  Assert(AFirstOutCount < High(TItemIndex));

  Assert(AMultiUseCount + AFirstUseCount + AFirstOutCount < High(TItemIndex));
  VHashSizeInBit := AHashSizeInBit;
  FCS := MakeSyncRW_Var(Self, False);

  if VHashSizeInBit < 6 then begin
    VHashSizeInBit := 6;
  end;
  if VHashSizeInBit > 30 then begin
    VHashSizeInBit := 30;
  end;
  FHashSize := 1 shl VHashSizeInBit;
  FHashMask := FHashSize - 1;

  FQueueMultiMaxCount := AMultiUseCount;
  FQueueFirstInMaxCount := AFirstUseCount;
  FQueueFirstOutMaxCount := AFirstOutCount;

  if FQueueMultiMaxCount <= 0 then begin
    FQueueMultiMaxCount := 0;
    FQueueFirstInMaxCount := 0;
    FQueueFirstOutMaxCount := 0;
  end;

  if FQueueFirstOutMaxCount <= 0 then begin
    FQueueFirstInMaxCount := 0;
    FQueueFirstOutMaxCount := 0;
  end;

  if FQueueMultiMaxCount <= 0 then begin
    FQueueMultiMaxCount := FHashSize shr 3;
  end;

  // Индекс равный FItemsCount будет использоваться в качестве терминатора в списках.
  FItemsCount := FQueueMultiMaxCount + FQueueFirstInMaxCount + FQueueFirstOutMaxCount;
  if FItemsCount > High(TItemIndex) then begin
    FItemsCount := High(TItemIndex);
    FQueueFirstInMaxCount := FItemsCount - (FQueueMultiMaxCount + FQueueFirstOutMaxCount);
    if FQueueFirstInMaxCount <= 0 then begin
      FQueueFirstInMaxCount := 0;
      FQueueFirstOutMaxCount := FItemsCount - FQueueMultiMaxCount;
      if FQueueFirstOutMaxCount <= 0 then begin
        FQueueFirstOutMaxCount := 0;
        if FQueueMultiMaxCount > FItemsCount then begin
          FQueueMultiMaxCount := FItemsCount;
        end;
      end;
    end;
  end;

  FItems := TItemsArray.Create(FItemsCount);
  FFreeItems := TStack.Create(FItems);

  for VIndex := FItemsCount - 1 downto 0 do begin
    FFreeItems.Push(VIndex, FItems.GetItemByIndex(VIndex));
  end;

  FQueueMulti := TQueue.Create(qtMulti, FItems);
  FQueueFirstIn := TQueue.Create(qtFirstIn, FItems);
  FQueueFirstOut := TQueue.Create(qtFirstOut, FItems);
  FHash := THashTable.Create(FHashSize, FItems);
end;

destructor THashCacheWithQueuesAbstract.Destroy;
begin
  FreeAndNil(FFreeItems);
  FreeAndNil(FQueueMulti);
  FreeAndNil(FQueueFirstIn);
  FreeAndNil(FQueueFirstOut);
  FreeAndNil(FHash);
  FreeAndNil(FItems);
  FCS := nil;
  inherited;
end;

procedure THashCacheWithQueuesAbstract.Clear;
var
  VItem: PCacheItem;
  VIndex: TItemIndex;
begin
  FCS.BeginWrite;
  try
    while FQueueMulti.Count > 0 do begin
      VItem := FQueueMulti.PopItemFromTail(VIndex);
      FHash.RemoveItem(GetIndexByKey(VItem.Key), VIndex, VItem);
      FFreeItems.Push(VIndex, VItem);
    end;
    while FQueueFirstIn.Count > 0 do begin
      VItem := FQueueFirstIn.PopItemFromTail(VIndex);
      FHash.RemoveItem(GetIndexByKey(VItem.Key), VIndex, VItem);
      FFreeItems.Push(VIndex, VItem);
    end;
    while FQueueFirstOut.Count > 0 do begin
      VItem := FQueueFirstOut.PopItemFromTail(VIndex);
      FHash.RemoveItem(GetIndexByKey(VItem.Key), VIndex, VItem);
      FFreeItems.Push(VIndex, VItem);
    end;
  finally
    FCS.EndWrite;
  end;
end;

procedure THashCacheWithQueuesAbstract.DeleteItem(const AKey: THashValue);
var
  VHashIndex: THashIndex;
  VCurrIndex: TItemIndex;
  VCurrItem: PCacheItem;
begin
  VHashIndex := GetIndexByKey(AKey);
  Assert(VHashIndex >= 0);
  Assert(VHashIndex < FHashSize);

  FCS.BeginWrite;
  try
    VCurrItem := FHash.GetItem(VHashIndex, AKey, VCurrIndex);
    if VCurrItem <> nil then begin
      Assert(VCurrItem.Key = AKey);
      Assert(VCurrItem.QueueType in [qtMulti, qtFirstIn, qtFirstOut]);
      Assert(FItems.GetItemByIndex(VCurrIndex) = VCurrItem);
      case VCurrItem.QueueType of
        qtMulti: begin
          FQueueMulti.ExcludeItem(VCurrIndex, VCurrItem);
          FHash.RemoveItem(VHashIndex, VCurrIndex, VCurrItem);
          FFreeItems.Push(VCurrIndex, VCurrItem);
        end;
        qtFirstIn: begin
          FQueueFirstIn.ExcludeItem(VCurrIndex, VCurrItem);
          FHash.RemoveItem(VHashIndex, VCurrIndex, VCurrItem);
          FFreeItems.Push(VCurrIndex, VCurrItem);
        end;
        qtFirstOut: begin
          FQueueFirstOut.ExcludeItem(VCurrIndex, VCurrItem);
          FHash.RemoveItem(VHashIndex, VCurrIndex, VCurrItem);
          FFreeItems.Push(VCurrIndex, VCurrItem);
        end;
      end;
    end;
  finally
    FCS.EndWrite;
  end;
end;

procedure THashCacheWithQueuesAbstract.FreeItemFromQueueMulti;
var
  VItem: PCacheItem;
  VIndex: TItemIndex;
begin
  VItem := FQueueMulti.PopItemFromTail(VIndex);
  FHash.RemoveItem(GetIndexByKey(VItem.Key), VIndex, VItem);
  FFreeItems.Push(VIndex, VItem);
end;

function THashCacheWithQueuesAbstract.GetIndexByKey(
  const AKey: THashValue): THashIndex;
begin
  Result := AKey and FHashMask;
end;

function THashCacheWithQueuesAbstract.GetOrCreateItem(
  const AKey: THashValue;
  AData: Pointer
): IInterface;
var
  VHashIndex: THashIndex;
  VCurrIndex: TItemIndex;
  VCurrItem: PCacheItem;
begin
  Result := nil;
  VHashIndex := GetIndexByKey(AKey);
  Assert(VHashIndex >= 0);
  Assert(VHashIndex < FHashSize);
  FCS.BeginWrite;
  try
    VCurrItem := FHash.GetItem(VHashIndex, AKey, VCurrIndex);
    if VCurrItem <> nil then begin
      Assert(VCurrItem.Key = AKey);
      Assert(VCurrItem.QueueType in [qtMulti, qtFirstIn, qtFirstOut]);
      Assert(FItems.GetItemByIndex(VCurrIndex) = VCurrItem);
      case VCurrItem.QueueType of
        qtMulti: begin
          Assert(VCurrItem.Value <> nil);
          Result := VCurrItem.Value;
          FQueueMulti.MoveItemToHead(VCurrIndex, VCurrItem);
        end;
        qtFirstIn: begin
          Assert(FQueueFirstInMaxCount > 0);
          Assert(VCurrItem.Value <> nil);
          Result := VCurrItem.Value;
        end;
        qtFirstOut: begin
          Assert(FQueueFirstOutMaxCount > 0);
          Assert(VCurrItem.Value = nil);
          FQueueFirstOut.MoveItemToHead(VCurrIndex, VCurrItem);
        end;
      end;
    end;
  finally
    FCS.EndWrite;
  end;

  if Result = nil then begin
    Result := CreateByKey(AKey, AData);
    FCS.BeginWrite;
    try
      VCurrItem := FHash.GetItem(VHashIndex, AKey, VCurrIndex);
      if VCurrItem <> nil then begin
        Assert(VCurrItem.Key = AKey);
        Assert(VCurrItem.QueueType <> qtEmpty);
        Assert(FItems.GetItemByIndex(VCurrIndex) = VCurrItem);
        case VCurrItem.QueueType of
          qtMulti: begin
          end;
          qtFirstIn: begin
            Assert(FQueueFirstInMaxCount > 0);
          end;
          qtFirstOut: begin
            Assert(FQueueFirstOutMaxCount > 0);
            Assert(VCurrItem.Value = nil);
            FQueueFirstOut.ExcludeItem(VCurrIndex, VCurrItem);
            VCurrItem.Value := Result;
            if FQueueMulti.Count + 1 > FQueueMultiMaxCount then begin
              FreeItemFromQueueMulti;
            end else begin
              if FQueueFirstInMaxCount > 0 then begin
                if FQueueFirstIn.Count + FQueueMulti.Count + 1 > FQueueMultiMaxCount + FQueueFirstInMaxCount then begin
                  MoveItemFromFirstInToFirstOut;
                end;
              end;
            end;
            FQueueMulti.PushItemToHead(VCurrIndex, VCurrItem);
          end;
        end;
      end else begin
        if FFreeItems.Count > 0 then begin
          VCurrItem := FFreeItems.Pop(VCurrIndex);
          if FQueueFirstInMaxCount > 0 then begin
            if FQueueFirstIn.Count + FQueueMulti.Count + 1 > FQueueMultiMaxCount + FQueueFirstInMaxCount then begin
              MoveItemFromFirstInToFirstOut;
            end;
          end;
        end else begin
          if FQueueFirstInMaxCount > 0 then begin
            if FQueueFirstIn.Count + 1 > FQueueFirstInMaxCount then begin
              VCurrItem := FQueueFirstOut.PopItemFromTail(VCurrIndex);
              FHash.RemoveItem(GetIndexByKey(VCurrItem.Key), VCurrIndex, VCurrItem);
              MoveItemFromFirstInToFirstOut;
            end else begin
              VCurrItem := FQueueMulti.PopItemFromTail(VCurrIndex);
              FHash.RemoveItem(GetIndexByKey(VCurrItem.Key), VCurrIndex, VCurrItem);
            end;
          end else if FQueueFirstOutMaxCount > 0 then begin
            if FQueueMulti.Count > FQueueMultiMaxCount then begin
              VCurrItem := FQueueMulti.PopItemFromTail(VCurrIndex);
              FHash.RemoveItem(GetIndexByKey(VCurrItem.Key), VCurrIndex, VCurrItem);
            end else begin
              VCurrItem := FQueueFirstOut.PopItemFromTail(VCurrIndex);
              FHash.RemoveItem(GetIndexByKey(VCurrItem.Key), VCurrIndex, VCurrItem);
            end;
          end else begin
            VCurrItem := FQueueMulti.PopItemFromTail(VCurrIndex);
            FHash.RemoveItem(GetIndexByKey(VCurrItem.Key), VCurrIndex, VCurrItem);
          end;
        end;
        VCurrItem.Key := AKey;
        if FQueueFirstInMaxCount > 0 then begin
          VCurrItem.Value := Result;
          FQueueFirstIn.PushItemToHead(VCurrIndex, VCurrItem);
        end else if FQueueFirstOutMaxCount > 0 then begin
          FQueueFirstOut.PushItemToHead(VCurrIndex, VCurrItem);
        end else begin
          VCurrItem.Value := Result;
          FQueueMulti.PushItemToHead(VCurrIndex, VCurrItem);
        end;
        FHash.AddItem(VHashIndex, VCurrIndex, VCurrItem);
      end;
    finally
      FCS.EndWrite;
    end;
  end;
end;

procedure THashCacheWithQueuesAbstract.MoveItemFromFirstInToFirstOut;
var
  VItem: PCacheItem;
  VIndex: TItemIndex;
begin
  VItem := FQueueFirstIn.PopItemFromTail(VIndex);
  Assert(VItem <> nil);
  if VItem <> nil then begin
    VItem.Value := nil;
    FQueueFirstOut.PushItemToHead(VIndex, VItem);
  end;
end;

end.
