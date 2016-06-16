unit u_TileStorageSQLiteList;

interface

uses
  Windows,
  SysUtils,
  Classes,
  t_NotifierOperationRec,
  i_MapVersionInfo,
  i_TileStorageSQLiteHandler;

type
  TSQLiteHandlerFactoryProc = function (
    const AOper: PNotifierOperationRec;
    const AXY: TPoint;
    const AZoom: Byte;
    const AVersionInfo: IMapVersionInfo;
    const AForceMakeDB: Boolean;
    var ADBNotFound: Boolean
  ): ITileStorageSQLiteHandler of object;

  TSQLiteFixedEntry = record
  private
    // 0 - not initialized,
    // 1 - initializing
    // 2 - initialized but no operations
    // >2 - used
    FOpState: Integer;

    FHandler: ITileStorageSQLiteHandler;
    FDBNotFound: Boolean;
    FFixSync: IReadWriteSync;
  private
    procedure InternalSync;
    procedure InternalUninit;
  end;
  PSQLiteFixedEntry = ^TSQLiteFixedEntry;

  TSQLiteDynamicEntry = record
    FOpCount: Integer; // count of operations
    FHandler: ITileStorageSQLiteHandler;
    FDBNotFound: Boolean;
    FShiftXY: TPoint;
  end;
  PSQLiteDynamicEntry = ^TSQLiteDynamicEntry;

  TSQLiteDynState = record
    FAddCount: Integer;
    FDelCount: Integer;
  end;
  
  TSQLiteDynList = class(TList)
  private
    FDynState: TSQLiteDynState;
  private
    function FindXYEntry(const AShiftedXY: TPoint; const AMinIndex: Integer): PSQLiteDynamicEntry;
    function AddXYEntry: PSQLiteDynamicEntry;
    procedure SyncItems;
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  end;

  TSQLiteDynamicList = record
  private
    FDynSync: IReadWriteSync; // to sync access to array
    FDynList: TSQLiteDynList; // TODO: optimize it to allocate memory by pages
  end;
  PSQLiteDynamicList = ^TSQLiteDynamicList;

  // list of (0-based) zooms with fixed database count per zoom
  // 0..8 - 9*1 db (2k div 16 = 128, 2k div 24 = 85)
  //    9 - 4 db (z10)
  //   10 - 16 db (z11)

const
  c_Max_Single_Zoom = 8;
  c_Max_Fixed_Zoom = 10;
  c_Max_Zoom = 23; // for z24

  cList9Max=1;
  cListAMax=3;

  cForeachSync=0;
  cForeachFree=1;

type
  TSQLiteSingleList = record
  private
    FFactoryProc: TSQLiteHandlerFactoryProc;
    // single zooms
    FList8: array [0..c_Max_Single_Zoom] of TSQLiteFixedEntry; // z1-z9
    // fixed zooms
    FList9: array [0..1, 0..1] of TSQLiteFixedEntry; // z10
    FListA: array [0..3, 0..3] of TSQLiteFixedEntry; // z11
    // dynamic zooms (up to z24)
    FDynamicSync: IReadWriteSync; // to make children
    FDynamicList: array [c_Max_Fixed_Zoom+1..c_Max_Zoom, 0..3, 0..3] of TSQLiteDynamicList;
  private
    procedure ForeachFixedEntry(const AMode: Byte);
    procedure ForeachFixedProc(const AMode: Byte; const AEntryPtr: PSQLiteFixedEntry);
    procedure ForeachDynEntry(const AMode: Byte);
  private
    function GetFixedHandler(
      const AOper: PNotifierOperationRec;
      const AFixedEntry: PSQLiteFixedEntry;
      const AZoom: Byte;
      const AXY: TPoint;
      const AVersionInfo: IMapVersionInfo;
      const AForceMakeDB: Boolean
    ): ITileStorageSQLiteHandler;

    function GetDynList(
      const AZoom: Byte;
      const AShiftedXY: TPoint
    ): PSQLiteDynamicList;

    function GetDynHandler(
      const AOper: PNotifierOperationRec;
      const ADynList: PSQLiteDynamicList;
      const AZoom: Byte;
      const AShiftedXY: TPoint;
      const AXY: TPoint;
      const AVersionInfo: IMapVersionInfo;
      const AForceMakeDB: Boolean
    ): ITileStorageSQLiteHandler;
  public
    Zero: TPoint; // (0,0)
  public
    procedure Init(const AFactoryProc: TSQLiteHandlerFactoryProc);
    procedure Uninit;
    procedure Sync;
    // get single database by xyz
    function GetHandler(
      const AOper: PNotifierOperationRec;
      const AZoom: Byte;
      const AXY: TPoint;
      const AVersionInfo: IMapVersionInfo;
      const AForceMakeDB: Boolean
    ): ITileStorageSQLiteHandler;
  end;

implementation

uses
  u_Synchronizer;

{ TSQLiteFixedEntry }

procedure TSQLiteFixedEntry.InternalSync;
var
  VPrevState: Integer;
begin
  // get value, set to 0 if was 0
  VPrevState := InterlockedCompareExchange(FOpState, 0, 0);

  // if not completely initialized
  if VPrevState < 2 then begin
    Exit;
  end;

  // if has operations
  if VPrevState > 2 then begin
    InterlockedExchange(FOpState, 2);
    Exit;
  end;

  // 2 = initialized but no operations
  FFixSync.BeginWrite;
  try
    VPrevState := InterlockedExchange(FOpState, 2);
    if VPrevState > 2 then begin
      // was accessed before lock
    end else begin
      // no operations
      FHandler := nil;
      // set uninitialized
      InterlockedExchange(FOpState, 0);
    end;
  finally
    FFixSync.EndWrite;
  end;
end;

procedure TSQLiteFixedEntry.InternalUninit;
var
  VPrevState: Integer;
begin
  repeat
    // get value, set to 0 if was 0
    VPrevState := InterlockedCompareExchange(FOpState, 0, 0);
    case VPrevState of
      0: begin
        // not initialized yet
        Exit;
      end;
      1: begin
        // initializing - wait
        Sleep(1);
      end;
    else
      begin
        // already initialized (FOpState >= 2)
        FFixSync.BeginWrite;
        try
          InterlockedExchange(FOpState, 1);
          FHandler := nil;
        finally
          FFixSync.EndWrite;
          FFixSync := nil;
          InterlockedExchange(FOpState, 0);
        end;
      end;
    end;
  until False;
end;

{ TSQLiteSingleList }

procedure TSQLiteSingleList.ForeachDynEntry(const AMode: Byte);
var
  I, J, K: Byte;
  VList: PSQLiteDynamicList;
begin
  for I := c_Max_Fixed_Zoom + 1 to c_Max_Zoom do begin
    for J := 0 to 3 do begin
      for K := 0 to 3 do begin
        // list holder
        VList := @(FDynamicList[I,J,K]);
        // check initialized
        FDynamicSync.BeginRead;
        try
          if VList^.FDynList = nil then begin
            VList := nil;
          end;
        finally
          FDynamicSync.EndRead;
        end;
        // if initialized
        if VList <> nil then begin
          VList^.FDynSync.BeginWrite;
          try
            case AMode of
              cForeachSync: begin
                VList^.FDynList.SyncItems;
              end;
              cForeachFree: begin
                // cleanup
                FreeAndNil(VList^.FDynList);
              end;
            end;
          finally
            VList^.FDynSync.EndWrite;
          end;
          // free
          if AMode = cForeachFree then begin
            // uninit
            FDynamicSync.BeginWrite;
            try
              VList^.FDynSync := nil;
            finally
              FDynamicSync.EndWrite;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TSQLiteSingleList.ForeachFixedEntry(const AMode: Byte);
var
  I, J: Byte;
begin
  // zooms 0..8 (z1..z9)
  for I := 0 to c_Max_Single_Zoom do begin
    ForeachFixedProc(AMode, @(FList8[I]));
  end;
  // 9 (z10)
  for I := 0 to cList9Max do begin
    for J := 0 to cList9Max do begin
      ForeachFixedProc(AMode, @(FList9[I, J]));
    end;
  end;
  // 10 (z11)
  for I := 0 to cListAMax do begin
    for J := 0 to cListAMax do begin
      ForeachFixedProc(AMode, @(FListA[I, J]));
    end;
  end;
end;

procedure TSQLiteSingleList.ForeachFixedProc(
  const AMode: Byte;
  const AEntryPtr: PSQLiteFixedEntry
);
begin
  case AMode of
    cForeachSync: begin
      AEntryPtr^.InternalSync;
    end;
    cForeachFree: begin
      AEntryPtr^.InternalUninit;
    end;
  end;
end;

function TSQLiteSingleList.GetDynHandler(
  const AOper: PNotifierOperationRec;
  const ADynList: PSQLiteDynamicList;
  const AZoom: Byte;
  const AShiftedXY: TPoint;
  const AXY: TPoint;
  const AVersionInfo: IMapVersionInfo;
  const AForceMakeDB: Boolean
): ITileStorageSQLiteHandler;
var
  VOldCount: Integer;
  VOldState: TSQLiteDynState; // FDynState;
  VDynEntry: PSQLiteDynamicEntry;
begin
  // TODO: check cache (XY -> Index)

  // search
  ADynList^.FDynSync.BeginRead;
  try
    // save state
    VOldCount := ADynList^.FDynList.Count;
    VOldState := ADynList^.FDynList.FDynState;
    // find
    VDynEntry := ADynList^.FDynList.FindXYEntry(AShiftedXY, 0);
    if VDynEntry <> nil then begin
      // found
      Result := VDynEntry^.FHandler;
      if (Result <> nil) or (VDynEntry^.FDBNotFound and not AForceMakeDB) then begin
        Exit;
      end;
    end;
  finally
    ADynList^.FDynSync.EndRead;
  end;

  // not found
  ADynList^.FDynSync.BeginWrite;
  try
    // may be item without database
    with ADynList^.FDynList do begin
      if FDynState.FAddCount = VOldState.FAddCount then begin
        if FDynState.FDelCount = VOldState.FDelCount then begin
          if VDynEntry <> nil then begin
            // make database
            if VDynEntry^.FHandler = nil then begin
              VDynEntry^.FHandler :=
                FFactoryProc(
                  AOper,
                  AXY,
                  AZoom,
                  AVersionInfo,
                  AForceMakeDB,
                  VDynEntry^.FDBNotFound
                );
            end;
            Result := VDynEntry^.FHandler;
            Exit;
          end;
        end;
      end;
    end;

    with ADynList^.FDynList do begin
      if FDynState.FAddCount > VOldState.FAddCount then begin
        // some was added - search again
        VOldState.FDelCount := VOldState.FDelCount - FDynState.FDelCount;
        if VOldState.FDelCount < 0 then begin
          // some was deleted
          VOldCount := VOldCount + VOldState.FDelCount;
          if VOldCount < 0 then begin
            VOldCount := 0;
          end;
        end;
        // find
        VDynEntry := ADynList^.FDynList.FindXYEntry(AShiftedXY, VOldCount);
        if VDynEntry <> nil then begin
          // found
          Result := VDynEntry^.FHandler;
          // may be no database
          if Result = nil then begin
            VDynEntry^.FHandler := FFactoryProc(
              AOper,
              AXY,
              AZoom,
              AVersionInfo,
              AForceMakeDB,
              VDynEntry^.FDBNotFound
            );
            Result := VDynEntry^.FHandler;
          end;
          Exit;
        end;
      end;
    end;
    // add
    VDynEntry := ADynList^.FDynList.AddXYEntry;
    // apply values
    with VDynEntry^ do begin
      FShiftXY := AShiftedXY;
      FHandler := FFactoryProc(
        AOper,
        AXY,
        AZoom,
        AVersionInfo,
        AForceMakeDB,
        FDBNotFound
      );
      // done
      Result := FHandler;
    end;
  finally
    ADynList^.FDynSync.EndWrite;
  end;
end;

function TSQLiteSingleList.GetDynList(
  const AZoom: Byte;
  const AShiftedXY: TPoint
): PSQLiteDynamicList;
begin
  // get result
  Result := @(FDynamicList[AZoom, (AShiftedXY.X mod 4), (AShiftedXY.Y mod 4)]);

  // check result
  FDynamicSync.BeginRead;
  try
    if Result^.FDynSync <> nil then begin
      // initialized
      Exit;
    end;
  finally
    FDynamicSync.EndRead;
  end;

  // not initialized
  FDynamicSync.BeginWrite;
  try
    // check again
    if Result^.FDynSync <> nil then begin
      // initialized
      Exit;
    end;
    // initialize
    Result^.FDynSync := GSync.SyncVariable.Make('TSQLiteSingleList.GetDynList');
    Result^.FDynList := TSQLiteDynList.Create;
    with Result^.FDynList do begin
      FillChar(FDynState, SizeOf(FDynState), 0);
    end;
  finally
    FDynamicSync.EndWrite;
  end;
end;

function TSQLiteSingleList.GetFixedHandler(
  const AOper: PNotifierOperationRec;
  const AFixedEntry: PSQLiteFixedEntry;
  const AZoom: Byte;
  const AXY: TPoint;
  const AVersionInfo: IMapVersionInfo;
  const AForceMakeDB: Boolean
): ITileStorageSQLiteHandler;
var
  VPrevState: Integer;
begin
  with AFixedEntry^ do
  repeat
    // check
    if AOper^.IsOperationCancelled then begin
      Result := nil;
      Exit;
    end;
    // get prev value, set to 1 if was 0
    VPrevState := InterlockedCompareExchange(FOpState, 1, 0);
    case VPrevState of
      0: begin
        // not initialized yet - initialize
        if AForceMakeDB or not FDBNotFound then begin
          // make objects
          FFixSync := GSync.SyncVariable.Make('TSQLiteSingleList.GetFixedHandler');
          FFixSync.BeginWrite;
          try
            FHandler := FFactoryProc(
              AOper,
              AXY,
              AZoom,
              AVersionInfo,
              AForceMakeDB,
              FDBNotFound
            );
            Result := FHandler;
          finally
            FFixSync.EndWrite;
          end;
          if FDBNotFound then begin
            // no database
            InterlockedExchange(FOpState, 0);
          end else begin
            // complete (even if NIL!)
            InterlockedExchange(FOpState, 2);
          end;
          Exit;
        end else begin
          // not need to initialize - not forced and not found
          InterlockedExchange(FOpState, 0);
          Result := nil;
          Exit;
        end;
      end;
      1: begin
        // initializing - wait
        Sleep(1);
      end;
    else
      begin
        // already initialized (FOpState>=2)
        InterlockedIncrement(FOpState);
        FFixSync.BeginRead;
        try
          // get object
          Result := FHandler;
          // if was unitialized - initialize again - else exit
          if InterlockedCompareExchange(FOpState, 0, 0) <> 0 then begin
            Exit;
          end;
        finally
          FFixSync.EndRead;
        end;
      end;
    end;
  until False;
end;

function TSQLiteSingleList.GetHandler(
  const AOper: PNotifierOperationRec;
  const AZoom: Byte;
  const AXY: TPoint;
  const AVersionInfo: IMapVersionInfo;
  const AForceMakeDB: Boolean
): ITileStorageSQLiteHandler;
var
  VLev: ShortInt;
  VPos: TPoint;
  VFixedEntry: PSQLiteFixedEntry;
  VDynList: PSQLiteDynamicList;
begin
  VLev := (AZoom - c_Max_Single_Zoom);
  if VLev <= 0 then begin
    // zoom 0..8
    VFixedEntry := @(FList8[AZoom]);
    Result :=
      GetFixedHandler(
        AOper,
        VFixedEntry,
        AZoom,
        Zero,
        AVersionInfo,
        AForceMakeDB
      );
  end else begin
    // zoom >= 9 - need shift
    VPos.X := AXY.X shr 8;
    VPos.Y := AXY.Y shr 8;
    case VLev of
      1: begin
        // 9 (z10)
        VFixedEntry := @(FList9[VPos.X, VPos.Y]);
        Result := GetFixedHandler(
          AOper,
          VFixedEntry,
          AZoom,
          AXY,
          AVersionInfo,
          AForceMakeDB
        );
      end;
      2: begin
        // 10 (z11)
        VFixedEntry := @(FListA[VPos.X, VPos.Y]);
        Result := GetFixedHandler(
          AOper,
          VFixedEntry,
          AZoom,
          AXY,
          AVersionInfo,
          AForceMakeDB
        );
      end;
    else
      begin
        // zoom is greater than fixed
        VDynList := GetDynList(AZoom, VPos);
        Result := GetDynHandler(
          AOper,
          VDynList,
          AZoom,
          VPos,
          AXY,
          AVersionInfo,
          AForceMakeDB
        );
      end;
    end;
  end;
end;

procedure TSQLiteSingleList.Init(const AFactoryProc: TSQLiteHandlerFactoryProc);
begin
  // clear all
  FillChar(Self, SizeOf(Self), 0);
  // initialize proc
  FFactoryProc := AFactoryProc;
  // root sync for dynamic items
  FDynamicSync := GSync.SyncVariable.Make('TSQLiteSingleList.Init');
end;

procedure TSQLiteSingleList.Sync;
begin
  // fixed items
  ForeachFixedEntry(cForeachSync);
  // dynamic items
  ForeachDynEntry(cForeachSync);
end;

procedure TSQLiteSingleList.Uninit;
begin
  // fixed items
  ForeachFixedEntry(cForeachFree);
  // dynamic items
  ForeachDynEntry(cForeachFree);
  // sync
  FDynamicSync := nil;
end;

{ TSQLiteDynList }

function TSQLiteDynList.AddXYEntry: PSQLiteDynamicEntry;
begin
  Result := HeapAlloc(GetProcessHeap, HEAP_ZERO_MEMORY, SizeOf(TSQLiteDynamicEntry));
  Inc(Result^.FOpCount);
  Self.Add(Result);
  Inc(FDynState.FAddCount);
end;

function TSQLiteDynList.FindXYEntry(
  const AShiftedXY: TPoint;
  const AMinIndex: Integer
): PSQLiteDynamicEntry;
var
  I: Integer;
begin
  // locked
  if Count > 0 then begin
    for I := Count - 1 downto AMinIndex do begin
      // check key
      Result := PSQLiteDynamicEntry(
        {$IF CompilerVersion < 23}
        List^[I] // D2007
        {$ELSE}
        List[I]  // XE2
        {$IFEND}
      );
      with Result^ do begin
        if (FShiftXY.X = AShiftedXY.X) and (FShiftXY.Y = AShiftedXY.Y) then begin
          // found (in many readers)
          InterlockedIncrement(Result^.FOpCount);
          Exit;
        end;
      end;
    end;
  end;
  Result := nil;
end;

procedure TSQLiteDynList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  //inherited;
  if Action in [lnDeleted] then begin
    if Ptr <> nil then begin
      // cleanup
      with PSQLiteDynamicEntry(Ptr)^ do begin
        FHandler := nil;
      end;
      // free
      HeapFree(GetProcessHeap, 0, Ptr);
    end;
  end;
end;

procedure TSQLiteDynList.SyncItems;
var
  I: Integer;
  VItem: PSQLiteDynamicEntry;
begin
  // locked
  if Count > 0 then begin
    for I := Count - 1 downto 0 do begin
      // check key
      VItem := PSQLiteDynamicEntry(
        {$IF CompilerVersion < 23}
        List^[I] // D2007
        {$ELSE}
        List[I]  // XE2
        {$IFEND}
      );
      with VItem^ do begin
        if FOpCount > 0 then begin
          // was operated
          FOpCount := 0;
        end else begin
          // not used - delete
          Delete(I);
          Inc(FDynState.FDelCount);
        end;
      end;
    end;
  end;
end;

end.