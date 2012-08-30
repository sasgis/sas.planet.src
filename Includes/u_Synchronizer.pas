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

unit u_Synchronizer;

interface

//////{$define DEBUG_GLOBAL_LOCKS}
//////{$define ALLOW_USE_EVENTPAIR}


uses
  Windows,
  SysUtils;

// generic functions (Read and Write)

// ARecursionAllowed SHOULD BE set to TRUE if you cannot assure work without recursion

// very short operation - about 1 or 2 simple variables read or write
// makes first available from { MakeSyncSRW, MakeSyncSpinLock, MakeSyncRes, MakeSyncMREW }
function MakeSyncRW_Var(const ACreator: TObject; const ARecursionAllowed: Boolean = FALSE): IReadWriteSync;

// small symmetrical operation
// makes first available from { MakeSyncRes, MakeSyncSRW, MakeSyncMREW }
function MakeSyncRW_Sym(const ACreator: TObject; const ARecursionAllowed: Boolean = FALSE): IReadWriteSync;

// others (many readers with 1-2 writers)
// makes first available from { MakeSyncSRW, MakeSyncRes, MakeSyncMREW }
function MakeSyncRW_Std(const ACreator: TObject; const ARecursionAllowed: Boolean = FALSE): IReadWriteSync;

// many concurrent readers and writers
// makes first available from { MakeSyncRes, MakeSyncMREW }
function MakeSyncRW_Big(const ACreator: TObject; const ARecursionAllowed: Boolean = FALSE): IReadWriteSync;



// generic functions (should use only BeginWrite-EndWrite routines)

// ARecursionAllowed SHOULD BE set to TRUE if you cannot assure work without recursion

// makes first available from { MakeSyncSRW, MakeSyncSpinLock }
function MakeSync_Tiny(const ACreator: TObject; const ARecursionAllowed: Boolean = FALSE): IReadWriteSync;

// makes first available from { MakeSyncSpinLock }
function MakeSync_Huge(const ACreator: TObject; const ARecursionAllowed: Boolean = FALSE): IReadWriteSync;



// direct functions

// makes TSynchronizerFake
function MakeSyncFake(const ACreator: TObject): IReadWriteSync;

// makes TMultiReadExclusiveWriteSynchronizer
function MakeSyncMREW(const ACreator: TObject): IReadWriteSync;

// makes TSynchronizerCS
function MakeSyncSection(const ACreator: TObject): IReadWriteSync;

// makes TSynchronizerCSSC
function MakeSyncSpinLock(const ACreator: TObject; const dwSpinCount: Cardinal): IReadWriteSync;

// makes TSynchronizerRes (semi-lightweight, used for PEB and LDR on NT)
function MakeSyncRes(const ACreator: TObject): IReadWriteSync;

// makes TSynchronizerSRW (very lightweight - but no recursion)
// available from Vista - but based on good old-known KeyedEvent NT kernel object (before Vista)
function MakeSyncSRW(const ACreator: TObject): IReadWriteSync;

type
  ESynchronizerNotAvailable = class(Exception);

  IEventPair = interface
  ['{F984DB4B-CDFC-4971-8696-7A7D229A4BA1}']
    function SetHigh: LongInt;
    function SetHighWaitLow: LongInt;
    function SetLow: LongInt;
    function SetLowWaitHigh: LongInt;
    function WaitHigh: LongInt;
    function WaitLow: LongInt;
  end;

{$if defined(ALLOW_USE_EVENTPAIR)}
// makes TSynchronizerEventPair
function MakeSyncEventPair(const ACreator: TObject): IEventPair;
{$ifend}

{$if defined(DEBUG_GLOBAL_LOCKS)}
var
  DebugGlobalLocks_Enabled: Boolean;
{$ifend}

implementation

type
  TSyncTypeInitData = packed record
    InitializePtr: Pointer;
    AcquireExclusivePtr: Pointer;
    ReleaseExclusivePtr: Pointer;
    AcquireSharedPtr: Pointer;
    ReleaseSharedPtr: Pointer;
    UninitializePtr: Pointer;
  end;
  PSyncTypeInitData = ^TSyncTypeInitData;

  TEventPairInitData = packed record
    NtCreateEventPair: Pointer;
    NtSetHighEventPair: Pointer;
    NtSetHighWaitLowEventPair: Pointer;
    NtSetLowEventPair: Pointer;
    NtSetLowWaitHighEventPair: Pointer;
    NtWaitHighEventPair: Pointer;
    NtWaitLowEventPair: Pointer;
    NtClose: Pointer;
  end;
  PEventPairInitData = ^TEventPairInitData;

  PVOID = Pointer;
  PLARGE_INTEGER = ^Int64;
  INT = Integer;

  RTL_RWLOCK = packed record
    rtlCS: RTL_CRITICAL_SECTION;
    hSharedReleaseSemaphore: THandle;
    uSharedWaiters: UINT;
    hExclusiveReleaseSemaphore: THandle;
    uExclusiveWaiters: UINT;
    iNumberActive: INT;
    hOwningThreadId: THandle;
    dwTimeoutBoost: DWORD;
    pDebugInfo: PVOID;
  end;
  LPRTL_RWLOCK = ^RTL_RWLOCK;

  // NULL only
  POBJECT_ATTRIBUTES = Pointer;

  TRtlAcquireResourceExclusive = function(rwl: LPRTL_RWLOCK; fWait: Byte): Byte; stdcall;
  // RtlAcquireResourceShared is the same
  TRtlInitializeResource = procedure(rwl: LPRTL_RWLOCK); stdcall;
  // RtlDeleteResource is the same
  // RtlDumpResource is the same
  // RtlReleaseResource is the same

  RTL_SRWLOCK = packed record
    Ptr: PVOID;
  end;
  PSRWLOCK = ^RTL_SRWLOCK;
  PRTL_SRWLOCK = ^RTL_SRWLOCK;

  TRtlInitializeSRWLock = procedure(SRWLock: PSRWLOCK); stdcall;
  // RtlAcquireSRWLockExclusive is the same
  // RtlReleaseSRWLockExclusive is the same
  // RtlAcquireSRWLockShared is the same
  // RtlReleaseSRWLockShared is the same


  // KeyedEvent - from XP (ver 5.1, build 3663)
  RTL_KEYED_EVENT = packed record
    Value1: Cardinal;
  end;
  PRTL_KEYED_EVENT = ^RTL_KEYED_EVENT;

  TNtCreateKeyedEvent = function(
    KeyedEventHandle: PHandle;
    DesiredAccess: ACCESS_MASK;
    ObjectAttributes: POBJECT_ATTRIBUTES;
    Flags: ULONG): LongInt; stdcall;

  TNtReleaseKeyedEvent = function(
    EventHandle: THandle;
    Key: PRTL_KEYED_EVENT;
    Alertable: BOOLEAN;
    Timeout: PLARGE_INTEGER): LongInt; stdcall;

  // NtWaitForKeyedEvent is the same

  TNtClose = function(ObjectHandle: THandle): LongInt; stdcall;

  TNtCurrentTeb = function: Pointer; stdcall;

  // EventPair - from NT 4 (build 1381)

  TNtCreateEventPair = function(
    EventPairHandle: PHandle;
    DesiredAccess: ACCESS_MASK;
    ObjectAttributes: POBJECT_ATTRIBUTES): LongInt; stdcall;
  
  TNtEventPairFunc = function(EventPairHandle: THandle): LongInt; stdcall;
  // NtSetHighEventPair
  // NtSetHighWaitLowEventPair
  // NtSetLowEventPair
  // NtSetLowWaitHighEventPair
  // NtWaitHighEventPair
  // NtWaitLowEventPair
  
  ISynchronizerFactory = interface
  ['{89031A22-CE84-4B4E-A8CE-F9B86BC1836C}']
    function GetSyncResInitData: PSyncTypeInitData;
    function GetSyncSRWInitData: PSyncTypeInitData;
    function GetEventPairInitData: PEventPairInitData;
  end;

  TSynchronizerFactory = class(TInterfacedObject, ISynchronizerFactory)
  private
    FResInitData: TSyncTypeInitData;
    FSRWInitData: TSyncTypeInitData;
  protected
    { ISynchronizerFactory }
    function GetSyncResInitData: PSyncTypeInitData;
    function GetSyncSRWInitData: PSyncTypeInitData;
    function GetEventPairInitData: PEventPairInitData; virtual;
  protected
    procedure InternalInitDLL(const ADLL: THandle); virtual;
  public
    constructor Create;
  end;

  TSynchronizerFactoryWithEventPair = class(TSynchronizerFactory)
  private
    FEventPairInitData: TEventPairInitData;
  protected
    { ISynchronizerFactory }
    function GetEventPairInitData: PEventPairInitData; override;
  protected
    procedure InternalInitDLL(const ADLL: THandle); override;
  end;

  TSynchronizerAbstract = class(TInterfacedObject)
  protected
    FCreatorClassName: ShortString;
  end;

var
  GSynchronizerFactory: ISynchronizerFactory;

{$if defined(DEBUG_GLOBAL_LOCKS)}
procedure DoDebugGlobalLocks(const ASync: TSynchronizerAbstract; const AProcedure, AEvent: String);
const
  c_SEP = ', ' + Chr(VK_TAB);
var
  VText: String;
begin
  if (not DebugGlobalLocks_Enabled) then
    Exit;
  VText := ASync.ClassName + ' at $'+ IntToHex(Integer(Pointer(ASync)), 8)+' (from '+ASync.FCreatorClassName+')' + c_SEP + 'ThreadId=' + IntToStr(GetCurrentThreadId) + c_SEP +  AProcedure + c_SEP + AEvent;
  OutputDebugString(PChar(VText));
end;
{$ifend}

type
  TSynchronizerFake = class(TSynchronizerAbstract, IReadWriteSync)
  protected
    { IReadWriteSync }
    procedure BeginRead;
    procedure EndRead;
    function BeginWrite: Boolean;
    procedure EndWrite;
  end;

  TSynchronizerCS = class(TSynchronizerAbstract, IReadWriteSync)
  private
    FLock: TRTLCriticalSection;
  protected
    { IReadWriteSync }
    procedure BeginRead;
    procedure EndRead;
    function BeginWrite: Boolean;
    procedure EndWrite;
  public
    constructor Create (const ACreator: TObject);
    destructor Destroy; override;
  end;

  TSynchronizerCSSC = class(TSynchronizerAbstract, IReadWriteSync)
  private
    FLock: TRTLCriticalSection;
  protected
    { IReadWriteSync }
    procedure BeginRead;
    procedure EndRead;
    function BeginWrite: Boolean;
    procedure EndWrite;
  public
    constructor Create(const ACreator: TObject; const dwSpinCount: Cardinal);
    destructor Destroy; override;
  end;

  TSynchronizerRes = class(TSynchronizerAbstract, IReadWriteSync)
  private
    FInitData: PSyncTypeInitData;
    FLock: RTL_RWLOCK;
  protected
    { IReadWriteSync }
    procedure BeginRead;
    procedure EndRead;
    function BeginWrite: Boolean;
    procedure EndWrite;
  public
    constructor Create(const ACreator: TObject; const AInitData: PSyncTypeInitData);
    destructor Destroy; override;
  end;

  TSynchronizerSRW = class(TSynchronizerAbstract, IReadWriteSync)
  private
    FInitData: PSyncTypeInitData;
    FLock: RTL_SRWLOCK;
  protected
    { IReadWriteSync }
    procedure BeginRead;
    procedure EndRead;
    function BeginWrite: Boolean;
    procedure EndWrite;
  public
    constructor Create(const ACreator: TObject; const AInitData: PSyncTypeInitData);
  end;

  TSynchronizerEventPair = class(TSynchronizerAbstract, IEventPair)
  private
    FInitData: PEventPairInitData;
    FEventPairHandle: THandle;
  protected
    { IEventPair }
    function SetHigh: LongInt;
    function SetHighWaitLow: LongInt;
    function SetLow: LongInt;
    function SetLowWaitHigh: LongInt;
    function WaitHigh: LongInt;
    function WaitLow: LongInt;
  public
    constructor Create(const ACreator: TObject; const AInitData: PEventPairInitData);
    destructor Destroy; override;
  end;

procedure RaiseNotAvailable(AClass: TClass);
begin
  raise ESynchronizerNotAvailable.Create(AClass.ClassName);
end;

// direct functions

function MakeSyncFake(const ACreator: TObject): IReadWriteSync;
begin
  // for testing purposes - on errors use TSynchronizerCS
  Result := TSynchronizerFake.Create;
end;

function MakeSyncMREW(const ACreator: TObject): IReadWriteSync;
begin
  Result := TMultiReadExclusiveWriteSynchronizer.Create;
end;

function MakeSyncSection(const ACreator: TObject): IReadWriteSync;
begin
  Result := TSynchronizerCS.Create(ACreator);
end;

function MakeSyncSpinLock(const ACreator: TObject; const dwSpinCount: Cardinal): IReadWriteSync;
begin
  Result := TSynchronizerCSSC.Create(ACreator, dwSpinCount);
end;

function MakeSyncRes(const ACreator: TObject): IReadWriteSync;
begin
  Result := TSynchronizerRes.Create(ACreator, GSynchronizerFactory.GetSyncResInitData);
end;

function MakeSyncSRW(const ACreator: TObject): IReadWriteSync;
begin
  Result := TSynchronizerSRW.Create(ACreator, GSynchronizerFactory.GetSyncSRWInitData);
end;

{$if defined(ALLOW_USE_EVENTPAIR)}
function MakeSyncEventPair(const ACreator: TObject): IEventPair;
begin
  Result := TSynchronizerEventPair.Create(ACreator, GSynchronizerFactory.GetEventPairInitData);
end;
{$ifend}

// generic functions RW

// very short operation - about 1 or 2 simple variables read or write
// makes first available from { MakeSyncSRW, MakeSyncSpinLock, MakeSyncRes, MakeSyncMREW }
function MakeSyncRW_Var(const ACreator: TObject; const ARecursionAllowed: Boolean): IReadWriteSync;
var FData: PSyncTypeInitData;
begin
  // SlimRW
  if (not ARecursionAllowed) then begin
    FData := GSynchronizerFactory.GetSyncSRWInitData;
    if (FData<>nil) then begin
      Result := TSynchronizerSRW.Create(ACreator, FData);
      Exit;
    end;
  end;

  // CriticalSection with Spinlock
  Result := TSynchronizerCSSC.Create(ACreator, 4096);

  // Resource
  // MREW
end;

// small symmetrical operation
// makes first available from { MakeSyncRes, MakeSyncSRW, MakeSyncMREW }
function MakeSyncRW_Sym(const ACreator: TObject; const ARecursionAllowed: Boolean): IReadWriteSync;
var FData: PSyncTypeInitData;
begin
  // Resource
  FData := GSynchronizerFactory.GetSyncResInitData;
  if (FData<>nil) then begin
    Result := TSynchronizerRes.Create(ACreator, FData);
    Exit;
  end;

  // SlimRW
  if (not ARecursionAllowed) then begin
    FData := GSynchronizerFactory.GetSyncSRWInitData;
    if (FData<>nil) then begin
      Result := TSynchronizerSRW.Create(ACreator, FData);
      Exit;
    end;
  end;

  // MREW
  Result := TMultiReadExclusiveWriteSynchronizer.Create;
end;

// others (many readers with 1-2 writers)
// makes first available from { MakeSyncSRW, MakeSyncRes, MakeSyncMREW }
function MakeSyncRW_Std(const ACreator: TObject; const ARecursionAllowed: Boolean): IReadWriteSync;
var FData: PSyncTypeInitData;
begin
  // SlimRW
  if (not ARecursionAllowed) then begin
    FData := GSynchronizerFactory.GetSyncSRWInitData;
    if (FData<>nil) then begin
      Result := TSynchronizerSRW.Create(ACreator, FData);
      Exit;
    end;
  end;

  // Resource
  FData := GSynchronizerFactory.GetSyncResInitData;
  if (FData<>nil) then begin
    Result := TSynchronizerRes.Create(ACreator, FData);
    Exit;
  end;

  // MREW
  Result := TMultiReadExclusiveWriteSynchronizer.Create;
end;

// many concurrent readers and writers
// makes first available from { MakeSyncRes, MakeSyncMREW }
function MakeSyncRW_Big(const ACreator: TObject; const ARecursionAllowed: Boolean): IReadWriteSync;
var FData: PSyncTypeInitData;
begin
  // Resource
  FData := GSynchronizerFactory.GetSyncResInitData;
  if (FData<>nil) then begin
    Result := TSynchronizerRes.Create(ACreator, FData);
    Exit;
  end;

  // MREW
  Result := TMultiReadExclusiveWriteSynchronizer.Create;
end;

// generic functions CS

// makes first available from { MakeSyncSRW, MakeSyncSpinLock }
function MakeSync_Tiny(const ACreator: TObject; const ARecursionAllowed: Boolean = FALSE): IReadWriteSync;
var FData: PSyncTypeInitData;
begin
  // SlimRW
  if (not ARecursionAllowed) then begin
    FData := GSynchronizerFactory.GetSyncSRWInitData;
    if (FData<>nil) then begin
      Result := TSynchronizerSRW.Create(ACreator, FData);
      Exit;
    end;
  end;

  Result := TSynchronizerCSSC.Create(ACreator, 4096);
end;

// makes first available from { MakeSyncSpinLock }
function MakeSync_Huge(const ACreator: TObject; const ARecursionAllowed: Boolean = FALSE): IReadWriteSync;
begin
  Result := TSynchronizerCSSC.Create(ACreator, 4096);
end;


{ TSynchronizerFake }

procedure TSynchronizerFake.BeginRead;
begin

end;

function TSynchronizerFake.BeginWrite: Boolean;
begin
  Result := FALSE;
end;

procedure TSynchronizerFake.EndRead;
begin

end;

procedure TSynchronizerFake.EndWrite;
begin

end;

{ TSynchronizerCSSC }

procedure TSynchronizerCSSC.BeginRead;
begin
{$if defined(DEBUG_GLOBAL_LOCKS)}
  DoDebugGlobalLocks(Self, 'BeginRead', 'IN');
{$ifend}

  EnterCriticalSection(FLock);
  
{$if defined(DEBUG_GLOBAL_LOCKS)}
  DoDebugGlobalLocks(Self, 'BeginRead', 'OUT');
{$ifend}
end;

function TSynchronizerCSSC.BeginWrite: Boolean;
begin
{$if defined(DEBUG_GLOBAL_LOCKS)}
  DoDebugGlobalLocks(Self, 'BeginWrite', 'IN');
{$ifend}
  
  EnterCriticalSection(FLock);
  Result := TRUE;
  
{$if defined(DEBUG_GLOBAL_LOCKS)}
  DoDebugGlobalLocks(Self, 'BeginWrite', 'OUT');
{$ifend}
end;

constructor TSynchronizerCSSC.Create(const ACreator: TObject; const dwSpinCount: Cardinal);
begin
  inherited Create;
  FCreatorClassName := AnsiString(ACreator.ClassName);
  InitializeCriticalSectionAndSpinCount(FLock, dwSpinCount);
end;

destructor TSynchronizerCSSC.Destroy;
begin
  inherited;
  DeleteCriticalSection(FLock);
end;

procedure TSynchronizerCSSC.EndRead;
begin
{$if defined(DEBUG_GLOBAL_LOCKS)}
  DoDebugGlobalLocks(Self, 'EndRead', 'IN');
{$ifend}
  
  LeaveCriticalSection(FLock);
  
{$if defined(DEBUG_GLOBAL_LOCKS)}
  DoDebugGlobalLocks(Self, 'EndRead', 'OUT');
{$ifend}
end;

procedure TSynchronizerCSSC.EndWrite;
begin
{$if defined(DEBUG_GLOBAL_LOCKS)}
  DoDebugGlobalLocks(Self, 'EndWrite', 'IN');
{$ifend}
  
  LeaveCriticalSection(FLock);
  
{$if defined(DEBUG_GLOBAL_LOCKS)}
  DoDebugGlobalLocks(Self, 'EndWrite', 'OUT');
{$ifend}
end;

{ TSynchronizerCS }

procedure TSynchronizerCS.BeginRead;
begin
{$if defined(DEBUG_GLOBAL_LOCKS)}
  DoDebugGlobalLocks(Self, 'BeginRead', 'IN');
{$ifend}
  
  EnterCriticalSection(FLock);
  
{$if defined(DEBUG_GLOBAL_LOCKS)}
  DoDebugGlobalLocks(Self, 'BeginRead', 'OUT');
{$ifend}
end;

function TSynchronizerCS.BeginWrite: Boolean;
begin
{$if defined(DEBUG_GLOBAL_LOCKS)}
  DoDebugGlobalLocks(Self, 'BeginWrite', 'IN');
{$ifend}
  
  EnterCriticalSection(FLock);
  Result := True;
  
{$if defined(DEBUG_GLOBAL_LOCKS)}
  DoDebugGlobalLocks(Self, 'BeginWrite', 'OUT');
{$ifend}
end;

constructor TSynchronizerCS.Create(const ACreator: TObject);
begin
  inherited Create;
  FCreatorClassName := AnsiString(ACreator.ClassName);
  InitializeCriticalSection(FLock);
end;

destructor TSynchronizerCS.Destroy;
begin
  inherited Destroy;
  DeleteCriticalSection(FLock);
end;

procedure TSynchronizerCS.EndRead;
begin
{$if defined(DEBUG_GLOBAL_LOCKS)}
  DoDebugGlobalLocks(Self, 'EndRead', 'IN');
{$ifend}
  
  LeaveCriticalSection(FLock);
  
{$if defined(DEBUG_GLOBAL_LOCKS)}
  DoDebugGlobalLocks(Self, 'EndRead', 'OUT');
{$ifend}
end;

procedure TSynchronizerCS.EndWrite;
begin
{$if defined(DEBUG_GLOBAL_LOCKS)}
  DoDebugGlobalLocks(Self, 'EndWrite', 'IN');
{$ifend}
  
  LeaveCriticalSection(FLock);
  
{$if defined(DEBUG_GLOBAL_LOCKS)}
  DoDebugGlobalLocks(Self, 'EndWrite', 'OUT');
{$ifend}
end;

{ TSynchronizerFactory }

constructor TSynchronizerFactory.Create;
begin
  InternalInitDLL(GetModuleHandle('ntdll.dll'));
end;

function TSynchronizerFactory.GetEventPairInitData: PEventPairInitData;
begin
  Result := nil;
end;

function TSynchronizerFactory.GetSyncResInitData: PSyncTypeInitData;
begin
  if (nil=FResInitData.InitializePtr) then
    Result := nil
  else
    Result := @FResInitData;
end;

function TSynchronizerFactory.GetSyncSRWInitData: PSyncTypeInitData;
begin
  if (nil=FSRWInitData.InitializePtr) then
    Result := nil
  else
    Result := @FSRWInitData;
end;

procedure TSynchronizerFactory.InternalInitDLL(const ADLL: THandle);
begin
  // Resource
  FillChar(FResInitData, sizeof(FResInitData), #0);
  if (0<>ADLL) then begin
    FResInitData.InitializePtr := GetProcAddress(ADLL,'RtlInitializeResource');
    FResInitData.AcquireExclusivePtr := GetProcAddress(ADLL,'RtlAcquireResourceExclusive');
    FResInitData.ReleaseExclusivePtr := GetProcAddress(ADLL,'RtlReleaseResource');
    FResInitData.AcquireSharedPtr := GetProcAddress(ADLL,'RtlAcquireResourceShared');
    FResInitData.ReleaseSharedPtr := FResInitData.ReleaseExclusivePtr;
    FResInitData.UninitializePtr := GetProcAddress(ADLL,'RtlDeleteResource');
    if (nil=FResInitData.InitializePtr) or
       (nil=FResInitData.AcquireExclusivePtr) or
       (nil=FResInitData.ReleaseExclusivePtr) or
       (nil=FResInitData.AcquireSharedPtr) or
       (nil=FResInitData.ReleaseSharedPtr) or
       (nil=FResInitData.UninitializePtr) then begin
      // very-very-very crazy (old?) system!
      FillChar(FResInitData, sizeof(FResInitData), #0);
    end;
  end;

  // SRWLock
  FillChar(FSRWInitData, sizeof(FSRWInitData), #0);
  if (0<>ADLL) then begin
    FSRWInitData.InitializePtr := GetProcAddress(ADLL,'RtlInitializeSRWLock');
    if (nil=FSRWInitData.InitializePtr) then begin
      // before Vista
      // nothing
    end else begin
      // Vista and newer
      FSRWInitData.AcquireExclusivePtr := GetProcAddress(ADLL,'RtlAcquireSRWLockExclusive');
      FSRWInitData.ReleaseExclusivePtr := GetProcAddress(ADLL,'RtlReleaseSRWLockExclusive');
      FSRWInitData.AcquireSharedPtr := GetProcAddress(ADLL,'RtlAcquireSRWLockShared');
      FSRWInitData.ReleaseSharedPtr := GetProcAddress(ADLL,'RtlReleaseSRWLockShared');
      if (nil=FSRWInitData.AcquireExclusivePtr) or
         (nil=FSRWInitData.ReleaseExclusivePtr) or
         (nil=FSRWInitData.AcquireSharedPtr) or
         (nil=FSRWInitData.ReleaseSharedPtr) then begin
        // very-very-very crazy Vista!
        FillChar(FSRWInitData, sizeof(FSRWInitData), #0);
      end;
    end;
  end;
end;

{ TSynchronizerRes }

procedure TSynchronizerRes.BeginRead;
begin
  TRtlAcquireResourceExclusive(FInitData^.AcquireSharedPtr)(@FLock, 1);
end;

function TSynchronizerRes.BeginWrite: Boolean;
begin
  Result := (TRtlAcquireResourceExclusive(FInitData^.AcquireExclusivePtr)(@FLock, 1) <> 0);
end;

constructor TSynchronizerRes.Create(const ACreator: TObject; const AInitData: PSyncTypeInitData);
begin
  if (nil=AInitData) then
    RaiseNotAvailable(ClassType);
  inherited Create;
  FCreatorClassName := AnsiString(ACreator.ClassName);
  FInitData := AInitData;
  TRtlInitializeResource(FInitData^.InitializePtr)(@FLock);
end;

destructor TSynchronizerRes.Destroy;
begin
  TRtlInitializeResource(FInitData^.UninitializePtr)(@FLock);
  inherited;
end;

procedure TSynchronizerRes.EndRead;
begin
  TRtlInitializeResource(FInitData^.ReleaseSharedPtr)(@FLock);
end;

procedure TSynchronizerRes.EndWrite;
begin
  TRtlInitializeResource(FInitData^.ReleaseExclusivePtr)(@FLock);
end;

{ TSynchronizerSRW }

procedure TSynchronizerSRW.BeginRead;
begin
  TRtlInitializeSRWLock(FInitData^.AcquireSharedPtr)(@FLock);
end;

function TSynchronizerSRW.BeginWrite: Boolean;
begin
  TRtlInitializeSRWLock(FInitData^.AcquireExclusivePtr)(@FLock);
  Result := FALSE;
end;

constructor TSynchronizerSRW.Create(const ACreator: TObject; const AInitData: PSyncTypeInitData);
begin
  if (nil=AInitData) then
    RaiseNotAvailable(ClassType);
  inherited Create;
  FCreatorClassName := AnsiString(ACreator.ClassName);
  FInitData := AInitData;
  TRtlInitializeSRWLock(FInitData^.InitializePtr)(@FLock);
end;

procedure TSynchronizerSRW.EndRead;
begin
  TRtlInitializeSRWLock(FInitData^.ReleaseSharedPtr)(@FLock);
end;

procedure TSynchronizerSRW.EndWrite;
begin
  TRtlInitializeSRWLock(FInitData^.ReleaseExclusivePtr)(@FLock);
end;

{ TSynchronizerEventPair }

constructor TSynchronizerEventPair.Create(const ACreator: TObject; const AInitData: PEventPairInitData);
begin
  if (nil=AInitData) then
    RaiseNotAvailable(ClassType);
  inherited Create;
  FCreatorClassName := AnsiString(ACreator.ClassName);
  FInitData := AInitData;
  FEventPairHandle := 0;
  TNtCreateEventPair(FInitData^.NtCreateEventPair)(@FEventPairHandle, STANDARD_RIGHTS_ALL, nil);
end;

destructor TSynchronizerEventPair.Destroy;
begin
  if (0<>FEventPairHandle) then begin
    TNtClose(FInitData^.NtClose)(FEventPairHandle);
    FEventPairHandle:=0;
  end;
  inherited;
end;

function TSynchronizerEventPair.SetHigh: LongInt;
begin
  Result := TNtEventPairFunc(FInitData^.NtSetHighEventPair)(FEventPairHandle);
end;

function TSynchronizerEventPair.SetHighWaitLow: LongInt;
begin
  Result := TNtEventPairFunc(FInitData^.NtSetHighWaitLowEventPair)(FEventPairHandle);
end;

function TSynchronizerEventPair.SetLow: LongInt;
begin
  Result := TNtEventPairFunc(FInitData^.NtSetLowEventPair)(FEventPairHandle);
end;

function TSynchronizerEventPair.SetLowWaitHigh: LongInt;
begin
  Result := TNtEventPairFunc(FInitData^.NtSetLowWaitHighEventPair)(FEventPairHandle);
end;

function TSynchronizerEventPair.WaitHigh: LongInt;
begin
  Result := TNtEventPairFunc(FInitData^.NtWaitHighEventPair)(FEventPairHandle);
end;

function TSynchronizerEventPair.WaitLow: LongInt;
begin
  Result := TNtEventPairFunc(FInitData^.NtWaitLowEventPair)(FEventPairHandle);
end;

{ TSynchronizerFactoryWithEventPair }

function TSynchronizerFactoryWithEventPair.GetEventPairInitData: PEventPairInitData;
begin
  if (nil=FEventPairInitData.NtCreateEventPair) then
    Result := nil
  else
    Result := @FEventPairInitData;
end;

procedure TSynchronizerFactoryWithEventPair.InternalInitDLL(const ADLL: THandle);
begin
  inherited;

  // EventPair
  FillChar(FEventPairInitData, sizeof(FEventPairInitData), #0);

  if (ADLL<>0) then begin
    FEventPairInitData.NtCreateEventPair := GetProcAddress(ADLL,'NtCreateEventPair');
    FEventPairInitData.NtSetHighEventPair := GetProcAddress(ADLL,'NtSetHighEventPair');
    FEventPairInitData.NtSetHighWaitLowEventPair := GetProcAddress(ADLL,'NtSetHighWaitLowEventPair');
    FEventPairInitData.NtSetLowEventPair := GetProcAddress(ADLL,'NtSetLowEventPair');
    FEventPairInitData.NtSetLowWaitHighEventPair := GetProcAddress(ADLL,'NtSetLowWaitHighEventPair');
    FEventPairInitData.NtWaitHighEventPair := GetProcAddress(ADLL,'NtWaitHighEventPair');
    FEventPairInitData.NtWaitLowEventPair := GetProcAddress(ADLL,'NtWaitLowEventPair');
    FEventPairInitData.NtClose := GetProcAddress(ADLL,'NtClose');
    if (nil=FEventPairInitData.NtCreateEventPair) or
       (nil=FEventPairInitData.NtSetHighEventPair) or
       (nil=FEventPairInitData.NtSetHighWaitLowEventPair) or
       (nil=FEventPairInitData.NtSetLowEventPair) or
       (nil=FEventPairInitData.NtSetLowWaitHighEventPair) or
       (nil=FEventPairInitData.NtWaitHighEventPair) or
       (nil=FEventPairInitData.NtWaitLowEventPair) or
       (nil=FEventPairInitData.NtClose) then begin
      // very-very-very crazy (old? NT 3.51?) system!
      FillChar(FEventPairInitData, sizeof(FEventPairInitData), #0);
    end;
  end;
end;

initialization
{$if defined(DEBUG_GLOBAL_LOCKS)}
  DebugGlobalLocks_Enabled := FALSE;
{$ifend}

{$if defined(ALLOW_USE_EVENTPAIR)}
  GSynchronizerFactory := TSynchronizerFactoryWithEventPair.Create;
{$else}
  GSynchronizerFactory := TSynchronizerFactory.Create;
{$ifend}

end.
