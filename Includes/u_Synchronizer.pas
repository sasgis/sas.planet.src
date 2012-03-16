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

/////{$define DEBUG_GLOBAL_LOCKS}

uses
  Windows,
  SysUtils;

// deprecated functions (use generic functions instead)

// can create TSynchronizerCS or TSynchronizerCSSC
function MakeSyncObj(const ACreator: TObject; const AWithSpinLocks: Boolean = FALSE): IReadWriteSync;

// can create TSynchronizerCSSC or TMultiReadExclusiveWriteSynchronizer
function MakeSyncMulti(const ACreator: TObject): IReadWriteSync;

// generic functions (Read and Write)

// makes first available from { MakeSyncSRW, MakeSyncRes, MakeSyncSpinLock }
// cannot be acquired recursively
// no upgrade or downgrade
// usually very short lightweight operations
function MakeSyncRWLight(const ACreator: TObject): IReadWriteSync;

// makes first available from { MakeSyncRes, MakeSyncSpinLock }
// small number of concurrent readers on short operation
function MakeSyncRWShort(const ACreator: TObject): IReadWriteSync;

// makes first available from { MakeSyncRes, MakeSyncMREW }
// small number of concurrent readers on long operation
function MakeSyncRWLong(const ACreator: TObject): IReadWriteSync;

// makes first available from { MakeSyncRes, MakeSyncMREW }
// many concurrent readers
function MakeSyncRWHuge(const ACreator: TObject): IReadWriteSync;


// generic functions (CS - Critical Section - should use only BeginWrite-EndWrite routines)

// makes first available from { MakeSyncSRW, MakeSyncSpinLock }
// cannot be acquired recursively
// no upgrade or downgrade
// usually very short lightweight operations
function MakeSyncCSLight(const ACreator: TObject): IReadWriteSync;

// makes first available from { MakeSyncSpinLock }
// small number of concurrent writers on short operation
function MakeSyncCSShort(const ACreator: TObject): IReadWriteSync;

// makes first available from { MakeSyncSection }
// small number of concurrent writers on long operation
function MakeSyncCSLong(const ACreator: TObject): IReadWriteSync;

// makes first available from { MakeSyncSpinLock }
// many concurrent writers
function MakeSyncCSHuge(const ACreator: TObject): IReadWriteSync;


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

  PVOID = Pointer;
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


  ISynchronizerFactory = interface
  ['{89031A22-CE84-4B4E-A8CE-F9B86BC1836C}']
    function GetSyncResInitData: PSyncTypeInitData;
    function GetSyncSRWInitData: PSyncTypeInitData;
  end;

  TSynchronizerFactory = class(TInterfacedObject, ISynchronizerFactory)
  private
    FResInitData: TSyncTypeInitData;
    FSRWInitData: TSyncTypeInitData;
  protected
    { ISynchronizerFactory }
    function GetSyncResInitData: PSyncTypeInitData;
    function GetSyncSRWInitData: PSyncTypeInitData;
  public
    constructor Create;
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

procedure RaiseNotAvailable(AClass: TClass);
begin
  raise ESynchronizerNotAvailable.Create(AClass.ClassName);
end;
  
// deprecated functions

function MakeSyncObj(const ACreator: TObject; const AWithSpinLocks: Boolean): IReadWriteSync;
begin
  if AWithSpinLocks then begin
    Result := TSynchronizerCSSC.Create(ACreator, 4096);
  end else begin
    Result := TSynchronizerCS.Create(ACreator);
  end;
end;

function MakeSyncMulti(const ACreator: TObject): IReadWriteSync;
begin
  Result := TMultiReadExclusiveWriteSynchronizer.Create;
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

// generic functions RW

// makes first available from { MakeSyncSRW, MakeSyncRes, MakeSyncSpinLock }
function MakeSyncRWLight(const ACreator: TObject): IReadWriteSync;
var FData: PSyncTypeInitData;
begin
  FData := GSynchronizerFactory.GetSyncSRWInitData;
  if (FData<>nil) then begin
    Result := TSynchronizerSRW.Create(ACreator, FData);
    Exit;
  end;

  FData := GSynchronizerFactory.GetSyncResInitData;
  if (FData<>nil) then
    Result := TSynchronizerRes.Create(ACreator, FData)
  else
    Result := TSynchronizerCSSC.Create(ACreator, 4096);
    //Result := TMultiReadExclusiveWriteSynchronizer.Create;
end;

// makes first available from { MakeSyncRes, MakeSyncSpinLock }
function MakeSyncRWShort(const ACreator: TObject): IReadWriteSync;
var FData: PSyncTypeInitData;
begin
  FData := GSynchronizerFactory.GetSyncResInitData;
  if (FData<>nil) then
    Result := TSynchronizerRes.Create(ACreator, FData)
  else
    Result := TSynchronizerCSSC.Create(ACreator, 4096);
end;

// makes first available from { MakeSyncRes, MakeSyncMREW }
function MakeSyncRWLong(const ACreator: TObject): IReadWriteSync;
var FData: PSyncTypeInitData;
begin
  FData := GSynchronizerFactory.GetSyncResInitData;
  if (FData<>nil) then
    Result := TSynchronizerRes.Create(ACreator, FData)
  else
    Result := TMultiReadExclusiveWriteSynchronizer.Create;
end;

// makes first available from { MakeSyncRes, MakeSyncMREW }
function MakeSyncRWHuge(const ACreator: TObject): IReadWriteSync;
var FData: PSyncTypeInitData;
begin
  FData := GSynchronizerFactory.GetSyncResInitData;
  if (FData<>nil) then
    Result := TSynchronizerRes.Create(ACreator, FData)
  else
    Result := TMultiReadExclusiveWriteSynchronizer.Create;
end;

// generic functions CS

// makes first available from { MakeSyncSRW, MakeSyncSpinLock }
function MakeSyncCSLight(const ACreator: TObject): IReadWriteSync;
var FData: PSyncTypeInitData;
begin
  FData := GSynchronizerFactory.GetSyncSRWInitData;
  if (FData<>nil) then begin
    Result := TSynchronizerSRW.Create(ACreator, FData);
    Exit;
  end;
  Result := TSynchronizerCSSC.Create(ACreator, 4096);
end;

// makes first available from { MakeSyncSpinLock }
function MakeSyncCSShort(const ACreator: TObject): IReadWriteSync;
begin
  Result := TSynchronizerCSSC.Create(ACreator, 4096);
end;

// makes first available from { MakeSyncSection }
function MakeSyncCSLong(const ACreator: TObject): IReadWriteSync;
begin
  Result := TSynchronizerCS.Create(ACreator);
end;

// makes first available from { MakeSyncSpinLock }
function MakeSyncCSHuge(const ACreator: TObject): IReadWriteSync;
begin
  Result := TSynchronizerCSSC.Create(ACreator, 2048);
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
  FCreatorClassName := ACreator.ClassName;
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
  FCreatorClassName := ACreator.ClassName;
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
var
  hDLL: THandle;
begin
  FillChar(FResInitData, sizeof(FResInitData), #0);
  FillChar(FSRWInitData, sizeof(FSRWInitData), #0);
  
  hDLL := GetModuleHandle('ntdll.dll');
  if (0<>hDLL) then begin
    // Resource
    FResInitData.InitializePtr := GetProcAddress(hDLL,'RtlInitializeResource');
    FResInitData.AcquireExclusivePtr := GetProcAddress(hDLL,'RtlAcquireResourceExclusive');
    FResInitData.ReleaseExclusivePtr := GetProcAddress(hDLL,'RtlReleaseResource');
    FResInitData.AcquireSharedPtr := GetProcAddress(hDLL,'RtlAcquireResourceShared');
    FResInitData.ReleaseSharedPtr := FResInitData.ReleaseExclusivePtr;
    FResInitData.UninitializePtr := GetProcAddress(hDLL,'RtlDeleteResource');
    if (nil=FResInitData.InitializePtr) or
       (nil=FResInitData.AcquireExclusivePtr) or
       (nil=FResInitData.ReleaseExclusivePtr) or
       (nil=FResInitData.AcquireSharedPtr) or
       (nil=FResInitData.ReleaseSharedPtr) or
       (nil=FResInitData.UninitializePtr) then begin
      // very-very-very crazy (old?) system!
      FillChar(FResInitData, sizeof(FResInitData), #0);
    end;

    // SRWLock
    FSRWInitData.InitializePtr := GetProcAddress(hDLL,'RtlInitializeSRWLock');
    if (nil=FSRWInitData.InitializePtr) then begin
      // before Vista
      // nothing
    end else begin
      // Vista and newer
      FSRWInitData.AcquireExclusivePtr := GetProcAddress(hDLL,'RtlAcquireSRWLockExclusive');
      FSRWInitData.ReleaseExclusivePtr := GetProcAddress(hDLL,'RtlReleaseSRWLockExclusive');
      FSRWInitData.AcquireSharedPtr := GetProcAddress(hDLL,'RtlAcquireSRWLockShared');
      FSRWInitData.ReleaseSharedPtr := GetProcAddress(hDLL,'RtlReleaseSRWLockShared');
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
  FCreatorClassName := ACreator.ClassName;
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
  FCreatorClassName := ACreator.ClassName;
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

initialization
{$if defined(DEBUG_GLOBAL_LOCKS)}
  DebugGlobalLocks_Enabled := FALSE;
{$ifend}
  GSynchronizerFactory := TSynchronizerFactory.Create;

end.
