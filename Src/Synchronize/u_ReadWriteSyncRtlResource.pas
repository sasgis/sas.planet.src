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

unit u_ReadWriteSyncRtlResource;

interface

uses
  Windows,
  SysUtils,
  i_ReadWriteSyncFactory;

type
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

  ISyncronizerRtlResourceDll = interface
    ['{D0071030-3989-4438-90E6-935DC4E5DC0E}']
    procedure Initialize(rwl: LPRTL_RWLOCK);
    procedure Delete(rwl: LPRTL_RWLOCK);
    function AcquireExclusive(rwl: LPRTL_RWLOCK; fWait: Byte): Byte;
    function AcquireShared(rwl: LPRTL_RWLOCK; fWait: Byte): Byte;
    procedure Release(rwl: LPRTL_RWLOCK);
  end;

  TSyncronizerRtlResourceDll = class(TInterfacedObject, ISyncronizerRtlResourceDll)
  private
    FInitializePtr: Pointer;
    FUninitializePtr: Pointer;
    FAcquireExclusivePtr: Pointer;
    FAcquireSharedPtr: Pointer;
    FReleasePtr: Pointer;
  private
    procedure Initialize(rwl: LPRTL_RWLOCK);
    procedure Delete(rwl: LPRTL_RWLOCK);
    function AcquireExclusive(rwl: LPRTL_RWLOCK; fWait: Byte): Byte;
    function AcquireShared(rwl: LPRTL_RWLOCK; fWait: Byte): Byte;
    procedure Release(rwl: LPRTL_RWLOCK);
  public
    constructor Create(
      AInitializePtr: Pointer;
      AUninitializePtr: Pointer;
      AAcquireExclusivePtr: Pointer;
      AAcquireSharedPtr: Pointer;
      AReleasePtr: Pointer
    );
  end;

  TSynchronizerRtlResource = class(TInterfacedObject, IReadWriteSync)
  private
    FDll: ISyncronizerRtlResourceDll;
    FLock: RTL_RWLOCK;
  protected
    { IReadWriteSync }
    procedure BeginRead;
    procedure EndRead;
    function BeginWrite: Boolean;
    procedure EndWrite;
  public
    constructor Create(
      const ADll: ISyncronizerRtlResourceDll
    );
    destructor Destroy; override;
  end;

  TSynchronizerRtlResourceFactory = class(TInterfacedObject, IReadWriteSyncFactory)
  private
    FDll: ISyncronizerRtlResourceDll;
  private
    function Make(const AName: string): IReadWriteSync;
  public
    constructor Create(
      const ADll: ISyncronizerRtlResourceDll
    );
  end;

function MakeSynchronizerRtlResourceFactory: IReadWriteSyncFactory;

implementation

type
  TRtlAcquireResourceExclusive = function(rwl: LPRTL_RWLOCK; fWait: Byte): Byte; stdcall;
  // RtlAcquireResourceShared is the same
  TRtlInitializeResource = procedure(rwl: LPRTL_RWLOCK); stdcall;
  // RtlDeleteResource is the same
  // RtlDumpResource is the same
  // RtlReleaseResource is the same

{ TSyncronizerRtlResourceDll }

constructor TSyncronizerRtlResourceDll.Create(
  AInitializePtr, AUninitializePtr,
  AAcquireExclusivePtr, AAcquireSharedPtr, AReleasePtr: Pointer
);
begin
  Assert(Assigned(AInitializePtr));
  Assert(Assigned(AUninitializePtr));
  Assert(Assigned(AAcquireExclusivePtr));
  Assert(Assigned(AAcquireSharedPtr));
  Assert(Assigned(AReleasePtr));
  inherited Create;
  FInitializePtr := AInitializePtr;
  FUninitializePtr := AUninitializePtr;
  FAcquireExclusivePtr := AAcquireExclusivePtr;
  FAcquireSharedPtr := AAcquireSharedPtr;
  FReleasePtr := AReleasePtr;
end;

function TSyncronizerRtlResourceDll.AcquireExclusive(
  rwl: LPRTL_RWLOCK;
  fWait: Byte
): Byte;
begin
  Result := TRtlAcquireResourceExclusive(FAcquireExclusivePtr)(rwl, fWait);
end;

function TSyncronizerRtlResourceDll.AcquireShared(rwl: LPRTL_RWLOCK;
  fWait: Byte): Byte;
begin
  Result := TRtlAcquireResourceExclusive(FAcquireSharedPtr)(rwl, fWait);
end;

procedure TSyncronizerRtlResourceDll.Delete(rwl: LPRTL_RWLOCK);
begin
  TRtlInitializeResource(FUninitializePtr)(rwl);
end;

procedure TSyncronizerRtlResourceDll.Initialize(rwl: LPRTL_RWLOCK);
begin
  TRtlInitializeResource(FInitializePtr)(rwl);
end;

procedure TSyncronizerRtlResourceDll.Release(rwl: LPRTL_RWLOCK);
begin
  TRtlInitializeResource(FReleasePtr)(rwl);
end;

{ TSynchronizerRtlResource }

constructor TSynchronizerRtlResource.Create(
  const ADll: ISyncronizerRtlResourceDll
);
begin
  Assert(ADll <> nil);
  inherited Create;
  FDll := ADll;
  FDll.Initialize(@FLock);
end;

destructor TSynchronizerRtlResource.Destroy;
begin
  if FDll <> nil then begin
    FDll.Delete(@FLock);
    FDll := nil;
  end;

  inherited;
end;

procedure TSynchronizerRtlResource.BeginRead;
begin
  FDll.AcquireShared(@FLock, 1);
end;

function TSynchronizerRtlResource.BeginWrite: Boolean;
begin
  Result := FDll.AcquireExclusive(@FLock, 1) <> 0;
end;

procedure TSynchronizerRtlResource.EndRead;
begin
  FDll.Release(@FLock);
end;

procedure TSynchronizerRtlResource.EndWrite;
begin
  FDll.Release(@FLock);
end;

{ TSynchronizerRtlResourceFactory }

constructor TSynchronizerRtlResourceFactory.Create(
  const ADll: ISyncronizerRtlResourceDll);
begin
  Assert(ADll <> nil);
  inherited Create;
  FDll := ADll;
end;

function TSynchronizerRtlResourceFactory.Make(
  const AName: string): IReadWriteSync;
begin
  Result := TSynchronizerRtlResource.Create(FDll);
end;

function MakeSynchronizerRtlResourceFactory: IReadWriteSyncFactory;
var
  VDllHandle: THandle;
  VInitializePtr: Pointer;
  VUninitializePtr: Pointer;
  VAcquireExclusivePtr: Pointer;
  VAcquireSharedPtr: Pointer;
  VReleasePtr: Pointer;
  VDll: ISyncronizerRtlResourceDll;
begin
  Result := nil;
  VDllHandle := GetModuleHandle('ntdll.dll');
  // Resource
  if (0<>VDllHandle) then begin
    VInitializePtr := GetProcAddress(VDllHandle,'RtlInitializeResource');
    if VInitializePtr <> nil then begin
      VAcquireExclusivePtr := GetProcAddress(VDllHandle,'RtlAcquireResourceExclusive');
      VReleasePtr := GetProcAddress(VDllHandle,'RtlReleaseResource');
      VAcquireSharedPtr := GetProcAddress(VDllHandle,'RtlAcquireResourceShared');
      VUninitializePtr := GetProcAddress(VDllHandle,'RtlDeleteResource');
      if
        (VInitializePtr <> nil) and
        (VAcquireExclusivePtr <> nil) and
        (VReleasePtr <> nil) and
        (VAcquireSharedPtr <> nil) and
        (VUninitializePtr <> nil)
      then begin
        VDll :=
          TSyncronizerRtlResourceDll.Create(
            VInitializePtr,
            VUninitializePtr,
            VAcquireExclusivePtr,
            VAcquireSharedPtr,
            VReleasePtr
          );
        Result := TSynchronizerRtlResourceFactory.Create(VDll);
      end;
    end;
  end;
end;

end.
