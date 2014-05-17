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

unit u_ReadWriteSyncSRW;

interface

uses
  Windows,
  SysUtils,
  i_ReadWriteSyncFactory;

type
  PVOID = Pointer;
  RTL_SRWLOCK = packed record
    Ptr: PVOID;
  end;
  PSRWLOCK = ^RTL_SRWLOCK;

  ISyncronizerSRWDll = interface
    ['{D0071030-3989-4438-90E6-935DC4E5DC0E}']
    procedure Initialize(SRWLock: PSRWLOCK);
    procedure AcquireExclusive(SRWLock: PSRWLOCK);
    procedure ReleaseExclusive(SRWLock: PSRWLOCK);
    procedure AcquireShared(SRWLock: PSRWLOCK);
    procedure ReleaseShared(SRWLock: PSRWLOCK);
  end;

  TSyncronizerSRWDll = class(TInterfacedObject, ISyncronizerSRWDll)
  private
    FInitializePtr: Pointer;
    FAcquireExclusivePtr: Pointer;
    FAcquireSharedPtr: Pointer;
    FReleaseExclusivePtr: Pointer;
    FReleaseSharedPtr: Pointer;
  private
    procedure Initialize(SRWLock: PSRWLOCK);
    procedure AcquireExclusive(SRWLock: PSRWLOCK);
    procedure ReleaseExclusive(SRWLock: PSRWLOCK);
    procedure AcquireShared(SRWLock: PSRWLOCK);
    procedure ReleaseShared(SRWLock: PSRWLOCK);
  public
    constructor Create(
      AInitializePtr: Pointer;
      AAcquireExclusivePtr: Pointer;
      AReleaseExclusivePtr: Pointer;
      AAcquireSharedPtr: Pointer;
      AReleaseSharedPtr: Pointer
    );
  end;

  TSynchronizerSRW = class(TInterfacedObject, IReadWriteSync)
  private
    FDll: ISyncronizerSRWDll;
    FLock: RTL_SRWLOCK;
  protected
    { IReadWriteSync }
    procedure BeginRead;
    procedure EndRead;
    function BeginWrite: Boolean;
    procedure EndWrite;
  public
    constructor Create(
      const ADll: ISyncronizerSRWDll
    );
  end;

  TSynchronizerSRWFactory = class(TInterfacedObject, IReadWriteSyncFactory)
  private
    FDll: ISyncronizerSRWDll;
  private
    function Make(const AName: AnsiString): IReadWriteSync;
  public
    constructor Create(
      const ADll: ISyncronizerSRWDll
    );
  end;

function MakeSynchronizerSRWFactory: IReadWriteSyncFactory;

implementation

type
  TRtlInitializeSRWLock = procedure(SRWLock: PSRWLOCK); stdcall;
  // RtlAcquireSRWLockExclusive is the same
  // RtlReleaseSRWLockExclusive is the same
  // RtlAcquireSRWLockShared is the same
  // RtlReleaseSRWLockShared is the same

{ TSyncronizerRtlResourceDll }

constructor TSyncronizerSRWDll.Create(
  AInitializePtr: Pointer;
  AAcquireExclusivePtr: Pointer;
  AReleaseExclusivePtr: Pointer;
  AAcquireSharedPtr: Pointer;
  AReleaseSharedPtr: Pointer
);
begin
  Assert(Assigned(AInitializePtr));
  Assert(Assigned(AAcquireExclusivePtr));
  Assert(Assigned(AReleaseExclusivePtr));
  Assert(Assigned(AAcquireSharedPtr));
  Assert(Assigned(AReleaseSharedPtr));
  inherited Create;
  FInitializePtr := AInitializePtr;
  FAcquireExclusivePtr := AAcquireExclusivePtr;
  FReleaseExclusivePtr := AReleaseExclusivePtr;
  FAcquireSharedPtr := AAcquireSharedPtr;
  FReleaseSharedPtr := AReleaseSharedPtr;
end;

procedure TSyncronizerSRWDll.AcquireExclusive(
  SRWLock: PSRWLOCK
);
begin
  TRtlInitializeSRWLock(FAcquireExclusivePtr)(SRWLock);
end;

procedure TSyncronizerSRWDll.AcquireShared(
  SRWLock: PSRWLOCK
);
begin
  TRtlInitializeSRWLock(FAcquireSharedPtr)(SRWLock);
end;

procedure TSyncronizerSRWDll.Initialize(SRWLock: PSRWLOCK);
begin
  TRtlInitializeSRWLock(FInitializePtr)(SRWLock);
end;

procedure TSyncronizerSRWDll.ReleaseExclusive(SRWLock: PSRWLOCK);
begin
  TRtlInitializeSRWLock(FReleaseExclusivePtr)(SRWLock);
end;

procedure TSyncronizerSRWDll.ReleaseShared(SRWLock: PSRWLOCK);
begin
  TRtlInitializeSRWLock(FReleaseSharedPtr)(SRWLock);
end;

{ TSynchronizerRtlResource }

constructor TSynchronizerSRW.Create(
  const ADll: ISyncronizerSRWDll
);
begin
  Assert(ADll <> nil);
  inherited Create;
  FDll := ADll;
  FDll.Initialize(@FLock);
end;


procedure TSynchronizerSRW.BeginRead;
begin
  FDll.AcquireShared(@FLock);
end;

function TSynchronizerSRW.BeginWrite: Boolean;
begin
  FDll.AcquireExclusive(@FLock);
  Result := False;
end;

procedure TSynchronizerSRW.EndRead;
begin
  FDll.ReleaseShared(@FLock);
end;

procedure TSynchronizerSRW.EndWrite;
begin
  FDll.ReleaseExclusive(@FLock);
end;

{ TSynchronizerRtlResourceFactory }

constructor TSynchronizerSRWFactory.Create(
  const ADll: ISyncronizerSRWDll);
begin
  Assert(ADll <> nil);
  inherited Create;
  FDll := ADll;
end;

function TSynchronizerSRWFactory.Make(
  const AName: AnsiString): IReadWriteSync;
begin
  Result := TSynchronizerSRW.Create(FDll);
end;

function MakeSynchronizerSRWFactory: IReadWriteSyncFactory;
var
  VDllHandle: THandle;
  VInitializePtr: Pointer;
  VAcquireExclusivePtr: Pointer;
  VAcquireSharedPtr: Pointer;
  VReleaseExclusivePtr: Pointer;
  VReleaseSharedPtr: Pointer;
  VDll: ISyncronizerSRWDll;
begin
  Result := nil;
  VDllHandle := GetModuleHandle('ntdll.dll');
  // Resource
  if (0<>VDllHandle) then begin
    VInitializePtr := GetProcAddress(VDllHandle,'RtlInitializeSRWLock');
    if VInitializePtr <> nil then begin
      // Vista and newer
      VAcquireExclusivePtr := GetProcAddress(VDllHandle,'RtlAcquireSRWLockExclusive');
      VReleaseExclusivePtr := GetProcAddress(VDllHandle,'RtlReleaseSRWLockExclusive');
      VAcquireSharedPtr := GetProcAddress(VDllHandle,'RtlAcquireSRWLockShared');
      VReleaseSharedPtr := GetProcAddress(VDllHandle,'RtlReleaseSRWLockShared');
      if
        (VInitializePtr <> nil) and
        (VAcquireExclusivePtr <> nil) and
        (VReleaseExclusivePtr <> nil) and
        (VAcquireSharedPtr <> nil) and
        (VReleaseSharedPtr <> nil)
      then begin
        VDll :=
          TSyncronizerSRWDll.Create(
            VInitializePtr,
            VAcquireExclusivePtr,
            VReleaseExclusivePtr,
            VAcquireSharedPtr,
            VReleaseSharedPtr
          );
        Result := TSynchronizerSRWFactory.Create(VDll);
      end;
    end;
  end;
end;

end.
