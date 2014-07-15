{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{**************************************************************************************************}
{                                                                                                  }
{ MidasSpeedFix unit - Unoffical speed fix for Midas (Delphi/C++Builder 6 to 2009)                 }
{ Version 1.2 (2009-03-11)                                                                         }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is MidasSpeedFix.pas.                                                          }
{                                                                                                  }
{ The Initial Developer of the Original Code is Andreas Hausladen (Andreas.Hausladen@gmx.de).      }
{ Portions created by Andreas Hausladen are Copyright (C) 2009 Andreas Hausladen.                  }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{**************************************************************************************************}

{ History:
    2009-03-11  (1.2):
      - Added $HPPEMIT for C++Builder
      - Replaced assertion with a debug output string (EXE packed with EXE-packers can run into the
        assertion on some computers).
    2009-01-22  (1.1):
      - Improved Midas memory manager detection
      - Added support for special FastMM 4.92 unit (reduced to the minimum) for Delphi 6-2005
    2009-01-21  (1.0):
      - Initial implementation

  How to use:
    Add the MidasSpeedFix.pas unit to your Delphi or C++ Project (ProjectManager: Add Unit)
}

unit MidasSpeedFix;

{ Enable USE_MIDAS_FASTMM if you want to use the FastMM4 from the MidasFastMM4 unit.
  This is usefull if you work with Delphi/C++Builder prior to Delphi/BCB 2006. This
  option has no effect if you use Delphi/BCB 2006 or newer unless you also define
  FORCE_USE_MIDAS_FASTMM. }
{.$DEFINE USE_MIDAS_FASTMM}

{ Enable FORCE_USE_MIDAS_FASTMM to force Delphi/BCB 2006 or newer to use the FastMM4
  from the MidasFastMM4 unit. }
{.$DEFINE FORCE_USE_MIDAS_FASTMM}

interface

{$HPPEMIT '#pragma link "MidasSpeedFix"'}

{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 15.0}
    {$WARN UNSAFE_TYPE OFF}
    {$WARN UNSAFE_CODE OFF}
    {$WARN UNSAFE_CAST OFF}
  {$IFEND}
  {$IF CompilerVersion >= 18.0}
    {$IFNDEF FORCE_USE_MIDAS_FASTMM}
      {$UNDEF USE_MIDAS_FASTMM}
    {$ENDIF ~FORCE_USE_MIDAS_FASTMM}
  {$IFEND}
{$ENDIF}

implementation

uses
  {$IFDEF USE_MIDAS_FASTMM}
  MidasFastMM4,
  {$ENDIF USE_MIDAS_FASTMM}
  Windows, SysUtils, DSIntf;

{------------------------------------------------------------------------------}

{$IFDEF USE_MIDAS_FASTMM}

function MidasMalloc(Size: LongWord): Pointer; stdcall;
begin
  if Size = 0 then
    Result := nil
  else
    Result := FastGetMem(Size);
end;

procedure MidasFree(P: Pointer); stdcall;
begin
  if P <> nil then
    FastFreeMem(P);
end;

function MidasCalloc(Count, ElementSize: LongWord): Pointer; stdcall;
begin
  if (Count > 0) and (ElementSize > 0) then
    Result := FastAllocMem(Count * ElementSize)
  else
    Result := nil;
end;

function MidasRealloc(P: Pointer; OldSize, NewSize: LongWord): Pointer; stdcall;
begin
  if P = nil then
  begin
    if NewSize = 0 then
      Result := nil
    else
      Result := FastAllocMem(NewSize)
  end
  else if NewSize = 0 then
  begin
    FastFreeMem(P);
    Result := nil;
  end
  else if OldSize = NewSize then
    Result := P
  else
  begin
    Result := FastReallocMem(P, NewSize);
    if (Result <> nil) and (OldSize < NewSize) then
      FillChar(Pointer(PAnsiChar(Result) + OldSize)^, NewSize - OldSize, 0);
  end;
end;

{$ELSE USE_MIDAS_FASTMM}

function MidasMalloc(Size: LongWord): Pointer; stdcall;
begin
  GetMem(Result, Size);
end;

procedure MidasFree(P: Pointer); stdcall;
begin
  FreeMem(P);
end;

function MidasCalloc(Count, ElementSize: LongWord): Pointer; stdcall;
begin
  Result := AllocMem(Count * ElementSize);
end;

function MidasRealloc(P: Pointer; OldSize, NewSize: LongWord): Pointer; stdcall;
begin
  ReallocMem(P, NewSize);
  Result := P;
  if (Result <> nil) and (OldSize < NewSize) then
    FillChar(Pointer(PAnsiChar(Result) + OldSize)^, NewSize - OldSize, 0);
end;

{$ENDIF USE_MIDAS_FASTMM}

{------------------------------------------------------------------------------}

{$UNDEF REQUIRE_NEW_MEMORY_MANAGER}

{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion < 18.0} // Delphi 2006 was the first shipped with FastMM
    {$DEFINE REQUIRE_NEW_MEMORY_MANAGER}
  {$IFEND}
{$ELSE}
  {$DEFINE REQUIRE_NEW_MEMORY_MANAGER}
{$ENDIF CONDITIONALEXPRESSIONS}

{$IFDEF USE_MIDAS_FASTMM}
  {$UNDEF REQUIRE_NEW_MEMORY_MANAGER}
{$ENDIF USE_MIDAS_FASTMM}

type
  TJumpOfs = Integer;
  PPointer = ^Pointer;

type
  PXRedirCode = ^TXRedirCode;
  TXRedirCode = packed record
    Jump: Byte;
    Offset: TJumpOfs;
  end;

{ Hooking }

procedure HookProc(Proc, Dest: Pointer; var BackupCode: TXRedirCode);
var
  n: DWORD;
  Code: TXRedirCode;
begin
  Assert(Proc <> nil);
  if ReadProcessMemory(GetCurrentProcess, Proc, @BackupCode, SizeOf(BackupCode), n) then
  begin
    Code.Jump := $E9;
    Code.Offset := PAnsiChar(Dest) - PAnsiChar(Proc) - SizeOf(Code);
    WriteProcessMemory(GetCurrentProcess, Proc, @Code, SizeOf(Code), n);
  end;
end;

procedure UnhookProc(Proc: Pointer; var BackupCode: TXRedirCode);
var
  n: Cardinal;
begin
  if (BackupCode.Jump <> 0) and (Proc <> nil) then
  begin
    WriteProcessMemory(GetCurrentProcess, Proc, @BackupCode, SizeOf(BackupCode), n);
    BackupCode.Jump := 0;
  end;
end;

function FindMethodPtr(Start: THandle; const Bytes: array of Smallint): Pointer;
var
  P, Address, BytePtr: PAnsiChar;
  MemInfo: TMemoryBasicInformation;
  I: Cardinal;
  FirstByte: Byte;
  StartOffset, Index: Integer;
  BytesLen: Integer;
  Found: Boolean;
  AllocBase: Pointer;
begin
  Result := nil;
  BytesLen := Length(Bytes);
  if (Start <> 0) and (BytesLen >= 8) then
  begin
    StartOffset := 0;
    while (StartOffset < BytesLen) and (Bytes[StartOffset] = -1) do
      Inc(StartOffset);
    if StartOffset = BytesLen then
      Exit;
    FirstByte := Byte(Bytes[StartOffset]);

    try
      Address := PAnsiChar(Start);
      if not VirtualQuery(Address, MemInfo, SizeOf(MemInfo)) = SizeOf(MemInfo) then
        Exit;
      AllocBase := MemInfo.AllocationBase;

      while (VirtualQuery(Address, MemInfo, SizeOf(MemInfo)) = SizeOf(MemInfo)) and
            (MemInfo.AllocationBase = AllocBase) do
      begin
        if MemInfo.Protect and $000000F0 <> 0 then // PAGE_EXECUTE | ...
        begin
          P := Address;
          for I := 0 to MemInfo.RegionSize - 1 do
          begin
            if (Byte(P[0]) = FirstByte) and
              (Byte(P[1]) = Byte(Bytes[1])) and (Byte(P[2]) = Byte(Bytes[2])) and
              (Byte(P[3]) = Byte(Bytes[3])) and (Byte(P[4]) = Byte(Bytes[4])) then
            begin
              BytePtr := P + 1;
              Found := True;
              for Index := StartOffset + 1 to BytesLen - 1 do
              begin
                if (Bytes[Index] <> -1) and (Byte(Bytes[Index]) <> Byte(BytePtr^)) then
                begin
                  Found := False;
                  Break;
                end;
                Inc(BytePtr);
              end;
              if Found then
              begin
                Result := P;
                Exit;
              end;
            end;
            Inc(P);
          end;
        end;
        Inc(Address, MemInfo.RegionSize);
      end;
    except
      on E: EAccessViolation do
        ;
      on E: EPrivilege do
        ;
    end;
  end;
end;

{------------------------------------------------------------------------------}
const
  MidasMallocBytes: array[0..15] of SmallInt = (
    $55,                   // push ebp
    $8B, $EC,              // mov ebp,esp
    $FF, $75, $08,         // push dword ptr [ebp+$08]
    $E8, -1, -1, -1, -1,   // call _malloc
    $59,                   // pop ecx
    $5D,                   // pop ebp
    $C2, $04, $00          // ret $0004
  );

  MidasMallocOptBytes: array[0..16] of SmallInt = (
    $55,                   // push ebp
    $8B, $EC,              // mov ebp,esp
    $8B, $45, $08,         // mov eax,[ebp+$08]
    $50,                   // push eax
    $E8, -1, -1, -1, -1,   // call _malloc
    $59,                   // pop ecx
    $5D,                   // pop ebp
    $C2, $04, $00          // ret $0004
  );

  MidasMallocBytesGlobal: array[0..56] of SmallInt = (
    $55,                   // push ebp
    $8B, $EC,              // mov ebp,esp
    $53,                   // push ebx
    $56,                   // push esi
    $57,                   // push edi
    $33, $DB,              // xor ebx, ebx
    $66, $BE, $02, $00,    // mov si, $0002
    $FF, $75, $08,         // push dword ptr [ebp+$08]
    $0F, $BF, $C6,         // movsx eax,si
    $50,                   // push eax
    $E8, -1, -1, -1, -1,   // call GlobalAlloc
    $8B, $F8,              // mov edi,eax
    $85, $C0,              // test eax,eax
    $74, $12,              // jz +$12
    $57,                   // push edi
    $E8, -1, -1, -1, -1,   // call GlobalLock
    $8B, $D8,              // mov ebx,eax
    $85, $C0,              // test eax,eax
    $75, $06,              // jnz +$06
    $57,                   // push edi
    $E8, -1, -1, -1, -1,   // call GlobalFree
    $8B, $C3,              // mov eax,ebx
    $5F,                   // pop edi
    $5E,                   // pop esi
    $5B,                   // pop ebx
    $5D,                   // pop ebp
    $C2, $04, $00          // ret $0004
  );

  MidasFreeBytes: array[0..20] of SmallInt = (
    $55,                   // push ebp
    $8B, $EC,              // mov ebp,esp
    $8B, $45, $08,         // mov eax,[ebp+$08]
    $85, $C0,              // test eax,eax
    $74, $07,              // jz +$07
    $50,                   // push eax
    $E8, -1, -1, -1, -1,   // call _free
    $59,                   // pop ecx
    $5D,                   // pop ebp
    $C2, $04, $00          // ret $0004
  );

  MidasFreeBytesGlobal: array[0..39] of SmallInt = (
    $55,                   // push ebp
    $8B, $EC,              // mov ebp,esp
    $53,                   // push ebx
    $8B, $45, $08,         // mov eax,[ebp+$08]
    $85, $C0,              // test eax,eax
    $74, $18,              // jz +$18
    $50,                   // push eax
    $E8, -1, -1, -1, -1,   // call GlobalHandle
    $8B, $D8,              // mov ebx,eax
    $85, $DB,              // test ebx,ebx
    $74, $0C,              // jz +$0c
    $53,                   // puch ebx
    $E8, -1, -1, -1, -1,   // call GlobalUnlock
    $53,                   // push ebx
    $E8, -1, -1, -1, -1,   // call GlobalFree
    $5B,                   // pop ebx
    $5D,                   // pop ebp
    $C2, $04, $00          // ret $0004
  );

  MidasCallocBytes: array[0..43] of SmallInt = (
    $55,                   // push ebp
    $8B, $EC,              // mov ebp,esp
    $53,                   // push ebx
    $56,                   // push esi
    $8B, $75, $08,         // mov esi,[ebp+$08]
    $0F, $AF, $75, $0C,    // imul esi,[ebp+$0c]
    $56,                   // push esi
    $E8, -1, -1, -1, -1,   // call $004d2097 MidasMalloc
    $8B, $D8,              // mov ebx,eax
    $85, $DB,              // test ebx,ebx
    $74, $0C,              // jz +$0c
    $56,                   // push esi
    $6A, $00,              // push $00
    $53,                   // push ebx
    $E8, -1, -1, -1, -1,   // call _memset
    $83, $C4, $0C,         // add esp,$0c
    $8B, $C3,              // mov eax,ebx
    $5E,                   // pop esi
    $5B,                   // pop ebx
    $5D,                   // pop ebp
    $C2, $08, $00          // ret $0008
  );

  MidasReallocBytes: array[0..54] of SmallInt = (
    $55,                   // push ebp
    $8B, $EC,              // mov ebp,esp
    $53,                   // push ebx
    $56,                   // push esi
    $8B, $75, $08,         // mov esi,[ebp+$08]
    $FF, $75, $10,         // push dword ptr [ebp+$10]
    $6A, $01,              // push $01
    $E8, -1, -1, -1, -1,   // call $004d20a7 MidasCalloc
    $8B, $D8,              // mov ebx,eax
    $85, $DB,              // test ebx,ebx
    $74, $17,              // jz +$17
    $85, $F6,              // test esi,esi
    $74, $13,              // jz +$13
    $FF, $75, $0C,         // push dword ptr [ebp+$0c]
    $56,                   // push esi
    $53,                   // push ebx
    $E8, -1, -1, -1, -1,   // call _memmove
    $83, $C4, $0C,         // add esp,$0c
    $56,                   // push esi
    $E8, -1, -1, -1, -1,   // call $004d20d3  MidasFree
    $8B, $C3,              // mov eax,ebx
    $5E,                   // pop esi
    $5B,                   // pop ebx
    $5D,                   // pop ebp
    $C2, $0C, $00          // ret $000c
  );

  MidasReallocNewBytes: array[0..40] of SmallInt = (
    $55,                   // push ebp
    $8B, $EC,              // mov ebp,esp
    $53,                   // push ebx
    $56,                   // push esi
    $57,                   // push edi
    $8B, $75, $10,         // mov esi,[ebp+$10]
    $8B, $7D, $0C,         // mov edi,[ebp+$0c]
    $8B, $45, $08,         // mov eax,[ebp+$08]
    $85, $C0,              // test eax,eax
    $75, $0A,              // jnz $00c12119
    $56,                   // push esi
    $6A, $01,              // push $01
    $E8, -1, -1, -1, -1,   // call MidasCalloc
    $EB, $26,              // jmp $00c1213f
    $56,                   // push esi
    $50,                   // push eax
    $E8, -1, -1, -1, -1,   // call _realloc
    $83, $C4, $08,         // add esp,$08
    $8B, $D8               // mov ebx,eax
  );

var
  Org_MidasMalloc: Pointer;
  Org_MidasFree: Pointer;
  Org_MidasCalloc: Pointer;
  Org_MidasRealloc: Pointer;

  MidasMallocHook: TXRedirCode;
  MidasFreeHook: TXRedirCode;
  MidasCallocHook: TXRedirCode;
  MidasReallocHook: TXRedirCode;

{$IFDEF REQUIRE_NEW_MEMORY_MANAGER}
function RequireNewerMemoryManager: Boolean;
var
  MemMan: TMemoryManager;
begin
  GetMemoryManager(MemMan);
  if @MemMan.GetMem = @SysGetMem then
  begin
    MessageBox(0, 'You must install an alternative memory manager like FastMM4. FastMM4 must run ' +
               'in RELEASE mode to see an improved performance.',
               'Delphi - Midas Speed Fix', MB_OK or MB_ICONERROR);
    Result := False;
  end
  else
    Result := True;
end;
{$ENDIF REQUIRE_NEW_MEMORY_MANAGER}

procedure Init;
var
  DSBase: IDSBase;
  IsCRTLMemMan: Boolean;
  IsMidasLib: Boolean;
  MidasDllHandle: HMODULE;
begin
  {$IFDEF REQUIRE_NEW_MEMORY_MANAGER}
  if not RequireNewerMemoryManager then
    Exit;
  {$ENDIF REQUIRE_NEW_MEMORY_MANAGER}
  IsCRTLMemMan := True; // C RTL memory manager
  IsMidasLib := True;

  { Find in MidasLib }
  Org_MidasMalloc := FindMethodPtr(HInstance, MidasMallocBytes);
  if Org_MidasMalloc = nil then
  begin
    Org_MidasMalloc := FindMethodPtr(HInstance, MidasMallocOptBytes);
    if Org_MidasMalloc = nil then
    begin
      Org_MidasMalloc := FindMethodPtr(HInstance, MidasMallocBytesGlobal);
      if Org_MidasMalloc <> nil then
        IsCRTLMemMan := False;
    end;
  end;

  if Org_MidasMalloc = nil then
  begin
    CreateDbClientObject(CLSID_DSBase, IDSBase, DSBase); // load midas.dll
    DSBase := nil; // release alloctated midas memory

    MidasDllHandle := GetModuleHandle('midas.dll');
    if MidasDllHandle <> 0 then
    begin
      IsMidasLib := False;
      Org_MidasMalloc := FindMethodPtr(MidasDllHandle, MidasMallocBytes);
      if Org_MidasMalloc = nil then
      begin
        Org_MidasMalloc := FindMethodPtr(MidasDllHandle, MidasMallocOptBytes);
        if Org_MidasMalloc = nil then
        begin
          Org_MidasMalloc := FindMethodPtr(MidasDllHandle, MidasMallocBytesGlobal);
          if Org_MidasMalloc <> nil then
            IsCRTLMemMan := False;
        end;
      end;
    end;
  end;

  if not IsMidasLib and (Org_MidasMalloc = nil) then
  begin
    { Do not raise EAssert if the application links to a not supported Midas.dll }
    OutputDebugString('MidasSpeedFix could not be installed.');
    Exit;
  end;
  if Org_MidasMalloc = nil then
  begin
    OutputDebugString('MidasSpeedFix and MidasLib must be in the same module/package. The usage the EXE-packers (like UPX) is not supported.');
    Exit;
  end;
  //Assert(Org_MidasMalloc <> nil, 'MidasSpeedFix and MidasLib must be in the same module/package');


  if IsCRTLMemMan then
    Org_MidasFree := FindMethodPtr(HINST(Org_MidasMalloc), MidasFreeBytes)
  else
    Org_MidasFree := FindMethodPtr(HINST(Org_MidasMalloc), MidasFreeBytesGlobal);

  Org_MidasCalloc := FindMethodPtr(HINST(Org_MidasMalloc), MidasCallocBytes);

  Org_MidasRealloc := FindMethodPtr(HINST(Org_MidasMalloc), MidasReallocBytes);
  if Org_MidasRealloc = nil then
  begin
    Org_MidasRealloc := FindMethodPtr(HINST(Org_MidasMalloc), MidasReallocNewBytes);

    { Do nothing if the MidasLib is used and the MidasRealloc is already the new code }
    if (Org_MidasRealloc <> nil) and IsMidasLib then
      Exit;
  end;

  Assert(Org_MidasFree <> nil);
  Assert(Org_MidasCalloc <> nil);
  Assert(Org_MidasRealloc <> nil);

  HookProc(Org_MidasMalloc, @MidasMalloc, MidasMallocHook);
  HookProc(Org_MidasFree, @MidasFree, MidasFreeHook);
  HookProc(Org_MidasCalloc, @MidasCalloc, MidasCallocHook);
  HookProc(Org_MidasRealloc, @MidasRealloc, MidasReallocHook);
end;

procedure Fini;
begin
  if Org_MidasMalloc <> nil then
  begin
    UnhookProc(Org_MidasMalloc, MidasMallocHook);
    UnhookProc(Org_MidasFree, MidasFreeHook);
    UnhookProc(Org_MidasCalloc, MidasCallocHook);
    UnhookProc(Org_MidasRealloc, MidasReallocHook);
  end;
end;

initialization
  Init;

finalization
  Fini;

end.

