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

// can create TSynchronizerCS or TSynchronizerCSSC
function MakeSyncObj(const ACreator: TObject; const AWithSpinLocks: Boolean = FALSE): IReadWriteSync;

// can create TSynchronizerFake or TSynchronizerCS
function MakeSyncFake(const ACreator: TObject): IReadWriteSync;

// can create TSynchronizerCSSC or TMultiReadExclusiveWriteSynchronizer
function MakeSyncMulti(const ACreator: TObject): IReadWriteSync;

{$if defined(DEBUG_GLOBAL_LOCKS)}
var
  DebugGlobalLocks_Enabled: Boolean;
{$ifend}

implementation

type
  TSynchronizerAbstract = class(TInterfacedObject)
  protected
    FCreatorClassName: ShortString;
  end;

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

function MakeSyncObj(const ACreator: TObject; const AWithSpinLocks: Boolean): IReadWriteSync;
begin
  if AWithSpinLocks then begin
    Result := TSynchronizerCSSC.Create(ACreator, 2048);
  end else begin
    Result := TSynchronizerCS.Create(ACreator);
  end;
end;

function MakeSyncFake(const ACreator: TObject): IReadWriteSync;
begin
  Result := TSynchronizerCS.Create(ACreator);  // TSynchronizerFake.Create; // for testing purposes
end;

function MakeSyncMulti(const ACreator: TObject): IReadWriteSync;
begin
  Result := TMultiReadExclusiveWriteSynchronizer.Create;  // TSynchronizerCSSC.Create(ACreator, 2048); // for testing purposes
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

{$if defined(DEBUG_GLOBAL_LOCKS)}
initialization
  DebugGlobalLocks_Enabled := FALSE;
{$ifend}

end.
