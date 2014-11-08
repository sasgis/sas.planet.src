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

unit u_ReadWriteSyncAbstract;

interface

uses
  SysUtils,
  i_InternalPerformanceCounter,
  i_ReadWriteSyncFactory;

type
  TSynchronizerFake = class(TInterfacedObject, IReadWriteSync)
  private
    procedure BeginRead;
    procedure EndRead;
    function BeginWrite: Boolean;
    procedure EndWrite;
  end;

  TReadWriteSyncDebugWrapper = class(TInterfacedObject, IReadWriteSync)
  private
    FLock: IReadWriteSync;
    FLockClassName: string;
    FName: string;
  protected
    procedure DoDebugGlobalLocks(const AProcedure, AEvent: string);
  private
    procedure BeginRead;
    procedure EndRead;
    function BeginWrite: Boolean;
    procedure EndWrite;
  public
    constructor Create(
      const ALock: IReadWriteSync;
      const ALockClassName: string;
      const AName: string
    );
  end;

  TReadWriteSyncCounterWrapper = class(TInterfacedObject, IReadWriteSync)
  private
    FLock: IReadWriteSync;
    FBeginReadCounter: IInternalPerformanceCounter;
    FEndReadCounter: IInternalPerformanceCounter;
    FBeginWriteCounter: IInternalPerformanceCounter;
    FEndWriteCounter: IInternalPerformanceCounter;
    FDestroyCounter: IInternalPerformanceCounter;
  private
    procedure BeginRead;
    procedure EndRead;
    function BeginWrite: Boolean;
    procedure EndWrite;
  public
    constructor Create(
      const ALock: IReadWriteSync;
      const ABeginReadCounter: IInternalPerformanceCounter;
      const AEndReadCounter: IInternalPerformanceCounter;
      const ABeginWriteCounter: IInternalPerformanceCounter;
      const AEndWriteCounter: IInternalPerformanceCounter;
      const ADestroyCounter: IInternalPerformanceCounter
    );
    destructor Destroy; override;
  end;

  TSynchronizerFakeFactory = class(TInterfacedObject, IReadWriteSyncFactory)
  private
    function Make(const AName: string): IReadWriteSync;
  end;

  TSynchronizerMREWFactory = class(TInterfacedObject, IReadWriteSyncFactory)
  private
    function Make(const AName: string): IReadWriteSync;
  end;

  TSynchronizerFactoryWithDebug = class(TInterfacedObject, IReadWriteSyncFactory)
  private
    FFactory: IReadWriteSyncFactory;
    FLockClassName: string;
  private
    function Make(const AName: string): IReadWriteSync;
  public
    constructor Create(
      const AFactory: IReadWriteSyncFactory;
      const ALockClassName: string
    );
  end;

  TSynchronizerFactoryWithCounters = class(TInterfacedObject, IReadWriteSyncFactory)
  private
    FFactory: IReadWriteSyncFactory;
    FBeginReadCounter: IInternalPerformanceCounter;
    FEndReadCounter: IInternalPerformanceCounter;
    FBeginWriteCounter: IInternalPerformanceCounter;
    FEndWriteCounter: IInternalPerformanceCounter;
    FDestroyCounter: IInternalPerformanceCounter;
  private
    function Make(const AName: string): IReadWriteSync;
  public
    constructor Create(
      const AFactory: IReadWriteSyncFactory;
      const ABeginReadCounter: IInternalPerformanceCounter;
      const AEndReadCounter: IInternalPerformanceCounter;
      const ABeginWriteCounter: IInternalPerformanceCounter;
      const AEndWriteCounter: IInternalPerformanceCounter;
      const ADestroyCounter: IInternalPerformanceCounter
    );
  end;

  TSynchronizerFactoryWithMakeCounter = class(TInterfacedObject, IReadWriteSyncFactory)
  private
    FFactory: IReadWriteSyncFactory;
    FCounter: IInternalPerformanceCounter;
  private
    function Make(const AName: string): IReadWriteSync;
  public
    constructor Create(
      const AFactory: IReadWriteSyncFactory;
      const ACounter: IInternalPerformanceCounter
    );
  end;


var
  DebugGlobalLocks_Enabled: Boolean;

implementation

uses
  Windows;

{ TSynchronizerFake }

procedure TSynchronizerFake.BeginRead;
begin
  // Do nothing
end;

function TSynchronizerFake.BeginWrite: Boolean;
begin
  // Do nothing
  Result := False;
end;

procedure TSynchronizerFake.EndRead;
begin
  // Do nothing
end;

procedure TSynchronizerFake.EndWrite;
begin
  // Do nothing
end;

{ TSynchronizerFakeFactory }

function TSynchronizerFakeFactory.Make(const AName: string): IReadWriteSync;
begin
  Result := TSynchronizerFake.Create;
end;

{ TReadWriteSyncDebugWrapper }

constructor TReadWriteSyncDebugWrapper.Create(
  const ALock: IReadWriteSync;
  const ALockClassName: string;
  const AName: string
);
begin
  Assert(ALock <> nil);
  inherited Create;
  FLock := ALock;
  FLockClassName := ALockClassName;
  FName := AName;
end;

procedure TReadWriteSyncDebugWrapper.BeginRead;
begin
  DoDebugGlobalLocks('BeginRead', 'IN');
  FLock.BeginRead;
  DoDebugGlobalLocks('BeginRead', 'OUT');
end;

function TReadWriteSyncDebugWrapper.BeginWrite: Boolean;
begin
  DoDebugGlobalLocks('BeginWrite', 'IN');
  Result := FLock.BeginWrite;
  DoDebugGlobalLocks('BeginWrite', 'OUT');
end;

procedure TReadWriteSyncDebugWrapper.DoDebugGlobalLocks(const AProcedure,
  AEvent: string);
const
  c_SEP: string = ', ' + Chr(VK_TAB);
var
  VText: string;
begin
  if (not DebugGlobalLocks_Enabled) then begin
    Exit;
  end;
  VText := FLockClassName + ' at $' + IntToHex(Integer(Pointer(Self)), 8) + ' (from ' + FName + ')' + c_SEP + 'ThreadId=' + IntToStr(GetCurrentThreadId) + c_SEP + AProcedure + c_SEP + AEvent;
  OutputDebugString(PChar(VText));
end;

procedure TReadWriteSyncDebugWrapper.EndRead;
begin
  DoDebugGlobalLocks('EndRead', 'IN');
  FLock.EndRead;
  DoDebugGlobalLocks('EndRead', 'OUT');
end;

procedure TReadWriteSyncDebugWrapper.EndWrite;
begin
  DoDebugGlobalLocks('EndWrite', 'IN');
  FLock.EndWrite;
  DoDebugGlobalLocks('EndWrite', 'OUT');
end;

{ TReadWriteSyncCounterWrapper }

constructor TReadWriteSyncCounterWrapper.Create(
  const ALock: IReadWriteSync;
  const ABeginReadCounter: IInternalPerformanceCounter;
  const AEndReadCounter: IInternalPerformanceCounter;
  const ABeginWriteCounter: IInternalPerformanceCounter;
  const AEndWriteCounter: IInternalPerformanceCounter;
  const ADestroyCounter: IInternalPerformanceCounter
);
begin
  Assert(ALock <> nil);
  inherited Create;
  FLock := ALock;
  FBeginReadCounter := ABeginReadCounter;
  FEndReadCounter := AEndReadCounter;
  FBeginWriteCounter := ABeginWriteCounter;
  FEndWriteCounter := AEndWriteCounter;
  FDestroyCounter := ADestroyCounter;
end;

destructor TReadWriteSyncCounterWrapper.Destroy;
var
  VCounter: Pointer;
  VContext: TInternalPerformanceCounterContext;
begin
  VCounter := Pointer(FDestroyCounter);
  if VCounter <> nil then begin
    VContext := IInternalPerformanceCounter(VCounter).StartOperation;
    FLock := nil;
    FBeginReadCounter := nil;
    FEndReadCounter := nil;
    FBeginWriteCounter := nil;
    FEndWriteCounter := nil;
    inherited;
    IInternalPerformanceCounter(VCounter).FinishOperation(VContext);
  end else begin
    FLock := nil;
    FBeginReadCounter := nil;
    FEndReadCounter := nil;
    FBeginWriteCounter := nil;
    FEndWriteCounter := nil;
    inherited;
  end;
end;

procedure TReadWriteSyncCounterWrapper.BeginRead;
var
  VCounter: Pointer;
  VContext: TInternalPerformanceCounterContext;
begin
  VCounter := Pointer(FBeginReadCounter);
  if VCounter <> nil then begin
    VContext := IInternalPerformanceCounter(VCounter).StartOperation;
    FLock.BeginRead;
    IInternalPerformanceCounter(VCounter).FinishOperation(VContext);
  end else begin
    FLock.BeginRead;
  end;
end;

function TReadWriteSyncCounterWrapper.BeginWrite: Boolean;
var
  VCounter: Pointer;
  VContext: TInternalPerformanceCounterContext;
begin
  VCounter := Pointer(FBeginWriteCounter);
  if VCounter <> nil then begin
    VContext := IInternalPerformanceCounter(VCounter).StartOperation;
    Result := FLock.BeginWrite;
    IInternalPerformanceCounter(VCounter).FinishOperation(VContext);
  end else begin
    Result := FLock.BeginWrite;
  end;
end;

procedure TReadWriteSyncCounterWrapper.EndRead;
var
  VCounter: Pointer;
  VContext: TInternalPerformanceCounterContext;
begin
  VCounter := Pointer(FEndReadCounter);
  if VCounter <> nil then begin
    VContext := IInternalPerformanceCounter(VCounter).StartOperation;
    FLock.EndRead;
    IInternalPerformanceCounter(VCounter).FinishOperation(VContext);
  end else begin
    FLock.EndRead;
  end;
end;

procedure TReadWriteSyncCounterWrapper.EndWrite;
var
  VCounter: Pointer;
  VContext: TInternalPerformanceCounterContext;
begin
  VCounter := Pointer(FEndWriteCounter);
  if VCounter <> nil then begin
    VContext := IInternalPerformanceCounter(VCounter).StartOperation;
    FLock.EndWrite;
    IInternalPerformanceCounter(VCounter).FinishOperation(VContext);
  end else begin
    FLock.EndWrite;
  end;
end;

{ TSynchronizerFactoryWithDebug }

constructor TSynchronizerFactoryWithDebug.Create(
  const AFactory: IReadWriteSyncFactory;
  const ALockClassName: string
);
begin
  Assert(AFactory <> nil);
  inherited Create;
  FFactory := AFactory;
  FLockClassName := ALockClassName;
end;

function TSynchronizerFactoryWithDebug.Make(
  const AName: string
): IReadWriteSync;
begin
  Result :=
    TReadWriteSyncDebugWrapper.Create(
      FFactory.Make(AName),
      FLockClassName,
      AName
    );
end;

{ TSynchronizerFactoryWithCounters }

constructor TSynchronizerFactoryWithCounters.Create(
  const AFactory: IReadWriteSyncFactory;
  const ABeginReadCounter: IInternalPerformanceCounter;
  const AEndReadCounter: IInternalPerformanceCounter;
  const ABeginWriteCounter: IInternalPerformanceCounter;
  const AEndWriteCounter: IInternalPerformanceCounter;
  const ADestroyCounter: IInternalPerformanceCounter
);
begin
  Assert(AFactory <> nil);
  inherited Create;
  FFactory := AFactory;
  FBeginReadCounter := ABeginReadCounter;
  FEndReadCounter := AEndReadCounter;
  FBeginWriteCounter := ABeginWriteCounter;
  FEndWriteCounter := AEndWriteCounter;
  FDestroyCounter := ADestroyCounter;
end;

function TSynchronizerFactoryWithCounters.Make(
  const AName: string): IReadWriteSync;
begin
  Result :=
    TReadWriteSyncCounterWrapper.Create(
      FFactory.Make(AName),
      FBeginReadCounter,
      FEndReadCounter,
      FBeginWriteCounter,
      FEndWriteCounter,
      FDestroyCounter
    );
end;

{ TSynchronizerFactoryWithMakeCounter }

constructor TSynchronizerFactoryWithMakeCounter.Create(
  const AFactory: IReadWriteSyncFactory;
  const ACounter: IInternalPerformanceCounter
);
begin
  Assert(AFactory <> nil);
  Assert(ACounter <> nil);
  inherited Create;
  FFactory := AFactory;
  FCounter := ACounter;
end;

function TSynchronizerFactoryWithMakeCounter.Make(
  const AName: string): IReadWriteSync;
var
  VContext: TInternalPerformanceCounterContext;
begin
  VContext := FCounter.StartOperation;
  try
    Result := FFactory.Make(AName);
  finally
    FCounter.FinishOperation(VContext);
  end;
end;

{ TSynchronizerMREWFactory }

function TSynchronizerMREWFactory.Make(const AName: string): IReadWriteSync;
begin
  Result := TMultiReadExclusiveWriteSynchronizer.Create;
end;

initialization
  DebugGlobalLocks_Enabled := FALSE;
end.
