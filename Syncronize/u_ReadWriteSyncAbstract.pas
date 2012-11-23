unit u_ReadWriteSyncAbstract;

interface

uses
  SysUtils,
  i_InternalPerformanceCounter,
  i_ReadWriteSyncFactory;

type
  TReadWriteSyncAbstract = class(TInterfacedObject)
  private
    FName: ShortString;
  public
    constructor Create(const AName: AnsiString);
  end;

  TReadWriteSyncDebugWrapper = class(TInterfacedObject, IReadWriteSync)
  private
    FLock: IReadWriteSync;
    FLockClassName: ShortString;
    FName: ShortString;
    FReadCounter: IInternalPerformanceCounter;
    FWriteCounter: IInternalPerformanceCounter;
  protected
    procedure DoDebugGlobalLocks(const AProcedure, AEvent: String);
  private
    procedure BeginRead;
    procedure EndRead;
    function BeginWrite: Boolean;
    procedure EndWrite;
  public
    constructor Create(
      const ALock: IReadWriteSync;
      const ALockClassName: AnsiString;
      const AName: AnsiString;
      const AReadCounter: IInternalPerformanceCounter;
      const AWriteCounter: IInternalPerformanceCounter
    );
  end;

  TSynchronizerFake = class(TReadWriteSyncAbstract, IReadWriteSync)
  private
    procedure BeginRead;
    procedure EndRead;
    function BeginWrite: Boolean;
    procedure EndWrite;
  end;

  TSynchronizerFakeFactory = class(TInterfacedObject, IReadWriteSyncFactory)
  private
    function Make(const AName: AnsiString): IReadWriteSync;
  end;

var
  DebugGlobalLocks_Enabled: Boolean;

implementation

uses
  Windows;

{ TReadWriteSyncAbstractSimple }

constructor TReadWriteSyncAbstract.Create(const AName: AnsiString);
begin
  inherited Create;
  FName := AName;
end;

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

function TSynchronizerFakeFactory.Make(const AName: AnsiString): IReadWriteSync;
begin
  Result := TSynchronizerFake.Create(AName);
end;

{ TReadWriteSyncDebugWrapper }

constructor TReadWriteSyncDebugWrapper.Create(
  const ALock: IReadWriteSync;
  const ALockClassName: AnsiString;
  const AName: AnsiString;
  const AReadCounter, AWriteCounter: IInternalPerformanceCounter
);
begin
  Assert(ALock <> nil);
  inherited Create;
  FLock := ALock;
  FLockClassName := ALockClassName;
  FName := AName;
  FReadCounter := AReadCounter;
  FWriteCounter := AWriteCounter;
end;

procedure TReadWriteSyncDebugWrapper.BeginRead;
var
  VCounter: Pointer;
  VContext: TInternalPerformanceCounterContext;
begin
  DoDebugGlobalLocks('BeginRead', 'IN');
  VCounter := Pointer(FReadCounter);
  if VCounter <> nil then begin
    VContext := IInternalPerformanceCounter(VCounter).StartOperation;
    FLock.BeginRead;
    IInternalPerformanceCounter(VCounter).FinishOperation(VContext);
  end else begin
    FLock.BeginRead;
  end;
  DoDebugGlobalLocks('BeginRead', 'OUT');
end;

function TReadWriteSyncDebugWrapper.BeginWrite: Boolean;
var
  VCounter: Pointer;
  VContext: TInternalPerformanceCounterContext;
begin
  DoDebugGlobalLocks('BeginWrite', 'IN');
  VCounter := Pointer(FWriteCounter);
  if VCounter <> nil then begin
    VContext := IInternalPerformanceCounter(VCounter).StartOperation;
    Result := FLock.BeginWrite;
    IInternalPerformanceCounter(VCounter).FinishOperation(VContext);
  end else begin
    Result := FLock.BeginWrite;
  end;
  DoDebugGlobalLocks('BeginWrite', 'OUT');
end;

procedure TReadWriteSyncDebugWrapper.DoDebugGlobalLocks(const AProcedure,
  AEvent: String);
const
  c_SEP: AnsiString = ', ' + Chr(VK_TAB);
var
  VText: AnsiString;
begin
  if (not DebugGlobalLocks_Enabled) then
    Exit;
  VText := FLockClassName + ' at $'+ IntToHex(Integer(Pointer(Self)), 8)+' (from '+FName+')' + c_SEP + 'ThreadId=' + IntToStr(GetCurrentThreadId) + c_SEP +  AProcedure + c_SEP + AEvent;
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

initialization
  DebugGlobalLocks_Enabled := FALSE;
end.
