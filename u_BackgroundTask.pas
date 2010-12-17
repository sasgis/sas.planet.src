unit u_BackgroundTask;

interface

uses
  Windows,
  SyncObjs,
  Classes,
  i_IBackgroundTask,
  u_InterfacedThread;

type
  TBackgroundTask = class(TInterfacedThread, IBackgroundTask)
  private
    FStopThread: TEvent;
    FAllowExecute: TEvent;
    FExecuteCS: TCriticalSection;
    FExecuteStopCounter: Longint;
  protected
    FNeedStopExecute: Boolean;
    procedure ExecuteTask; virtual; abstract;
    procedure Execute; override;
    procedure Terminate; override;
  protected
    procedure StartExecute; virtual;
    procedure StopExecute; virtual;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils;

{ TBackgroundTask }

constructor TBackgroundTask.Create;
begin
  inherited;
  FStopThread := TEvent.Create(nil, True, False, '');
  FAllowExecute := TEvent.Create(nil, True, False, '');
  FExecuteCS := TCriticalSection.Create;
end;

destructor TBackgroundTask.Destroy;
begin
  Terminate;
  FreeAndNil(FStopThread);
  FreeAndNil(FAllowExecute);
  FreeAndNil(FExecuteCS);
  inherited;
end;

procedure TBackgroundTask.Execute;
var
  VHandles: array [0..1] of THandle;
  VWaitResult: DWORD;
begin
  inherited;
  VHandles[0] := FAllowExecute.Handle;
  VHandles[1] := FStopThread.Handle;
  while not Terminated do begin
    VWaitResult := WaitForMultipleObjects(Length(VHandles), @VHandles[0], False, INFINITE);
    case VWaitResult of
      WAIT_OBJECT_0:
      begin
        FExecuteCS.Acquire;
        try
          ExecuteTask;
          if not FNeedStopExecute then begin
            FAllowExecute.ResetEvent;
          end;
        finally
          FExecuteCS.Release;
        end;
      end;
    end;
  end;
end;

procedure TBackgroundTask.StartExecute;
var
  VCouner: Longint;
begin
  VCouner := InterlockedDecrement(FExecuteStopCounter);
  if VCouner = 0 then begin
    FNeedStopExecute := False;
    FExecuteCS.Acquire;
    try
      FAllowExecute.SetEvent;
    finally
      FExecuteCS.Release;
    end;
  end;
end;

procedure TBackgroundTask.StopExecute;
begin
  InterlockedIncrement(FExecuteStopCounter);
  FNeedStopExecute := True;
  FExecuteCS.Acquire;
  try
    FAllowExecute.ResetEvent;
  finally
    FExecuteCS.Release;
  end;
end;

procedure TBackgroundTask.Terminate;
begin
  StopExecute;
  inherited Terminate;
  FStopThread.SetEvent;
end;

end.
