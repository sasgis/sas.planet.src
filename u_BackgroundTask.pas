unit u_BackgroundTask;

interface

uses
  Windows,
  SyncObjs,
  Classes,
  i_BackgroundTask,
  u_InterfacedThread;

type
  TBackgroundTask = class(TInterfacedThread, IBackgroundTask)
  private
    FStopThread: TEvent;
    FAllowExecute: TEvent;
    FCS: TCriticalSection;
    FNeedStopExecute: Boolean;
  protected
    procedure ExecuteTask; virtual; abstract;
    procedure Execute; override;
    procedure Terminate; override;
    function IsNeedStopExecute(): Boolean;
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
  FCS := TCriticalSection.Create;
  SetPriority(tpLowest);
end;

destructor TBackgroundTask.Destroy;
begin
  Terminate;
  FreeAndNil(FStopThread);
  FreeAndNil(FAllowExecute);
  FreeAndNil(FCS);
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
        FCS.Acquire;
        try
          FNeedStopExecute := False;
        finally
          FCS.Release;
        end;
        ExecuteTask;
        FCS.Acquire;
        try
          if not FNeedStopExecute then begin
            FAllowExecute.ResetEvent;
          end;
        finally
          FCS.Release;
        end;
      end;
    end;
  end;
end;

function TBackgroundTask.IsNeedStopExecute: Boolean;
begin
  Result := FNeedStopExecute;
end;

procedure TBackgroundTask.StartExecute;
begin
  FCS.Acquire;
  try
    FAllowExecute.SetEvent;
  finally
    FCS.Release;
  end;
end;

procedure TBackgroundTask.StopExecute;
begin
  FCS.Acquire;
  try
    FNeedStopExecute := True;
    FAllowExecute.ResetEvent;
  finally
    FCS.Release;
  end;
end;

procedure TBackgroundTask.Terminate;
begin
  StopExecute;
  inherited Terminate;
  FStopThread.SetEvent;
end;

end.
