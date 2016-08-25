unit u_RegionProcessWorker;

interface

uses
  Classes,
  i_Listener,
  i_NotifierOperation,
  i_RegionProcessTask,
  i_RegionProcessProgressInfo;

type
  TRegionProcessWorker = class(TThread)
  private
    FTask: IRegionProcessTask;
    FOperationID: Integer;
    FCancelListener: IListener;

    FMessageForShow: string;
    FCancelNotifier: INotifierOperation;
    FDebugThreadName: string;
    procedure OnCancel;
    procedure SynShowMessage;
    {$HINTS OFF}
    // Disable hint: "Private symbol 'ShowMessageSync' declared but never used"
    // in case we catch exceptions by EurekaLog (see below)
    procedure ShowMessageSync(const AMessage: string);
    {$HINTS ON}
  protected
    procedure Execute; override;
  public
    constructor Create(
      const ATask: IRegionProcessTask;
      const AProgressInfo: IRegionProcessProgressInfoInternal;
      const ADebugThreadName: string = ''
    );
    destructor Destroy; override;
  end;


implementation

uses
  {$IFDEF EUREKALOG}
  ExceptionLog,
  {$ENDIF}
  SysUtils,
  Dialogs,
  u_ReadableThreadNames,
  u_ListenerByEvent;

{ TRegionProcessWorker }

constructor TRegionProcessWorker.Create(
  const ATask: IRegionProcessTask;
  const AProgressInfo: IRegionProcessProgressInfoInternal;
  const ADebugThreadName: string
);
begin
  Assert(Assigned(ATask));
  Assert(Assigned(AProgressInfo));
  inherited Create(True);
  FTask := ATask;
  FDebugThreadName := ADebugThreadName;
  Priority := tpLowest;
  FreeOnTerminate := true;
  FCancelNotifier := AProgressInfo.CancelNotifier;
  FOperationID := AProgressInfo.OperationID;
  if not FCancelNotifier.IsOperationCanceled(FOperationID) then begin
    FCancelListener := TNotifyNoMmgEventListener.Create(Self.OnCancel);
    FCancelNotifier.AddListener(FCancelListener);
  end;
  if FCancelNotifier.IsOperationCanceled(FOperationID) then begin
    Terminate;
  end;
end;

destructor TRegionProcessWorker.Destroy;
begin
  if Assigned(FCancelListener) and Assigned(FCancelNotifier) then begin
    FCancelNotifier.RemoveListener(FCancelListener);
    FCancelListener := nil;
    FCancelNotifier := nil;
  end;
  inherited;
end;

procedure TRegionProcessWorker.Execute;
begin
  SetCurrentThreadName(FDebugThreadName);
  try
    FTask.ProcessRegion;
  except
  {$IFDEF EUREKALOG}
    ShowLastExceptionData;
  {$ELSE}
    on E: Exception do begin
      ShowMessageSync(E.ClassName + ': ' + E.Message);
    end;
  {$ENDIF}
  end;
end;

procedure TRegionProcessWorker.OnCancel;
begin
  Terminate;
end;

procedure TRegionProcessWorker.ShowMessageSync(const AMessage: string);
begin
  FMessageForShow := AMessage;
  Synchronize(SynShowMessage);
end;

procedure TRegionProcessWorker.SynShowMessage;
begin
  ShowMessage(FMessageForShow);
end;

end.
