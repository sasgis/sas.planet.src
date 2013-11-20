unit u_ThreadRegionProcessAbstract;

interface

uses
  Classes,
  i_Listener,
  i_VectorItemLonLat,
  i_NotifierOperation,
  i_RegionProcessProgressInfo;

type
  TThreadRegionProcessAbstract = class(TThread)
  private
    FOperationID: Integer;
    FCancelListener: IListener;
    FProgressInfo: IRegionProcessProgressInfoInternal;
    FPolygLL: ILonLatPolygon;

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
    procedure ProcessRegion; virtual; abstract;
    procedure Execute; override;

    property CancelNotifier: INotifierOperation read FCancelNotifier;
    property OperationID: Integer read FOperationID;
    property ProgressInfo: IRegionProcessProgressInfoInternal read FProgressInfo;
    property PolygLL: ILonLatPolygon read FPolygLL;
  public
    constructor Create(
      const AProgressInfo: IRegionProcessProgressInfoInternal;
      const APolygon: ILonLatPolygon;
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

constructor TThreadRegionProcessAbstract.Create(
  const AProgressInfo: IRegionProcessProgressInfoInternal;
  const APolygon: ILonLatPolygon;
  const ADebugThreadName: string = ''
);
begin
  inherited Create(True);
  FDebugThreadName := ADebugThreadName;
  Priority := tpLowest;
  FreeOnTerminate := true;
  FCancelNotifier := AProgressInfo.CancelNotifier;
  FOperationID := AProgressInfo.OperationID;
  FProgressInfo := AProgressInfo;
  FPolygLL := APolygon;
  if not FCancelNotifier.IsOperationCanceled(FOperationID) then begin
    FCancelListener := TNotifyNoMmgEventListener.Create(Self.OnCancel);
    FCancelNotifier.AddListener(FCancelListener);
  end;
  if FCancelNotifier.IsOperationCanceled(FOperationID) then begin
    Terminate;
  end;
end;

destructor TThreadRegionProcessAbstract.Destroy;
begin
  if Assigned(FCancelListener) and Assigned(FCancelNotifier) then begin
    FCancelNotifier.RemoveListener(FCancelListener);
    FCancelListener := nil;
    FCancelNotifier := nil;
  end;
  FPolygLL := nil;
  if Assigned(FProgressInfo) then begin
    FProgressInfo.Finish;
    FProgressInfo := nil;
  end;
  inherited;
end;

procedure TThreadRegionProcessAbstract.Execute;
begin
  SetCurrentThreadName(FDebugThreadName);
  try
    ProcessRegion;
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

procedure TThreadRegionProcessAbstract.OnCancel;
begin
  Terminate;
end;

procedure TThreadRegionProcessAbstract.ShowMessageSync(const AMessage: string);
begin
  FMessageForShow := AMessage;
  Synchronize(SynShowMessage);
end;

procedure TThreadRegionProcessAbstract.SynShowMessage;
begin
  ShowMessage(FMessageForShow);
end;

end.
