unit u_WindowLayerBasic;

interface

uses
  i_Listener,
  i_NotifierOperation,
  i_InternalPerformanceCounter,
  i_ListenerNotifierLinksList;

type
  TWindowLayerAbstract = class(TInterfacedObject)
  private
    FPerfList: IInternalPerformanceCounterList;
    FAppStartedNotifier: INotifierOneOperation;
    FAppClosingNotifier: INotifierOneOperation;

    FAppStartedListener: IListener;
    FAppClosingListener: IListener;
    FLinksList: IListenerNotifierLinksList;
    procedure OnAppStarted;
    procedure OnAppClosing;
  protected
    procedure StartThreads; virtual;
    procedure SendTerminateToThreads; virtual;

    property LinksList: IListenerNotifierLinksList read FLinksList;
    property PerfList: IInternalPerformanceCounterList read FPerfList;
  public
    procedure AfterConstruction; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_ListenerByEvent,
  u_ListenerNotifierLinksList;

{ TWindowLayerAbstract }

constructor TWindowLayerAbstract.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation
);
begin
  inherited Create;
  FAppStartedNotifier := AAppStartedNotifier;
  FAppClosingNotifier := AAppClosingNotifier;
  FPerfList := APerfList.CreateAndAddNewSubList(ClassName);

  FLinksList := TListenerNotifierLinksList.Create;
  FAppStartedListener := TNotifyNoMmgEventListener.Create(Self.OnAppStarted);
  FAppClosingListener := TNotifyNoMmgEventListener.Create(Self.OnAppClosing);
end;

destructor TWindowLayerAbstract.Destroy;
begin
  FLinksList := nil;
  if FAppStartedNotifier <> nil then begin
    FAppStartedNotifier.Remove(FAppStartedListener);
    FAppStartedNotifier := nil;
  end;
  if FAppClosingNotifier <> nil then begin
    FAppClosingNotifier.Remove(FAppClosingListener);
    FAppClosingNotifier := nil;
  end;
  inherited;
end;

procedure TWindowLayerAbstract.AfterConstruction;
begin
  inherited;
  FAppStartedNotifier.Add(FAppStartedListener);
  if FAppStartedNotifier.IsExecuted then begin
    StartThreads;
  end;
  FAppClosingNotifier.Add(FAppClosingListener);
  if FAppClosingNotifier.IsExecuted then begin
    SendTerminateToThreads;
  end;
end;

procedure TWindowLayerAbstract.OnAppClosing;
begin
  SendTerminateToThreads;
end;

procedure TWindowLayerAbstract.OnAppStarted;
begin
  StartThreads;
end;

procedure TWindowLayerAbstract.SendTerminateToThreads;
begin
  FLinksList.DeactivateLinks;
end;

procedure TWindowLayerAbstract.StartThreads;
begin
  FLinksList.ActivateLinks;
end;

end.
