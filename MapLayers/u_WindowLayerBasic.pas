unit u_WindowLayerBasic;

interface

uses
  i_InternalPerformanceCounter,
  i_ListenerNotifierLinksList;

type
  TWindowLayerAbstract = class
  private
    FPerfList: IInternalPerformanceCounterList;
    FLinksList: IListenerNotifierLinksList;
  protected
    property LinksList: IListenerNotifierLinksList read FLinksList;
    property PerfList: IInternalPerformanceCounterList read FPerfList;
  public
    constructor Create(const APerfList: IInternalPerformanceCounterList);
    destructor Destroy; override;
    procedure StartThreads; virtual;
    procedure SendTerminateToThreads; virtual;
  end;

implementation

uses
  u_ListenerNotifierLinksList;

{ TWindowLayerAbstract }

constructor TWindowLayerAbstract.Create(
  const APerfList: IInternalPerformanceCounterList
);
begin
  inherited Create;
  FPerfList := APerfList.CreateAndAddNewSubList(ClassName);
  FLinksList := TListenerNotifierLinksList.Create;
end;

destructor TWindowLayerAbstract.Destroy;
begin
  FLinksList := nil;
  inherited;
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
