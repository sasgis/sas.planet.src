unit u_WindowLayerBasic;

interface

uses
  i_InternalPerformanceCounter,
  i_JclListenerNotifierLinksList;

type
  TWindowLayerAbstract = class
  private
    FPerfList: IInternalPerformanceCounterList;
    FLinksList: IJclListenerNotifierLinksList;
  protected
    property LinksList: IJclListenerNotifierLinksList read FLinksList;
    property PerfList: IInternalPerformanceCounterList read FPerfList;
  public
    constructor Create(const APerfList: IInternalPerformanceCounterList);
    destructor Destroy; override;
    procedure StartThreads; virtual;
    procedure SendTerminateToThreads; virtual;
  end;

implementation

uses
  u_JclListenerNotifierLinksList;

{ TWindowLayerAbstract }

constructor TWindowLayerAbstract.Create(
  const APerfList: IInternalPerformanceCounterList
);
begin
  inherited Create;
  FPerfList := APerfList.CreateAndAddNewSubList(ClassName);
  FLinksList := TJclListenerNotifierLinksList.Create;
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
