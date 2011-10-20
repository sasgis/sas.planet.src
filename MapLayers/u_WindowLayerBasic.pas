unit u_WindowLayerBasic;

interface

uses
  Windows,
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
    constructor Create(APerfList: IInternalPerformanceCounterList);
    destructor Destroy; override;
    procedure StartThreads; virtual;
    procedure SendTerminateToThreads; virtual;
  end;

implementation

uses
  SysUtils,
  Types,
  u_JclListenerNotifierLinksList;

{ TWindowLayerAbstract }

constructor TWindowLayerAbstract.Create(APerfList: IInternalPerformanceCounterList);
begin
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
