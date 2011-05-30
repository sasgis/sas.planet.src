unit u_WindowLayerBasic;

interface

uses
  Windows,
  SyncObjs,
  i_InternalPerformanceCounter,
  i_JclListenerNotifierLinksList;

type
  TWindowLayerAbstract = class
  private
    FPerfList: IInternalPerformanceCounterList;
    FLinksList: IJclListenerNotifierLinksList;
  protected
    procedure SetPerfList(const Value: IInternalPerformanceCounterList); virtual;
    property LinksList: IJclListenerNotifierLinksList read FLinksList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure StartThreads; virtual;
    procedure SendTerminateToThreads; virtual;
    property PerfList: IInternalPerformanceCounterList read FPerfList write SetPerfList;
  end;

implementation

uses
  SysUtils,
  Types,
  u_JclListenerNotifierLinksList;

{ TWindowLayerAbstract }

constructor TWindowLayerAbstract.Create;
begin
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

procedure TWindowLayerAbstract.SetPerfList(
  const Value: IInternalPerformanceCounterList);
begin
  FPerfList := Value;
end;

procedure TWindowLayerAbstract.StartThreads;
begin
  FLinksList.ActivateLinks;
end;

end.
