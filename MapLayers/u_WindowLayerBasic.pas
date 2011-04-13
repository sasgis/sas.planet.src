unit u_WindowLayerBasic;

interface

uses
  Windows,
  SyncObjs,
  GR32,
  i_JclListenerNotifierLinksList,
  i_LocalCoordConverter,
  i_ViewPortState;

type
  TWindowLayerAbstract = class
  private
    FCS: TCriticalSection;
    FRedrawCounter: Cardinal;
    FRedrawTime: TDateTime;
  private
    FLinksList: IJclListenerNotifierLinksList;
    FViewPortState: IViewPortState;
    FVisualCoordConverter: ILocalCoordConverter;
    procedure OnPosChange(Sender: TObject); virtual;
  protected
    procedure SetVisualCoordConverter(ANewVisualCoordConverter: ILocalCoordConverter); virtual;
    procedure PosChange(ANewVisualCoordConverter: ILocalCoordConverter); virtual; abstract;
    procedure PreparePosChange(ANewVisualCoordConverter: ILocalCoordConverter); virtual;
    procedure AfterPosChange; virtual;

    procedure IncRedrawCounter(ATime: TDateTime);
    property LinksList: IJclListenerNotifierLinksList read FLinksList;
    property ViewPortState: IViewPortState read FViewPortState;
    property VisualCoordConverter: ILocalCoordConverter read FVisualCoordConverter;
  public
    constructor Create(AViewPortState: IViewPortState);
    destructor Destroy; override;
    procedure StartThreads; virtual;
    procedure SendTerminateToThreads; virtual;
    property RedrawCounter: Cardinal read FRedrawCounter;
    property RedrawTime: TDateTime read FRedrawTime;
  end;

implementation

uses
  SysUtils,
  Forms,
  Types,
  u_JclListenerNotifierLinksList,
  u_NotifyEventListener;

{ TWindowLayerAbstract }

procedure TWindowLayerAbstract.AfterPosChange;
begin
end;

constructor TWindowLayerAbstract.Create(AViewPortState: IViewPortState);
begin
  FCS := TCriticalSection.Create;
  FRedrawCounter := 0;
  FRedrawTime  := 0;
  FViewPortState := AViewPortState;
  FLinksList := TJclListenerNotifierLinksList.Create;

  LinksList.Add(
    TNotifyEventListener.Create(Self.OnPosChange),
    FViewPortState.GetChangeNotifier
  );
end;

destructor TWindowLayerAbstract.Destroy;
begin
  FLinksList := nil;
  FViewPortState := nil;
  FreeAndNil(FCS);
  inherited;
end;

procedure TWindowLayerAbstract.PreparePosChange(
  ANewVisualCoordConverter: ILocalCoordConverter);
begin
  SetVisualCoordConverter(ANewVisualCoordConverter);
end;

procedure TWindowLayerAbstract.IncRedrawCounter(ATime: TDateTime);
begin
  FCS.Acquire;
  try
    Inc(FRedrawCounter);
    FRedrawTime := FRedrawTime + ATime;
  finally
    FCS.Release;
  end;
end;

procedure TWindowLayerAbstract.OnPosChange(Sender: TObject);
begin
  PosChange(FViewPortState.GetVisualCoordConverter);
end;

procedure TWindowLayerAbstract.SendTerminateToThreads;
begin
  FLinksList.DeactivateLinks;
end;

procedure TWindowLayerAbstract.SetVisualCoordConverter(
  ANewVisualCoordConverter: ILocalCoordConverter);
begin
  FVisualCoordConverter := ANewVisualCoordConverter;
end;

procedure TWindowLayerAbstract.StartThreads;
begin
  FVisualCoordConverter := FViewPortState.GetVisualCoordConverter;
  FLinksList.ActivateLinks;
end;

end.
