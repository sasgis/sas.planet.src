unit u_DebugInfoWindow;

interface

uses
  i_DebugInfoWindow,
  i_InternalPerformanceCounter,
  i_InternalDebugConfig,
  u_BaseInterfacedObject,
  frm_DebugInfo;

type
  TDebugInfoWindow = class(TBaseInterfacedObject, IDebugInfoWindow)
  private
    FPerfCounterList: IInternalPerformanceCounterList;
    FInternalDebugConfig: IInternalDebugConfig;
    FfrmDebugInfo: TfrmDebugInfo;
  private
    procedure Show;
  public
    constructor Create(
      const AInternalDebugConfig: IInternalDebugConfig;
      const APerfCounterList: IInternalPerformanceCounterList
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils;

{ TDebugInfoWindow }

constructor TDebugInfoWindow.Create(
  const AInternalDebugConfig: IInternalDebugConfig;
  const APerfCounterList: IInternalPerformanceCounterList
);
begin
  inherited Create;
  FInternalDebugConfig := AInternalDebugConfig;
  FPerfCounterList := APerfCounterList;
end;

destructor TDebugInfoWindow.Destroy;
begin
  if FfrmDebugInfo <> nil then begin
    FreeAndNil(FfrmDebugInfo);
  end;
  inherited;
end;

procedure TDebugInfoWindow.Show;
begin
  if FfrmDebugInfo = nil then begin
    if FInternalDebugConfig.IsShowDebugInfo then begin
      FfrmDebugInfo := TfrmDebugInfo.Create(nil, FPerfCounterList);
    end;
  end;
  if FfrmDebugInfo <> nil then begin
    FfrmDebugInfo.Show;
  end;
end;

end.
