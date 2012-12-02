unit u_DebugInfoWindow;

interface

uses
  i_DebugInfoWindow,
  i_InternalPerformanceCounter,
  i_GlobalAppConfig,
  u_BaseInterfacedObject,
  frm_DebugInfo;

type
  TDebugInfoWindow = class(TBaseInterfacedObject, IDebugInfoWindow)
  private
    FPerfCounterList: IInternalPerformanceCounterList;
    FAppConfig: IGlobalAppConfig;
    FfrmDebugInfo: TfrmDebugInfo;
  private
    procedure Show;
  public
    constructor Create(
      const AAppConfig: IGlobalAppConfig;
      const APerfCounterList: IInternalPerformanceCounterList
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils;

{ TDebugInfoWindow }

constructor TDebugInfoWindow.Create(
  const AAppConfig: IGlobalAppConfig;
  const APerfCounterList: IInternalPerformanceCounterList
);
begin
  inherited Create;
  FAppConfig := AAppConfig;
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
    if FAppConfig.IsShowDebugInfo then begin
      FfrmDebugInfo := TfrmDebugInfo.Create(nil, FPerfCounterList);
    end;
  end;
  if FfrmDebugInfo <> nil then begin
    FfrmDebugInfo.Show;
  end;
end;

end.
