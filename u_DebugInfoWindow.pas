unit u_DebugInfoWindow;

interface

uses
  i_DebugInfoWindow,
  i_DebugInfoSubSystem,
  i_InternalDebugConfig,
  u_BaseInterfacedObject,
  frm_DebugInfo;

type
  TDebugInfoWindow = class(TBaseInterfacedObject, IDebugInfoWindow)
  private
    FDebugInfoSubSystem: IDebugInfoSubSystem;
    FInternalDebugConfig: IInternalDebugConfig;
    FfrmDebugInfo: TfrmDebugInfo;
  private
    procedure Show;
  public
    constructor Create(
      const AInternalDebugConfig: IInternalDebugConfig;
      const ADebugInfoSubSystem: IDebugInfoSubSystem
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils;

{ TDebugInfoWindow }

constructor TDebugInfoWindow.Create(
  const AInternalDebugConfig: IInternalDebugConfig;
  const ADebugInfoSubSystem: IDebugInfoSubSystem
);
begin
  inherited Create;
  FInternalDebugConfig := AInternalDebugConfig;
  FDebugInfoSubSystem := ADebugInfoSubSystem;
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
      FfrmDebugInfo := TfrmDebugInfo.Create(nil, FDebugInfoSubSystem);
    end;
  end;
  if FfrmDebugInfo <> nil then begin
    FfrmDebugInfo.Show;
  end;
end;

end.
