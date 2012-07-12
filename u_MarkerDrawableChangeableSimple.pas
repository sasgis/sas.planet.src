unit u_MarkerDrawableChangeableSimple;

interface

uses
  SysUtils,
  GR32,
  t_GeoTypes,
  i_Notifier,
  i_Listener,
  i_MarkerDrawable,
  i_MarkerSimpleConfig,
  u_MarkerDrawableSimpleAbstract,
  u_ChangeableBase;

type
  TMarkerDrawableChangeableSimple = class(TChangeableBase, IMarkerDrawableChangeable)
  private
    FMarkerClass: TMarkerDrawableSimpleAbstractClass;
    FConfig: IMarkerSimpleConfig;

    FConfigListener: IListener;

    FStaticCS: IReadWriteSync;
    FStatic: IMarkerDrawable;
    procedure OnConfigChange;
  private
    function GetStatic: IMarkerDrawable;
  public
    constructor Create(
      AMarkerClass: TMarkerDrawableSimpleAbstractClass;
      const AConfig: IMarkerSimpleConfig
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_ListenerByEvent,
  u_Synchronizer;

{ TMarkerDrawableChangeableSimple }

constructor TMarkerDrawableChangeableSimple.Create(
  AMarkerClass: TMarkerDrawableSimpleAbstractClass;
  const AConfig: IMarkerSimpleConfig);
begin
  inherited Create;
  FMarkerClass := AMarkerClass;
  FConfig := AConfig;

  FConfigListener := TNotifyNoMmgEventListener.Create(Self.OnConfigChange);
  FConfig.ChangeNotifier.Add(FConfigListener);
  OnConfigChange;
end;

destructor TMarkerDrawableChangeableSimple.Destroy;
begin
  if FConfig <> nil then begin
    FConfig.ChangeNotifier.Remove(FConfigListener);
    FConfig := nil;
    FConfigListener := nil;
  end;
  inherited;
end;

function TMarkerDrawableChangeableSimple.GetStatic: IMarkerDrawable;
begin
  FStaticCS.BeginRead;
  try
    Result := FStatic;
  finally
    FStaticCS.EndRead;
  end;
end;

procedure TMarkerDrawableChangeableSimple.OnConfigChange;
var
  VStatic: IMarkerDrawable;
begin
  VStatic := FMarkerClass.Create(FConfig.GetStatic);
  FStaticCS.BeginWrite;
  try
    FStatic := VStatic;
  finally
    FStaticCS.EndWrite;
  end;
  DoChangeNotify;
end;

end.

