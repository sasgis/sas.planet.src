unit u_MarkerDrawableChangeableSimple;

interface

uses
  SysUtils,
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

type
  TMarkerDrawableWithDirectionChangeableSimple = class(TChangeableBase, IMarkerDrawableWithDirectionChangeable)
  private
    FMarkerClass: TMarkerDrawableWithDirectionSimpleAbstractClass;
    FConfig: IMarkerSimpleConfig;

    FConfigListener: IListener;

    FStaticCS: IReadWriteSync;
    FStatic: IMarkerDrawableWithDirection;
    procedure OnConfigChange;
  private
    function GetStatic: IMarkerDrawableWithDirection;
  public
    constructor Create(
      AMarkerClass: TMarkerDrawableWithDirectionSimpleAbstractClass;
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

  FStaticCS := MakeSyncRW_Var(Self, False);

  FConfigListener := TNotifyNoMmgEventListener.Create(Self.OnConfigChange);
  FConfig.ChangeNotifier.Add(FConfigListener);
  OnConfigChange;
end;

destructor TMarkerDrawableChangeableSimple.Destroy;
begin
  if Assigned(FConfig) and Assigned(FConfigListener) then begin
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

{ TMarkerDrawableWithDirectionChangeableSimple }

constructor TMarkerDrawableWithDirectionChangeableSimple.Create(
  AMarkerClass: TMarkerDrawableWithDirectionSimpleAbstractClass;
  const AConfig: IMarkerSimpleConfig);
begin
  inherited Create;
  FMarkerClass := AMarkerClass;
  FConfig := AConfig;

  FStaticCS := MakeSyncRW_Var(Self, False);

  FConfigListener := TNotifyNoMmgEventListener.Create(Self.OnConfigChange);
  FConfig.ChangeNotifier.Add(FConfigListener);
  OnConfigChange;
end;

destructor TMarkerDrawableWithDirectionChangeableSimple.Destroy;
begin
  if Assigned(FConfig) and Assigned(FConfigListener) then begin
    FConfig.ChangeNotifier.Remove(FConfigListener);
    FConfig := nil;
    FConfigListener := nil;
  end;
  inherited;
end;

function TMarkerDrawableWithDirectionChangeableSimple.GetStatic: IMarkerDrawableWithDirection;
begin
  FStaticCS.BeginRead;
  try
    Result := FStatic;
  finally
    FStaticCS.EndRead;
  end;
end;

procedure TMarkerDrawableWithDirectionChangeableSimple.OnConfigChange;
var
  VStatic: IMarkerDrawableWithDirection;
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

