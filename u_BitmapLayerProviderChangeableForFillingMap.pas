unit u_BitmapLayerProviderChangeableForFillingMap;

interface

uses
  i_Listener,
  i_FillingMapLayerConfig,
  i_MapTypes,
  i_BitmapLayerProvider,
  i_BitmapLayerProviderChangeable,
  u_BitmapLayerProviderChangeableBase;

type
  TBitmapLayerProviderChangeableForFillingMap = class(TBitmapLayerProviderChangeableBase)
  private
    FConfig: IFillingMapLayerConfig;

    FVersionListener: IListener;
    FSourceMapLast: IMapType;
    procedure OnMapVersionChange;
    procedure OnConfigChange;
  protected
    function CreateStatic: IInterface; override;
  public
    constructor Create(
      const AConfig: IFillingMapLayerConfig
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_ListenerByEvent,
  u_ListenerNotifierLinksList,
  u_BitmapLayerProviderFillingMap;

{ TBitmapLayerProviderChangeableForFillingMap }

constructor TBitmapLayerProviderChangeableForFillingMap.Create(
  const AConfig: IFillingMapLayerConfig
);
begin
  inherited Create;
  FConfig := AConfig;

  FVersionListener := TNotifyNoMmgEventListener.Create(Self.OnMapVersionChange);

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(OnConfigChange),
    FConfig.ChangeNotifier
  );
end;

destructor TBitmapLayerProviderChangeableForFillingMap.Destroy;
begin
  if Assigned(FSourceMapLast) and Assigned(FVersionListener) then begin
    FSourceMapLast.MapType.VersionConfig.ChangeNotifier.Remove(FVersionListener);
    FSourceMapLast := nil;
    FVersionListener := nil;
  end;
  inherited;
end;

function TBitmapLayerProviderChangeableForFillingMap.CreateStatic: IInterface;
var
  VConfig: IFillingMapLayerConfigStatic;
  VResult: IBitmapLayerProvider;
  VMap: IMapType;
begin
  VResult := nil;
  VConfig := FConfig.GetStatic;
  if VConfig.Visible then begin
    VMap := VConfig.SourceMap;
    if FSourceMapLast <> VMap then begin
      if Assigned(FSourceMapLast) then begin
        FSourceMapLast.MapType.VersionConfig.ChangeNotifier.Remove(FVersionListener);
      end;
      FSourceMapLast := VMap;
      if Assigned(FSourceMapLast) then begin
        FSourceMapLast.MapType.VersionConfig.ChangeNotifier.Add(FVersionListener);
      end;
    end;
    VResult :=
      TBitmapLayerProviderFillingMap.Create(
        VMap,
        VMap.MapType.VersionConfig.Version,
        VConfig.UseRelativeZoom,
        VConfig.Zoom,
        VConfig.Colorer
      );
  end;
  Result := VResult;
end;

procedure TBitmapLayerProviderChangeableForFillingMap.OnConfigChange;
begin
  LockWrite;
  try
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

procedure TBitmapLayerProviderChangeableForFillingMap.OnMapVersionChange;
begin
  LockWrite;
  try
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

end.
