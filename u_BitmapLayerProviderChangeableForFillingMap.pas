unit u_BitmapLayerProviderChangeableForFillingMap;

interface

uses
  i_Listener,
  i_FillingMapLayerConfig,
  i_MapTypes,
  i_Bitmap32StaticFactory,
  i_BitmapLayerProvider,
  u_BitmapLayerProviderChangeableBase;

type
  TBitmapLayerProviderChangeableForFillingMap = class(TBitmapLayerProviderChangeableBase)
  private
    FBitmapFactory: IBitmap32StaticFactory;
    FConfig: IFillingMapLayerConfig;

    FVersionListener: IListener;
    FSourceMapLast: IMapType;
    procedure OnMapVersionChange;
    procedure OnConfigChange;
  protected
    function CreateStatic: IInterface; override;
  public
    constructor Create(
      const ABitmapFactory: IBitmap32StaticFactory;
      const AConfig: IFillingMapLayerConfig
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_ListenerByEvent,
  u_BitmapLayerProviderFillingMap;

{ TBitmapLayerProviderChangeableForFillingMap }

constructor TBitmapLayerProviderChangeableForFillingMap.Create(
  const ABitmapFactory: IBitmap32StaticFactory;
  const AConfig: IFillingMapLayerConfig
);
begin
  inherited Create;
  FBitmapFactory := ABitmapFactory;
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
    FSourceMapLast.VersionRequestConfig.ChangeNotifier.Remove(FVersionListener);
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
        FSourceMapLast.VersionRequestConfig.ChangeNotifier.Remove(FVersionListener);
      end;
      FSourceMapLast := VMap;
      if Assigned(FSourceMapLast) then begin
        FSourceMapLast.VersionRequestConfig.ChangeNotifier.Add(FVersionListener);
      end;
    end;
    VResult :=
      TBitmapLayerProviderFillingMap.Create(
        FBitmapFactory,
        VMap.TileStorage,
        VMap.VersionRequestConfig.GetStatic,
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
