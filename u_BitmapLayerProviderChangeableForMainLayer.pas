unit u_BitmapLayerProviderChangeableForMainLayer;

interface

uses
  i_Listener,
  i_TileError,
  i_BitmapPostProcessing,
  i_BitmapLayerProvider,
  i_UseTilePrevZoomConfig,
  i_Bitmap32StaticFactory,
  i_MapTypes,
  i_MapTypeListStatic,
  i_MapTypeListChangeable,
  i_BitmapLayerProviderChangeable,
  i_ListenerNotifierLinksList,
  u_BitmapLayerProviderChangeableBase;

type
  TBitmapLayerProviderChangeableForMainLayer = class(TBitmapLayerProviderChangeableBase)
  private
    FErrorLogger: ITileErrorLogger;
    FBitmapFactory: IBitmap32StaticFactory;
    FPostProcessing: IBitmapPostProcessingChangeable;
    FUseTilePrevZoomConfig: IUseTilePrevZoomConfig;
    FMainMap: IMapTypeChangeable;
    FLayesList: IMapTypeListChangeable;

    FVersionListener: IListener;
    FMainMapLast: IMapType;
    FLayesListLast: IMapTypeListStatic;

    procedure OnMainMapChange;
    procedure OnLayerListChange;
    procedure OnConfigChange;
    procedure OnMapVersionChange;
  protected
    function CreateStatic: IInterface; override;
  public
    constructor Create(
      const AMainMap: IMapTypeChangeable;
      const ALayesList: IMapTypeListChangeable;
      const APostProcessing: IBitmapPostProcessingChangeable;
      const AUseTilePrevZoomConfig: IUseTilePrevZoomConfig;
      const ABitmapFactory: IBitmap32StaticFactory;
      const AErrorLogger: ITileErrorLogger
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_ListenerByEvent,
  u_ListenerNotifierLinksList,
  u_BitmapLayerProviderForViewMaps;

{ TBitmapLayerProviderChangeableForMainLayer }

constructor TBitmapLayerProviderChangeableForMainLayer.Create(
  const AMainMap: IMapTypeChangeable;
  const ALayesList: IMapTypeListChangeable;
  const APostProcessing: IBitmapPostProcessingChangeable;
  const AUseTilePrevZoomConfig: IUseTilePrevZoomConfig;
  const ABitmapFactory: IBitmap32StaticFactory;
  const AErrorLogger: ITileErrorLogger
);
begin
  inherited Create;
  FMainMap := AMainMap;
  FLayesList := ALayesList;
  FErrorLogger := AErrorLogger;
  FBitmapFactory := ABitmapFactory;
  FPostProcessing := APostProcessing;
  FUseTilePrevZoomConfig := AUseTilePrevZoomConfig;

  FVersionListener := TNotifyNoMmgEventListener.Create(Self.OnMapVersionChange);

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnMainMapChange),
    FMainMap.ChangeNotifier
  );

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnLayerListChange),
    FLayesList.ChangeNotifier
  );

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FUseTilePrevZoomConfig.ChangeNotifier
  );

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FPostProcessing.ChangeNotifier
  );

end;

function TBitmapLayerProviderChangeableForMainLayer.CreateStatic: IInterface;
var
  i: Integer;
  VMap: IMapType;
  VMainMap: IMapType;
  VPostProcessingConfig: IBitmapPostProcessing;
  VLayersList: IMapTypeListStatic;
  VUsePrevConfig: IUseTilePrevZoomTileConfigStatic;
  VResult: IBitmapLayerProvider;
begin
  VMainMap := FMainMap.GetStatic;
  if FMainMapLast <> VMainMap then begin
    if Assigned(FMainMapLast) then begin
      FMainMapLast.MapType.VersionConfig.ChangeNotifier.Remove(FVersionListener);
    end;
    FMainMapLast := VMainMap;
    if Assigned(FMainMapLast) then begin
      FMainMapLast.MapType.VersionConfig.ChangeNotifier.Add(FVersionListener);
    end;
  end;
  VLayersList := FLayesList.List;
  if (not Assigned(FLayesListLast) and Assigned(VLayersList)) or
    (Assigned(FLayesListLast) and FLayesListLast.IsEqual(VLayersList))
  then begin
    if Assigned(FLayesListLast) then begin
      for i := 0 to FLayesListLast.Count - 1 do begin
        VMap := FLayesListLast.Items[i];
        VMap.MapType.VersionConfig.ChangeNotifier.Remove(FVersionListener);
      end;
    end;
    FLayesListLast := VLayersList;
    if Assigned(FLayesListLast) then begin
      for i := 0 to FLayesListLast.Count - 1 do begin
        VMap := FLayesListLast.Items[i];
        VMap.MapType.VersionConfig.ChangeNotifier.Add(FVersionListener);
      end;
    end;
  end;
  VUsePrevConfig := FUseTilePrevZoomConfig.GetStatic;
  VPostProcessingConfig := FPostProcessing.GetStatic;

  VResult :=
    TBitmapLayerProviderForViewMaps.Create(
      FBitmapFactory,
      VMainMap,
      VLayersList,
      VUsePrevConfig.UsePrevZoomAtMap,
      VUsePrevConfig.UsePrevZoomAtLayer,
      True,
      VPostProcessingConfig,
      FErrorLogger
    );
  Result := VResult;
end;

destructor TBitmapLayerProviderChangeableForMainLayer.Destroy;
var
  i: Integer;
  VMap: IMapType;
begin
  if Assigned(FMainMapLast) and Assigned(FVersionListener) then begin
    FMainMapLast.MapType.VersionConfig.ChangeNotifier.Remove(FVersionListener);
    FMainMapLast := nil;
  end;
  if Assigned(FLayesListLast) and Assigned(FVersionListener) then begin
    for i := 0 to FLayesListLast.Count - 1 do begin
      VMap := FLayesListLast.Items[i];
      VMap.MapType.VersionConfig.ChangeNotifier.Remove(FVersionListener);
    end;
    FLayesListLast := nil;
  end;
  inherited;
end;

procedure TBitmapLayerProviderChangeableForMainLayer.OnConfigChange;
begin
  LockWrite;
  try
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

procedure TBitmapLayerProviderChangeableForMainLayer.OnLayerListChange;
begin
  LockWrite;
  try
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

procedure TBitmapLayerProviderChangeableForMainLayer.OnMainMapChange;
begin
  LockWrite;
  try
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

procedure TBitmapLayerProviderChangeableForMainLayer.OnMapVersionChange;
begin
  LockWrite;
  try
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

end.
