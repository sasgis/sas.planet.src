unit u_MapMainLayerNew;

interface

uses
  Types,
  SysUtils,
  GR32_Image,
  i_JclNotify,
  i_TileError,
  i_BitmapPostProcessingConfig,
  i_ActiveMapsConfig,
  i_MainMapLayerConfig,
  i_LocalCoordConverter,
  i_LocalCoordConverterFactorySimpe,
  i_BitmapLayerProvider,
  i_MapTypes,
  i_MapTypeListChangeable,
  i_InternalPerformanceCounter,
  i_ViewPortState,
  i_ImageResamplerConfig,
  u_TiledLayerWithThreadBase;

type
  TMapMainLayerNew = class(TTiledLayerWithThreadBase)
  private
    FErrorLogger: ITileErrorLogger;
    FPostProcessingConfig: IBitmapPostProcessingConfig;
    FMapsConfig: IMainMapsConfig;
    FConfig: IMainMapLayerConfig;
    FMainMap: IMapType;
    FMainMapCS: IReadWriteSync;
    FLayersSet: IMapTypeSet;
    FLayersSetCS: IReadWriteSync;

    FUsePrevZoomAtMap: Boolean;
    FUsePrevZoomAtLayer: Boolean;
    procedure OnMainMapChange;
    procedure OnLayerSetChange;
    procedure OnConfigChange;
    function GetLayersSet: IMapTypeSet;
    function GetMainMap: IMapType;
    procedure SetLayersSet(const Value: IMapTypeSet);
    procedure SetMainMap(const Value: IMapType);

    property MainMap: IMapType read GetMainMap write SetMainMap;
    property LayersSet: IMapTypeSet read GetLayersSet write SetLayersSet;
  protected
    function CreateLayerProvider(
      const ALayerConverter: ILocalCoordConverter
    ): IBitmapLayerProvider; override;
  public
    procedure StartThreads; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppClosingNotifier: IJclNotifier;
      AParentMap: TImage32;
      const AViewPortState: IViewPortState;
      const AResamplerConfig: IImageResamplerConfig;
      const AConverterFactory: ILocalCoordConverterFactorySimpe;
      const AMapsConfig: IMainMapsConfig;
      const APostProcessingConfig: IBitmapPostProcessingConfig;
      const AConfig: IMainMapLayerConfig;
      const AErrorLogger: ITileErrorLogger;
      const ATimerNoifier: IJclNotifier
    );
  end;

implementation

uses
  ActiveX,
  t_GeoTypes,
  i_TileRectUpdateNotifier,
  i_CoordConverter,
  u_NotifyEventListener,
  u_MapTypeListStatic,
  u_BitmapLayerProviderForViewMaps,
  u_Synchronizer;

{ TMapMainLayerNew }

constructor TMapMainLayerNew.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppClosingNotifier: IJclNotifier;
  AParentMap: TImage32;
  const AViewPortState: IViewPortState;
  const AResamplerConfig: IImageResamplerConfig;
  const AConverterFactory: ILocalCoordConverterFactorySimpe;
  const AMapsConfig: IMainMapsConfig;
  const APostProcessingConfig: IBitmapPostProcessingConfig;
  const AConfig: IMainMapLayerConfig;
  const AErrorLogger: ITileErrorLogger;
  const ATimerNoifier: IJclNotifier
);
begin
  inherited Create(
    APerfList,
    AAppClosingNotifier,
    AParentMap,
    AViewPortState,
    AResamplerConfig,
    AConverterFactory,
    ATimerNoifier,
    False,
    AConfig.ThreadConfig
  );
  FMapsConfig := AMapsConfig;
  FErrorLogger := AErrorLogger;
  FPostProcessingConfig := APostProcessingConfig;
  FConfig := AConfig;

  FMainMapCS := MakeSyncRW_Var(Self);
  FLayersSetCS := MakeSyncRW_Var(Self);

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnMainMapChange),
    FMapsConfig.GetActiveMap.GetChangeNotifier
  );

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnLayerSetChange),
    FMapsConfig.GetActiveBitmapLayersSet.GetChangeNotifier
  );

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FConfig.GetChangeNotifier
  );

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FPostProcessingConfig.GetChangeNotifier
  );
end;

function TMapMainLayerNew.CreateLayerProvider(
  const ALayerConverter: ILocalCoordConverter
): IBitmapLayerProvider;
var
  VMainMap: IMapType;
  VLayersSet: IMapTypeSet;
  VUsePrevZoomAtMap, VUsePrevZoomAtLayer: Boolean;
  VPostProcessingConfig: IBitmapPostProcessingConfigStatic;

  VLayers: array of IMapType;
  VLayersList: IMapTypeListStatic;
  VItem: IMapType;
  VEnum: IEnumGUID;
  VGUID: TGUID;
  VCnt: Cardinal;
  i: Integer;
  VLayersCount: Integer;
  VZOrder: Integer;
  VIndex: Integer;
begin
  VMainMap := MainMap;
  VLayersSet := LayersSet;
  VUsePrevZoomAtMap := FUsePrevZoomAtMap;
  VUsePrevZoomAtLayer := FUsePrevZoomAtLayer;
  VPostProcessingConfig := FPostProcessingConfig.GetStatic;

  VLayersCount := 0;
  try
    if VLayersSet <> nil then begin
      VEnum := VLayersSet.GetIterator;
      while VEnum.Next(1, VGUID, VCnt) = S_OK do begin
        VItem := VLayersSet.GetMapTypeByGUID(VGUID);
        if VItem.MapType.IsBitmapTiles then begin
          VZOrder := VItem.MapType.LayerDrawConfig.LayerZOrder;
          Inc(VLayersCount);
          SetLength(VLayers, VLayersCount);
          VIndex := 0;
          if VLayersCount > 1 then begin
            for i := VLayersCount - 2 downto 0 do begin
              if VLayers[i].MapType.LayerDrawConfig.LayerZOrder > VZOrder then begin
                VLayers[i + 1] := VLayers[i];
              end else begin
                VIndex := i + 1;
                Break;
              end;
            end;
          end;
          VLayers[VIndex] := VItem;
        end;
      end;
    end;
    VLayersList := TMapTypeListStatic.Create(VLayers);
  finally
    for i := 0 to Length(VLayers) - 1 do begin
      VLayers[i] := nil;
    end;
    VLayers := nil;
  end;
  Result :=
    TBitmapLayerProviderForViewMaps.Create(
      VMainMap,
      VLayersList,
      VUsePrevZoomAtMap,
      VUsePrevZoomAtLayer,
      True,
      VPostProcessingConfig,
      FErrorLogger
    );
end;

function TMapMainLayerNew.GetLayersSet: IMapTypeSet;
begin
  FLayersSetCS.BeginRead;
  try
    Result := FLayersSet;
  finally
    FLayersSetCS.EndRead;
  end;
end;

function TMapMainLayerNew.GetMainMap: IMapType;
begin
  FMainMapCS.BeginRead;
  try
    Result := FMainMap;
  finally
    FMainMapCS.EndRead;
  end;
end;

procedure TMapMainLayerNew.OnConfigChange;
begin
  ViewUpdateLock;
  try
    FConfig.LockRead;
    try
      FUsePrevZoomAtMap := FConfig.UsePrevZoomAtMap;
      FUsePrevZoomAtLayer := FConfig.UsePrevZoomAtLayer;
    finally
      FConfig.UnlockRead;
    end;
    SetNeedUpdateLayerProvider;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMapMainLayerNew.OnLayerSetChange;
var
  VNewLayersSet: IMapTypeSet;
begin
  ViewUpdateLock;
  try
    VNewLayersSet := FMapsConfig.GetActiveBitmapLayersSet.GetSelectedMapsSet;

    FLayersSetCS.BeginWrite;
    try
      FLayersSet := VNewLayersSet;
    finally
      FLayersSetCS.EndWrite;
    end;
    SetNeedUpdateLayerProvider;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMapMainLayerNew.OnMainMapChange;
var
  VOldMainMap: IMapType;
  VNewMainMap: IMapType;
begin
  ViewUpdateLock;
  try
    VNewMainMap := FMapsConfig.GetSelectedMapType;

    FMainMapCS.BeginWrite;
    try
      VOldMainMap := FMainMap;
      FMainMap := VNewMainMap;
    finally
      FMainMapCS.EndWrite;
    end;
    SetNeedUpdateLayerProvider;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMapMainLayerNew.SetLayersSet(const Value: IMapTypeSet);
begin
  FLayersSetCS.BeginWrite;
  try
    FLayersSet := Value;
  finally
    FLayersSetCS.EndWrite;
  end;
end;

procedure TMapMainLayerNew.SetMainMap(const Value: IMapType);
begin
  FMainMapCS.BeginWrite;
  try
    FMainMap := Value;
  finally
    FMainMapCS.EndWrite;
  end;
end;

procedure TMapMainLayerNew.StartThreads;
begin
  OnConfigChange;
  OnMainMapChange;
  OnLayerSetChange;
  inherited;
end;

end.
