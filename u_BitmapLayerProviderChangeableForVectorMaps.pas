unit u_BitmapLayerProviderChangeableForVectorMaps;

interface

uses
  i_VectorItemDrawConfig,
  i_Bitmap32StaticFactory,
  i_MapTypeSetChangeable,
  i_TileError,
  i_VectorItemSubsetChangeable,
  i_ProjectedGeometryProvider,
  i_ListenerNotifierLinksList,
  i_BitmapLayerProvider,
  i_BitmapLayerProviderChangeable,
  u_BitmapLayerProviderChangeableBase;

type
  TBitmapLayerProviderChangeableForVectorMaps = class(TBitmapLayerProviderChangeableBase)
  private
    FConfig: IVectorItemDrawConfig;
    FBitmapFactory: IBitmap32StaticFactory;
    FLayersSet: IMapTypeSetChangeable;
    FErrorLogger: ITileErrorLogger;
    FProjectedProvider: IProjectedGeometryProvider;
    FVectorItems: IVectorItemSubsetChangeable;

    procedure OnConfigChange;
    procedure OnLayerSetChange;
    procedure OnItemsUpdated;
  protected
    function CreateStatic: IInterface; override;
  public
    constructor Create(
      const AConfig: IVectorItemDrawConfig;
      const ABitmapFactory: IBitmap32StaticFactory;
      const ALayersSet: IMapTypeSetChangeable;
      const AErrorLogger: ITileErrorLogger;
      const AProjectedProvider: IProjectedGeometryProvider;
      const AVectorItems: IVectorItemSubsetChangeable
    );
  end;

implementation

uses
  i_VectorItemSubset,
  u_ListenerByEvent,
  u_ListenerNotifierLinksList,
  u_BitmapLayerProviderByVectorSubset;

{ TBitmapLayerProviderChangeableForVectorMaps }

constructor TBitmapLayerProviderChangeableForVectorMaps.Create(
  const AConfig: IVectorItemDrawConfig;
  const ABitmapFactory: IBitmap32StaticFactory;
  const ALayersSet: IMapTypeSetChangeable;
  const AErrorLogger: ITileErrorLogger;
  const AProjectedProvider: IProjectedGeometryProvider;
  const AVectorItems: IVectorItemSubsetChangeable
);
begin
  inherited Create;
  FConfig := AConfig;
  FBitmapFactory := ABitmapFactory;
  FLayersSet := ALayersSet;
  FErrorLogger := AErrorLogger;
  FProjectedProvider := AProjectedProvider;
  FVectorItems := AVectorItems;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FConfig.ChangeNotifier
  );

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnLayerSetChange),
    FLayersSet.ChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnItemsUpdated),
    FVectorItems.ChangeNotifier
  );
end;

function TBitmapLayerProviderChangeableForVectorMaps.CreateStatic: IInterface;
var
  VConfig: IVectorItemDrawConfigStatic;
  VResult: IBitmapLayerProvider;
  VVectorItems: IVectorItemSubset;
begin
  VResult := nil;
  VConfig := FConfig.GetStatic;

  VVectorItems := FVectorItems.GetStatic;
  if Assigned(VVectorItems) and not VVectorItems.IsEmpty then begin
    VResult :=
      TBitmapLayerProviderByVectorSubset.Create(
        VConfig.MainColor,
        VConfig.ShadowColor,
        VConfig.PointColor,
        FBitmapFactory,
        FProjectedProvider,
        VVectorItems
      );
  end;
  Result := VResult;
end;

procedure TBitmapLayerProviderChangeableForVectorMaps.OnConfigChange;
begin
  LockWrite;
  try
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

procedure TBitmapLayerProviderChangeableForVectorMaps.OnItemsUpdated;
begin
  LockWrite;
  try
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

procedure TBitmapLayerProviderChangeableForVectorMaps.OnLayerSetChange;
begin
  LockWrite;
  try
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

end.
