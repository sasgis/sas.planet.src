unit u_BitmapLayerProviderChangeableForVectorMaps;

interface

uses
  i_VectorItemDrawConfig,
  i_Bitmap32StaticFactory,
  i_VectorItemSubsetChangeable,
  i_GeometryProjectedProvider,
  i_ListenerNotifierLinksList,
  i_MarkerDrawable,
  i_BitmapLayerProvider,
  u_BitmapLayerProviderChangeableBase;

type
  TBitmapLayerProviderChangeableForVectorMaps = class(TBitmapLayerProviderChangeableBase)
  private
    FConfig: IVectorItemDrawConfig;
    FBitmapFactory: IBitmap32StaticFactory;
    FPointMarker: IMarkerDrawableChangeable;
    FProjectedProvider: IGeometryProjectedProvider;
    FVectorItems: IVectorItemSubsetChangeable;

    procedure OnConfigChange;
    procedure OnItemsUpdated;
  protected
    function CreateStatic: IInterface; override;
  public
    constructor Create(
      const AConfig: IVectorItemDrawConfig;
      const APointMarker: IMarkerDrawableChangeable;
      const ABitmapFactory: IBitmap32StaticFactory;
      const AProjectedProvider: IGeometryProjectedProvider;
      const AVectorItems: IVectorItemSubsetChangeable
    );
  end;

implementation

uses
  i_VectorItemSubset,
  u_ListenerByEvent,
  u_BitmapLayerProviderByVectorSubset;

{ TBitmapLayerProviderChangeableForVectorMaps }

constructor TBitmapLayerProviderChangeableForVectorMaps.Create(
  const AConfig: IVectorItemDrawConfig;
  const APointMarker: IMarkerDrawableChangeable;
  const ABitmapFactory: IBitmap32StaticFactory;
  const AProjectedProvider: IGeometryProjectedProvider;
  const AVectorItems: IVectorItemSubsetChangeable
);
begin
  inherited Create;
  FConfig := AConfig;
  FPointMarker := APointMarker;
  FBitmapFactory := ABitmapFactory;
  FProjectedProvider := AProjectedProvider;
  FVectorItems := AVectorItems;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FConfig.ChangeNotifier
  );

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FPointMarker.ChangeNotifier
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
        FPointMarker.GetStatic,
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

end.
