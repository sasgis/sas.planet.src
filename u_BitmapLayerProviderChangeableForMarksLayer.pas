unit u_BitmapLayerProviderChangeableForMarksLayer;

interface

uses
  i_MarksDrawConfig,
  i_Bitmap32StaticFactory,
  i_MapTypeSetChangeable,
  i_TileError,
  i_VectorItemSubsetChangeable,
  i_ProjectedGeometryProvider,
  i_ListenerNotifierLinksList,
  i_MarkerProviderForVectorItem,
  i_BitmapLayerProvider,
  i_BitmapLayerProviderChangeable,
  u_BitmapLayerProviderChangeableBase;

type
  TBitmapLayerProviderChangeableForMarksLayer = class(TBitmapLayerProviderChangeableBase)
  private
    FConfig: IMarksDrawConfig;
    FBitmapFactory: IBitmap32StaticFactory;
    FProjectedProvider: IProjectedGeometryProvider;
    FMarkerProvider: IMarkerProviderForVectorItem;
    FVectorItems: IVectorItemSubsetChangeable;

    procedure OnConfigChange;
    procedure OnVectorItemsChange;
  protected
    function CreateStatic: IInterface; override;
  public
    constructor Create(
      const AConfig: IMarksDrawConfig;
      const ABitmapFactory: IBitmap32StaticFactory;
      const AProjectedProvider: IProjectedGeometryProvider;
      const AMarkerProvider: IMarkerProviderForVectorItem;
      const AVectorItems: IVectorItemSubsetChangeable
    );
  end;

implementation

uses
  i_VectorItemSubset,
  u_ListenerByEvent,
  u_ListenerNotifierLinksList,
  u_BitmapLayerProviderByMarksSubset;

{ TBitmapLayerProviderChangeableForMarksLayer }

constructor TBitmapLayerProviderChangeableForMarksLayer.Create(
  const AConfig: IMarksDrawConfig;
  const ABitmapFactory: IBitmap32StaticFactory;
  const AProjectedProvider: IProjectedGeometryProvider;
  const AMarkerProvider: IMarkerProviderForVectorItem;
  const AVectorItems: IVectorItemSubsetChangeable
);
begin
  inherited Create;
  FConfig := AConfig;
  FBitmapFactory := ABitmapFactory;
  FProjectedProvider := AProjectedProvider;
  FMarkerProvider := AMarkerProvider;
  FVectorItems := AVectorItems;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FConfig.ChangeNotifier
  );

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnVectorItemsChange),
    FVectorItems.ChangeNotifier
  );
end;

function TBitmapLayerProviderChangeableForMarksLayer.CreateStatic: IInterface;
var
  VVectorItems: IVectorItemSubset;
  VResult: IBitmapLayerProvider;
begin
  VResult := nil;
  VVectorItems := FVectorItems.GetStatic;
  if Assigned(VVectorItems) and not VVectorItems.IsEmpty then begin
    VResult :=
      TBitmapLayerProviderByMarksSubset.Create(
        FConfig.GetStatic,
        FBitmapFactory,
        FProjectedProvider,
        FMarkerProvider,
        VVectorItems
      );
  end;
  Result := VResult;
end;

procedure TBitmapLayerProviderChangeableForMarksLayer.OnConfigChange;
begin
  LockWrite;
  try
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

procedure TBitmapLayerProviderChangeableForMarksLayer.OnVectorItemsChange;
begin
  LockWrite;
  try
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

end.

