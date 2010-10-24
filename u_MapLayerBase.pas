unit u_MapLayerBase;

interface

uses
  Types,
  GR32_Image,
  i_JclNotify,
  i_ICoordConverter,
  u_MapViewPortState,
  u_WindowLayerBasic;

type
  TMapLayerBase = class(TWindowLayerBasic)
  protected
    FScreenCenterPos: TPoint;
    FZoom: Byte;
    FGeoConvert: ICoordConverter;

    FMapPosChangeListener: IJclListener;
  public
    constructor Create(AParentMap: TImage32; AViewPortState: TMapViewPortState);
    destructor Destroy; override;

  end;

implementation

{ TMapLayerBase }

constructor TMapLayerBase.Create(AParentMap: TImage32;
  AViewPortState: TMapViewPortState);
begin
  inherited;
//  FMapPosChangeListener :=
end;

destructor TMapLayerBase.Destroy;
begin
  FViewPortState.PosChangeNotifier.Remove(FMapPosChangeListener);
  FMapPosChangeListener := nil;
  inherited;
end;

end.
