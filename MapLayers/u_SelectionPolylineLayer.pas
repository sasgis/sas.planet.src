unit u_SelectionPolylineLayer;

interface

uses
  GR32,
  GR32_Polygons,
  GR32_Image,
  t_GeoTypes,
  i_LocalCoordConverter,
  i_InternalPerformanceCounter,
  i_ViewPortState,
  i_VectorItemProjected,
  i_VectorItemLocal,
  i_VectorItemLonLat,
  i_VectorItmesFactory,
  i_LineOnMapEdit,
  i_SelectionPolylineLayerConfig,
  u_PolyLineLayerBase,
  u_ClipPolygonByRect,
  u_MapLayerBasic;

type
  TSelectionPolylineShadowLayer = class(TPolygonLayerBase)
  private
    FLineOnMapEdit: IPathOnMapEdit;
    FConfig: ISelectionPolylineShadowLayerConfig;
    FRadius: Double;
    FLine: ILonLatPathWithSelected;
    procedure OnLineChange;
  protected
    function GetLine(ALocalConverter: ILocalCoordConverter): ILonLatPolygon; override;
  protected
    procedure DoConfigChange; override;
  public
    constructor Create(
      APerfList: IInternalPerformanceCounterList;
      AParentMap: TImage32;
      AViewPortState: IViewPortState;
      AFactory: IVectorItmesFactory;
      ALineOnMapEdit: IPathOnMapEdit;
      AConfig: ISelectionPolylineShadowLayerConfig
    );
  end;

implementation

uses
  SysUtils,
  i_CoordConverter,
  i_EnumDoublePoint,
  u_EnumDoublePointLine2Poly,
  u_EnumDoublePointsByArray,
  u_EnumDoublePointLonLatToMapPixel,
  u_EnumDoublePointMapPixelToLocalPixel,
  u_EnumDoublePointWithClip,
  u_EnumDoublePointFilterFirstSegment,
  u_EnumDoublePointFilterEqual,
  u_NotifyEventListener,
  u_GeoFun;

{ TSelectionPolylineShadowLayer }

constructor TSelectionPolylineShadowLayer.Create(
  APerfList: IInternalPerformanceCounterList;
  AParentMap: TImage32;
  AViewPortState: IViewPortState;
  AFactory: IVectorItmesFactory;
  ALineOnMapEdit: IPathOnMapEdit;
  AConfig: ISelectionPolylineShadowLayerConfig
);
begin
  inherited Create(
    APerfList,
    AParentMap,
    AViewPortState,
    AFactory,
    AConfig
  );
  FConfig := AConfig;
  FLineOnMapEdit := ALineOnMapEdit;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnLineChange),
    FLineOnMapEdit.GetChangeNotifier
  );
end;

procedure TSelectionPolylineShadowLayer.DoConfigChange;
begin
  inherited;
  FRadius := FConfig.Radius;
end;

function TSelectionPolylineShadowLayer.GetLine(ALocalConverter: ILocalCoordConverter): ILonLatPolygon;
var
  VLine: ILonLatPathWithSelected;
begin
  VLine := FLine;
  if VLine <> nil then begin
    Result :=
      Factory.CreateLonLatPolygonByLonLatPathAndFilter(
        VLine,
        TLonLatPointFilterLine2Poly.Create(
          FRadius,
          ALocalConverter.ProjectionInfo
        )
      );
  end;
end;

procedure TSelectionPolylineShadowLayer.OnLineChange;
begin
  ViewUpdateLock;
  try
    FLine := FLineOnMapEdit.Path;
    if FLine.Count > 0 then begin
      SetNeedRedraw;
      Show;
    end else begin
      Hide;
    end;
    ChangedSource;
  finally
    ViewUpdateUnlock;
  end;
  ViewUpdate;
end;


end.
