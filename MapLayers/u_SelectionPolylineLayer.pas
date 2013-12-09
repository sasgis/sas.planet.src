unit u_SelectionPolylineLayer;

interface

uses
  GR32_Image,
  i_NotifierOperation,
  i_LocalCoordConverter,
  i_LocalCoordConverterChangeable,
  i_InternalPerformanceCounter,
  i_GeometryLonLat,
  i_VectorItemsFactory,
  i_LineOnMapEdit,
  i_SelectionPolylineLayerConfig,
  u_PolyLineLayerBase,
  u_MapLayerBasic;

type
  TSelectionPolylineShadowLayer = class(TPolygonLayerBase)
  private
    FLineOnMapEdit: IPathOnMapEdit;
    FConfig: ISelectionPolylineShadowLayerConfig;
    FRadius: Double;
    FVectorGeometryLonLatFactory: IVectorGeometryLonLatFactory;
    FLine: ILonLatPathWithSelected;
    procedure OnLineChange;
  protected
    function GetLine(const ALocalConverter: ILocalCoordConverter): IGeometryLonLatMultiPolygon; override;
  protected
    procedure DoConfigChange; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      AParentMap: TImage32;
      const AView: ILocalCoordConverterChangeable;
      const AVectorGeometryProjectedFactory: IVectorGeometryProjectedFactory;
      const AVectorGeometryLonLatFactory: IVectorGeometryLonLatFactory;
      const AVectorGeometryLocalFactory: IVectorGeometryLocalFactory;
      const ALineOnMapEdit: IPathOnMapEdit;
      const AConfig: ISelectionPolylineShadowLayerConfig
    );
  end;

implementation

uses
  i_DoublePointFilter,
  u_EnumDoublePointLine2Poly,
  u_ListenerByEvent;

{ TSelectionPolylineShadowLayer }

constructor TSelectionPolylineShadowLayer.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const AView: ILocalCoordConverterChangeable;
  const AVectorGeometryProjectedFactory: IVectorGeometryProjectedFactory;
  const AVectorGeometryLonLatFactory: IVectorGeometryLonLatFactory;
  const AVectorGeometryLocalFactory: IVectorGeometryLocalFactory;
  const ALineOnMapEdit: IPathOnMapEdit;
  const AConfig: ISelectionPolylineShadowLayerConfig
);
begin
  inherited Create(
    APerfList,
    AAppStartedNotifier,
    AAppClosingNotifier,
    AParentMap,
    AView,
    AVectorGeometryProjectedFactory,
    AVectorGeometryLocalFactory,
    AConfig
  );
  FConfig := AConfig;
  FLineOnMapEdit := ALineOnMapEdit;
  FVectorGeometryLonLatFactory := AVectorGeometryLonLatFactory;

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

function TSelectionPolylineShadowLayer.GetLine(
  const ALocalConverter: ILocalCoordConverter
): IGeometryLonLatMultiPolygon;
var
  VLine: ILonLatPathWithSelected;
  VFilter: ILonLatPointFilter;
begin
  Result := nil;
  VLine := FLine;
  if VLine <> nil then begin
    VFilter :=
      TLonLatPointFilterLine2Poly.Create(
        FRadius,
        ALocalConverter.ProjectionInfo
      );
    Result :=
      FVectorGeometryLonLatFactory.CreateLonLatPolygonByLonLatPathAndFilter(
        VLine,
        VFilter
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
end;


end.
