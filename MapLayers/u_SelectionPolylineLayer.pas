unit u_SelectionPolylineLayer;

interface

uses
  GR32_Image,
  i_LocalCoordConverter,
  i_InternalPerformanceCounter,
  i_ViewPortState,
  i_VectorItemLonLat,
  i_VectorItmesFactory,
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
    FLine: ILonLatPathWithSelected;
    procedure OnLineChange;
  protected
    function GetLine(const ALocalConverter: ILocalCoordConverter): ILonLatPolygon; override;
  protected
    procedure DoConfigChange; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      AParentMap: TImage32;
      const AViewPortState: IViewPortState;
      const AFactory: IVectorItmesFactory;
      const ALineOnMapEdit: IPathOnMapEdit;
      const AConfig: ISelectionPolylineShadowLayerConfig
    );
  end;

implementation

uses
  i_DoublePointFilter,
  u_EnumDoublePointLine2Poly,
  u_NotifyEventListener;

{ TSelectionPolylineShadowLayer }

constructor TSelectionPolylineShadowLayer.Create(
  const APerfList: IInternalPerformanceCounterList;
  AParentMap: TImage32;
  const AViewPortState: IViewPortState;
  const AFactory: IVectorItmesFactory;
  const ALineOnMapEdit: IPathOnMapEdit;
  const AConfig: ISelectionPolylineShadowLayerConfig
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

function TSelectionPolylineShadowLayer.GetLine(
  const ALocalConverter: ILocalCoordConverter
): ILonLatPolygon;
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
      Factory.CreateLonLatPolygonByLonLatPathAndFilter(
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
