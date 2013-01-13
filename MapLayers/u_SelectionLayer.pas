unit u_SelectionLayer;

interface

uses
  GR32_Image,
  i_NotifierOperation,
  i_LocalCoordConverter,
  i_LocalCoordConverterChangeable,
  i_InternalPerformanceCounter,
  i_LastSelectionLayerConfig,
  i_LastSelectionInfo,
  i_VectorItemLonLat,
  i_VectorItemsFactory,
  u_PolyLineLayerBase;

type
  TSelectionLayer = class(TPolygonLayerBase)
  private
    FConfig: ILastSelectionLayerConfig;
    FLastSelectionInfo: ILastSelectionInfo;

    FLine: ILonLatPolygon;
    FVisible: Boolean;

    procedure OnChangeSelection;
  protected
    function GetLine(const ALocalConverter: ILocalCoordConverter): ILonLatPolygon; override;
    procedure DoConfigChange; override;
    procedure StartThreads; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      AParentMap: TImage32;
      const AView: ILocalCoordConverterChangeable;
      const AVectorItemsFactory: IVectorItemsFactory;
      const AConfig: ILastSelectionLayerConfig;
      const ALastSelectionInfo: ILastSelectionInfo
    );
  end;


implementation

uses
  u_ListenerByEvent;

{ TSelectionLayer }

constructor TSelectionLayer.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const AView: ILocalCoordConverterChangeable;
  const AVectorItemsFactory: IVectorItemsFactory;
  const AConfig: ILastSelectionLayerConfig;
  const ALastSelectionInfo: ILastSelectionInfo
);
begin
  inherited Create(
    APerfList,
    AAppStartedNotifier,
    AAppClosingNotifier,
    AParentMap,
    AView,
    AVectorItemsFactory,
    AConfig
  );
  FConfig := AConfig;
  FLastSelectionInfo := ALastSelectionInfo;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnChangeSelection),
    FLastSelectionInfo.GetChangeNotifier
  );
end;

procedure TSelectionLayer.DoConfigChange;
begin
  inherited;
  SetNeedRedraw;
  FVisible := FConfig.Visible;
  SetVisible(FVisible);
end;

function TSelectionLayer.GetLine(
  const ALocalConverter: ILocalCoordConverter
): ILonLatPolygon;
begin
  if FVisible then begin
    Result := FLine;
  end else begin
    Result := nil;
  end;
end;

procedure TSelectionLayer.OnChangeSelection;
begin
  ViewUpdateLock;
  try
    FLine := FLastSelectionInfo.Polygon;
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

procedure TSelectionLayer.StartThreads;
begin
  inherited;
  OnChangeSelection;
end;

end.
