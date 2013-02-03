unit u_RegionProcessProgressInfoInternalFactory;

interface

uses
  i_VectorItemLonLat,
  i_RegionProcessProgressInfo,
  i_MapViewGoto,
  i_NotifierOperation,
  i_NotifierTime,
  i_RegionProcessProgressInfoInternalFactory,
  u_BaseInterfacedObject;

type
  TRegionProcessProgressInfoInternalFactory = class(TBaseInterfacedObject, IRegionProcessProgressInfoInternalFactory)
  private
    FAppClosingNotifier: INotifierOneOperation;
    FTimerNoifier: INotifierTime;
    FMapGoto: IMapViewGoto;
  private
    function Build(
      const APolygon: ILonLatPolygon
    ): IRegionProcessProgressInfoInternal;
  public
    constructor Create(
      const AAppClosingNotifier: INotifierOneOperation;
      const ATimerNoifier: INotifierTime;
      const AMapGoto: IMapViewGoto
    );
  end;

implementation

uses
  Forms,
  u_RegionProcessProgressInfo,
  u_NotifierOperation,
  u_Notifier,
  frm_ProgressSimple;

{ TRegionProcessProgressInfoInternalFactory }

constructor TRegionProcessProgressInfoInternalFactory.Create(
  const AAppClosingNotifier: INotifierOneOperation;
  const ATimerNoifier: INotifierTime; const AMapGoto: IMapViewGoto);
begin
  inherited Create;
  FAppClosingNotifier := AAppClosingNotifier;
  FTimerNoifier := ATimerNoifier;
  FMapGoto := AMapGoto;
end;

function TRegionProcessProgressInfoInternalFactory.Build(
  const APolygon: ILonLatPolygon
): IRegionProcessProgressInfoInternal;
var
  VCancelNotifierInternal: INotifierOperationInternal;
  VProgressInfo: TRegionProcessProgressInfo;
  VOperationID: Integer;
begin
  VCancelNotifierInternal := TNotifierOperation.Create(TNotifierBase.Create);
  VOperationID := VCancelNotifierInternal.CurrentOperation;
  VProgressInfo := TRegionProcessProgressInfo.Create(VCancelNotifierInternal, VOperationID);
  Result := VProgressInfo;

  TfrmProgressSimple.Create(
    Application,
    FAppClosingNotifier,
    FTimerNoifier,
    VCancelNotifierInternal,
    VProgressInfo,
    FMapGoto,
    APolygon
  );
end;

end.
