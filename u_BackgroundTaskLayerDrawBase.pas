unit u_BackgroundTaskLayerDrawBase;

interface

uses
  GR32,
  t_CommonTypes,
  i_LocalCoordConverter,
  u_BackgroundTask;

type
  TBgPaintLayerEvent = procedure(AIsStop: TIsCancelChecker) of object;

  TBackgroundTaskLayerDrawBase = class(TBackgroundTask)
  private
    FOnBgPaintLayer: TBgPaintLayerEvent;
  protected
    procedure ExecuteTask; override;
  public
    constructor Create(AOnBgPaintLayer: TBgPaintLayerEvent);
  end;

implementation

uses
  Types;

{ TBackgroundTaskLayerDrawBase }

constructor TBackgroundTaskLayerDrawBase.Create(AOnBgPaintLayer: TBgPaintLayerEvent);
begin
  inherited Create;
  FOnBgPaintLayer := AOnBgPaintLayer;
end;

procedure TBackgroundTaskLayerDrawBase.ExecuteTask;
begin
  inherited;
  if Assigned(FOnBgPaintLayer) then begin
    FOnBgPaintLayer(IsNeedStopExecute);
  end;
end;

end.
