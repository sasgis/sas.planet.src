unit u_BackgroundTaskLayerDrawBase;

interface

uses
  Classes,
  t_CommonTypes,
  u_BackgroundTask;

type
  TBgPaintLayerEvent = procedure(AIsStop: TIsCancelChecker) of object;

  TBackgroundTaskLayerDrawBase = class(TBackgroundTask)
  private
    FOnBgPaintLayer: TBgPaintLayerEvent;
  protected
    procedure ExecuteTask; override;
  public
    constructor Create(AOnBgPaintLayer: TBgPaintLayerEvent; APriority: TThreadPriority = tpLowest);
  end;

implementation

uses
  Types;

{ TBackgroundTaskLayerDrawBase }

constructor TBackgroundTaskLayerDrawBase.Create(AOnBgPaintLayer: TBgPaintLayerEvent; APriority: TThreadPriority);
begin
  inherited Create(APriority);
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
