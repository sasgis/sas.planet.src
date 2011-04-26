unit u_MapLayerWithThreadDraw;

interface

uses
  Classes,
  GR32,
  GR32_Image,
  GR32_Layers,
  i_LocalCoordConverter,
  i_LocalCoordConverterFactorySimpe,
  i_BackgroundTaskLayerDraw,
  i_ViewPortState,
  u_MapLayerBasic;

type
  TMapLayerWithThreadDraw = class(TMapLayerBasic)
  protected
    FDrawTask: IBackgroundTaskLayerDraw;
    property DrawTask: IBackgroundTaskLayerDraw read FDrawTask;
  protected
    procedure SetNeedRedraw; override;
    procedure SetNeedUpdateLayerSize; override;
    procedure DoRedraw; override;
  public
    constructor Create(AParentMap: TImage32; AViewPortState: IViewPortState; ATaskFactory: IBackgroundTaskLayerDrawFactory);
    destructor Destroy; override;
    procedure StartThreads; override;
    procedure SendTerminateToThreads; override;
  end;

implementation

uses
  Types,
  SysUtils,
  t_GeoTypes,
  u_LocalCoordConverterFactorySimpe;

{ TMapLayerWithThreadDraw }

constructor TMapLayerWithThreadDraw.Create(AParentMap: TImage32;
  AViewPortState: IViewPortState;  ATaskFactory: IBackgroundTaskLayerDrawFactory);
begin
  inherited Create(AParentMap, AViewPortState);
  FDrawTask := ATaskFactory.GetTask(Layer.Bitmap);
end;

destructor TMapLayerWithThreadDraw.Destroy;
begin
  FDrawTask := nil;
  inherited;
end;

procedure TMapLayerWithThreadDraw.DoRedraw;
begin
  inherited;
  FDrawTask.StartExecute;
end;

procedure TMapLayerWithThreadDraw.SendTerminateToThreads;
begin
  inherited;
  FDrawTask.Terminate;
end;

procedure TMapLayerWithThreadDraw.SetNeedRedraw;
begin
  inherited;
  FDrawTask.StopExecute;
end;

procedure TMapLayerWithThreadDraw.SetNeedUpdateLayerSize;
begin
  inherited;
  FDrawTask.StopExecute;
end;

procedure TMapLayerWithThreadDraw.StartThreads;
begin
  inherited;
  FDrawTask.Start;
  FDrawTask.ChangePos(LayerCoordConverter);
end;

end.
