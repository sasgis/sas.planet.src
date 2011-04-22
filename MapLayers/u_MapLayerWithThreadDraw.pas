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
  TMapLayerWithThreadDraw = class(TMapLayerBasicFullView)
  protected
    FDrawTask: IBackgroundTaskLayerDraw;
    FBitmapCoordConverter: ILocalCoordConverter;
    FBitmapCoordConverterFactory: ILocalCoordConverterFactorySimpe;
    FLayer: TBitmapLayer;
    property DrawTask: IBackgroundTaskLayerDraw read FDrawTask;
    function BuildBitmapCoordConverter(ANewVisualCoordConverter: ILocalCoordConverter): ILocalCoordConverter; virtual;

    procedure UpdateLayerSize(ANewSize: TPoint); virtual;
    procedure DoUpdateLayerSize(ANewSize: TPoint); virtual;
  protected
    procedure DoRedraw; override;
    procedure DoHide; override;
    procedure DoShow; override;
    function GetMapLayerLocationRect: TFloatRect; override;
    procedure PreparePosChange(ANewVisualCoordConverter: ILocalCoordConverter); override;
    procedure AfterPosChange; override;
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

procedure TMapLayerWithThreadDraw.AfterPosChange;
begin
  inherited;
  FDrawTask.StopExecute;
  try
    UpdateLayerSize(FBitmapCoordConverter.GetLocalRectSize);
    UpdateLayerLocation;
    FDrawTask.ChangePos(FBitmapCoordConverter);
  finally
    FDrawTask.StartExecute;
  end;
end;

function TMapLayerWithThreadDraw.BuildBitmapCoordConverter(
  ANewVisualCoordConverter: ILocalCoordConverter): ILocalCoordConverter;
begin
  if Visible then begin
    Result := ANewVisualCoordConverter;
  end else begin
    Result := nil;
  end;
end;

constructor TMapLayerWithThreadDraw.Create(AParentMap: TImage32;
  AViewPortState: IViewPortState;  ATaskFactory: IBackgroundTaskLayerDrawFactory);
begin
  FLayer := TBitmapLayer.Create(AParentMap.Layers);
  inherited Create(FLayer, AViewPortState);
  FLayer.Bitmap.DrawMode := dmBlend;
  FLayer.Bitmap.CombineMode := cmMerge;
  FDrawTask := ATaskFactory.GetTask(FLayer.Bitmap);
  FBitmapCoordConverterFactory := TLocalCoordConverterFactorySimpe.Create;
end;

destructor TMapLayerWithThreadDraw.Destroy;
begin
  FDrawTask := nil;
  FBitmapCoordConverterFactory := nil;
  inherited;
end;

procedure TMapLayerWithThreadDraw.DoHide;
begin
  inherited;
  FBitmapCoordConverter := BuildBitmapCoordConverter(VisualCoordConverter);
  FDrawTask.ChangePos(FBitmapCoordConverter);
end;

procedure TMapLayerWithThreadDraw.PreparePosChange(
  ANewVisualCoordConverter: ILocalCoordConverter);
begin
  inherited;
  FBitmapCoordConverter := BuildBitmapCoordConverter(VisualCoordConverter);
end;

procedure TMapLayerWithThreadDraw.DoRedraw;
begin
  inherited;
  FDrawTask.StopExecute;
  FDrawTask.StartExecute;
end;

procedure TMapLayerWithThreadDraw.DoShow;
begin
  inherited;
  FBitmapCoordConverter := BuildBitmapCoordConverter(VisualCoordConverter);
  UpdateLayerSize(FBitmapCoordConverter.GetLocalRectSize);
  UpdateLayerLocation;
  FDrawTask.ChangePos(FBitmapCoordConverter);
end;

procedure TMapLayerWithThreadDraw.DoUpdateLayerSize(ANewSize: TPoint);
begin

end;

function TMapLayerWithThreadDraw.GetMapLayerLocationRect: TFloatRect;
var
  VBitmapOnMapRect: TDoubleRect;
  VBitmapOnVisualRect: TDoubleRect;
  VBitmapConverter: ILocalCoordConverter;
  VVisualConverter: ILocalCoordConverter;
begin
  VBitmapConverter := FBitmapCoordConverter;
  VVisualConverter := VisualCoordConverter;
  if (VBitmapConverter <> nil) and (VVisualConverter <> nil) then begin
    VBitmapOnMapRect := VBitmapConverter.GetRectInMapPixelFloat;
    VBitmapOnVisualRect := VVisualConverter.MapRectFloat2LocalRectFloat(VBitmapOnMapRect);
    Result := FloatRect(VBitmapOnVisualRect.Left, VBitmapOnVisualRect.Top, VBitmapOnVisualRect.Right, VBitmapOnVisualRect.Bottom);
  end else begin
    Result := FloatRect(0, 0, 0, 0);
  end;
end;

procedure TMapLayerWithThreadDraw.SendTerminateToThreads;
begin
  inherited;
  FDrawTask.Terminate;
end;

procedure TMapLayerWithThreadDraw.StartThreads;
begin
  inherited;
  FDrawTask.Start;
  FDrawTask.ChangePos(FBitmapCoordConverter);
end;

procedure TMapLayerWithThreadDraw.UpdateLayerSize(ANewSize: TPoint);
begin

end;

end.
