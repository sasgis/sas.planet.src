unit u_MapLayerWithThreadDraw;

interface

uses
  Classes,
  GR32,
  GR32_Image,
  GR32_Layers,
  i_ILocalCoordConverter,
  i_ILocalCoordConverterFactorySimpe,
  i_IBackgroundTaskLayerDraw,
  i_IViewPortState,
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

    procedure DoRedraw; override;
    procedure DoHide; override;
    procedure DoShow; override;
    function GetMapLayerLocationRect: TFloatRect; override;
    procedure DoPosChange(ANewVisualCoordConverter: ILocalCoordConverter); override;
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
  FBitmapCoordConverter := BuildBitmapCoordConverter(FVisualCoordConverter);
  FDrawTask.ChangePos(FBitmapCoordConverter);
end;

procedure TMapLayerWithThreadDraw.DoPosChange(
  ANewVisualCoordConverter: ILocalCoordConverter);
begin
  inherited;
  FBitmapCoordConverter := BuildBitmapCoordConverter(FVisualCoordConverter);
  FDrawTask.ChangePos(FBitmapCoordConverter);
  UpdateLayerLocation(GetMapLayerLocationRect);
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
  FBitmapCoordConverter := BuildBitmapCoordConverter(FVisualCoordConverter);
  FDrawTask.ChangePos(FBitmapCoordConverter);
  UpdateLayerLocation(GetMapLayerLocationRect);
end;

function TMapLayerWithThreadDraw.GetMapLayerLocationRect: TFloatRect;
var
  VBitmapOnMapRect: TDoubleRect;
  VBitmapOnVisualRect: TDoubleRect;
  VBitmapConverter: ILocalCoordConverter;
  VVisualConverter: ILocalCoordConverter;
begin
  VBitmapConverter := FBitmapCoordConverter;
  VVisualConverter := FVisualCoordConverter;
  if (VBitmapConverter <> nil) and (VVisualConverter <> nil) then begin
    VBitmapOnMapRect := FBitmapCoordConverter.GetRectInMapPixelFloat;
    VBitmapOnVisualRect := FVisualCoordConverter.MapRectFloat2LocalRectFloat(VBitmapOnMapRect);
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

end.
