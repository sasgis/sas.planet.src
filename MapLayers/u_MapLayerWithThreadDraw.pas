unit u_MapLayerWithThreadDraw;

interface

uses
  Classes,
  SyncObjs,
  GR32,
  GR32_Image,
  GR32_Layers,
  i_ILocalCoordConverter,
  u_ThreadDrawMapLayer,
  u_MapViewPortState,
  u_MapLayerBasic;

type
  TMapLayerWithThreadDraw = class(TMapLayerBasicNoBitmap)
  protected
    FThread: TThreadDrawMapLayer;
    FBitmapCoordConverter: ILocalCoordConverter;
    FLayer: TBitmapLayer;
    procedure DoRedraw; override;
    procedure DoHide; override;
    procedure DoShow; override;
    function GetMapLayerLocationRect: TFloatRect; override;
    procedure DoUpdateLayerSize; override;
    procedure OnPosChange(Sender: TObject); override;
  public
    constructor Create(AParentMap: TImage32; AViewPortState: TMapViewPortState; AThreadFactory: TThreadDrawMapLayerFactory);
    destructor Destroy; override;
    procedure StartThreads; override;
    procedure SendTerminateToThreads; override;
  end;

implementation

uses
  SysUtils,
  t_GeoTypes,
  Ugeofun;

{ TMapLayerWithThreadDraw }

constructor TMapLayerWithThreadDraw.Create(AParentMap: TImage32;
  AViewPortState: TMapViewPortState; AThreadFactory: TThreadDrawMapLayerFactory);
begin
  FLayer := TBitmapLayer.Create(AParentMap.Layers);
  inherited Create(FLayer, AViewPortState);
  FLayer.Bitmap.DrawMode := dmBlend;
  FLayer.Bitmap.CombineMode := cmMerge;
  FThread := AThreadFactory.CreateThread(FLayer.Bitmap);
end;

destructor TMapLayerWithThreadDraw.Destroy;
begin
  FreeAndNil(FThread);
  inherited;
end;

procedure TMapLayerWithThreadDraw.DoHide;
begin
  inherited;
  FThread.StopDraw;
end;

procedure TMapLayerWithThreadDraw.DoRedraw;
begin
  inherited;
  FThread.StopDraw;
  FThread.StartDraw;
end;

procedure TMapLayerWithThreadDraw.DoShow;
begin
  inherited;
  FThread.StartDraw;
end;

procedure TMapLayerWithThreadDraw.DoUpdateLayerSize;
begin
  inherited;
  FThread.ChangePosOrSize(FMapViewSize, FBitmapCoordConverter);
end;

function TMapLayerWithThreadDraw.GetMapLayerLocationRect: TFloatRect;
var
  VBitmapRect: TDoubleRect;
  VBitmapOnMapRect: TDoubleRect;
  VBitmapOnVisualRect: TDoubleRect;
begin
  VBitmapRect := DoubleRect(0, 0, FMapViewSize.X, FMapViewSize.Y);
  VBitmapOnMapRect := FBitmapCoordConverter.LocalRectFloat2MapRectFloat(VBitmapRect);
  VBitmapOnVisualRect := FVisualCoordConverter.MapRectFloat2LocalRectFloat(VBitmapOnMapRect);
  Result := FloatRect(VBitmapOnVisualRect.Left, VBitmapOnVisualRect.Top, VBitmapOnVisualRect.Right, VBitmapOnVisualRect.Bottom);
end;

procedure TMapLayerWithThreadDraw.OnPosChange(Sender: TObject);
begin
  inherited;
  FBitmapCoordConverter :=  FVisualCoordConverter;
  FThread.ChangePosOrSize(FMapViewSize, FBitmapCoordConverter);
  UpdateLayerLocation;
end;

procedure TMapLayerWithThreadDraw.SendTerminateToThreads;
begin
  inherited;
  FThread.Terminate;
end;

procedure TMapLayerWithThreadDraw.StartThreads;
begin
  inherited;
  FThread.Resume;
end;

end.
