unit u_BackgroundTaskLayerDrawBase;

interface

uses
  GR32,
  i_ILocalCoordConverter,
  i_BackgroundTaskLayerDraw,
  u_BackgroundTask;

type
  TBackgroundTaskLayerDrawBase = class(TBackgroundTask, IBackgroundTaskLayerDraw)
  private
    FBitmap: TCustomBitmap32;
    FConverter: ILocalCoordConverter;
  protected
    procedure DrawBitmap; virtual; abstract;
    procedure ResizeBitmap(ABitmapSize: TPoint);
    procedure ExecuteTask; override;
    property Converter: ILocalCoordConverter read FConverter;
    property Bitmap: TCustomBitmap32 read FBitmap;
  protected
    procedure ChangePos(AConverter: ILocalCoordConverter);
  public
    constructor Create(ABitmap: TCustomBitmap32);
  end;

implementation

uses
  Types;

{ TBackgroundTaskLayerDrawBase }

constructor TBackgroundTaskLayerDrawBase.Create(ABitmap: TCustomBitmap32);
begin
  inherited Create;
  FBitmap := ABitmap;
end;

procedure TBackgroundTaskLayerDrawBase.ChangePos(
  AConverter: ILocalCoordConverter);
begin
  StopExecute;
  try
    FConverter := AConverter;
  finally
    StartExecute;
  end;
end;

procedure TBackgroundTaskLayerDrawBase.ExecuteTask;
var
  VBitmapSize: TPoint;
begin
  inherited;
  if FConverter <> nil then begin
    VBitmapSize := FConverter.GetLocalRectSize;
  end else begin
    VBitmapSize := Point(0, 0);
  end;
  ResizeBitmap(VBitmapSize);
  if (VBitmapSize.X <> 0) and (VBitmapSize.Y <> 0) then begin
    DrawBitmap;
  end;
end;

procedure TBackgroundTaskLayerDrawBase.ResizeBitmap(ABitmapSize: TPoint);
begin
  FBitmap.Lock;
  try
    if (FBitmap.Width <> ABitmapSize.X) or (FBitmap.Height <> ABitmapSize.Y) then begin
      FBitmap.SetSize(ABitmapSize.X, ABitmapSize.Y);
    end;
  finally
    FBitmap.Unlock;
  end;
end;

end.
