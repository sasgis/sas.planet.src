unit u_BackgroundTaskLayerDrawBase;

interface

uses
  GR32,
  i_ILocalCoordConverter,
  i_IBackgroundTaskLayerDraw,
  u_BackgroundTask;

type
  TBackgroundTaskLayerDrawBase = class(TBackgroundTask, IBackgroundTaskLayerDraw)
  private
    FBitmap: TCustomBitmap32;
    FBitmapSize: TPoint;
    FConverter: ILocalCoordConverter;
  protected
    procedure DrawBitmap; virtual; abstract;
    procedure ResizeBitmap;
    procedure ExecuteTask; override;
    property Converter: ILocalCoordConverter read FConverter;
    property BitmapSize: TPoint read FBitmapSize;
    property Bitmap: TCustomBitmap32 read FBitmap;
  protected
    procedure ChangePos(AConverter: ILocalCoordConverter);
    procedure ChangeSize(ANewSize: TPoint);
  public
    constructor Create(ABitmap: TCustomBitmap32);
  end;
  
implementation

{ TBackgroundTaskLayerDrawBase }

constructor TBackgroundTaskLayerDrawBase.Create(ABitmap: TCustomBitmap32);
begin
  inherited Create;
  FBitmap := ABitmap;
  FBitmapSize := Point(FBitmap.Width, FBitmap.Height);
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

procedure TBackgroundTaskLayerDrawBase.ChangeSize(ANewSize: TPoint);
begin
  StopExecute;
  try
    FBitmapSize := ANewSize;
  finally
    StartExecute;
  end;
end;

procedure TBackgroundTaskLayerDrawBase.ExecuteTask;
begin
  if FConverter <> nil then begin
    inherited;
    ResizeBitmap;
    DrawBitmap;
  end;
end;

procedure TBackgroundTaskLayerDrawBase.ResizeBitmap;
begin
  FBitmap.Lock;
  try
    if (FBitmap.Width <> FBitmapSize.X) or (FBitmap.Height <> FBitmapSize.Y) then begin
      FBitmap.SetSize(FBitmapSize.X, FBitmapSize.Y);
    end;
  finally
    FBitmap.Unlock;
  end;
end;

end.
