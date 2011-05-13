unit u_LayerBitmapClearStrategy;

interface

uses
  GR32,
  t_GeoTypes,
  i_LayerBitmapClearStrategy;

type
  TLayerBitmapClearStrategyNOP = class(TInterfacedObject, ILayerBitmapClearStrategy)
  protected
    procedure Clear(ABitmap: TCustomBitmap32);
  end;

  TLayerBitmapClearStrategySimpleClear = class(TInterfacedObject, ILayerBitmapClearStrategy)
  protected
    procedure Clear(ABitmap: TCustomBitmap32);
  end;

  TLayerBitmapClearStrategyMoveImage = class(TInterfacedObject, ILayerBitmapClearStrategy)
  private
    FDelta: TPoint;
  protected
    procedure Clear(ABitmap: TCustomBitmap32);
  public
    constructor Create(ADelta: TPoint);
  end;

  TLayerBitmapClearStrategyImageResize = class(TInterfacedObject, ILayerBitmapClearStrategy)
  private
    FSourceBitmap: TCustomBitmap32;
    FInTargetTopLeft: TPoint;
  protected
    procedure Clear(ABitmap: TCustomBitmap32);
  public
    constructor Create(ASourceBitmap: TCustomBitmap32; ATargetRectInSource: TRect);
    destructor Destroy; override;
  end;


  TLayerBitmapClearStrategyZoomChange = class(TInterfacedObject, ILayerBitmapClearStrategy)
  private
    FSourceBitmap: TCustomBitmap32;
    FTargetRect: TRect;
  protected
    procedure Clear(ABitmap: TCustomBitmap32);
  public
    constructor Create(
      AResumpler: TCustomResampler;
      ASourceBitmap: TCustomBitmap32;
      ASourceRect: TRect;
      ATargetRect: TRect
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  GR32_Resamplers;

{ TLayerBitmapClearStrategyNOP }

procedure TLayerBitmapClearStrategyNOP.Clear(ABitmap: TCustomBitmap32);
begin
//NOP
end;

{ TLayerBitmapClearStrategySimpleClear }

procedure TLayerBitmapClearStrategySimpleClear.Clear(ABitmap: TCustomBitmap32);
begin
  ABitmap.Clear(0);
end;

{ TLayerBitmapClearStrategyMoveImage }

constructor TLayerBitmapClearStrategyMoveImage.Create(ADelta: TPoint);
begin
  FDelta := ADelta;
end;

procedure TLayerBitmapClearStrategyMoveImage.Clear(ABitmap: TCustomBitmap32);
begin
  ABitmap.Roll(FDelta.X, FDelta.Y, True, 0);
end;

{ TLayerBitmapClearStrategyImageResize }

constructor TLayerBitmapClearStrategyImageResize.Create(
  ASourceBitmap: TCustomBitmap32;
  ATargetRectInSource: TRect
);
var
  VCopyRect: TRect;
begin
  FSourceBitmap := TCustomBitmap32.Create;

  if ATargetRectInSource.Left <= 0 then begin
    VCopyRect.Left := 0;
    FInTargetTopLeft.X := -ATargetRectInSource.Left;
  end else begin
    VCopyRect.Left := ATargetRectInSource.Left;
    FInTargetTopLeft.X := 0;
  end;

  if ATargetRectInSource.Top <= 0 then begin
    VCopyRect.Top := 0;
    FInTargetTopLeft.Y := - ATargetRectInSource.Top;
  end else begin
    VCopyRect.Top := ATargetRectInSource.Top;
    FInTargetTopLeft.Y := 0;
  end;

  if ATargetRectInSource.Right >= ASourceBitmap.Width then begin
    VCopyRect.Right := ASourceBitmap.Width;
  end else begin
    VCopyRect.Right := ATargetRectInSource.Right;
  end;

  if ATargetRectInSource.Bottom >= ASourceBitmap.Height then begin
    VCopyRect.Bottom := ASourceBitmap.Height;
  end else begin
    VCopyRect.Bottom := ATargetRectInSource.Bottom;
  end;

  FSourceBitmap.SetSize(VCopyRect.Right - VCopyRect.Left, VCopyRect.Bottom - VCopyRect.Top);
  BlockTransfer(FSourceBitmap, 0, 0, FSourceBitmap.ClipRect, ASourceBitmap, VCopyRect, dmOpaque, nil);
end;

destructor TLayerBitmapClearStrategyImageResize.Destroy;
begin
  FreeAndNil(FSourceBitmap);
  inherited;
end;

procedure TLayerBitmapClearStrategyImageResize.Clear(ABitmap: TCustomBitmap32);
begin
  ABitmap.Clear(0);
  FSourceBitmap.DrawTo(ABitmap, FInTargetTopLeft.X, FInTargetTopLeft.Y);
end;

{ TLayerBitmapClearStrategyZoomChange }

constructor TLayerBitmapClearStrategyZoomChange.Create(
  AResumpler: TCustomResampler;
  ASourceBitmap: TCustomBitmap32;
  ASourceRect: TRect;
  ATargetRect: TRect
);
begin
  FSourceBitmap := TCustomBitmap32.Create;
  FSourceBitmap.SetSize(ASourceRect.Right - ASourceRect.Left, ASourceRect.Bottom - ASourceRect.Top);
  BlockTransfer(FSourceBitmap, 0, 0, FSourceBitmap.ClipRect, ASourceBitmap, ASourceRect, dmOpaque, nil);
  if AResumpler <> nil then begin
    FSourceBitmap.Resampler := AResumpler;
    end else begin
    FSourceBitmap.Resampler := TDraftResampler.Create;
  end;
  FTargetRect := ATargetRect;
end;

destructor TLayerBitmapClearStrategyZoomChange.Destroy;
begin
  FreeAndNil(FSourceBitmap);
  inherited;
end;

procedure TLayerBitmapClearStrategyZoomChange.Clear(ABitmap: TCustomBitmap32);
begin
  ABitmap.Clear(0);
  FSourceBitmap.DrawTo(ABitmap, FTargetRect, FSourceBitmap.ClipRect);
end;

end.
