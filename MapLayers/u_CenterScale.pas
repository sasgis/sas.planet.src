unit u_CenterScale;

interface

uses
  Types,
  GR32,
  GR32_Image,
  i_BitmapMarker,
  i_LocalCoordConverter,
  i_ViewPortState,
  i_InternalPerformanceCounter,
  i_CenterScaleConfig,
  u_WindowLayerWithPos;

type
  TCenterScale = class(TWindowLayerFixedSizeWithBitmap)
  private
    FConfig: ICenterScaleConfig;
    FMarker: IBitmapMarker;
    procedure OnConfigChange;
  protected
    function GetMapLayerLocationRect: TFloatRect; override;
    procedure SetViewCoordConverter(const AValue: ILocalCoordConverter); override;
  public
    procedure StartThreads; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      AParentMap: TImage32;
      const AViewPortState: IViewPortState;
      const AConfig: ICenterScaleConfig
    );
  end;

implementation

uses
  u_NotifyEventListener;

{ TCenterScale }

constructor TCenterScale.Create(
  const APerfList: IInternalPerformanceCounterList;
  AParentMap: TImage32;
  const AViewPortState: IViewPortState;
  const AConfig: ICenterScaleConfig
);
begin
  inherited Create(APerfList, AParentMap, AViewPortState);
  FConfig := AConfig;
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FConfig.GetChangeNotifier
  );
end;

function TCenterScale.GetMapLayerLocationRect: TFloatRect;
var
  VSize: TPoint;
  VViewSize: TPoint;
  VBitmap: IBitmapMarker;
begin
  VBitmap := FMarker;
  if VBitmap <> nil then begin
    VSize := VBitmap.BitmapSize;
    VViewSize := ViewCoordConverter.GetLocalRectSize;
    Result.Left := VViewSize.X / 2 - VBitmap.AnchorPoint.X;
    Result.Top := VViewSize.Y / 2 - VBitmap.AnchorPoint.Y;
    Result.Right := Result.Left + VSize.X;
    Result.Bottom := Result.Top + VSize.Y;
  end else begin
    Result.Left := 0;
    Result.Top := 0;
    Result.Right := 0;
    Result.Bottom := 0;
  end;
end;

procedure TCenterScale.OnConfigChange;
var
  VBitmap: IBitmapMarker;
begin
  ViewUpdateLock;
  try
    if FConfig.Visible then begin
      VBitmap := FConfig.Bitmap;
      FMarker := VBitmap;
      if VBitmap <> nil then begin
        FLayer.Bitmap.Assign(VBitmap.Bitmap);
      end else begin
        FLayer.Bitmap.Delete;
      end;
      FLayer.Bitmap.DrawMode := dmBlend;
      SetNeedRedraw;
    end;
    SetVisible(FConfig.Visible);
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TCenterScale.SetViewCoordConverter(const AValue: ILocalCoordConverter);
begin
  inherited;
  SetNeedUpdateLocation;
end;

procedure TCenterScale.StartThreads;
begin
  inherited;
  OnConfigChange;
end;

end.
