unit u_CenterScale;

interface

uses
  GR32,
  GR32_Image,
  i_NotifierOperation,
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
    function GetMapLayerLocationRect(const ANewVisualCoordConverter: ILocalCoordConverter): TFloatRect; override;
    procedure SetViewCoordConverter(const AValue: ILocalCoordConverter); override;
  public
    procedure StartThreads; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      AParentMap: TImage32;
      const AViewPortState: IViewPortState;
      const AConfig: ICenterScaleConfig
    );
  end;

implementation

uses
  u_ListenerByEvent;

{ TCenterScale }

constructor TCenterScale.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const AViewPortState: IViewPortState;
  const AConfig: ICenterScaleConfig
);
begin
  inherited Create(
    APerfList,
    AAppStartedNotifier,
    AAppClosingNotifier,
    AParentMap,
    AViewPortState
  );
  FConfig := AConfig;
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FConfig.GetChangeNotifier
  );
end;

function TCenterScale.GetMapLayerLocationRect(const ANewVisualCoordConverter: ILocalCoordConverter): TFloatRect;
var
  VSize: TPoint;
  VViewSize: TPoint;
  VBitmap: IBitmapMarker;
begin
  VBitmap := FMarker;
  if (VBitmap <> nil) and (ANewVisualCoordConverter <> nil) then begin
    VSize := VBitmap.BitmapSize;
    VViewSize := ANewVisualCoordConverter.GetLocalRectSize;
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
        Layer.Bitmap.Assign(VBitmap.Bitmap);
      end else begin
        Layer.Bitmap.Delete;
      end;
      Layer.Bitmap.DrawMode := dmBlend;
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
