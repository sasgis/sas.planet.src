unit u_CenterScale;

interface

uses
  GR32,
  GR32_Image,
  GR32_Layers,
  i_NotifierOperation,
  i_BitmapMarker,
  i_LocalCoordConverter,
  i_LocalCoordConverterChangeable,
  i_InternalPerformanceCounter,
  i_CenterScaleConfig,
  u_WindowLayerBasic;

type
  TCenterScale = class(TWindowLayerAbstract)
  private
    FConfig: ICenterScaleConfig;
    FLayer: TBitmapLayer;
    FPosition: ILocalCoordConverterChangeable;

    procedure OnConfigChange;
    procedure OnPosChange;
    function GetMapLayerLocationRect(const ANewVisualCoordConverter: ILocalCoordConverter): TFloatRect;
  protected
    procedure StartThreads; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      AParentMap: TImage32;
      const APosition: ILocalCoordConverterChangeable;
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
  const APosition: ILocalCoordConverterChangeable;
  const AConfig: ICenterScaleConfig
);
begin
  inherited Create(
    APerfList,
    AAppStartedNotifier,
    AAppClosingNotifier
  );
  FConfig := AConfig;
  FPosition := APosition;

  FLayer := TBitmapLayer.Create(AParentMap.Layers);

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FConfig.ChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnPosChange),
    FPosition.ChangeNotifier
  );
end;

function TCenterScale.GetMapLayerLocationRect(const ANewVisualCoordConverter: ILocalCoordConverter): TFloatRect;
var
  VSize: TPoint;
  VViewSize: TPoint;
  VBitmap: IBitmapMarker;
begin
  VBitmap := FConfig.BitmapProvider.GetStatic.GetMarker;
  if (VBitmap <> nil) and (ANewVisualCoordConverter <> nil) and FLayer.Visible then begin
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
  VVisible: Boolean;
  VBitmap: IBitmapMarker;
begin
  VVisible := FConfig.Visible;
  VBitmap := FConfig.BitmapProvider.GetStatic.GetMarker;
  if VVisible and (VBitmap <> nil) then begin
    FLayer.Bitmap.Assign(VBitmap.Bitmap);
    FLayer.Bitmap.DrawMode := dmBlend;
    FLayer.Location := GetMapLayerLocationRect(FPosition.GetStatic);
    FLayer.Visible := True;
  end else begin
    FLayer.Visible := False;
    FLayer.Bitmap.Delete;
  end;
end;

procedure TCenterScale.OnPosChange;
begin
  if FLayer.Visible then begin
    FLayer.Location := GetMapLayerLocationRect(FPosition.GetStatic);
  end;
end;

procedure TCenterScale.StartThreads;
begin
  inherited;
  OnConfigChange;
end;

end.
