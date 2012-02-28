unit u_CenterScale;

interface

uses
  Types,
  GR32,
  GR32_Image,
  i_LocalCoordConverter,
  i_ViewPortState,
  i_InternalPerformanceCounter,
  i_CenterScaleConfig,
  u_WindowLayerWithPos;

type
  TCenterScale = class(TWindowLayerFixedSizeWithBitmap)
  private
    FConfig: ICenterScaleConfig;
    procedure OnConfigChange;
  protected
    function GetMapLayerLocationRect: TFloatRect; override;
    procedure SetViewCoordConverter(AValue: ILocalCoordConverter); override;
  public
    procedure StartThreads; override;
  public
    constructor Create(
      APerfList: IInternalPerformanceCounterList;
      AParentMap: TImage32;
      AViewPortState: IViewPortState;
      AConfig: ICenterScaleConfig
    );
  end;

implementation

uses
  i_Bitmap32Static,
  u_NotifyEventListener;

{ TCenterScale }

constructor TCenterScale.Create(
  APerfList: IInternalPerformanceCounterList;
  AParentMap: TImage32;
  AViewPortState: IViewPortState;
  AConfig: ICenterScaleConfig
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
begin
  VSize := Point(FLayer.Bitmap.Width, FLayer.Bitmap.Height);
  VViewSize := ViewCoordConverter.GetLocalRectSize;
  Result.Left := VViewSize.X / 2 - VSize.X / 2;
  Result.Top := VViewSize.Y / 2 - VSize.Y / 2;
  Result.Right := Result.Left + VSize.X;
  Result.Bottom := Result.Top + VSize.Y;
end;

procedure TCenterScale.OnConfigChange;
var
  VBitmap: IBitmap32Static;
begin
  ViewUpdateLock;
  try
    if FConfig.Visible then begin
      VBitmap := FConfig.Bitmap;
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
  ViewUpdate;
end;

procedure TCenterScale.SetViewCoordConverter(AValue: ILocalCoordConverter);
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
