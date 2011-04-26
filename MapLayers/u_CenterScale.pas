unit u_CenterScale;

interface

uses
  Types,
  GR32,
  GR32_Image,
  i_ViewPortState,
  i_CenterScaleConfig,
  u_WindowLayerWithPos;

type
  TCenterScale = class(TWindowLayerFixedSizeWithBitmap)
  private
    FConfig: ICenterScaleConfig;
    procedure OnConfigChange(Sender: TObject);
  protected
    function GetMapLayerLocationRect: TFloatRect; override;
  public
    procedure StartThreads; override;
  public
    constructor Create(AParentMap: TImage32; AViewPortState: IViewPortState; AConfig: ICenterScaleConfig);
  end;

implementation

uses
  SysUtils,
  u_NotifyEventListener;

{ TCenterScale }

constructor TCenterScale.Create(AParentMap: TImage32; AViewPortState: IViewPortState; AConfig: ICenterScaleConfig);
begin
  inherited Create(AParentMap, AViewPortState);
  FConfig := AConfig;
  LinksList.Add(
    TNotifyEventListener.Create(Self.OnConfigChange),
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

procedure TCenterScale.OnConfigChange(Sender: TObject);
var
  VBitmap: TCustomBitmap32;
begin
  if FConfig.Visible then begin
    VBitmap := FConfig.Bitmap;
    try
      FLayer.Bitmap.Assign(VBitmap);
      FLayer.Bitmap.DrawMode := dmBlend;
      FLayer.Bitmap.CombineMode := cmMerge;
    finally
      VBitmap.Free;
    end;
    Redraw;
  end;
  SetVisible(FConfig.Visible);
end;

procedure TCenterScale.StartThreads;
begin
  inherited;
  OnConfigChange(nil);
end;

end.
