unit u_BitmapMarkerProviderSimpleBase;

interface

uses
  Types,
  GR32,
  i_JclNotify,
  i_BitmapMarker,
  i_BitmapMarkerProviderSimpleConfig;

type
  TBitmapMarkerProviderSimpleBase = class(TInterfacedObject, IBitmapMarkerProvider)
  private
    FConfig: IBitmapMarkerProviderSimpleConfig;
    FUseDirection: Boolean;
    FDefaultDirection: Double;
    FMarker: IBitmapMarker;
    FConfigChangeListener: IJclListener;
    FChangeNotifier: IJclNotifier;
    procedure OnConfigChange(Sender: TObject);
    function ModifyMarkerWithRotation(ASourceMarker: IBitmapMarker; AAngle: Double): IBitmapMarker;
  protected
    property Config: IBitmapMarkerProviderSimpleConfig read FConfig;
    function CreateMarker(ASize: Integer): IBitmapMarker; virtual; abstract;
  protected
    function GetUseDirection: Boolean;

    function GetMarker: IBitmapMarker;
    function GetMarkerBySize(ASize: Integer): IBitmapMarker;
    function GetMarkerWithRotation(AAngle: Double): IBitmapMarker;
    function GetMarkerWithRotationBySize(AAngle: Double;  ASize: Integer): IBitmapMarker;

    function GetChangeNotifier: IJclNotifier;
  public
    constructor Create(
      AUseDirection: Boolean;
      ADefaultDirection: Double;
      AConfig: IBitmapMarkerProviderSimpleConfig
    );
    destructor Destroy; override;
  end;

implementation

uses
  GR32_Blend,
  GR32_Rasterizers,
  GR32_Resamplers,
  GR32_Transforms,
  u_JclNotify,
  u_NotifyEventListener,
  u_GeoFun,
  u_BitmapMarker;

const
  CAngleDelta = 1.0;

{ TBitmapMarkerProviderSimpleBase }

constructor TBitmapMarkerProviderSimpleBase.Create(
  AUseDirection: Boolean;
  ADefaultDirection: Double;
  AConfig: IBitmapMarkerProviderSimpleConfig
);
begin
  FConfig := AConfig;
  FUseDirection := AUseDirection;
  FDefaultDirection := ADefaultDirection;

  FConfigChangeListener := TNotifyEventListener.Create(Self.OnConfigChange);
  FConfig.GetChangeNotifier.Add(FConfigChangeListener);

  FChangeNotifier := TJclBaseNotifier.Create;
  OnConfigChange(nil);
end;

destructor TBitmapMarkerProviderSimpleBase.Destroy;
begin
  FConfig.GetChangeNotifier.Remove(FConfigChangeListener);
  FConfigChangeListener := nil;

  inherited;
end;

function TBitmapMarkerProviderSimpleBase.GetChangeNotifier: IJclNotifier;
begin
  Result := FChangeNotifier;
end;

function TBitmapMarkerProviderSimpleBase.GetMarker: IBitmapMarker;
begin
  Result := FMarker;
end;

function TBitmapMarkerProviderSimpleBase.GetMarkerBySize(
  ASize: Integer): IBitmapMarker;
begin
  if ASize = FConfig.MarkerSize then begin
    Result := FMarker;
  end else begin
    Result := CreateMarker(ASize);
  end;
end;

function TBitmapMarkerProviderSimpleBase.GetMarkerWithRotation(
  AAngle: Double): IBitmapMarker;
begin
  if (not FUseDirection) or (Abs(CalcAngleDelta(AAngle, FDefaultDirection)) < CAngleDelta) then begin
    Result := FMarker;
  end else begin
    Result := ModifyMarkerWithRotation(FMarker, AAngle);
  end;
end;

function TBitmapMarkerProviderSimpleBase.GetMarkerWithRotationBySize(
  AAngle: Double; ASize: Integer): IBitmapMarker;
begin
  if (not FUseDirection) or (Abs(CalcAngleDelta(AAngle, FDefaultDirection)) < CAngleDelta) then begin
    Result := GetMarkerBySize(ASize);
  end else begin
    Result := ModifyMarkerWithRotation(GetMarkerBySize(ASize), AAngle);
  end;
end;

function TBitmapMarkerProviderSimpleBase.GetUseDirection: Boolean;
begin
  Result := FUseDirection;
end;

function TBitmapMarkerProviderSimpleBase.ModifyMarkerWithRotation(
  ASourceMarker: IBitmapMarker; AAngle: Double): IBitmapMarker;
var
  VSizeSource: TPoint;
  VTargetRect: TFloatRect;
  VSizeTarget: TPoint;
  VBitmap: TCustomBitmap32;
  VFixedOnBitmap: TFloatPoint;
  VTransform: TAffineTransformation;
  VRasterizer: TRasterizer;
  VTransformer: TTransformer;
  VCombineInfo: TCombineInfo;
  VSampler: TCustomResampler;
begin
  VTransform := TAffineTransformation.Create;
  try
    VSizeSource := ASourceMarker.BitmapSize;
    VTransform.SrcRect := FloatRect(0, 0, VSizeSource.X, VSizeSource.Y);
    VTransform.Rotate(0, 0, ASourceMarker.Direction - AAngle);
    VTargetRect := VTransform.GetTransformedBounds;
    VSizeTarget.X := Trunc(VTargetRect.Right - VTargetRect.Left) + 1;
    VSizeTarget.Y := Trunc(VTargetRect.Bottom - VTargetRect.Top) + 1;
    VTransform.Translate(-VTargetRect.Left, -VTargetRect.Top);
    VBitmap := TCustomBitmap32.Create;
    try
      VBitmap.SetSize(VSizeTarget.X, VSizeTarget.Y);
      VBitmap.Clear(0);

      VRasterizer := TRegularRasterizer.Create;
      try
        VSampler := TLinearResampler.Create;
        try
          VSampler.Bitmap := ASourceMarker.Bitmap;
          VTransformer := TTransformer.Create(VSampler, VTransform);
          try
            VRasterizer.Sampler := VTransformer;
            VCombineInfo.SrcAlpha := 255;
            VCombineInfo.DrawMode := dmOpaque;
            VCombineInfo.TransparentColor := 0;
            VRasterizer.Rasterize(VBitmap, VBitmap.BoundsRect, VCombineInfo);
          finally
            EMMS;
            VTransformer.Free;
          end;
        finally
          VSampler.Free;
        end;
      finally
        VRasterizer.Free;
      end;

      VFixedOnBitmap := VTransform.Transform(FloatPoint(ASourceMarker.AnchorPoint.X, ASourceMarker.AnchorPoint.Y));
      Result :=
        TBitmapMarker.Create(
          VBitmap,
          DoublePoint(VFixedOnBitmap.X, VFixedOnBitmap.Y),
          True,
          AAngle
        );
    finally
      VBitmap.Free;
    end;
  finally
    VTransform.Free;
  end;
end;

procedure TBitmapMarkerProviderSimpleBase.OnConfigChange(Sender: TObject);
begin
  FMarker := CreateMarker(FConfig.MarkerSize);
  FChangeNotifier.Notify(nil);
end;

end.

