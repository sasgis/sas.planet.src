unit u_MarkerProviderForVectorItemForMarkPoints;

interface

uses
  GR32,
  i_MarkerDrawable,
  i_VectorDataItemSimple,
  i_MarksDrawConfig,
  i_Bitmap32Static,
  i_BitmapMarker,
  i_MarkerProviderForVectorItem;

type
  TMarkerProviderForVectorItemForMarkPoints = class(TInterfacedObject, IMarkerProviderForVectorItem)
  private
    FConfig: IMarksDrawConfigStatic;
    FMarkerDefault: IMarkerDrawableChangeable;

    FBitmapWithText: TBitmap32;

    function GetCaptionBitmap(
      ACaption: string;
      AFontSize: Integer;
      ATextColor: TColor32;
      ATextBgColor: TColor32;
      ASolidBgDraw: Boolean
    ): IBitmap32Static;
    function GetCaptionMarker(
      ACaption: string;
      AFontSize: Integer;
      ATextColor: TColor32;
      ATextBgColor: TColor32;
      ASolidBgDraw: Boolean;
      AMarkSize: Integer
    ): IMarkerDrawable;


    function GetIconMarker(
      ASourceMarker: IBitmapMarker;
      ASize: Integer
    ): IMarkerDrawable;
    function ModifyMarkerWithResize(
      const ASourceMarker: IBitmapMarker;
      ASize: Integer
    ): IBitmapMarker;
  private
    function GetMarker(const AItem: IVectorDataItemSimple): IMarkerDrawable;
  public
    constructor Create(
      const AMarkerDefault: IMarkerDrawableChangeable;
      const AConfig: IMarksDrawConfigStatic
    );
    destructor Destroy; override;
  end;

implementation

uses
  Types,
  SysUtils,
  GR32_Blend,
  GR32_Rasterizers,
  GR32_Resamplers,
  GR32_Transforms,
  t_GeoTypes,
  i_MarksSimple,
  u_Bitmap32Static,
  u_BitmapMarker,
  u_MarkerDrawableByBitmapMarker,
  u_MarkerDrawableByBitmap32Static,
  u_MarkerDrawableComplex,
  u_GeoFun;

{ TMarkerProviderForVectorItemForMarkPoints }

constructor TMarkerProviderForVectorItemForMarkPoints.Create(
  const AMarkerDefault: IMarkerDrawableChangeable;
  const AConfig: IMarksDrawConfigStatic
);
begin
  inherited Create;
  FConfig := AConfig;
  FMarkerDefault := AMarkerDefault;

  FBitmapWithText := TBitmap32.Create;
  FBitmapWithText.Font.Name := 'Tahoma';
  FBitmapWithText.Font.Style := [];
  FBitmapWithText.DrawMode := dmBlend;
  FBitmapWithText.CombineMode := cmMerge;
  FBitmapWithText.Resampler := TLinearResampler.Create;
end;

destructor TMarkerProviderForVectorItemForMarkPoints.Destroy;
begin
  FreeAndNil(FBitmapWithText);
  inherited;
end;

function TMarkerProviderForVectorItemForMarkPoints.GetCaptionBitmap(
  ACaption: string; AFontSize: Integer; ATextColor, ATextBgColor: TColor32;
  ASolidBgDraw: Boolean): IBitmap32Static;
var
  VTextSize: TSize;
  VBitmap: TCustomBitmap32;
begin
  Result := nil;
  if (AFontSize > 0) and (ACaption <> '') then begin
    FBitmapWithText.MasterAlpha:=AlphaComponent(ATextColor);
    FBitmapWithText.Font.Size := AFontSize;
    VTextSize := FBitmapWithText.TextExtent(ACaption);
    VTextSize.cx:=VTextSize.cx+2;
    VTextSize.cy:=VTextSize.cy+2;
    FBitmapWithText.SetSize(VTextSize.cx + 2,VTextSize.cy + 2);
    if FConfig.UseSolidCaptionBackground then begin
      FBitmapWithText.Clear(ATextBgColor);
      FBitmapWithText.RenderText(2, 2, ACaption, 1, SetAlpha(ATextColor,255));
    end else begin
      FBitmapWithText.Clear(0);
      FBitmapWithText.RenderText(2, 2, ACaption, 1, SetAlpha(ATextBgColor,255));
      FBitmapWithText.RenderText(1, 1, ACaption, 1, SetAlpha(ATextColor,255));
    end;
    VBitmap := TCustomBitmap32.Create;
    try
      VBitmap.SetSizeFrom(FBitmapWithText);
      VBitmap.Clear(0);
      VBitmap.Draw(0, 0, FBitmapWithText);
      Result := TBitmap32Static.CreateWithOwn(VBitmap);
      VBitmap := nil;
    finally
      VBitmap.Free;
    end;
  end;
end;

function TMarkerProviderForVectorItemForMarkPoints.GetCaptionMarker(
  ACaption: string;
  AFontSize: Integer;
  ATextColor, ATextBgColor: TColor32;
  ASolidBgDraw: Boolean;
  AMarkSize: Integer
): IMarkerDrawable;
var
  VTextSize: TSize;
  VBitmapStatic: IBitmap32Static;
  VAnchorPoint: TDoublePoint;
begin
  Result := nil;
  VBitmapStatic := GetCaptionBitmap(ACaption, AFontSize, ATextColor, ATextBgColor, ASolidBgDraw);
  if VBitmapStatic <> nil then begin
    VAnchorPoint := DoublePoint(- AMarkSize / 2, AMarkSize / 2 + VTextSize.cy / 2);
    Result := TMarkerDrawableByBitmap32Static.Create(VBitmapStatic, VAnchorPoint);
  end;
end;

function TMarkerProviderForVectorItemForMarkPoints.GetIconMarker(
  ASourceMarker: IBitmapMarker;
  ASize: Integer
): IMarkerDrawable;
var
  VMarker: IBitmapMarker;
begin
  Result := nil;
  if ASourceMarker = nil then begin
    if FMarkerDefault <> nil then begin
      Result := FMarkerDefault.GetStatic;
    end;
  end else begin
    VMarker := ASourceMarker;
    if ASize <> VMarker.BitmapSize.X then begin
      VMarker := ModifyMarkerWithResize(VMarker, ASize);
    end;
    Result := TMarkerDrawableByBitmapMarker.Create(VMarker);
  end;
end;

function TMarkerProviderForVectorItemForMarkPoints.GetMarker(
  const AItem: IVectorDataItemSimple): IMarkerDrawable;
var
  VMarker: IBitmapMarker;
  VMarkPoint: IMarkPoint;
  VMarkerIcon: IMarkerDrawable;
  VMarkerCaption: IMarkerDrawable;
begin
  Result := nil;
  if not Supports(AItem, IMarkPoint, VMarkPoint) then begin
    Exit;
  end;

  VMarker := nil;
  if (VMarkPoint.Pic <> nil) then begin
    VMarker := VMarkPoint.Pic.GetMarker;
  end;
  VMarkerIcon := GetIconMarker(VMarker, VMarkPoint.MarkerSize);

  VMarkerCaption := nil;
  if FConfig.ShowPointCaption then begin
    VMarkerCaption :=
      GetCaptionMarker(
        VMarkPoint.Name,
        VMarkPoint.FontSize,
        VMarkPoint.TextColor,
        VMarkPoint.TextBgColor,
        FConfig.UseSolidCaptionBackground,
        VMarkPoint.MarkerSize
      );
  end;

  if (VMarkerCaption <> nil) and (VMarkerIcon <> nil) then begin
    Result := TMarkerDrawableComplex.Create(VMarkerIcon, VMarkerCaption);
  end else if VMarkerCaption <> nil then begin
    Result := VMarkerCaption;
  end else if VMarkerIcon <> nil then begin
    Result := VMarkerIcon;
  end;
end;

function TMarkerProviderForVectorItemForMarkPoints.ModifyMarkerWithResize(
  const ASourceMarker: IBitmapMarker; ASize: Integer): IBitmapMarker;
var
  VTransform: TAffineTransformation;
  VSizeSource: TPoint;
  VTargetRect: TFloatRect;
  VSizeTarget: TPoint;
  VBitmap: TCustomBitmap32;
  VFixedOnBitmap: TFloatPoint;
  VScale: Double;
  VRasterizer: TRasterizer;
  VTransformer: TTransformer;
  VCombineInfo: TCombineInfo;
  VSampler: TCustomResampler;
  VBitmapStatic: IBitmap32Static;
begin
  VTransform := TAffineTransformation.Create;
  try
    VSizeSource := ASourceMarker.BitmapSize;
    VTransform.SrcRect := FloatRect(0, 0, VSizeSource.X, VSizeSource.Y);
    VScale := ASize / ASourceMarker.BitmapSize.X;
    VTransform.Scale(VScale, VScale);
    VTargetRect := VTransform.GetTransformedBounds;
    VSizeTarget.X := Trunc(VTargetRect.Right - VTargetRect.Left) + 1;
    VSizeTarget.Y := Trunc(VTargetRect.Bottom - VTargetRect.Top) + 1;
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
            VCombineInfo.CombineMode := cmBlend;
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
      VBitmapStatic := TBitmap32Static.CreateWithOwn(VBitmap);
      VBitmap := nil;
    finally
      VBitmap.Free;
    end;
    Result :=
      TBitmapMarker.Create(
        VBitmapStatic,
        DoublePoint(VFixedOnBitmap.X, VFixedOnBitmap.Y)
      );
  finally
    VTransform.Free;
  end;
end;

end.
