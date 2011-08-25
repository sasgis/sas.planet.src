unit u_BitmapMarkerProviderStaticFromDataProvider;

interface

uses
  Types,
  GR32,
  i_JclNotify,
  t_GeoTypes,
  i_ConfigDataProvider,
  i_ContentTypeManager,
  i_BitmapMarker;

type
  TBitmapMarkerProviderStaticFromDataProvider = class(TInterfacedObject, IBitmapMarkerProvider)
  private
    FUseDirection: Boolean;
    FDefaultDirection: Double;
    FMarker: IBitmapMarker;
    FChangeNotifier: IJclNotifier;
    function ModifyMarkerWithRotation(ASourceMarker: IBitmapMarker; AAngle: Double): IBitmapMarker;
    function ModifyMarkerWithResize(ASourceMarker: IBitmapMarker; ASize: Integer): IBitmapMarker;
    function ModifyMarkerWithRotationAndResize(ASourceMarker: IBitmapMarker; ASize: Integer; AAngle: Double): IBitmapMarker;
  protected
    function GetUseDirection: Boolean;

    function GetMarker: IBitmapMarker;
    function GetMarkerWithRotation(AAngle: Double): IBitmapMarker;
    function GetMarkerBySize(ASize: Integer): IBitmapMarker;
    function GetMarkerWithRotationBySize(AAngle: Double;  ASize: Integer): IBitmapMarker;

    function GetChangeNotifier: IJclNotifier;
  public
    constructor Create(
      AResourceDataProvider: IConfigDataProvider;
      AContentTypeManager: IContentTypeManager;
      AResourceName: string;
      AAnchorPoint: TDoublePoint;
      AUseDirection: Boolean;
      ADefaultDirection: Double
    );
  end;

implementation

uses
  Classes,
  SysUtils,
  GR32_Blend,
  GR32_Rasterizers,
  GR32_Resamplers,
  GR32_Transforms,
  u_JclNotify,
  i_ContentTypeInfo,
  u_GeoFun,
  u_BitmapMarker;

const
  CAngleDelta = 1.0;

{ TBitmapMarkerProviderStaticFromDataProvider }

constructor TBitmapMarkerProviderStaticFromDataProvider.Create(
  AResourceDataProvider: IConfigDataProvider;
  AContentTypeManager: IContentTypeManager;
  AResourceName: string;
  AAnchorPoint: TDoublePoint;
  AUseDirection: Boolean;
  ADefaultDirection: Double
);
var
  VFileName: string;
  VFileExt: string;
  VInfoBasic: IContentTypeInfoBasic;
  VBitmapContntType: IContentTypeInfoBitmap;
  VBitmap: TCustomBitmap32;
  VStream: TMemoryStream;
begin
  FUseDirection := AUseDirection;
  FDefaultDirection := ADefaultDirection;

  FChangeNotifier := TJclBaseNotifier.Create;

  VFileName := ExtractFileName(AResourceName);
  VFileExt := ExtractFileExt(VFileName);
  VBitmap := TCustomBitmap32.Create;
  try
    VInfoBasic := AContentTypeManager.GetInfoByExt(VFileExt);
    if VInfoBasic <> nil then begin
      if Supports(VInfoBasic, IContentTypeInfoBitmap, VBitmapContntType) then begin
        VStream := TMemoryStream.Create;
        try
          if AResourceDataProvider.ReadBinaryStream(VFileName, VStream) > 0 then begin
            VStream.Position := 0;
            try
              VBitmapContntType.GetLoader.LoadFromStream(VStream, VBitmap);
            except
              Assert(False, 'Ошибка при загрузке картинки ' + AResourceName);
            end;
          end;
        finally
          VStream.Free;
        end;
      end;
    end;

    FMarker :=
      TBitmapMarker.Create(
        VBitmap,
        AAnchorPoint,
        AUseDirection,
        ADefaultDirection
      );
  finally
    VBitmap.Free;
  end;
end;

function TBitmapMarkerProviderStaticFromDataProvider.GetChangeNotifier: IJclNotifier;
begin
  Result := FChangeNotifier;
end;

function TBitmapMarkerProviderStaticFromDataProvider.GetMarker: IBitmapMarker;
begin
  Result := FMarker;
end;

function TBitmapMarkerProviderStaticFromDataProvider.GetMarkerBySize(
  ASize: Integer): IBitmapMarker;
var
  VMarker: IBitmapMarker;
begin
  VMarker := FMarker;
  if ASize = VMarker.BitmapSize.X then begin
    Result := VMarker;
  end else begin
    Result := ModifyMarkerWithResize(VMarker, ASize);
  end;
end;

function TBitmapMarkerProviderStaticFromDataProvider.GetMarkerWithRotation(
  AAngle: Double): IBitmapMarker;
var
  VMarker: IBitmapMarker;
begin
  VMarker := FMarker;
  if (not FUseDirection) or (Abs(CalcAngleDelta(AAngle, VMarker.Direction)) < CAngleDelta) then begin
    Result := VMarker;
  end else begin
    Result := ModifyMarkerWithRotation(VMarker, AAngle);
  end;
end;

function TBitmapMarkerProviderStaticFromDataProvider.GetMarkerWithRotationBySize(
  AAngle: Double; ASize: Integer): IBitmapMarker;
var
  VMarker: IBitmapMarker;
begin
  VMarker := FMarker;
  if (not FUseDirection) or (Abs(CalcAngleDelta(AAngle, VMarker.Direction)) < CAngleDelta) then begin
    if (VMarker.BitmapSize.X = ASize) then begin
      Result := VMarker;
    end else begin
      Result := ModifyMarkerWithResize(VMarker, ASize);
    end;
  end else begin
    if (VMarker.BitmapSize.X = ASize) then begin
      Result := ModifyMarkerWithRotation(VMarker, AAngle);
    end else begin
      Result := ModifyMarkerWithRotationAndResize(VMarker, ASize, AAngle);
    end;
  end;
end;

function TBitmapMarkerProviderStaticFromDataProvider.GetUseDirection: Boolean;
begin
  Result := FUseDirection;
end;

function TBitmapMarkerProviderStaticFromDataProvider.ModifyMarkerWithResize(
  ASourceMarker: IBitmapMarker; ASize: Integer): IBitmapMarker;
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
      Result :=
        TBitmapMarker.Create(
          VBitmap,
          DoublePoint(VFixedOnBitmap.X, VFixedOnBitmap.Y),
          ASourceMarker.UseDirection,
          ASourceMarker.Direction
        );
    finally
      VBitmap.Free;
    end;
  finally
    VTransform.Free;
  end;
end;

function TBitmapMarkerProviderStaticFromDataProvider.ModifyMarkerWithRotation(
  ASourceMarker: IBitmapMarker; AAngle: Double): IBitmapMarker;
var
  VTransform: TAffineTransformation;
  VSizeSource: TPoint;
  VTargetRect: TFloatRect;
  VSizeTarget: TPoint;
  VBitmap: TCustomBitmap32;
  VFixedOnBitmap: TFloatPoint;
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

function TBitmapMarkerProviderStaticFromDataProvider.ModifyMarkerWithRotationAndResize(
  ASourceMarker: IBitmapMarker; ASize: Integer; AAngle: Double): IBitmapMarker;
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
begin
  VTransform := TAffineTransformation.Create;
  try
    VSizeSource := ASourceMarker.BitmapSize;
    VTransform.SrcRect := FloatRect(0, 0, VSizeSource.X, VSizeSource.Y);
    VTransform.Rotate(0, 0, ASourceMarker.Direction - AAngle);
    VScale := ASize / ASourceMarker.BitmapSize.X;
    VTransform.Scale(VScale, VScale);
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

end.
