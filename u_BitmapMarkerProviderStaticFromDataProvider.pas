{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
{* This program is free software: you can redistribute it and/or modify       *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* This program is distributed in the hope that it will be useful,            *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *}
{*                                                                            *}
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_BitmapMarkerProviderStaticFromDataProvider;

interface

uses
  GR32,
  t_GeoTypes,
  i_ConfigDataProvider,
  i_ContentTypeManager,
  i_BitmapMarker;

type
  TBitmapMarkerProviderStatic = class(TInterfacedObject, IBitmapMarkerProvider)
  private
    FMarker: IBitmapMarker;
    function ModifyMarkerWithResize(
      const ASourceMarker: IBitmapMarker;
      ASize: Integer
    ): IBitmapMarker;
  protected
    function GetMarker: IBitmapMarker;
    function GetMarkerBySize(ASize: Integer): IBitmapMarker;
  public
    constructor Create(const AMarker: IBitmapMarker);
  end;

  TBitmapMarkerProviderStaticFromDataProvider = class(TBitmapMarkerProviderStatic)
  public
    constructor Create(
      const AResourceDataProvider: IConfigDataProvider;
      const AContentTypeManager: IContentTypeManager;
      const AResourceName: string;
      const AAnchorPoint: TDoublePoint
    );
  end;

  TBitmapMarkerWithDirectionProviderStaticFromDataProvider = class(TInterfacedObject, IBitmapMarkerProvider, IBitmapMarkerWithDirectionProvider)
  private
    FMarker: IBitmapMarkerWithDirection;
    function ModifyMarkerWithRotation(
      const ASourceMarker: IBitmapMarkerWithDirection;
      const AAngle: Double
    ): IBitmapMarkerWithDirection;
    function ModifyMarkerWithResize(
      const ASourceMarker: IBitmapMarkerWithDirection;
      ASize: Integer
    ): IBitmapMarkerWithDirection;
    function ModifyMarkerWithRotationAndResize(
      const ASourceMarker: IBitmapMarkerWithDirection;
      ASize: Integer;
      const AAngle: Double
    ): IBitmapMarkerWithDirection;
  protected
    function GetMarker: IBitmapMarker;
    function GetMarkerWithRotation(const AAngle: Double): IBitmapMarkerWithDirection;
    function GetMarkerBySize(ASize: Integer): IBitmapMarker;
    function GetMarkerWithRotationBySize(const AAngle: Double;  ASize: Integer): IBitmapMarkerWithDirection;
  public
    constructor Create(
      const AResourceDataProvider: IConfigDataProvider;
      const AContentTypeManager: IContentTypeManager;
      const AResourceName: string;
      const AAnchorPoint: TDoublePoint;
      const ADefaultDirection: Double
    );
  end;

implementation

uses
  SysUtils,
  GR32_Blend,
  GR32_Rasterizers,
  GR32_Resamplers,
  GR32_Transforms,
  i_BinaryData,
  i_Bitmap32Static,
  i_ContentTypeInfo,
  u_GeoFun,
  u_Bitmap32Static,
  u_BitmapMarker;

const
  CAngleDelta = 1.0;

{ TBitmapMarkerProviderStatic }

constructor TBitmapMarkerProviderStatic.Create(const AMarker: IBitmapMarker);
begin
  FMarker := AMarker;
end;

function TBitmapMarkerProviderStatic.GetMarker: IBitmapMarker;
begin
  Result := FMarker;
end;

function TBitmapMarkerProviderStatic.GetMarkerBySize(
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

function TBitmapMarkerProviderStatic.ModifyMarkerWithResize(
  const ASourceMarker: IBitmapMarker;
  ASize: Integer
): IBitmapMarker;
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

{ TBitmapMarkerProviderStaticFromDataProvider }

constructor TBitmapMarkerProviderStaticFromDataProvider.Create(
  const AResourceDataProvider: IConfigDataProvider;
  const AContentTypeManager: IContentTypeManager;
  const AResourceName: string;
  const AAnchorPoint: TDoublePoint
);
var
  VFileName: string;
  VFileExt: string;
  VInfoBasic: IContentTypeInfoBasic;
  VBitmapContntType: IContentTypeInfoBitmap;
  VBitmap: IBitmap32Static;
  VData: IBinaryData;
begin
  VFileName := ExtractFileName(AResourceName);
  VFileExt := ExtractFileExt(VFileName);
  VBitmap := nil;
  VInfoBasic := AContentTypeManager.GetInfoByExt(VFileExt);
  if VInfoBasic <> nil then begin
    if Supports(VInfoBasic, IContentTypeInfoBitmap, VBitmapContntType) then begin
      VData := AResourceDataProvider.ReadBinary(VFileName);
      if VData <> nil then begin
        try
          VBitmap := VBitmapContntType.GetLoader.Load(VData);
        except
          Assert(False, 'Ошибка при загрузке картинки ' + AResourceName);
        end;
      end;
    end;
  end;
  inherited Create(
    TBitmapMarker.Create(
      VBitmap,
      AAnchorPoint
    )
  );
end;

{ TBitmapMarkerWithDirectionProviderStaticFromDataProvider }

constructor TBitmapMarkerWithDirectionProviderStaticFromDataProvider.Create(
  const AResourceDataProvider: IConfigDataProvider;
  const AContentTypeManager: IContentTypeManager;
  const AResourceName: string;
  const AAnchorPoint: TDoublePoint;
  const ADefaultDirection: Double
);
var
  VFileName: string;
  VFileExt: string;
  VInfoBasic: IContentTypeInfoBasic;
  VBitmapContntType: IContentTypeInfoBitmap;
  VBitmap: IBitmap32Static;
  VData: IBinaryData;
begin
  VFileName := ExtractFileName(AResourceName);
  VFileExt := ExtractFileExt(VFileName);
  VBitmap := nil;
  VInfoBasic := AContentTypeManager.GetInfoByExt(VFileExt);
  if VInfoBasic <> nil then begin
    if Supports(VInfoBasic, IContentTypeInfoBitmap, VBitmapContntType) then begin
      VData := AResourceDataProvider.ReadBinary(VFileName);
      if VData <> nil then begin
        try
          VBitmap := VBitmapContntType.GetLoader.Load(VData);
        except
          Assert(False, 'Ошибка при загрузке картинки ' + AResourceName);
        end;
      end;
    end;
  end;

  FMarker :=
    TBitmapMarkerWithDirection.Create(
      VBitmap,
      AAnchorPoint,
      ADefaultDirection
    );
end;

function TBitmapMarkerWithDirectionProviderStaticFromDataProvider.GetMarker: IBitmapMarker;
begin
  Result := FMarker;
end;

function TBitmapMarkerWithDirectionProviderStaticFromDataProvider.GetMarkerBySize(
  ASize: Integer): IBitmapMarker;
var
  VMarker: IBitmapMarkerWithDirection;
begin
  VMarker := FMarker;
  if ASize = VMarker.BitmapSize.X then begin
    Result := VMarker;
  end else begin
    Result := ModifyMarkerWithResize(VMarker, ASize);
  end;
end;

function TBitmapMarkerWithDirectionProviderStaticFromDataProvider.GetMarkerWithRotation(
  const AAngle: Double
): IBitmapMarkerWithDirection;
var
  VMarker: IBitmapMarkerWithDirection;
begin
  VMarker := FMarker;
  Result := VMarker;
  if Abs(CalcAngleDelta(AAngle, VMarker.Direction)) > CAngleDelta then begin
    Result := ModifyMarkerWithRotation(VMarker, AAngle);
  end;
end;

function TBitmapMarkerWithDirectionProviderStaticFromDataProvider.GetMarkerWithRotationBySize(
  const AAngle: Double;
  ASize: Integer
): IBitmapMarkerWithDirection;
var
  VMarker: IBitmapMarkerWithDirection;
begin
  VMarker := FMarker;
  Result := VMarker;
  if Abs(CalcAngleDelta(AAngle, VMarker.Direction)) > CAngleDelta then begin
    if (VMarker.BitmapSize.X = ASize) then begin
      Result := ModifyMarkerWithRotation(VMarker, AAngle);
    end else begin
      Result := ModifyMarkerWithRotationAndResize(VMarker, ASize, AAngle);
    end;
  end else begin
    if (VMarker.BitmapSize.X <> ASize) then begin
      Result := ModifyMarkerWithResize(VMarker, ASize);
    end;
  end;
end;

function TBitmapMarkerWithDirectionProviderStaticFromDataProvider.ModifyMarkerWithResize(
  const ASourceMarker: IBitmapMarkerWithDirection;
  ASize: Integer
): IBitmapMarkerWithDirection;
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
  VMarkerWithDirection: IBitmapMarkerWithDirection;
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
      TBitmapMarkerWithDirection.Create(
        VBitmapStatic,
        DoublePoint(VFixedOnBitmap.X, VFixedOnBitmap.Y),
        VMarkerWithDirection.Direction
      );
  finally
    VTransform.Free;
  end;
end;

function TBitmapMarkerWithDirectionProviderStaticFromDataProvider.ModifyMarkerWithRotation(
  const ASourceMarker: IBitmapMarkerWithDirection;
  const AAngle: Double
): IBitmapMarkerWithDirection;
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
  VBitmapStatic: IBitmap32Static;
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
      VBitmapStatic := TBitmap32Static.CreateWithOwn(VBitmap);
      VBitmap := nil;
    finally
      VBitmap.Free;
    end;
    Result :=
      TBitmapMarkerWithDirection.Create(
        VBitmapStatic,
        DoublePoint(VFixedOnBitmap.X, VFixedOnBitmap.Y),
        AAngle
      );
  finally
    VTransform.Free;
  end;
end;

function TBitmapMarkerWithDirectionProviderStaticFromDataProvider.ModifyMarkerWithRotationAndResize(
  const ASourceMarker: IBitmapMarkerWithDirection;
  ASize: Integer;
  const AAngle: Double
): IBitmapMarkerWithDirection;
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
      VBitmapStatic := TBitmap32Static.CreateWithOwn(VBitmap);
      VBitmap := nil;
    finally
      VBitmap.Free;
    end;
    Result :=
      TBitmapMarkerWithDirection.Create(
        VBitmapStatic,
        DoublePoint(VFixedOnBitmap.X, VFixedOnBitmap.Y),
        AAngle
      );
  finally
    VTransform.Free;
  end;
end;

end.
