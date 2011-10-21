{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
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
    FMarker: IBitmapMarker;
    function ModifyMarkerWithResize(ASourceMarker: IBitmapMarker; ASize: Integer): IBitmapMarker;
  protected
    function GetMarker: IBitmapMarker;
    function GetMarkerBySize(ASize: Integer): IBitmapMarker;
  public
    constructor Create(
      AResourceDataProvider: IConfigDataProvider;
      AContentTypeManager: IContentTypeManager;
      AResourceName: string;
      AAnchorPoint: TDoublePoint
    );
  end;

  TBitmapMarkerWithDirectionProviderStaticFromDataProvider = class(TInterfacedObject, IBitmapMarkerProvider, IBitmapMarkerWithDirectionProvider)
  private
    FMarker: IBitmapMarkerWithDirection;
    function ModifyMarkerWithRotation(ASourceMarker: IBitmapMarkerWithDirection; AAngle: Double): IBitmapMarkerWithDirection;
    function ModifyMarkerWithResize(ASourceMarker: IBitmapMarkerWithDirection; ASize: Integer): IBitmapMarkerWithDirection;
    function ModifyMarkerWithRotationAndResize(ASourceMarker: IBitmapMarkerWithDirection; ASize: Integer; AAngle: Double): IBitmapMarkerWithDirection;
  protected
    function GetMarker: IBitmapMarker;
    function GetMarkerWithRotation(AAngle: Double): IBitmapMarkerWithDirection;
    function GetMarkerBySize(ASize: Integer): IBitmapMarker;
    function GetMarkerWithRotationBySize(AAngle: Double;  ASize: Integer): IBitmapMarkerWithDirection;
  public
    constructor Create(
      AResourceDataProvider: IConfigDataProvider;
      AContentTypeManager: IContentTypeManager;
      AResourceName: string;
      AAnchorPoint: TDoublePoint;
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
  AAnchorPoint: TDoublePoint
);
var
  VFileName: string;
  VFileExt: string;
  VInfoBasic: IContentTypeInfoBasic;
  VBitmapContntType: IContentTypeInfoBitmap;
  VBitmap: TCustomBitmap32;
  VStream: TMemoryStream;
begin
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
        AAnchorPoint
      );
  finally
    VBitmap.Free;
  end;
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

//function TBitmapMarkerProviderStaticFromDataProvider.GetMarkerWithRotation(
//  AAngle: Double): IBitmapMarkerWithDirection;
//var
//  VMarker: IBitmapMarker;
//  VMarkerWithDirection: IBitmapMarkerWithDirection;
//begin
//  VMarker := FMarker;
//  Result := VMarker;
//  if Supports(VMarker, IBitmapMarkerWithDirection, VMarkerWithDirection) then begin
//    if Abs(CalcAngleDelta(AAngle, VMarkerWithDirection.Direction)) > CAngleDelta then begin
//      Result := ModifyMarkerWithRotation(VMarkerWithDirection, AAngle);
//    end;
//  end;
//end;
//
//function TBitmapMarkerProviderStaticFromDataProvider.GetMarkerWithRotationBySize(
//  AAngle: Double; ASize: Integer): IBitmapMarkerWithDirection;
//var
//  VMarker: IBitmapMarker;
//  VMarkerWithDirection: IBitmapMarkerWithDirection;
//begin
//  VMarker := FMarker;
//  Result := VMarker;
//  if Supports(VMarker, IBitmapMarkerWithDirection, VMarkerWithDirection) then begin
//    if Abs(CalcAngleDelta(AAngle, VMarkerWithDirection.Direction)) > CAngleDelta then begin
//      if (VMarker.BitmapSize.X = ASize) then begin
//        Result := ModifyMarkerWithRotation(VMarkerWithDirection, AAngle);
//      end else begin
//        Result := ModifyMarkerWithRotationAndResize(VMarkerWithDirection, ASize, AAngle);
//      end;
//    end else begin
//      if (VMarker.BitmapSize.X <> ASize) then begin
//        Result := ModifyMarkerWithResize(VMarker, ASize);
//      end;
//    end;
//  end else begin
//    if (VMarker.BitmapSize.X = ASize) then begin
//      Result := VMarker;
//    end else begin
//      Result := ModifyMarkerWithResize(VMarker, ASize);
//    end;
//  end;
//
//end;

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
  VMarkerWithDirection: IBitmapMarkerWithDirection;
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

      if Supports(ASourceMarker, IBitmapMarkerWithDirection, VMarkerWithDirection) then begin
        Result :=
          TBitmapMarkerWithDirection.Create(
            VBitmap,
            DoublePoint(VFixedOnBitmap.X, VFixedOnBitmap.Y),
            VMarkerWithDirection.Direction
          );
      end else begin
        Result :=
          TBitmapMarker.Create(
            VBitmap,
            DoublePoint(VFixedOnBitmap.X, VFixedOnBitmap.Y)
          );
      end;
    finally
      VBitmap.Free;
    end;
  finally
    VTransform.Free;
  end;
end;

//function TBitmapMarkerProviderStaticFromDataProvider.ModifyMarkerWithRotation(
//  ASourceMarker: IBitmapMarkerWithDirection; AAngle: Double): IBitmapMarker;
//var
//  VTransform: TAffineTransformation;
//  VSizeSource: TPoint;
//  VTargetRect: TFloatRect;
//  VSizeTarget: TPoint;
//  VBitmap: TCustomBitmap32;
//  VFixedOnBitmap: TFloatPoint;
//  VRasterizer: TRasterizer;
//  VTransformer: TTransformer;
//  VCombineInfo: TCombineInfo;
//  VSampler: TCustomResampler;
//begin
//  VTransform := TAffineTransformation.Create;
//  try
//    VSizeSource := ASourceMarker.BitmapSize;
//    VTransform.SrcRect := FloatRect(0, 0, VSizeSource.X, VSizeSource.Y);
//    VTransform.Rotate(0, 0, ASourceMarker.Direction - AAngle);
//    VTargetRect := VTransform.GetTransformedBounds;
//    VSizeTarget.X := Trunc(VTargetRect.Right - VTargetRect.Left) + 1;
//    VSizeTarget.Y := Trunc(VTargetRect.Bottom - VTargetRect.Top) + 1;
//    VTransform.Translate(-VTargetRect.Left, -VTargetRect.Top);
//    VBitmap := TCustomBitmap32.Create;
//    try
//      VBitmap.SetSize(VSizeTarget.X, VSizeTarget.Y);
//      VBitmap.Clear(0);
//      VRasterizer := TRegularRasterizer.Create;
//      try
//        VSampler := TLinearResampler.Create;
//        try
//          VSampler.Bitmap := ASourceMarker.Bitmap;
//          VTransformer := TTransformer.Create(VSampler, VTransform);
//          try
//            VRasterizer.Sampler := VTransformer;
//            VCombineInfo.SrcAlpha := 255;
//            VCombineInfo.DrawMode := dmOpaque;
//            VCombineInfo.TransparentColor := 0;
//            VRasterizer.Rasterize(VBitmap, VBitmap.BoundsRect, VCombineInfo);
//          finally
//            EMMS;
//            VTransformer.Free;
//          end;
//        finally
//          VSampler.Free;
//        end;
//      finally
//        VRasterizer.Free;
//      end;
//      VFixedOnBitmap := VTransform.Transform(FloatPoint(ASourceMarker.AnchorPoint.X, ASourceMarker.AnchorPoint.Y));
//      Result :=
//        TBitmapMarkerWithDirection.Create(
//          VBitmap,
//          DoublePoint(VFixedOnBitmap.X, VFixedOnBitmap.Y),
//          AAngle
//        );
//    finally
//      VBitmap.Free;
//    end;
//  finally
//    VTransform.Free;
//  end;
//end;

//function TBitmapMarkerProviderStaticFromDataProvider.ModifyMarkerWithRotationAndResize(
//  ASourceMarker: IBitmapMarkerWithDirection;
//  ASize: Integer;
//  AAngle: Double
//): IBitmapMarker;
//var
//  VTransform: TAffineTransformation;
//  VSizeSource: TPoint;
//  VTargetRect: TFloatRect;
//  VSizeTarget: TPoint;
//  VBitmap: TCustomBitmap32;
//  VFixedOnBitmap: TFloatPoint;
//  VScale: Double;
//  VRasterizer: TRasterizer;
//  VTransformer: TTransformer;
//  VCombineInfo: TCombineInfo;
//  VSampler: TCustomResampler;
//begin
//  VTransform := TAffineTransformation.Create;
//  try
//    VSizeSource := ASourceMarker.BitmapSize;
//    VTransform.SrcRect := FloatRect(0, 0, VSizeSource.X, VSizeSource.Y);
//    VTransform.Rotate(0, 0, ASourceMarker.Direction - AAngle);
//    VScale := ASize / ASourceMarker.BitmapSize.X;
//    VTransform.Scale(VScale, VScale);
//    VTargetRect := VTransform.GetTransformedBounds;
//    VSizeTarget.X := Trunc(VTargetRect.Right - VTargetRect.Left) + 1;
//    VSizeTarget.Y := Trunc(VTargetRect.Bottom - VTargetRect.Top) + 1;
//    VTransform.Translate(-VTargetRect.Left, -VTargetRect.Top);
//    VBitmap := TCustomBitmap32.Create;
//    try
//      VBitmap.SetSize(VSizeTarget.X, VSizeTarget.Y);
//      VBitmap.Clear(0);
//      VRasterizer := TRegularRasterizer.Create;
//      try
//        VSampler := TLinearResampler.Create;
//        try
//          VSampler.Bitmap := ASourceMarker.Bitmap;
//          VTransformer := TTransformer.Create(VSampler, VTransform);
//          try
//            VRasterizer.Sampler := VTransformer;
//            VCombineInfo.SrcAlpha := 255;
//            VCombineInfo.DrawMode := dmOpaque;
//            VCombineInfo.TransparentColor := 0;
//            VRasterizer.Rasterize(VBitmap, VBitmap.BoundsRect, VCombineInfo);
//          finally
//            EMMS;
//            VTransformer.Free;
//          end;
//        finally
//          VSampler.Free;
//        end;
//      finally
//        VRasterizer.Free;
//      end;
//      VFixedOnBitmap := VTransform.Transform(FloatPoint(ASourceMarker.AnchorPoint.X, ASourceMarker.AnchorPoint.Y));
//      Result :=
//        TBitmapMarkerWithDirection.Create(
//          VBitmap,
//          DoublePoint(VFixedOnBitmap.X, VFixedOnBitmap.Y),
//          AAngle
//        );
//    finally
//      VBitmap.Free;
//    end;
//  finally
//    VTransform.Free;
//  end;
//end;

{ TBitmapMarkerWithDirectionProviderStaticFromDataProvider }

constructor TBitmapMarkerWithDirectionProviderStaticFromDataProvider.Create(
  AResourceDataProvider: IConfigDataProvider;
  AContentTypeManager: IContentTypeManager; AResourceName: string;
  AAnchorPoint: TDoublePoint; ADefaultDirection: Double);
var
  VFileName: string;
  VFileExt: string;
  VInfoBasic: IContentTypeInfoBasic;
  VBitmapContntType: IContentTypeInfoBitmap;
  VBitmap: TCustomBitmap32;
  VStream: TMemoryStream;
begin
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
      TBitmapMarkerWithDirection.Create(
        VBitmap,
        AAnchorPoint,
        ADefaultDirection
      );
  finally
    VBitmap.Free;
  end;
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
  AAngle: Double): IBitmapMarkerWithDirection;
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
  AAngle: Double; ASize: Integer): IBitmapMarkerWithDirection;
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
  ASourceMarker: IBitmapMarkerWithDirection;
  ASize: Integer): IBitmapMarkerWithDirection;
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
        TBitmapMarkerWithDirection.Create(
          VBitmap,
          DoublePoint(VFixedOnBitmap.X, VFixedOnBitmap.Y),
          VMarkerWithDirection.Direction
        );
    finally
      VBitmap.Free;
    end;
  finally
    VTransform.Free;
  end;
end;

function TBitmapMarkerWithDirectionProviderStaticFromDataProvider.ModifyMarkerWithRotation(
  ASourceMarker: IBitmapMarkerWithDirection;
  AAngle: Double): IBitmapMarkerWithDirection;
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
        TBitmapMarkerWithDirection.Create(
          VBitmap,
          DoublePoint(VFixedOnBitmap.X, VFixedOnBitmap.Y),
          AAngle
        );
    finally
      VBitmap.Free;
    end;
  finally
    VTransform.Free;
  end;
end;

function TBitmapMarkerWithDirectionProviderStaticFromDataProvider.ModifyMarkerWithRotationAndResize(
  ASourceMarker: IBitmapMarkerWithDirection; ASize: Integer;
  AAngle: Double): IBitmapMarkerWithDirection;
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
        TBitmapMarkerWithDirection.Create(
          VBitmap,
          DoublePoint(VFixedOnBitmap.X, VFixedOnBitmap.Y),
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
