{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
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
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

unit u_MarkerDrawableWithDirectionByBitmapMarker;

interface

uses
  Types,
  SysUtils,
  GR32,
  t_GeoTypes,
  i_MarkerDrawable,
  i_Bitmap32BufferFactory,
  i_BitmapMarker,
  u_BaseInterfacedObject;

type
  TMarkerDrawableWithDirectionByBitmapMarker = class(TBaseInterfacedObject, IMarkerDrawableWithDirection)
  private
    FBitmap32StaticFactory: IBitmap32StaticFactory;
    FMarker: IBitmapMarkerWithDirection;
    FCachedMarkerCS: IReadWriteSync;
    FCachedMarker: IBitmapMarkerWithDirection;

    function ModifyMarkerWithRotation(
      const ASourceMarker: IBitmapMarkerWithDirection;
      const AAngle: Double
    ): IBitmapMarkerWithDirection;
    procedure DrawToBitmap(
      const AMarker: IBitmapMarker;
      ABitmap: TCustomBitmap32;
      const APosition: TDoublePoint
    );
  private
    function DrawToBitmapWithDirection(
      ABitmap: TCustomBitmap32;
      const APosition: TDoublePoint;
      const AAngle: Double
    ): Boolean;
  public
    constructor Create(
      const ABitmap32StaticFactory: IBitmap32StaticFactory;
      const AMarker: IBitmapMarkerWithDirection
    );
  end;

implementation

uses
  Math,
  GR32_Blend,
  GR32_Rasterizers,
  GR32_Resamplers,
  GR32_Transforms,
  i_Bitmap32Static,
  u_GeoFunc,
  u_BitmapFunc,
  u_Bitmap32ByStaticBitmap,
  u_BitmapMarker,
  u_Synchronizer;

const
  CAngleDelta = 1.0;


{ TMarkerDrawableWithDirectionByBitmapMarker }

constructor TMarkerDrawableWithDirectionByBitmapMarker.Create(
  const ABitmap32StaticFactory: IBitmap32StaticFactory;
  const AMarker: IBitmapMarkerWithDirection
);
begin
  Assert(Assigned(ABitmap32StaticFactory));
  Assert(Assigned(AMarker));
  inherited Create;
  FBitmap32StaticFactory := ABitmap32StaticFactory;
  FMarker := AMarker;
  FCachedMarkerCS := GSync.SyncVariable.Make(Self.ClassName);
end;

procedure TMarkerDrawableWithDirectionByBitmapMarker.DrawToBitmap(
  const AMarker: IBitmapMarker;
  ABitmap: TCustomBitmap32;
  const APosition: TDoublePoint
);
var
  VTargetPoint: TPoint;
  VTargetPointFloat: TDoublePoint;
begin
  VTargetPointFloat :=
    DoublePoint(
      APosition.X - AMarker.AnchorPoint.X,
      APosition.Y - AMarker.AnchorPoint.Y
    );
  VTargetPoint := PointFromDoublePoint(VTargetPointFloat, prToTopLeft);
  BlockTransferFull(
    ABitmap,
    VTargetPoint.X, VTargetPoint.Y,
    AMarker,
    dmBlend,
    ABitmap.CombineMode
  );
end;

function TMarkerDrawableWithDirectionByBitmapMarker.DrawToBitmapWithDirection(
  ABitmap: TCustomBitmap32;
  const APosition: TDoublePoint;
  const AAngle: Double
): Boolean;
var
  VCachedMarker: IBitmapMarkerWithDirection;
  VMarkerToDraw: IBitmapMarkerWithDirection;
  VSourceSize: TPoint;
  VAnchorPoint: TDoublePoint;
  VHalfSize: Double;
  VTargetRect: TRect;
  VTargetDoubleRect: TDoubleRect;
begin
  VSourceSize := FMarker.Size;
  VAnchorPoint := FMarker.AnchorPoint;
  VHalfSize :=
    Min(
      Min(VAnchorPoint.X + VAnchorPoint.Y, VSourceSize.X - VAnchorPoint.X + VAnchorPoint.Y),
      Min(VAnchorPoint.X + VSourceSize.Y - VAnchorPoint.Y, VSourceSize.X - VAnchorPoint.X + VSourceSize.Y - VAnchorPoint.Y)
    );
  VTargetDoubleRect.Left := APosition.X - VHalfSize;
  VTargetDoubleRect.Top := APosition.Y - VHalfSize;
  VTargetDoubleRect.Right := APosition.X + VHalfSize;
  VTargetDoubleRect.Bottom := APosition.Y + VHalfSize;
  VTargetRect := RectFromDoubleRect(VTargetDoubleRect, rrOutside);

  Types.IntersectRect(VTargetRect, ABitmap.ClipRect, VTargetRect);
  if Types.IsRectEmpty(VTargetRect) then begin
    Result := False;
    Exit;
  end;

  if ABitmap.MeasuringMode then begin
    ABitmap.Changed(VTargetRect);
  end else begin
    if Abs(CalcAngleDelta(AAngle, FMarker.Direction)) > CAngleDelta then begin
      FCachedMarkerCS.BeginRead;
      try
        VCachedMarker := FCachedMarker;
      finally
        FCachedMarkerCS.EndRead;
      end;

      if VCachedMarker <> nil then begin
        if Abs(CalcAngleDelta(AAngle, VCachedMarker.Direction)) > CAngleDelta then begin
          VMarkerToDraw := nil;
        end else begin
          VMarkerToDraw := VCachedMarker;
        end;
      end else begin
        VMarkerToDraw := nil;
      end;
      if VMarkerToDraw = nil then begin
        VMarkerToDraw := ModifyMarkerWithRotation(FMarker, AAngle);
      end;
      if (VMarkerToDraw <> nil) and (VMarkerToDraw <> VCachedMarker) then begin
        FCachedMarkerCS.BeginWrite;
        try
          FCachedMarker := VMarkerToDraw;
        finally
          FCachedMarkerCS.EndWrite;
        end;
      end;
    end else begin
      VMarkerToDraw := FMarker;
    end;
    DrawToBitmap(VMarkerToDraw, ABitmap, APosition);
  end;
  Result := True;
end;

function TMarkerDrawableWithDirectionByBitmapMarker.ModifyMarkerWithRotation(
  const ASourceMarker: IBitmapMarkerWithDirection;
  const AAngle: Double
): IBitmapMarkerWithDirection;
var
  VTransform: TAffineTransformation;
  VSizeSource: TPoint;
  VTargetRect: TFloatRect;
  VSizeTarget: TPoint;
  VBitmap: TBitmap32ByStaticBitmap;
  VBitmapSource: TCustomBitmap32;
  VFixedOnBitmap: TFloatPoint;
  VRasterizer: TRasterizer;
  VTransformer: TTransformer;
  VCombineInfo: TCombineInfo;
  VSampler: TCustomResampler;
  VBitmapStatic: IBitmap32Static;
begin
  VTransform := TAffineTransformation.Create;
  try
    VSizeSource := ASourceMarker.Size;
    VTransform.SrcRect := FloatRect(0, 0, VSizeSource.X, VSizeSource.Y);
    VTransform.Rotate(0, 0, ASourceMarker.Direction - AAngle);
    VTargetRect := VTransform.GetTransformedBounds;
    VSizeTarget.X := Trunc(VTargetRect.Right - VTargetRect.Left) + 1;
    VSizeTarget.Y := Trunc(VTargetRect.Bottom - VTargetRect.Top) + 1;
    VTransform.Translate(-VTargetRect.Left, -VTargetRect.Top);
    VBitmap := TBitmap32ByStaticBitmap.Create(FBitmap32StaticFactory);
    try
      VBitmap.SetSize(VSizeTarget.X, VSizeTarget.Y);
      VBitmap.Clear(0);
      VRasterizer := TRegularRasterizer.Create;
      try
        VSampler := TLinearResampler.Create;
        try
          VBitmapSource := TCustomBitmap32.Create;
          try
            AssignStaticToBitmap32(VBitmapSource, ASourceMarker);
            VSampler.Bitmap := VBitmapSource;
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
            VBitmapSource.Free;
          end;
        finally
          VSampler.Free;
        end;
      finally
        VRasterizer.Free;
      end;
      VFixedOnBitmap := VTransform.Transform(FloatPoint(ASourceMarker.AnchorPoint.X, ASourceMarker.AnchorPoint.Y));
      VBitmapStatic := VBitmap.MakeAndClear;
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
