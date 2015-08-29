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

unit u_LocalCoordConverterFactorySimpe;

interface

uses
  Types,
  t_GeoTypes,
  i_CoordConverter,
  i_CoordConverterFactory,
  i_ProjectionInfo,
  i_LocalCoordConverter,
  i_LocalCoordConverterFactory,
  i_LocalCoordConverterFactorySimpe,
  u_BaseInterfacedObject;

type
  TLocalCoordConverterFactorySimpe = class(TBaseInterfacedObject, ILocalCoordConverterFactorySimpe)
  private
    FFactory: ILocalCoordConverterFactory;
    FProjectionFactory: IProjectionInfoFactory;
  private
    function CreateConverter(
      const ALocalRect: TRect;
      const AProjection: IProjectionInfo;
      const AMapScale: Double;
      const AMapPixelAtLocalZero: TDoublePoint
    ): ILocalCoordConverter;
    function CreateConverterNoScale(
      const ALocalRect: TRect;
      const AProjection: IProjectionInfo;
      const AMapPixelAtLocalZero: TPoint
    ): ILocalCoordConverter;

    function ChangeCenterLonLat(
      const ASource: ILocalCoordConverter;
      const ALonLat: TDoublePoint
    ): ILocalCoordConverter;
    function ChangeCenterLonLatAndZoom(
      const ASource: ILocalCoordConverter;
      AZoom: Byte;
      const ALonLat: TDoublePoint
    ): ILocalCoordConverter;
    function ChangeByLocalDelta(
      const ASource: ILocalCoordConverter;
      const ADelta: TDoublePoint
    ): ILocalCoordConverter;
    function ChangeCenterToLocalPoint(
      const ASource: ILocalCoordConverter;
      const AVisualPoint: TPoint
    ): ILocalCoordConverter;
    function ChangeZoomWithFreezeAtVisualPoint(
      const ASource: ILocalCoordConverter;
      const AZoom: Byte;
      const AFreezePoint: TPoint
    ): ILocalCoordConverter;
    function ChangeZoomWithFreezeAtCenter(
      const ASource: ILocalCoordConverter;
      const AZoom: Byte
    ): ILocalCoordConverter;
    function ChangeConverter(
      const ASource: ILocalCoordConverter;
      const AConverter: ICoordConverter
    ): ILocalCoordConverter;

    function CreateForTile(
      const AProjection: IProjectionInfo;
      const ATile: TPoint
    ): ILocalCoordConverter;
  public
    constructor Create(
      const AFactory: ILocalCoordConverterFactory;
      const AProjectionFactory: IProjectionInfoFactory
    );
  end;

implementation

uses
  Math,
  u_GeoFunc;

{ TLocalCoordConverterFactorySimpe }

constructor TLocalCoordConverterFactorySimpe.Create(
  const AFactory: ILocalCoordConverterFactory;
  const AProjectionFactory: IProjectionInfoFactory
);
begin
  inherited Create;
  FFactory := AFactory;
  FProjectionFactory := AProjectionFactory;
end;

function TLocalCoordConverterFactorySimpe.ChangeByLocalDelta(
  const ASource: ILocalCoordConverter;
  const ADelta: TDoublePoint
): ILocalCoordConverter;
var
  VLocalRect: TRect;
  VScale: Double;
  VTopLefAtMap: TDoublePoint;
begin
  VLocalRect := ASource.GetLocalRect;
  VScale := ASource.GetScale;
  VTopLefAtMap := ASource.LocalPixelFloat2MapPixelFloat(DoublePoint(VLocalRect.Left + ADelta.X, VLocalRect.Top + ADelta.Y));
  Result :=
    CreateConverter(
      VLocalRect,
      ASource.ProjectionInfo,
      VScale,
      VTopLefAtMap
    );
end;

function TLocalCoordConverterFactorySimpe.ChangeCenterLonLat(
  const ASource: ILocalCoordConverter;
  const ALonLat: TDoublePoint
): ILocalCoordConverter;
var
  VProjection: IProjectionInfo;
  VLocalRect: TRect;
  VLocalCenter: TDoublePoint;
  VScale: Double;
  VLonLat: TDoublePoint;
  VCenterMapPixel: TDoublePoint;
  VCenterMapPixelNew: TDoublePoint;
  VTopLefAtMap: TDoublePoint;
begin
  VProjection := ASource.ProjectionInfo;
  VLonLat := ALonLat;
  VProjection.ProjectionType.ValidateLonLatPos(VLonLat);
  VCenterMapPixel := ASource.GetCenterMapPixelFloat;
  VCenterMapPixelNew := VProjection.LonLat2PixelPosFloat(VLonLat);
  if (Abs(VCenterMapPixel.X - VCenterMapPixelNew.X) < 0.001) and
    (Abs(VCenterMapPixel.Y - VCenterMapPixelNew.Y) < 0.001) then begin
    Result := ASource;
    Exit;
  end;
  VLocalRect := ASource.GetLocalRect;
  VScale := ASource.GetScale;
  VLocalCenter := RectCenter(VLocalRect);
  VTopLefAtMap.X := VCenterMapPixelNew.X - VLocalCenter.X / VScale;
  VTopLefAtMap.Y := VCenterMapPixelNew.Y - VLocalCenter.Y / VScale;
  Result :=
    CreateConverter(
      VLocalRect,
      VProjection,
      VScale,
      VTopLefAtMap
    );
end;

function TLocalCoordConverterFactorySimpe.ChangeCenterLonLatAndZoom(
  const ASource: ILocalCoordConverter;
  AZoom: Byte;
  const ALonLat: TDoublePoint
): ILocalCoordConverter;
var
  VZoomNew: Byte;
  VConverter: ICoordConverter;
  VProjectionOld: IProjectionInfo;
  VProjectionNew: IProjectionInfo;
  VLocalRect: TRect;
  VLocalCenter: TDoublePoint;
  VScale: Double;
  VLonLat: TDoublePoint;
  VCenterMapPixelNew: TDoublePoint;
  VTopLefAtMap: TDoublePoint;
begin
  VZoomNew := AZoom;
  VProjectionOld := ASource.ProjectionInfo;
  VConverter := VProjectionOld.GeoConverter;
  VConverter.ValidateZoom(VZoomNew);
  if VZoomNew = VProjectionOld.Zoom then begin
    Result := ChangeCenterLonLat(ASource, ALonLat);
    Exit;
  end;

  VLonLat := ALonLat;
  VConverter.ValidateLonLatPos(VLonLat);
  VProjectionNew := FProjectionFactory.GetByConverterAndZoom(VConverter, VZoomNew);
  VCenterMapPixelNew := VProjectionNew.LonLat2PixelPosFloat(VLonLat);
  VLocalRect := ASource.GetLocalRect;
  VScale := ASource.GetScale;
  VLocalCenter := RectCenter(VLocalRect);
  VTopLefAtMap.X := VCenterMapPixelNew.X - VLocalCenter.X / VScale;
  VTopLefAtMap.Y := VCenterMapPixelNew.Y - VLocalCenter.Y / VScale;
  Result :=
    CreateConverter(
      VLocalRect,
      VProjectionNew,
      VScale,
      VTopLefAtMap
    );
end;

function TLocalCoordConverterFactorySimpe.ChangeCenterToLocalPoint(
  const ASource: ILocalCoordConverter;
  const AVisualPoint: TPoint
): ILocalCoordConverter;
var
  VProjection: IProjectionInfo;
  VLocalRect: TRect;
  VLocalCenter: TDoublePoint;
  VScale: Double;
  VCenterMapPixel: TDoublePoint;
  VCenterMapPixelNew: TDoublePoint;
  VTopLefAtMap: TDoublePoint;
begin
  VProjection := ASource.ProjectionInfo;
  VCenterMapPixel := ASource.GetCenterMapPixelFloat;
  VCenterMapPixelNew := ASource.LocalPixel2MapPixelFloat(AVisualPoint);
  VProjection.ValidatePixelPosFloatStrict(VCenterMapPixelNew, True);
  if (Abs(VCenterMapPixel.X - VCenterMapPixelNew.X) < 0.001) and
    (Abs(VCenterMapPixel.Y - VCenterMapPixelNew.Y) < 0.001) then begin
    Result := ASource;
    Exit;
  end;
  VLocalRect := ASource.GetLocalRect;
  VScale := ASource.GetScale;
  VLocalCenter := RectCenter(VLocalRect);
  VTopLefAtMap.X := VCenterMapPixelNew.X - VLocalCenter.X / VScale;
  VTopLefAtMap.Y := VCenterMapPixelNew.Y - VLocalCenter.Y / VScale;
  Result :=
    CreateConverter(
      VLocalRect,
      VProjection,
      VScale,
      VTopLefAtMap
    );
end;

function TLocalCoordConverterFactorySimpe.ChangeConverter(
  const ASource: ILocalCoordConverter;
  const AConverter: ICoordConverter
): ILocalCoordConverter;
var
  VProjection: IProjectionInfo;
  VLocalRect: TRect;
  VLocalCenter: TDoublePoint;
  VScale: Double;
  VCenterLonLat: TDoublePoint;
  VCenterMapPixelNew: TDoublePoint;
  VTopLefAtMap: TDoublePoint;
begin
  if AConverter = nil then begin
    Result := ASource;
    Exit;
  end;

  if ASource.GeoConverter.IsSameConverter(AConverter) then begin
    Result := ASource;
    Exit;
  end;
  VCenterLonLat := ASource.GetCenterLonLat;
  AConverter.ValidateLonLatPos(VCenterLonLat);
  VProjection := FProjectionFactory.GetByConverterAndZoom(AConverter, ASource.Zoom);
  VScale := ASource.GetScale;
  VLocalRect := ASource.GetLocalRect;

  VCenterMapPixelNew := VProjection.LonLat2PixelPosFloat(VCenterLonLat);
  VLocalCenter := RectCenter(VLocalRect);
  VTopLefAtMap.X := VCenterMapPixelNew.X - VLocalCenter.X / VScale;
  VTopLefAtMap.Y := VCenterMapPixelNew.Y - VLocalCenter.Y / VScale;

  Result :=
    CreateConverter(
      VLocalRect,
      VProjection,
      VScale,
      VTopLefAtMap
    );
end;

function TLocalCoordConverterFactorySimpe.ChangeZoomWithFreezeAtCenter(
  const ASource: ILocalCoordConverter;
  const AZoom: Byte
): ILocalCoordConverter;
var
  VZoomOld: Byte;
  VZoomNew: Byte;
  VConverter: ICoordConverter;
  VLocalRect: TRect;
  VScale: Double;
  VCenterMapPixel: TDoublePoint;
  VCenterMapPixelNew: TDoublePoint;
  VTopLefAtMap: TDoublePoint;
  VRelativePoint: TDoublePoint;
  VProjectionNew: IProjectionInfo;
  VProjectionOld: IProjectionInfo;
begin
  VZoomOld := ASource.Zoom;
  VProjectionOld := ASource.ProjectionInfo;
  VZoomNew := AZoom;
  VConverter := ASource.GeoConverter;
  VConverter.ValidateZoom(VZoomNew);
  if VZoomOld = VZoomNew then begin
    Result := ASource;
    Exit;
  end;
  VProjectionNew := FProjectionFactory.GetByConverterAndZoom(VConverter, VZoomNew);

  VCenterMapPixel := ASource.GetCenterMapPixelFloat;
  VRelativePoint := VProjectionOld.PixelPosFloat2Relative(VCenterMapPixel);
  VCenterMapPixelNew := VProjectionNew.Relative2PixelPosFloat(VRelativePoint);
  VLocalRect := ASource.GetLocalRect;
  VScale := ASource.GetScale;
  VTopLefAtMap.X := VCenterMapPixelNew.X - ((VLocalRect.Right - VLocalRect.Left) / 2) / VScale;
  VTopLefAtMap.Y := VCenterMapPixelNew.Y - ((VLocalRect.Bottom - VLocalRect.Top) / 2) / VScale;
  Result :=
    CreateConverter(
      VLocalRect,
      VProjectionNew,
      VScale,
      VTopLefAtMap
    );
end;

function TLocalCoordConverterFactorySimpe.ChangeZoomWithFreezeAtVisualPoint(
  const ASource: ILocalCoordConverter;
  const AZoom: Byte;
  const AFreezePoint: TPoint
): ILocalCoordConverter;
var
  VZoomOld: Byte;
  VZoomNew: Byte;
  VConverter: ICoordConverter;
  VLocalRect: TRect;
  VScale: Double;
  VTopLefAtMap: TDoublePoint;
  VFreezePoint: TDoublePoint;
  VFreezeMapPoint: TDoublePoint;
  VRelativeFreezePoint: TDoublePoint;
  VMapFreezPointAtNewZoom: TDoublePoint;
  VProjectionNew: IProjectionInfo;
  VProjectionOld: IProjectionInfo;
begin
  VProjectionOld := ASource.ProjectionInfo;
  VZoomOld := ASource.Zoom;
  VZoomNew := AZoom;
  VConverter := ASource.GeoConverter;
  VConverter.ValidateZoom(VZoomNew);
  if VZoomOld = VZoomNew then begin
    Result := ASource;
    Exit;
  end;
  VProjectionNew := FProjectionFactory.GetByConverterAndZoom(VConverter, VZoomNew);

  VFreezeMapPoint := ASource.LocalPixel2MapPixelFloat(AFreezePoint);
  VProjectionOld.ValidatePixelPosFloatStrict(VFreezeMapPoint, False);
  VFreezePoint := ASource.MapPixelFloat2LocalPixelFloat(VFreezeMapPoint);
  VRelativeFreezePoint := VProjectionOld.PixelPosFloat2Relative(VFreezeMapPoint);
  VMapFreezPointAtNewZoom := VProjectionNew.Relative2PixelPosFloat(VRelativeFreezePoint);

  VLocalRect := ASource.GetLocalRect;
  VScale := ASource.GetScale;

  VTopLefAtMap.X := VMapFreezPointAtNewZoom.X - VFreezePoint.X / VScale;
  VTopLefAtMap.Y := VMapFreezPointAtNewZoom.Y - VFreezePoint.Y / VScale;

  Result :=
    CreateConverter(
      VLocalRect,
      VProjectionNew,
      VScale,
      VTopLefAtMap
    );
end;

function TLocalCoordConverterFactorySimpe.CreateConverter(
  const ALocalRect: TRect;
  const AProjection: IProjectionInfo;
  const AMapScale: Double;
  const AMapPixelAtLocalZero: TDoublePoint
): ILocalCoordConverter;
var
  VLocalCenterMapPixelFloat: TDoublePoint;
  VLocalCenterMapPixel: TPoint;
  VLocalCenter: TDoublePoint;
  VTopLeftMapPixelFloat: TDoublePoint;
  VTopLeftMapPixel: TPoint;
begin
  VLocalCenter := RectCenter(ALocalRect);
  if Abs(AMapScale - 1) < 0.001 then begin
    VTopLeftMapPixel := PointFromDoublePoint(AMapPixelAtLocalZero, prClosest);
    if (Abs(AMapPixelAtLocalZero.X - VTopLeftMapPixel.X) < 0.001) and
      (Abs(AMapPixelAtLocalZero.Y - VTopLeftMapPixel.Y) < 0.001)
    then begin
      VLocalCenterMapPixelFloat.X := AMapPixelAtLocalZero.X + VLocalCenter.X;
      VLocalCenterMapPixelFloat.Y := AMapPixelAtLocalZero.Y + VLocalCenter.Y;
      VLocalCenterMapPixel := PointFromDoublePoint(VLocalCenterMapPixelFloat, prClosest);
      AProjection.ValidatePixelPosStrict(VLocalCenterMapPixel, False);
      VTopLeftMapPixelFloat.X := VLocalCenterMapPixel.X - VLocalCenter.X;
      VTopLeftMapPixelFloat.Y := VLocalCenterMapPixel.Y - VLocalCenter.Y;
      VTopLeftMapPixel := PointFromDoublePoint(VTopLeftMapPixelFloat, prClosest);
      Result :=
        FFactory.CreateNoScaleIntDelta(
          ALocalRect,
          AProjection,
          VTopLeftMapPixel
        );
    end else begin
      VLocalCenterMapPixelFloat.X := AMapPixelAtLocalZero.X + VLocalCenter.X;
      VLocalCenterMapPixelFloat.Y := AMapPixelAtLocalZero.Y + VLocalCenter.Y;
      AProjection.ValidatePixelPosFloatStrict(VLocalCenterMapPixelFloat, False);
      VTopLeftMapPixelFloat.X := VLocalCenterMapPixelFloat.X - VLocalCenter.X;
      VTopLeftMapPixelFloat.Y := VLocalCenterMapPixelFloat.Y - VLocalCenter.Y;

      Result :=
        FFactory.CreateNoScale(
          ALocalRect,
          AProjection,
          VTopLeftMapPixelFloat
        );
    end;
  end else begin
    VLocalCenter.X := VLocalCenter.X / AMapScale;
    VLocalCenter.Y := VLocalCenter.Y / AMapScale;
    VLocalCenterMapPixelFloat.X := AMapPixelAtLocalZero.X + VLocalCenter.X;
    VLocalCenterMapPixelFloat.Y := AMapPixelAtLocalZero.Y + VLocalCenter.Y;
    AProjection.ValidatePixelPosFloatStrict(VLocalCenterMapPixelFloat, False);
    VTopLeftMapPixelFloat.X := VLocalCenterMapPixelFloat.X - VLocalCenter.X;
    VTopLeftMapPixelFloat.Y := VLocalCenterMapPixelFloat.Y - VLocalCenter.Y;

    Result :=
      FFactory.CreateScaled(
        ALocalRect,
        AProjection,
        AMapScale,
        VTopLeftMapPixelFloat
      );
  end;
end;

function TLocalCoordConverterFactorySimpe.CreateConverterNoScale(
  const ALocalRect: TRect;
  const AProjection: IProjectionInfo;
  const AMapPixelAtLocalZero: TPoint
): ILocalCoordConverter;
begin
  Result := FFactory.CreateNoScaleIntDelta(
    ALocalRect,
    AProjection,
    AMapPixelAtLocalZero
  );
end;

function TLocalCoordConverterFactorySimpe.CreateForTile(
  const AProjection: IProjectionInfo;
  const ATile: TPoint
): ILocalCoordConverter;
var
  VPixelRect: TRect;
  VBitmapTileRect: TRect;
begin
  VPixelRect := AProjection.TilePos2PixelRect(ATile);
  VBitmapTileRect.Left := 0;
  VBitmapTileRect.Top := 0;
  VBitmapTileRect.Right := VPixelRect.Right - VPixelRect.Left;
  VBitmapTileRect.Bottom := VPixelRect.Bottom - VPixelRect.Top;
  Result := FFactory.CreateNoScaleIntDelta(
    VBitmapTileRect,
    AProjection,
    VPixelRect.TopLeft
  );
end;

end.
