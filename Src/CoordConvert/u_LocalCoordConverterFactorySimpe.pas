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
  i_ProjectionInfo,
  i_LocalCoordConverter,
  i_LocalCoordConverterFactory,
  i_LocalCoordConverterFactorySimpe,
  u_BaseInterfacedObject;

type
  TLocalCoordConverterFactorySimpe = class(TBaseInterfacedObject, ILocalCoordConverterFactorySimpe)
  private
    FFactory: ILocalCoordConverterFactory;
  private
    function CreateConverter(
      const ALocalRect: TRect;
      const AProjection: IProjection;
      const AMapScale: Double;
      const AMapPixelAtLocalZero: TDoublePoint
    ): ILocalCoordConverter;
    function CreateConverterNoScale(
      const ALocalRect: TRect;
      const AProjection: IProjection;
      const AMapPixelAtLocalZero: TPoint
    ): ILocalCoordConverter;

    function ChangeCenterLonLat(
      const ASource: ILocalCoordConverter;
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
    function ChangeCenterLonLatAndProjection(
      const ASource: ILocalCoordConverter;
      const AProjection: IProjection;
      const ALonLat: TDoublePoint
    ): ILocalCoordConverter;
    function ChangeProjectionWithFreezeAtVisualPoint(
      const ASource: ILocalCoordConverter;
      const AProjection: IProjection;
      const AFreezePoint: TPoint
    ): ILocalCoordConverter;
    function ChangeProjectionWithFreezeAtCenter(
      const ASource: ILocalCoordConverter;
      const AProjection: IProjection
    ): ILocalCoordConverter;
    function ChangeProjectionWithScaleUpdate(
      const ASource: ILocalCoordConverter;
      const AProjection: IProjection
    ): ILocalCoordConverter;

    function CreateForTile(
      const AProjection: IProjection;
      const ATile: TPoint
    ): ILocalCoordConverter;
  public
    constructor Create(
      const AFactory: ILocalCoordConverterFactory
    );
  end;

implementation

uses
  Math,
  u_GeoFunc;

{ TLocalCoordConverterFactorySimpe }

constructor TLocalCoordConverterFactorySimpe.Create(
  const AFactory: ILocalCoordConverterFactory
);
begin
  inherited Create;
  FFactory := AFactory;
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
      ASource.Projection,
      VScale,
      VTopLefAtMap
    );
end;

function TLocalCoordConverterFactorySimpe.ChangeCenterLonLat(
  const ASource: ILocalCoordConverter;
  const ALonLat: TDoublePoint
): ILocalCoordConverter;
var
  VProjection: IProjection;
  VLocalRect: TRect;
  VLocalCenter: TDoublePoint;
  VScale: Double;
  VLonLat: TDoublePoint;
  VCenterMapPixel: TDoublePoint;
  VCenterMapPixelNew: TDoublePoint;
  VTopLefAtMap: TDoublePoint;
begin
  VProjection := ASource.Projection;
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

function TLocalCoordConverterFactorySimpe.ChangeCenterLonLatAndProjection(
  const ASource: ILocalCoordConverter;
  const AProjection: IProjection;
  const ALonLat: TDoublePoint
): ILocalCoordConverter;
var
  VProjectionOld: IProjection;
  VLocalRect: TRect;
  VLocalCenter: TDoublePoint;
  VScale: Double;
  VLonLat: TDoublePoint;
  VCenterMapPixelNew: TDoublePoint;
  VTopLefAtMap: TDoublePoint;
begin
  VProjectionOld := ASource.Projection;
  if VProjectionOld.IsSame(AProjection) then begin
    Result := ChangeCenterLonLat(ASource, ALonLat);
    Exit;
  end;

  VLonLat := ALonLat;
  AProjection.ProjectionType.ValidateLonLatPos(VLonLat);
  VCenterMapPixelNew := AProjection.LonLat2PixelPosFloat(VLonLat);
  VLocalRect := ASource.GetLocalRect;
  VScale := ASource.GetScale;
  VLocalCenter := RectCenter(VLocalRect);
  VTopLefAtMap.X := VCenterMapPixelNew.X - VLocalCenter.X / VScale;
  VTopLefAtMap.Y := VCenterMapPixelNew.Y - VLocalCenter.Y / VScale;
  Result :=
    CreateConverter(
      VLocalRect,
      AProjection,
      VScale,
      VTopLefAtMap
    );
end;

function TLocalCoordConverterFactorySimpe.ChangeCenterToLocalPoint(
  const ASource: ILocalCoordConverter;
  const AVisualPoint: TPoint
): ILocalCoordConverter;
var
  VProjection: IProjection;
  VLocalRect: TRect;
  VLocalCenter: TDoublePoint;
  VScale: Double;
  VCenterMapPixel: TDoublePoint;
  VCenterMapPixelNew: TDoublePoint;
  VTopLefAtMap: TDoublePoint;
begin
  VProjection := ASource.Projection;
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

function TLocalCoordConverterFactorySimpe.ChangeProjectionWithFreezeAtCenter(
  const ASource: ILocalCoordConverter;
  const AProjection: IProjection
): ILocalCoordConverter;
var
  VLocalRect: TRect;
  VScale: Double;
  VCenterMapPixel: TDoublePoint;
  VCenterMapPixelNew: TDoublePoint;
  VTopLefAtMap: TDoublePoint;
  VRelativePoint: TDoublePoint;
  VLonLatPoint: TDoublePoint;
  VProjectionOld: IProjection;
begin
  VProjectionOld := ASource.Projection;
  if VProjectionOld.IsSame(AProjection) then begin
    Result := ASource;
    Exit;
  end;
  VCenterMapPixel := ASource.GetCenterMapPixelFloat;
  if VProjectionOld.ProjectionType.IsSame(AProjection.ProjectionType) then begin
    VRelativePoint := VProjectionOld.PixelPosFloat2Relative(VCenterMapPixel);
    VCenterMapPixelNew := AProjection.Relative2PixelPosFloat(VRelativePoint);
  end else begin
    VLonLatPoint := VProjectionOld.PixelPosFloat2LonLat(VCenterMapPixel);
    AProjection.ProjectionType.ValidateLonLatPos(VLonLatPoint);
    VCenterMapPixelNew := AProjection.LonLat2PixelPosFloat(VLonLatPoint);
  end;
  VLocalRect := ASource.GetLocalRect;
  VScale := ASource.GetScale;
  VTopLefAtMap.X := VCenterMapPixelNew.X - ((VLocalRect.Right - VLocalRect.Left) / 2) / VScale;
  VTopLefAtMap.Y := VCenterMapPixelNew.Y - ((VLocalRect.Bottom - VLocalRect.Top) / 2) / VScale;
  Result :=
    CreateConverter(
      VLocalRect,
      AProjection,
      VScale,
      VTopLefAtMap
    );
end;

function TLocalCoordConverterFactorySimpe.ChangeProjectionWithFreezeAtVisualPoint(
  const ASource: ILocalCoordConverter;
  const AProjection: IProjection;
  const AFreezePoint: TPoint
): ILocalCoordConverter;
var
  VLocalRect: TRect;
  VScale: Double;
  VTopLefAtMap: TDoublePoint;
  VFreezePoint: TDoublePoint;
  VFreezeMapPoint: TDoublePoint;
  VRelativeFreezePoint: TDoublePoint;
  VLonLatFreezePoint: TDoublePoint;
  VMapFreezPointAtNew: TDoublePoint;
  VProjectionOld: IProjection;
begin
  VProjectionOld := ASource.Projection;
  if VProjectionOld.IsSame(AProjection) then begin
    Result := ASource;
    Exit;
  end;

  VFreezeMapPoint := ASource.LocalPixel2MapPixelFloat(AFreezePoint);
  VProjectionOld.ValidatePixelPosFloatStrict(VFreezeMapPoint, False);
  VFreezePoint := ASource.MapPixelFloat2LocalPixelFloat(VFreezeMapPoint);

  if VProjectionOld.ProjectionType.IsSame(AProjection.ProjectionType) then begin
    VRelativeFreezePoint := VProjectionOld.PixelPosFloat2Relative(VFreezeMapPoint);
    VMapFreezPointAtNew := AProjection.Relative2PixelPosFloat(VRelativeFreezePoint);
  end else begin
    VLonLatFreezePoint := VProjectionOld.PixelPosFloat2LonLat(VFreezeMapPoint);
    AProjection.ProjectionType.ValidateLonLatPos(VLonLatFreezePoint);
    VMapFreezPointAtNew := AProjection.LonLat2PixelPosFloat(VLonLatFreezePoint);
  end;

  VLocalRect := ASource.GetLocalRect;
  VScale := ASource.GetScale;

  VTopLefAtMap.X := VMapFreezPointAtNew.X - VFreezePoint.X / VScale;
  VTopLefAtMap.Y := VMapFreezPointAtNew.Y - VFreezePoint.Y / VScale;

  Result :=
    CreateConverter(
      VLocalRect,
      AProjection,
      VScale,
      VTopLefAtMap
    );
end;

function TLocalCoordConverterFactorySimpe.ChangeProjectionWithScaleUpdate(
  const ASource: ILocalCoordConverter;
  const AProjection: IProjection
): ILocalCoordConverter;
var
  VLocalRect: TRect;
  VScaleOld: Double;
  VScaleNew: Double;
  VTopLefAtMap: TDoublePoint;
  VFreezePoint: TDoublePoint;
  VCenterMapPixel: TDoublePoint;
  VRelativePoint: TDoublePoint;
  VLonLatPoint: TDoublePoint;
  VCenterMapPixelAtNew: TDoublePoint;
  VProjectionOld: IProjection;
begin
  VProjectionOld := ASource.Projection;
  if VProjectionOld.IsSame(AProjection) then begin
    Result := ASource;
    Exit;
  end;

  VCenterMapPixel := ASource.GetCenterMapPixelFloat;

  if VProjectionOld.ProjectionType.IsSame(AProjection.ProjectionType) then begin
    VRelativePoint := VProjectionOld.PixelPosFloat2Relative(VCenterMapPixel);
    VCenterMapPixelAtNew := AProjection.Relative2PixelPosFloat(VRelativePoint);
  end else begin
    VLonLatPoint := VProjectionOld.PixelPosFloat2LonLat(VCenterMapPixel);
    AProjection.ProjectionType.ValidateLonLatPos(VLonLatPoint);
    VCenterMapPixelAtNew := AProjection.LonLat2PixelPosFloat(VLonLatPoint);
  end;

  VLocalRect := ASource.GetLocalRect;
  VFreezePoint := RectCenter(VLocalRect);
  VScaleOld := ASource.GetScale;

  VScaleNew := VScaleOld * (VProjectionOld.GetPixelsFloat / AProjection.GetPixelsFloat);
  VTopLefAtMap.X := VCenterMapPixelAtNew.X - VFreezePoint.X / VScaleNew;
  VTopLefAtMap.Y := VCenterMapPixelAtNew.Y - VFreezePoint.Y / VScaleNew;
  Result :=
    CreateConverter(
      VLocalRect,
      AProjection,
      VScaleNew,
      VTopLefAtMap
    );
end;

function TLocalCoordConverterFactorySimpe.CreateConverter(
  const ALocalRect: TRect;
  const AProjection: IProjection;
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
  const AProjection: IProjection;
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
  const AProjection: IProjection;
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
