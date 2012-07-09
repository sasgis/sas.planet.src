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

unit u_LocalCoordConverterFactorySimpe;

interface

uses
  Types,
  t_GeoTypes,
  i_CoordConverter,
  i_CoordConverterFactory,
  i_LocalCoordConverter,
  i_LocalCoordConverterFactorySimpe;

type
  TLocalCoordConverterFactorySimpe = class(TInterfacedObject, ILocalCoordConverterFactorySimpe)
  private
    FProjectionFactory: IProjectionInfoFactory;
  private
    function CreateConverter(
      const ALocalRect: TRect;
      const AZoom: Byte;
      const AGeoConverter: ICoordConverter;
      const AMapScale: Double;
      const ALocalTopLeftAtMap: TDoublePoint
    ): ILocalCoordConverter;
    function CreateConverterNoScale(
      const ALocalRect: TRect;
      const AZoom: Byte;
      const AGeoConverter: ICoordConverter;
      const ALocalTopLeftAtMap: TPoint
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
    function ChangeByMapPixelDelta(
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
    function ChangeZoomAndScaleSaveRelativeRect(
      const ASource: ILocalCoordConverter;
      const AZoom: Byte
    ): ILocalCoordConverter;

    function CreateForTile(
      const ATile: TPoint;
      const AZoom: Byte;
      const AGeoConverter: ICoordConverter
    ): ILocalCoordConverter;
    function CreateBySourceWithStableTileRect(
      const ASource: ILocalCoordConverter
    ): ILocalCoordConverter;
    function CreateBySourceWithStableTileRectAndOtherGeo(
      const ASource: ILocalCoordConverter;
      const AGeoConverter: ICoordConverter
    ): ILocalCoordConverter;
  public
    constructor Create(
      const AProjectionFactory: IProjectionInfoFactory
    );
  end;


implementation

uses
  u_GeoFun,
  u_LocalCoordConverter;

{ TLocalCoordConverterFactorySimpe }

constructor TLocalCoordConverterFactorySimpe.Create(
  const AProjectionFactory: IProjectionInfoFactory
);
begin
  inherited Create;
  FProjectionFactory := AProjectionFactory;
end;

function TLocalCoordConverterFactorySimpe.ChangeByMapPixelDelta(
  const ASource: ILocalCoordConverter;
  const ADelta: TDoublePoint): ILocalCoordConverter;
var
  VZoom: Byte;
  VConverter: ICoordConverter;
  VLocalRect: TRect;
  VScale: Double;
  VCenterMapPixel: TDoublePoint;
  VCenterMapPixelNew: TDoublePoint;
  VTopLefAtMap: TDoublePoint;
begin
  VZoom := ASource.Zoom;
  VConverter := ASource.GeoConverter;
  VCenterMapPixel := ASource.GetCenterMapPixelFloat;
  VCenterMapPixelNew.X := VCenterMapPixel.X + ADelta.X;
  VCenterMapPixelNew.Y := VCenterMapPixel.Y + ADelta.Y;
  VConverter.CheckPixelPosFloatStrict(VCenterMapPixelNew, VZoom, True);
  if (Abs(VCenterMapPixel.X - VCenterMapPixelNew.X) < 0.001) and
    (Abs(VCenterMapPixel.Y - VCenterMapPixelNew.Y) < 0.001) then begin
    Result := ASource;
    Exit;
  end;
  VLocalRect := ASource.GetLocalRect;
  VScale := ASource.GetScale;
  VTopLefAtMap.X := VCenterMapPixelNew.X - ((VLocalRect.Right - VLocalRect.Left) / 2) / VScale;
  VTopLefAtMap.Y := VCenterMapPixelNew.Y - ((VLocalRect.Bottom - VLocalRect.Top) / 2) / VScale;
  Result :=
    CreateConverter(
      VLocalRect,
      VZoom,
      VConverter,
      VScale,
      VTopLefAtMap
    );
end;

function TLocalCoordConverterFactorySimpe.ChangeCenterLonLat(
  const ASource: ILocalCoordConverter;
  const ALonLat: TDoublePoint): ILocalCoordConverter;
var
  VZoom: Byte;
  VConverter: ICoordConverter;
  VLocalRect: TRect;
  VScale: Double;
  VLonLat: TDoublePoint;
  VCenterMapPixel: TDoublePoint;
  VCenterMapPixelNew: TDoublePoint;
  VTopLefAtMap: TDoublePoint;
begin
  VZoom := ASource.Zoom;
  VConverter := ASource.GeoConverter;
  VLonLat := ALonLat;
  VConverter.CheckLonLatPos(VLonLat);
  VCenterMapPixel := ASource.GetCenterMapPixelFloat;
  VCenterMapPixelNew := VConverter.LonLat2PixelPosFloat(VLonLat, VZoom);
  if (Abs(VCenterMapPixel.X - VCenterMapPixelNew.X) < 0.001) and
    (Abs(VCenterMapPixel.Y - VCenterMapPixelNew.Y) < 0.001) then begin
    Result := ASource;
    Exit;
  end;
  VLocalRect := ASource.GetLocalRect;
  VScale := ASource.GetScale;
  VTopLefAtMap.X := VCenterMapPixelNew.X - ((VLocalRect.Right - VLocalRect.Left) / 2) / VScale;
  VTopLefAtMap.Y := VCenterMapPixelNew.Y - ((VLocalRect.Bottom - VLocalRect.Top) / 2) / VScale;
  Result :=
    CreateConverter(
      VLocalRect,
      VZoom,
      VConverter,
      VScale,
      VTopLefAtMap
    );
end;

function TLocalCoordConverterFactorySimpe.ChangeCenterLonLatAndZoom(
  const ASource: ILocalCoordConverter;
  AZoom: Byte;
  const ALonLat: TDoublePoint): ILocalCoordConverter;
var
  VZoomOld: Byte;
  VZoomNew: Byte;
  VConverter: ICoordConverter;
  VLocalRect: TRect;
  VScale: Double;
  VLonLat: TDoublePoint;
  VCenterMapPixelNew: TDoublePoint;
  VTopLefAtMap: TDoublePoint;
begin
  VZoomNew := AZoom;
  VZoomOld := ASource.Zoom;
  VConverter := ASource.GeoConverter;
  VConverter.CheckZoom(VZoomNew);
  if VZoomNew = VZoomOld then begin
    Result := ChangeCenterLonLat(ASource, ALonLat);
    Exit;
  end;

  VLonLat := ALonLat;
  VConverter.CheckLonLatPos(VLonLat);
  VCenterMapPixelNew := VConverter.LonLat2PixelPosFloat(VLonLat, VZoomNew);
  VLocalRect := ASource.GetLocalRect;
  VScale := ASource.GetScale;
  VTopLefAtMap.X := VCenterMapPixelNew.X - ((VLocalRect.Right - VLocalRect.Left) / 2) / VScale;
  VTopLefAtMap.Y := VCenterMapPixelNew.Y - ((VLocalRect.Bottom - VLocalRect.Top) / 2) / VScale;
  Result :=
    CreateConverter(
      VLocalRect,
      VZoomNew,
      VConverter,
      VScale,
      VTopLefAtMap
    );
end;

function TLocalCoordConverterFactorySimpe.ChangeCenterToLocalPoint(
  const ASource: ILocalCoordConverter;
  const AVisualPoint: TPoint): ILocalCoordConverter;
var
  VZoom: Byte;
  VConverter: ICoordConverter;
  VLocalRect: TRect;
  VScale: Double;
  VCenterMapPixel: TDoublePoint;
  VCenterMapPixelNew: TDoublePoint;
  VTopLefAtMap: TDoublePoint;
begin
  VZoom := ASource.Zoom;
  VConverter := ASource.GeoConverter;
  VCenterMapPixel := ASource.GetCenterMapPixelFloat;
  VCenterMapPixelNew := ASource.LocalPixel2MapPixelFloat(AVisualPoint);
  VConverter.CheckPixelPosFloatStrict(VCenterMapPixelNew, VZoom, True);
  if (Abs(VCenterMapPixel.X - VCenterMapPixelNew.X) < 0.001) and
    (Abs(VCenterMapPixel.Y - VCenterMapPixelNew.Y) < 0.001) then begin
    Result := ASource;
    Exit;
  end;
  VLocalRect := ASource.GetLocalRect;
  VScale := ASource.GetScale;
  VTopLefAtMap.X := VCenterMapPixelNew.X - ((VLocalRect.Right - VLocalRect.Left) / 2) / VScale;
  VTopLefAtMap.Y := VCenterMapPixelNew.Y - ((VLocalRect.Bottom - VLocalRect.Top) / 2) / VScale;
  Result :=
    CreateConverter(
      VLocalRect,
      VZoom,
      VConverter,
      VScale,
      VTopLefAtMap
    );
end;

function TLocalCoordConverterFactorySimpe.ChangeConverter(
  const ASource: ILocalCoordConverter;
  const AConverter: ICoordConverter
): ILocalCoordConverter;
var
  VZoom: Byte;
  VLocalRect: TRect;
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
  AConverter.CheckLonLatPos(VCenterLonLat);
  VZoom := ASource.Zoom;
  VScale := ASource.GetScale;
  VLocalRect := ASource.GetLocalRect;

  VCenterMapPixelNew := AConverter.LonLat2PixelPosFloat(VCenterLonLat, VZoom);
  VTopLefAtMap.X := VCenterMapPixelNew.X - ((VLocalRect.Right - VLocalRect.Left) / 2) / VScale;
  VTopLefAtMap.Y := VCenterMapPixelNew.Y - ((VLocalRect.Bottom - VLocalRect.Top) / 2) / VScale;

  Result :=
    CreateConverter(
      VLocalRect,
      VZoom,
      AConverter,
      VScale,
      VTopLefAtMap
    );
end;

function TLocalCoordConverterFactorySimpe.ChangeZoomAndScaleSaveRelativeRect(
  const ASource: ILocalCoordConverter; const AZoom: Byte): ILocalCoordConverter;
var
  VZoomOld: Byte;
  VZoomNew: Byte;
  VConverter: ICoordConverter;
  VLocalRect: TRect;
  VScale: Double;
  VMapPixelRectOld: TDoubleRect;
  VMapPixelRectNew: TDoubleRect;
  VMapLocalRect: TDoubleRect;
  VMapRelativeRect: TDoubleRect;
  VCenterRelative: TDoublePoint;
  VCenterMapPixelNew: TDoublePoint;
  VTopLefAtMap: TDoublePoint;
begin
  VZoomOld := ASource.Zoom;
  VZoomNew := AZoom;
  VConverter := ASource.GeoConverter;
  VConverter.CheckZoom(VZoomNew);
  if VZoomOld = VZoomNew then begin
    Result := ASource;
    Exit;
  end;

  VMapPixelRectOld := ASource.GetRectInMapPixelFloat;
  VConverter.CheckPixelRectFloat(VMapPixelRectOld, VZoomOld);
  VMapLocalRect := ASource.MapRectFloat2LocalRectFloat(VMapPixelRectOld);
  VMapRelativeRect := VConverter.PixelRectFloat2RelativeRect(VMapPixelRectOld, VZoomOld);
  VMapPixelRectNew := VConverter.RelativeRect2PixelRectFloat(VMapRelativeRect, VZoomNew);

  VScale := (VMapPixelRectNew.Right - VMapPixelRectNew.Left) / (VMapPixelRectOld.Right - VMapPixelRectOld.Left);

  VCenterRelative := VConverter.PixelPosFloat2Relative(ASource.GetCenterMapPixelFloat, VZoomOld);
  VCenterMapPixelNew := VConverter.Relative2PixelPosFloat(VCenterRelative, VZoomNew);

  VTopLefAtMap.X := VCenterMapPixelNew.X - ((VLocalRect.Right - VLocalRect.Left) / 2) / VScale;
  VTopLefAtMap.Y := VCenterMapPixelNew.Y - ((VLocalRect.Bottom - VLocalRect.Top) / 2) / VScale;

  Result :=
    CreateConverter(
      VLocalRect,
      VZoomNew,
      VConverter,
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
begin
  VZoomOld := ASource.Zoom;
  VZoomNew := AZoom;
  VConverter := ASource.GeoConverter;
  VConverter.CheckZoom(VZoomNew);
  if VZoomOld = VZoomNew then begin
    Result := ASource;
    Exit;
  end;

  VCenterMapPixel := ASource.GetCenterMapPixelFloat;
  VRelativePoint := VConverter.PixelPosFloat2Relative(VCenterMapPixel, VZoomOld);
  VCenterMapPixelNew := VConverter.Relative2PixelPosFloat(VRelativePoint, VZoomNew);
  VLocalRect := ASource.GetLocalRect;
  VScale := ASource.GetScale;
  VTopLefAtMap.X := VCenterMapPixelNew.X - ((VLocalRect.Right - VLocalRect.Left) / 2) / VScale;
  VTopLefAtMap.Y := VCenterMapPixelNew.Y - ((VLocalRect.Bottom - VLocalRect.Top) / 2) / VScale;
  Result :=
    CreateConverter(
      VLocalRect,
      VZoomNew,
      VConverter,
      VScale,
      VTopLefAtMap
    );
end;

function TLocalCoordConverterFactorySimpe.ChangeZoomWithFreezeAtVisualPoint(
  const ASource: ILocalCoordConverter; const AZoom: Byte;
  const AFreezePoint: TPoint): ILocalCoordConverter;
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
  VCenterMapPixelNew: TDoublePoint;
begin
  VZoomOld := ASource.Zoom;
  VZoomNew := AZoom;
  VConverter := ASource.GeoConverter;
  VConverter.CheckZoom(VZoomNew);
  if VZoomOld = VZoomNew then begin
    Result := ASource;
    Exit;
  end;

  VFreezeMapPoint := ASource.LocalPixel2MapPixelFloat(AFreezePoint);
  VConverter.CheckPixelPosFloatStrict(VFreezeMapPoint, VZoomOld, False);
  VFreezePoint := ASource.MapPixelFloat2LocalPixelFloat(VFreezeMapPoint);
  VRelativeFreezePoint := VConverter.PixelPosFloat2Relative(VFreezeMapPoint, VZoomOld);
  VMapFreezPointAtNewZoom := VConverter.Relative2PixelPosFloat(VRelativeFreezePoint, VZoomNew);

  VLocalRect := ASource.GetLocalRect;
  VScale := ASource.GetScale;

  VCenterMapPixelNew.X := VMapFreezPointAtNewZoom.X + ((VLocalRect.Right - VLocalRect.Left) / 2 - VFreezePoint.X) / VScale;
  VCenterMapPixelNew.Y := VMapFreezPointAtNewZoom.Y + ((VLocalRect.Bottom - VLocalRect.Top) / 2 - VFreezePoint.Y) / VScale;
  VConverter.CheckPixelPosFloatStrict(VCenterMapPixelNew, VZoomNew, False);

  VTopLefAtMap.X := VCenterMapPixelNew.X - ((VLocalRect.Right - VLocalRect.Left) / 2) / VScale;
  VTopLefAtMap.Y := VCenterMapPixelNew.Y - ((VLocalRect.Bottom - VLocalRect.Top) / 2) / VScale;

  Result :=
    CreateConverter(
      VLocalRect,
      VZoomNew,
      VConverter,
      VScale,
      VTopLefAtMap
    );
end;

function TLocalCoordConverterFactorySimpe.CreateBySourceWithStableTileRect(
  const ASource: ILocalCoordConverter
): ILocalCoordConverter;
var
  VZoom: Byte;
  VSourcePixelRect: TRect;
  VConverter: ICoordConverter;
  VTileRect: TRect;
  VResultPixelRect: TRect;
  VViewSize: TPoint;
  VMovedTile: TPoint;
  VMovedPixelRect: TRect;
  VMovedTileRect: TRect;
begin
  VConverter := ASource.GetGeoConverter;
  VZoom := ASource.GetZoom;
  VSourcePixelRect := ASource.GetRectInMapPixel;
  VViewSize.X := VSourcePixelRect.Right - VSourcePixelRect.Left;
  VViewSize.Y := VSourcePixelRect.Bottom - VSourcePixelRect.Top;

  VConverter.CheckPixelRect(VSourcePixelRect, VZoom);

  VTileRect := VConverter.PixelRect2TileRect(VSourcePixelRect, VZoom);
  VMovedTile := VTileRect.TopLeft;
  Inc(VMovedTile.X);
  Inc(VMovedTile.Y);
  VConverter.CheckTilePosStrict(VMovedTile, VZoom, False);

  VMovedPixelRect.TopLeft := VConverter.TilePos2PixelPos(VMovedTile, VZoom);
  VMovedPixelRect.Right := VMovedPixelRect.Left + VViewSize.X;
  VMovedPixelRect.Bottom := VMovedPixelRect.Top + VViewSize.Y;
  VConverter.CheckPixelRect(VMovedPixelRect, VZoom);

  VMovedTileRect := VConverter.PixelRect2TileRect(VMovedPixelRect, VZoom);
  VTileRect.Right := VMovedTileRect.Right;
  VTileRect.Bottom := VMovedTileRect.Bottom;

  VResultPixelRect := VConverter.TileRect2PixelRect(VTileRect, VZoom);
  if EqualRect(VSourcePixelRect, VResultPixelRect) then begin
    Result := ASource;
  end else begin
    Result := TLocalCoordConverterNoScaleIntDelta.Create(
      Rect(
        0, 0,
        VResultPixelRect.Right - VResultPixelRect.Left,
        VResultPixelRect.Bottom - VResultPixelRect.Top
      ),
      ASource.ProjectionInfo,
      VResultPixelRect.TopLeft
    );
  end;
end;

function TLocalCoordConverterFactorySimpe.CreateBySourceWithStableTileRectAndOtherGeo(
  const ASource: ILocalCoordConverter;
  const AGeoConverter: ICoordConverter
): ILocalCoordConverter;
var
  VZoom: Byte;
  VSourcePixelRect: TRect;
  VSourceLonLatRect: TDoubleRect;
  VConverter: ICoordConverter;
  VTileRect: TRect;
  VResultPixelRect: TRect;
  VViewSize: TPoint;
  VMovedTile: TPoint;
  VMovedPixelRect: TRect;
  VMovedTileRect: TRect;
begin
  VConverter := ASource.GetGeoConverter;
  VZoom := ASource.GetZoom;
  VSourcePixelRect := ASource.GetRectInMapPixel;
  VConverter.CheckPixelRect(VSourcePixelRect, VZoom);
  VSourceLonLatRect := VConverter.PixelRect2LonLatRect(VSourcePixelRect, VZoom);
  AGeoConverter.CheckZoom(VZoom);
  AGeoConverter.CheckLonLatRect(VSourceLonLatRect);
  VSourcePixelRect := AGeoConverter.LonLatRect2PixelRect(VSourceLonLatRect, VZoom);

  VViewSize.X := VSourcePixelRect.Right - VSourcePixelRect.Left;
  VViewSize.Y := VSourcePixelRect.Bottom - VSourcePixelRect.Top;

  AGeoConverter.CheckPixelRect(VSourcePixelRect, VZoom);

  VTileRect := AGeoConverter.PixelRect2TileRect(VSourcePixelRect, VZoom);
  VMovedTile := VTileRect.TopLeft;
  Inc(VMovedTile.X);
  Inc(VMovedTile.Y);
  AGeoConverter.CheckTilePosStrict(VMovedTile, VZoom, False);

  VMovedPixelRect.TopLeft := AGeoConverter.TilePos2PixelPos(VMovedTile, VZoom);
  VMovedPixelRect.Right := VMovedPixelRect.Left + VViewSize.X;
  VMovedPixelRect.Bottom := VMovedPixelRect.Top + VViewSize.Y;
  AGeoConverter.CheckPixelRect(VMovedPixelRect, VZoom);

  VMovedTileRect := AGeoConverter.PixelRect2TileRect(VMovedPixelRect, VZoom);
  VTileRect.Right := VMovedTileRect.Right;
  VTileRect.Bottom := VMovedTileRect.Bottom;

  VResultPixelRect := AGeoConverter.TileRect2PixelRect(VTileRect, VZoom);

  Result := TLocalCoordConverterNoScaleIntDelta.Create(
    Rect(0, 0, VResultPixelRect.Right - VResultPixelRect.Left, VResultPixelRect.Bottom - VResultPixelRect.Top),
    FProjectionFactory.GetByConverterAndZoom(AGeoConverter, VZoom),
    VResultPixelRect.TopLeft
  );
end;

function TLocalCoordConverterFactorySimpe.CreateConverter(
  const ALocalRect: TRect;
  const AZoom: Byte;
  const AGeoConverter: ICoordConverter;
  const AMapScale: Double;
  const ALocalTopLeftAtMap: TDoublePoint
): ILocalCoordConverter;
begin
  if Abs(AMapScale - 1) < 0.01  then begin
    Result :=
      TLocalCoordConverterNoScale.Create(
        ALocalRect,
        FProjectionFactory.GetByConverterAndZoom(AGeoConverter, AZoom),
        ALocalTopLeftAtMap
      );
  end else begin
    Result :=
      TLocalCoordConverter.Create(
        ALocalRect,
        FProjectionFactory.GetByConverterAndZoom(AGeoConverter, AZoom),
        AMapScale,
        ALocalTopLeftAtMap
      );
  end;
end;

function TLocalCoordConverterFactorySimpe.CreateConverterNoScale(
  const ALocalRect: TRect;
  const AZoom: Byte;
  const AGeoConverter: ICoordConverter;
  const ALocalTopLeftAtMap: TPoint
): ILocalCoordConverter;
begin
  Result := TLocalCoordConverterNoScaleIntDelta.Create(
    ALocalRect,
    FProjectionFactory.GetByConverterAndZoom(AGeoConverter, AZoom),
    ALocalTopLeftAtMap
  );
end;

function TLocalCoordConverterFactorySimpe.CreateForTile(
  const ATile: TPoint;
  const AZoom: Byte;
  const AGeoConverter: ICoordConverter
): ILocalCoordConverter;
var
  VPixelRect: TRect;
  VBitmapTileRect: TRect;
begin
  VPixelRect := AGeoConverter.TilePos2PixelRect(ATile, AZoom);
  VBitmapTileRect.Left := 0;
  VBitmapTileRect.Top := 0;
  VBitmapTileRect.Right := VPixelRect.Right - VPixelRect.Left;
  VBitmapTileRect.Bottom := VPixelRect.Bottom - VPixelRect.Top;
  Result := TLocalCoordConverterNoScaleIntDelta.Create(
    VBitmapTileRect,
    FProjectionFactory.GetByConverterAndZoom(AGeoConverter, AZoom),
    VPixelRect.TopLeft
  );
end;

end.
