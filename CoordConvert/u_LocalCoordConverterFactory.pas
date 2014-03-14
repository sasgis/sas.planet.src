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

unit u_LocalCoordConverterFactory;

interface

uses
  Types,
  t_Hash,
  t_GeoTypes,
  i_HashFunction,
  i_ProjectionInfo,
  i_LocalCoordConverter,
  i_LocalCoordConverterFactory,
  u_HashCacheWithQueuesAbstract;

type
  TLocalCoordConverterFactory = class(THashCacheWithQueuesAbstract, ILocalCoordConverterFactory)
  private
    FHashFunction: IHashFunction;
  protected
    function CreateByKey(
      const AKey: THashValue;
      AData: Pointer
    ): IInterface; override;
  private
    function CreateNoScaleIntDelta(
      const ALocalRect: TRect;
      const AProjection: IProjectionInfo;
      const AMapPixelAtLocalZero: TPoint
    ): ILocalCoordConverter;
    function CreateNoScale(
      const ALocalRect: TRect;
      const AProjection: IProjectionInfo;
      const AMapPixelAtLocalZero: TDoublePoint
    ): ILocalCoordConverter;
    function CreateScaled(
      const ALocalRect: TRect;
      const AProjection: IProjectionInfo;
      const AMapScale: Double;
      const AMapPixelAtLocalZero: TDoublePoint
    ): ILocalCoordConverter;
  public
    constructor Create(
      const AHashFunction: IHashFunction
    );
  end;

implementation

uses
  u_LocalCoordConverter,
  u_GeoFunc;

type
  PDataRecord = ^TDataRecord;
  TDataRecord = record
    Hash: THashValue;
    LocalRect: TRect;
    RectInMapPixel: TRect;
    RectInMapPixelFloat: TDoubleRect;
    Projection: IProjectionInfo;
    MapPixelAtLocalZeroDouble: TDoublePoint;
    MapPixelAtLocalZeroInteger: TPoint;
    MapPixelCenter: TDoublePoint;
    MapScale: Double;
    ConverterType: (ctNoScale, ctNoScaleIntDelta, ctScaled);
  end;

{ TLocalCoordConverterFactory }

constructor TLocalCoordConverterFactory.Create(
  const AHashFunction: IHashFunction
);
begin
  inherited Create(13, 0, 1024, 0); // 2^13 elements in hash-table, LRU 1024 elements
  FHashFunction := AHashFunction;
end;

function TLocalCoordConverterFactory.CreateByKey(
  const AKey: THashValue;
  AData: Pointer
): IInterface;
var
  VData: PDataRecord;
  VResult: ILocalCoordConverter;
begin
  inherited;
  VResult := nil;
  VData := PDataRecord(AData);
  case VData.ConverterType of
    ctNoScale: begin
      VResult :=
        TLocalCoordConverterNoScale.Create(
          VData.Hash,
          VData.LocalRect,
          VData.RectInMapPixel,
          VData.RectInMapPixelFloat,
          VData.MapPixelCenter,
          VData.Projection,
          VData.MapPixelAtLocalZeroDouble
        );
    end;
    ctNoScaleIntDelta: begin
      VResult :=
        TLocalCoordConverterNoScaleIntDelta.Create(
          VData.Hash,
          VData.LocalRect,
          VData.RectInMapPixel,
          VData.RectInMapPixelFloat,
          VData.MapPixelCenter,
          VData.Projection,
          VData.MapPixelAtLocalZeroInteger
        );
    end;
    ctScaled: begin
      VResult :=
        TLocalCoordConverter.Create(
          VData.Hash,
          VData.LocalRect,
          VData.RectInMapPixel,
          VData.RectInMapPixelFloat,
          VData.MapPixelCenter,
          VData.Projection,
          VData.MapScale,
          VData.MapPixelAtLocalZeroDouble
        );
    end;
  end;

  Result := VResult;
end;

function TLocalCoordConverterFactory.CreateNoScale(
  const ALocalRect: TRect;
  const AProjection: IProjectionInfo;
  const AMapPixelAtLocalZero: TDoublePoint
): ILocalCoordConverter;
var
  VHash: THashValue;
  VData: TDataRecord;
  VZoom: Byte;
  VLocalCenter: TDoublePoint;
begin
  VHash := $2eb7867c2318cc59;
  FHashFunction.UpdateHashByRect(VHash, ALocalRect);
  FHashFunction.UpdateHashByHash(VHash, AProjection.Hash);
  FHashFunction.UpdateHashByDoublePoint(VHash, AMapPixelAtLocalZero);
  VData.ConverterType := ctNoScale;
  VData.LocalRect := ALocalRect;
  VZoom := AProjection.Zoom;
  VLocalCenter := RectCenter(ALocalRect);
  VData.MapPixelCenter.X := VLocalCenter.X + AMapPixelAtLocalZero.X;
  VData.MapPixelCenter.Y := VLocalCenter.Y + AMapPixelAtLocalZero.Y;
  AProjection.GeoConverter.CheckPixelPosFloatStrict(VData.MapPixelCenter, VZoom, False);

  VData.RectInMapPixelFloat.Left := ALocalRect.Left + AMapPixelAtLocalZero.X;
  VData.RectInMapPixelFloat.Top := ALocalRect.Top + AMapPixelAtLocalZero.Y;
  VData.RectInMapPixelFloat.Right := ALocalRect.Right + AMapPixelAtLocalZero.X;
  VData.RectInMapPixelFloat.Bottom := ALocalRect.Bottom + AMapPixelAtLocalZero.Y;
  AProjection.GeoConverter.CheckPixelRectFloat(VData.RectInMapPixelFloat, VZoom);

  VData.RectInMapPixel := RectFromDoubleRect(VData.RectInMapPixelFloat, rrClosest);
  AProjection.GeoConverter.CheckPixelRect(VData.RectInMapPixel, VZoom);

  VData.Projection := AProjection;
  VData.MapPixelAtLocalZeroDouble := AMapPixelAtLocalZero;

  Result := ILocalCoordConverter(GetOrCreateItem(VHash, @VData));
end;

function TLocalCoordConverterFactory.CreateNoScaleIntDelta(
  const ALocalRect: TRect;
  const AProjection: IProjectionInfo;
  const AMapPixelAtLocalZero: TPoint
): ILocalCoordConverter;
var
  VHash: THashValue;
  VData: TDataRecord;
  VZoom: Byte;
  VLocalCenter: TDoublePoint;
begin
  Assert(ALocalRect.Left <= ALocalRect.Right);
  Assert(ALocalRect.Top <= ALocalRect.Bottom);
  VHash := $801bc862120f6bf5;
  FHashFunction.UpdateHashByRect(VHash, ALocalRect);
  FHashFunction.UpdateHashByHash(VHash, AProjection.Hash);
  FHashFunction.UpdateHashByPoint(VHash, AMapPixelAtLocalZero);
  VData.ConverterType := ctNoScaleIntDelta;
  VData.LocalRect := ALocalRect;
  VZoom := AProjection.Zoom;

  VLocalCenter := RectCenter(ALocalRect);
  VData.MapPixelCenter.X := VLocalCenter.X + AMapPixelAtLocalZero.X;
  VData.MapPixelCenter.Y := VLocalCenter.Y + AMapPixelAtLocalZero.Y;
  AProjection.GeoConverter.CheckPixelPosFloatStrict(VData.MapPixelCenter, VZoom, False);

  VData.RectInMapPixel.Left := ALocalRect.Left + AMapPixelAtLocalZero.X;
  VData.RectInMapPixel.Top := ALocalRect.Top + AMapPixelAtLocalZero.Y;
  VData.RectInMapPixel.Right := ALocalRect.Right + AMapPixelAtLocalZero.X;
  VData.RectInMapPixel.Bottom := ALocalRect.Bottom + AMapPixelAtLocalZero.Y;
  AProjection.GeoConverter.CheckPixelRect(VData.RectInMapPixel, VZoom);

  VData.RectInMapPixelFloat := DoubleRect(VData.RectInMapPixel);

  VData.Projection := AProjection;
  VData.MapPixelAtLocalZeroInteger := AMapPixelAtLocalZero;

  Result := ILocalCoordConverter(GetOrCreateItem(VHash, @VData));
end;

function TLocalCoordConverterFactory.CreateScaled(
  const ALocalRect: TRect;
  const AProjection: IProjectionInfo;
  const AMapScale: Double;
  const AMapPixelAtLocalZero: TDoublePoint
): ILocalCoordConverter;
var
  VHash: THashValue;
  VData: TDataRecord;
  VZoom: Byte;
  VLocalCenter: TDoublePoint;
begin
  VHash := $de6a45ffc3ed1159;
  FHashFunction.UpdateHashByRect(VHash, ALocalRect);
  FHashFunction.UpdateHashByHash(VHash, AProjection.Hash);
  FHashFunction.UpdateHashByDouble(VHash, AMapScale);
  FHashFunction.UpdateHashByDoublePoint(VHash, AMapPixelAtLocalZero);
  VData.ConverterType := ctScaled;
  VData.LocalRect := ALocalRect;
  VZoom := AProjection.Zoom;

  VLocalCenter := RectCenter(ALocalRect);
  VData.MapPixelCenter.X := VLocalCenter.X / AMapScale + AMapPixelAtLocalZero.X;
  VData.MapPixelCenter.Y := VLocalCenter.Y / AMapScale + AMapPixelAtLocalZero.Y;
  AProjection.GeoConverter.CheckPixelPosFloatStrict(VData.MapPixelCenter, VZoom, False);

  VData.RectInMapPixelFloat.Left := ALocalRect.Left / AMapScale + AMapPixelAtLocalZero.X;
  VData.RectInMapPixelFloat.Top := ALocalRect.Top / AMapScale + AMapPixelAtLocalZero.Y;
  VData.RectInMapPixelFloat.Right := ALocalRect.Right / AMapScale + AMapPixelAtLocalZero.X;
  VData.RectInMapPixelFloat.Bottom := ALocalRect.Bottom / AMapScale + AMapPixelAtLocalZero.Y;
  AProjection.GeoConverter.CheckPixelRectFloat(VData.RectInMapPixelFloat, VZoom);

  VData.RectInMapPixel := RectFromDoubleRect(VData.RectInMapPixelFloat, rrClosest);
  AProjection.GeoConverter.CheckPixelRect(VData.RectInMapPixel, VZoom);

  VData.Projection := AProjection;
  VData.MapScale := AMapScale;
  VData.MapPixelAtLocalZeroDouble := AMapPixelAtLocalZero;

  Result := ILocalCoordConverter(GetOrCreateItem(VHash, @VData));
end;

end.
