{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_BitmapTileProviderWithBGColor;

interface

uses
  Types,
  t_Bitmap32,
  i_NotifierOperation,
  i_Bitmap32Static,
  i_Bitmap32BufferFactory,
  i_Projection,
  i_GeometryProjected,
  i_BitmapTileProvider,
  u_BaseInterfacedObject,
  u_Bitmap32ByStaticBitmap;

type
  TBitmapTileProviderWithBgColor = class(TBaseInterfacedObject, IBitmapTileProvider)
  private
    FBackGroundColor: TColor32;
    FEmptyColor: TColor32;
    FPolygon: IGeometryProjectedPolygon;
    FBitmap32StaticFactory: IBitmap32StaticFactory;
    FSourceProvider: IBitmapTileProvider;

    FMapRect: TRect;
    FUsePreciseCropping: Boolean;

    FEmptyTile: IBitmap32Static;
    FProjection: IProjection;

    procedure CropByPolygon(
      const ABitmap: TBitmap32ByStaticBitmap;
      const ATileSize: TPoint;
      const ACopyMapRect: TRect;
      const ACopyRectAtSource: TRect
    );
  private
    { IBitmapTileProvider }
    function GetProjection: IProjection;

    function GetTile(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ATile: TPoint
    ): IBitmap32Static;
  public
    constructor Create(
      const AUsePreciseCropping: Boolean;
      const ABackGroundColor: TColor32;
      const AEmptyColor: TColor32;
      const APolygon: IGeometryProjectedPolygon;
      const ABitmap32StaticFactory: IBitmap32StaticFactory;
      const ASourceProvider: IBitmapTileProvider
    );
  end;

implementation

uses
  GR32,
  Math,
  t_GeoTypes,
  u_BitmapFunc,
  u_GeoFunc,
  u_GeometryFunc;

{ TBitmapTileProviderWithBGColor }

constructor TBitmapTileProviderWithBgColor.Create(
  const AUsePreciseCropping: Boolean;
  const ABackGroundColor: TColor32;
  const AEmptyColor: TColor32;
  const APolygon: IGeometryProjectedPolygon;
  const ABitmap32StaticFactory: IBitmap32StaticFactory;
  const ASourceProvider: IBitmapTileProvider
);
var
  VTileSize: TPoint;
  VTargetBmp: TBitmap32ByStaticBitmap;
begin
  Assert(Assigned(APolygon));
  Assert(Assigned(ASourceProvider));
  Assert(Assigned(ABitmap32StaticFactory));

  inherited Create;

  FBackGroundColor := ABackGroundColor;
  FEmptyColor := AEmptyColor;
  FBitmap32StaticFactory := ABitmap32StaticFactory;
  FSourceProvider := ASourceProvider;

  VTargetBmp := TBitmap32ByStaticBitmap.Create(FBitmap32StaticFactory);
  try
    VTileSize := FSourceProvider.Projection.GetTileSize(Types.Point(0, 0));
    VTargetBmp.SetSize(VTileSize.X, VTileSize.Y);
    VTargetBmp.Clear(FEmptyColor);
    FEmptyTile := VTargetBmp.MakeAndClear;
  finally
    VTargetBmp.Free;
  end;

  FProjection := FSourceProvider.Projection;

  FUsePreciseCropping :=
    AUsePreciseCropping and
    not IsProjectedPolygonSimpleRect(APolygon);

  if FUsePreciseCropping then begin
    FPolygon := APolygon;
    FMapRect := RectFromDoubleRect(FPolygon.Bounds, rrOutside);
  end else begin
    FPolygon := nil;
  end;
end;

function TBitmapTileProviderWithBgColor.GetProjection: IProjection;
begin
  Result := FProjection;
end;

function TBitmapTileProviderWithBgColor.GetTile(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ATile: TPoint
): IBitmap32Static;
var
  VTileSize: TPoint;
  VTargetBmp: TBitmap32ByStaticBitmap;
  VTileMapRect: TRect;
  VCopyMapRect: TRect;
  VCopyRectAtSource: TRect;
begin
  Result :=
    FSourceProvider.GetTile(
      AOperationID,
      ACancelNotifier,
      ATile
    );
  if Result <> nil then begin
    if FUsePreciseCropping or (FBackGroundColor <> 0) then begin
      VTargetBmp := TBitmap32ByStaticBitmap.Create(FBitmap32StaticFactory);
      try
        VTileSize := FProjection.GetTileSize(ATile);
        VTargetBmp.SetSize(VTileSize.X, VTileSize.Y);
        VTargetBmp.Clear(FBackGroundColor);

        if FUsePreciseCropping then begin
          VTileMapRect := FProjection.TilePos2PixelRect(ATile);
          Types.IntersectRect(VCopyMapRect, VTileMapRect, FMapRect);
          VCopyRectAtSource := RectMove(VCopyMapRect, VTileMapRect.TopLeft);

          BlockTransfer(
            VTargetBmp,
            VCopyRectAtSource.Left,
            VCopyRectAtSource.Top,
            Result,
            VCopyRectAtSource,
            dmBlend
          );

          if FPolygon.IsRectIntersectBorder( DoubleRect(VCopyMapRect) ) then begin
            CropByPolygon(VTargetBmp, VTileSize, VCopyMapRect, VCopyRectAtSource);
          end;
        end else begin
          BlockTransferFull(
            VTargetBmp,
            0,
            0,
            Result,
            dmBlend
          );
        end;

        Result := VTargetBmp.MakeAndClear;
      finally
        VTargetBmp.Free;
      end;
    end;
  end else begin
    VTileSize := FProjection.GetTileSize(ATile);
    if IsPointsEqual(VTileSize, FEmptyTile.Size) then begin
      Result := FEmptyTile;
    end else begin
      VTargetBmp := TBitmap32ByStaticBitmap.Create(FBitmap32StaticFactory);
      try
        VTargetBmp.SetSize(VTileSize.X, VTileSize.Y);
        VTargetBmp.Clear(FEmptyColor);
        Result := VTargetBmp.MakeAndClear;
      finally
        VTargetBmp.Free;
      end;
    end;
  end;
end;

procedure TBitmapTileProviderWithBgColor.CropByPolygon(
  const ABitmap: TBitmap32ByStaticBitmap;
  const ATileSize: TPoint;
  const ACopyMapRect: TRect;
  const ACopyRectAtSource: TRect
);
var
  I, J: Integer;
  VTileMapRect: TRect;
  VCopyRectSize: TPoint;
  VPix: PColor32;
  VPixelPoint: TDoublePoint;
begin
  VCopyRectSize := RectSize(ACopyMapRect);

  for I := 0 to VCopyRectSize.Y - 1 do begin
    VPix := @ABitmap.Bits[ACopyRectAtSource.Left + (I + ACopyRectAtSource.Top) * ATileSize.X];

    for J := 0 to VCopyRectSize.X - 1 do begin
      if VPix^ <> FBackGroundColor then begin
        VPixelPoint.X := ACopyMapRect.Left + J;
        VPixelPoint.Y := ACopyMapRect.Top + I;
        if not FPolygon.IsPointInPolygon(VPixelPoint) then begin
          VPix^ := FBackGroundColor;
        end;
      end;
      Inc(VPix);
    end;
  end;
end;

end.
