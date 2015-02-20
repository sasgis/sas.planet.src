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

unit u_BitmapTileProviderByOtherProjection;

interface

uses
  Types,
  i_NotifierOperation,
  i_Bitmap32Static,
  i_ProjectionInfo,
  i_BitmapTileProvider,
  i_Bitmap32BufferFactory,
  i_ImageResamplerFactory,
  u_BaseInterfacedObject;

type
  TBitmapTileProviderByOtherBase = class(TBaseInterfacedObject, IBitmapTileProvider)
  private
    FProvider: IBitmapTileProvider;
    FProjectionInfo: IProjectionInfo;
    FBitmap32StaticFactory: IBitmap32StaticFactory;
    FImageResamplerFactory: IImageResamplerFactory;
  private
    function GetProjectionInfo: IProjectionInfo;
  protected
    function GetTile(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ATile: TPoint
    ): IBitmap32Static; virtual; abstract;
  public
    constructor Create(
      const ABitmap32StaticFactory: IBitmap32StaticFactory;
      const AImageResamplerFactory: IImageResamplerFactory;
      const AProvider: IBitmapTileProvider;
      const AProjectionInfo: IProjectionInfo
    );
  end;

  TBitmapTileProviderByOtherProjection = class(TBitmapTileProviderByOtherBase)
  protected
    function GetTile(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ATile: TPoint
    ): IBitmap32Static; override;
  public
    constructor Create(
      const ABitmap32StaticFactory: IBitmap32StaticFactory;
      const AImageResamplerFactory: IImageResamplerFactory;
      const AProvider: IBitmapTileProvider;
      const AProjectionInfo: IProjectionInfo
    );
  end;

  TBitmapTileProviderBySameProjection = class(TBitmapTileProviderByOtherBase)
  protected
    function GetTile(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ATile: TPoint
    ): IBitmap32Static; override;
  public
    constructor Create(
      const ABitmap32StaticFactory: IBitmap32StaticFactory;
      const AImageResamplerFactory: IImageResamplerFactory;
      const AProvider: IBitmapTileProvider;
      const AProjectionInfo: IProjectionInfo
    );
  end;

implementation

uses
  GR32,
  t_GeoTypes,
  i_CoordConverter,
  i_TileIterator,
  u_TileIteratorByRect,
  u_BitmapFunc,
  u_Bitmap32ByStaticBitmap,
  u_GeoFunc;

{ TBitmapTileProviderByOtherBase }

constructor TBitmapTileProviderByOtherBase.Create(
  const ABitmap32StaticFactory: IBitmap32StaticFactory;
  const AImageResamplerFactory: IImageResamplerFactory;
  const AProvider: IBitmapTileProvider;
  const AProjectionInfo: IProjectionInfo
);
begin
  Assert(Assigned(ABitmap32StaticFactory));
  Assert(Assigned(AImageResamplerFactory));
  Assert(Assigned(AProvider));
  Assert(Assigned(AProjectionInfo));
  Assert(not AProvider.ProjectionInfo.GetIsSameProjectionInfo(AProjectionInfo));
  inherited Create;
  FBitmap32StaticFactory := ABitmap32StaticFactory;
  FImageResamplerFactory := AImageResamplerFactory;
  FProvider := AProvider;
  FProjectionInfo := AProjectionInfo;
end;

function TBitmapTileProviderByOtherBase.GetProjectionInfo: IProjectionInfo;
begin
  Result := FProjectionInfo;
end;

{ TBitmapTileProviderByOtherProjection }

constructor TBitmapTileProviderByOtherProjection.Create(
  const ABitmap32StaticFactory: IBitmap32StaticFactory;
  const AImageResamplerFactory: IImageResamplerFactory;
  const AProvider: IBitmapTileProvider;
  const AProjectionInfo: IProjectionInfo
);
begin
  inherited Create(ABitmap32StaticFactory, AImageResamplerFactory, AProvider, AProjectionInfo);
  Assert(not AProjectionInfo.GeoConverter.IsSameConverter(AProvider.ProjectionInfo.GeoConverter));
end;

procedure TileToBufferSameProjType(
  const AResultMapRect: TRect;
  const AResultZoom: Byte;
  const AResult: TCustomBitmap32;
  const ASourceTile: TPoint;
  const ASourceZoom: Byte;
  const ASourceImage: IBitmap32Static;
  const ACoordConverter: ICoordConverter;
  AResampler: TCustomResampler
); inline;
var
  VSourcePixelRect: TRect;
  VSourceRelativeRect: TDoubleRect;
  VTargetRectFloat: TDoubleRect;
  VTargetRect: TRect;
begin
  Assert(Assigned(AResult));
  Assert(Assigned(ASourceImage));
  Assert(Assigned(ACoordConverter));
  Assert(AResultZoom <> ASourceZoom);

  VSourcePixelRect := ACoordConverter.TilePos2PixelRect(ASourceTile, ASourceZoom);

  VSourceRelativeRect := ACoordConverter.TilePos2RelativeRect(ASourceTile, ASourceZoom);
  VTargetRectFloat := ACoordConverter.RelativeRect2PixelRectFloat(VSourceRelativeRect, AResultZoom);
  VTargetRect := RectFromDoubleRect(VTargetRectFloat, rrClosest);
  Dec(VTargetRect.Left, AResultMapRect.Left);
  Dec(VTargetRect.Top, AResultMapRect.Top);
  Dec(VTargetRect.Right, AResultMapRect.Left);
  Dec(VTargetRect.Bottom, AResultMapRect.Top);
  StretchTransferFull(
    AResult,
    VTargetRect,
    ASourceImage,
    AResampler,
    dmOpaque
  );
end;

procedure TileToBufferOtherProjType(
  const AResultMapRect: TRect;
  const AResultZoom: Byte;
  const AResultCoordConverter: ICoordConverter;
  const AResult: TCustomBitmap32;
  const ASourceTile: TPoint;
  const ASourceZoom: Byte;
  const ASourceCoordConverter: ICoordConverter;
  const ASourceImage: IBitmap32Static;
  AResampler: TCustomResampler
); inline;
var
  VSourcePixelRect: TRect;
  VSourceLonLatRect: TDoubleRect;
  VTargetRectFloat: TDoubleRect;
  VTargetRect: TRect;
begin
  Assert(Assigned(AResult));
  Assert(Assigned(ASourceImage));
  Assert(Assigned(AResultCoordConverter));
  Assert(Assigned(ASourceCoordConverter));
  Assert(not AResultCoordConverter.IsSameConverter(ASourceCoordConverter));

  VSourcePixelRect := ASourceCoordConverter.TilePos2PixelRect(ASourceTile, ASourceZoom);

  VSourceLonLatRect := ASourceCoordConverter.TilePos2LonLatRect(ASourceTile, ASourceZoom);
  AResultCoordConverter.ValidateLonLatRect(VSourceLonLatRect);
  VTargetRectFloat := AResultCoordConverter.LonLatRect2PixelRectFloat(VSourceLonLatRect, AResultZoom);
  VTargetRect := RectFromDoubleRect(VTargetRectFloat, rrClosest);
  Dec(VTargetRect.Left, AResultMapRect.Left);
  Dec(VTargetRect.Top, AResultMapRect.Top);
  Dec(VTargetRect.Right, AResultMapRect.Left);
  Dec(VTargetRect.Bottom, AResultMapRect.Top);
  StretchTransferFull(
    AResult,
    VTargetRect,
    ASourceImage,
    AResampler,
    dmOpaque
  );
end;

function TBitmapTileProviderByOtherProjection.GetTile(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ATile: TPoint
): IBitmap32Static;
var
  VTile: TPoint;
  VTargetPixelRect: TRect;
  VTargetTileSize: TPoint;
  VSourceConverter: ICoordConverter;
  VSourceZoom: Byte;
  VTargetConverter: ICoordConverter;
  VTargetZoom: Byte;
  VLonLatRect: TDoubleRect;
  VTargetPixelRectAtSource: TDoubleRect;
  VSourceTileRect: TRect;
  VTileIterator: ITileIterator;
  VSourceTile: TPoint;
  VResampler: TCustomResampler;
  VBitmap: TBitmap32ByStaticBitmap;
  VSourceImage: IBitmap32Static;
begin
  Result := nil;
  VTile := ATile;
  VSourceConverter := FProvider.ProjectionInfo.GeoConverter;
  VSourceZoom := FProvider.ProjectionInfo.Zoom;
  VTargetConverter := FProjectionInfo.GeoConverter;
  VTargetZoom := FProjectionInfo.Zoom;

  if not VTargetConverter.CheckTilePosStrict(VTile, VTargetZoom) then begin
    Exit;
  end;
  VTargetPixelRect := VTargetConverter.TilePos2PixelRect(VTile, VTargetZoom);
  VTargetTileSize := Types.Point(VTargetPixelRect.Right - VTargetPixelRect.Left, VTargetPixelRect.Bottom - VTargetPixelRect.Top);
  VLonLatRect := VTargetConverter.PixelRect2LonLatRect(VTargetPixelRect, VTargetZoom);
  VSourceConverter.ValidateLonLatRect(VLonLatRect);
  VTargetPixelRectAtSource := VSourceConverter.LonLatRect2PixelRectFloat(VLonLatRect, VSourceZoom);
  VSourceTileRect := RectFromDoubleRect(VSourceConverter.PixelRectFloat2TileRectFloat(VTargetPixelRectAtSource, VSourceZoom), rrOutside);
  Assert(VSourceTileRect.Right > VSourceTileRect.Left);
  Assert(VSourceTileRect.Bottom > VSourceTileRect.Top);
  VBitmap := nil;
  VResampler := FImageResamplerFactory.CreateResampler;
  try
    if (VSourceTileRect.Right - VSourceTileRect.Left = 1) and (VSourceTileRect.Bottom - VSourceTileRect.Top = 1) then begin
      VSourceTile := VSourceTileRect.TopLeft;
      VSourceImage := FProvider.GetTile(AOperationID, ACancelNotifier, VSourceTile);
      if Assigned(VSourceImage) then begin
        VBitmap := TBitmap32ByStaticBitmap.Create(FBitmap32StaticFactory);
        VBitmap.SetSize(VTargetTileSize.X, VTargetTileSize.Y);
        VBitmap.Clear(0);
        TileToBufferOtherProjType(
          VTargetPixelRect,
          VTargetZoom,
          VTargetConverter,
          VBitmap,
          VSourceTile,
          VSourceZoom,
          VSourceConverter,
          VSourceImage,
          VResampler
        );
        Result := VBitmap.MakeAndClear;
      end;
    end else begin
      VTileIterator := TTileIteratorByRect.Create(VSourceTileRect);
      while VTileIterator.Next(VSourceTile) do begin
        VSourceImage := FProvider.GetTile(AOperationID, ACancelNotifier, VSourceTile);
        if Assigned(VSourceImage) then begin
          if not Assigned(VBitmap) then begin
            VBitmap := TBitmap32ByStaticBitmap.Create(FBitmap32StaticFactory);
            VBitmap.SetSize(VTargetTileSize.X, VTargetTileSize.Y);
            VBitmap.Clear(0);
          end;
          TileToBufferOtherProjType(
            VTargetPixelRect,
            VTargetZoom,
            VTargetConverter,
            VBitmap,
            VSourceTile,
            VSourceZoom,
            VSourceConverter,
            VSourceImage,
            VResampler
          );
        end;
      end;
      if Assigned(VBitmap) then begin
        Result := VBitmap.MakeAndClear;
      end;
    end;
  finally
    VBitmap.Free;
    VResampler.Free;
  end;
end;

{ TBitmapTileProviderBySameProjection }

constructor TBitmapTileProviderBySameProjection.Create(
  const ABitmap32StaticFactory: IBitmap32StaticFactory;
  const AImageResamplerFactory: IImageResamplerFactory;
  const AProvider: IBitmapTileProvider;
  const AProjectionInfo: IProjectionInfo
);
begin
  inherited Create(ABitmap32StaticFactory, AImageResamplerFactory, AProvider, AProjectionInfo);
  Assert(AProjectionInfo.GeoConverter.IsSameConverter(AProvider.ProjectionInfo.GeoConverter));
end;

function TBitmapTileProviderBySameProjection.GetTile(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ATile: TPoint
): IBitmap32Static;
var
  VTile: TPoint;
  VTargetPixelRect: TRect;
  VTargetTileSize: TPoint;
  VConverter: ICoordConverter;
  VSourceZoom: Byte;
  VTargetZoom: Byte;
  VRelativeRect: TDoubleRect;
  VTargetPixelRectAtSource: TDoubleRect;
  VSourceTileRect: TRect;
  VTileIterator: ITileIterator;
  VSourceTile: TPoint;
  VResampler: TCustomResampler;
  VBitmap: TBitmap32ByStaticBitmap;
  VSourceImage: IBitmap32Static;
begin
  Result := nil;
  VTile := ATile;
  VConverter := FProjectionInfo.GeoConverter;
  VTargetZoom := FProjectionInfo.Zoom;
  VSourceZoom := FProvider.ProjectionInfo.Zoom;

  if not VConverter.CheckTilePosStrict(VTile, VTargetZoom) then begin
    Exit;
  end;
  VTargetPixelRect := VConverter.TilePos2PixelRect(VTile, VTargetZoom);
  VTargetTileSize := Types.Point(VTargetPixelRect.Right - VTargetPixelRect.Left, VTargetPixelRect.Bottom - VTargetPixelRect.Top);
  VRelativeRect := VConverter.PixelRect2RelativeRect(VTargetPixelRect, VTargetZoom);
  VTargetPixelRectAtSource := VConverter.RelativeRect2PixelRectFloat(VRelativeRect, VSourceZoom);
  VSourceTileRect := RectFromDoubleRect(VConverter.PixelRectFloat2TileRectFloat(VTargetPixelRectAtSource, VSourceZoom), rrOutside);
  Assert(VSourceTileRect.Right > VSourceTileRect.Left);
  Assert(VSourceTileRect.Bottom > VSourceTileRect.Top);
  VBitmap := nil;
  VResampler := FImageResamplerFactory.CreateResampler;
  try
    if (VSourceTileRect.Right - VSourceTileRect.Left = 1) and (VSourceTileRect.Bottom - VSourceTileRect.Top = 1) then begin
      VSourceTile := VSourceTileRect.TopLeft;
      VSourceImage := FProvider.GetTile(AOperationID, ACancelNotifier, VSourceTile);
      if Assigned(VSourceImage) then begin
        VBitmap := TBitmap32ByStaticBitmap.Create(FBitmap32StaticFactory);
        VBitmap.SetSize(VTargetTileSize.X, VTargetTileSize.Y);
        VBitmap.Clear(0);
        TileToBufferSameProjType(
          VTargetPixelRect,
          VTargetZoom,
          VBitmap,
          VSourceTile,
          VSourceZoom,
          VSourceImage,
          VConverter,
          VResampler
        );
        Result := VBitmap.MakeAndClear;
      end;
    end else begin
      VTileIterator := TTileIteratorByRect.Create(VSourceTileRect);
      while VTileIterator.Next(VSourceTile) do begin
        VSourceImage := FProvider.GetTile(AOperationID, ACancelNotifier, VSourceTile);
        if Assigned(VSourceImage) then begin
          if not Assigned(VBitmap) then begin
            VBitmap := TBitmap32ByStaticBitmap.Create(FBitmap32StaticFactory);
            VBitmap.SetSize(VTargetTileSize.X, VTargetTileSize.Y);
            VBitmap.Clear(0);
          end;
          TileToBufferSameProjType(
            VTargetPixelRect,
            VTargetZoom,
            VBitmap,
            VSourceTile,
            VSourceZoom,
            VSourceImage,
            VConverter,
            VResampler
          );
        end;
      end;
      if Assigned(VBitmap) then begin
        Result := VBitmap.MakeAndClear;
      end;
    end;
  finally
    VBitmap.Free;
    VResampler.Free;
  end;
end;

end.

