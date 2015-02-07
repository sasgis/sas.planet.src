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

unit u_VectorTileProviderByOtherProjection;

interface

uses
  Types,
  i_NotifierOperation,
  i_VectorItemSubset,
  i_ProjectionInfo,
  i_VectorTileProvider,
  i_VectorItemSubsetBuilder,
  u_BaseInterfacedObject;

type
  TVectorTileProviderByOtherBase = class(TBaseInterfacedObject, IVectorTileProvider)
  private
    FProvider: IVectorTileProvider;
    FProjectionInfo: IProjectionInfo;
    FOversize: TRect;
    FVectorSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  private
    function GetProjectionInfo: IProjectionInfo;
  protected
    function GetTile(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ATile: TPoint
    ): IVectorItemSubset; virtual; abstract;
  public
    constructor Create(
      const AVectorSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
      const AOversize: TRect;
      const AProvider: IVectorTileProvider;
      const AProjectionInfo: IProjectionInfo
    );
  end;

  TVectorTileProviderByOtherProjection = class(TVectorTileProviderByOtherBase)
  protected
    function GetTile(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ATile: TPoint
    ): IVectorItemSubset; override;
  public
    constructor Create(
      const AVectorSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
      const AOversize: TRect;
      const AProvider: IVectorTileProvider;
      const AProjectionInfo: IProjectionInfo
    );
  end;

  TVectorTileProviderBySameProjection = class(TVectorTileProviderByOtherBase)
  protected
    function GetTile(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ATile: TPoint
    ): IVectorItemSubset; override;
  public
    constructor Create(
      const AVectorSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
      const AOversize: TRect;
      const AProvider: IVectorTileProvider;
      const AProjectionInfo: IProjectionInfo
    );
  end;

implementation

uses
  t_GeoTypes,
  i_CoordConverter,
  i_VectorDataItemSimple,
  i_LonLatRect,
  i_TileIterator,
  u_TileIteratorByRect,
  u_GeoFunc;

{ TVectorTileProviderByOtherBase }

constructor TVectorTileProviderByOtherBase.Create(
  const AVectorSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const AOversize: TRect;
  const AProvider: IVectorTileProvider;
  const AProjectionInfo: IProjectionInfo
);
begin
  Assert(Assigned(AVectorSubsetBuilderFactory));
  Assert(AOversize.Left >= 0);
  Assert(AOversize.Left < 4096);
  Assert(AOversize.Top >= 0);
  Assert(AOversize.Top < 4096);
  Assert(AOversize.Right >= 0);
  Assert(AOversize.Right < 4096);
  Assert(AOversize.Bottom >= 0);
  Assert(AOversize.Bottom < 4096);
  Assert(Assigned(AProvider));
  Assert(Assigned(AProjectionInfo));
  Assert(not AProvider.ProjectionInfo.GetIsSameProjectionInfo(AProjectionInfo));
  inherited Create;
  FVectorSubsetBuilderFactory := AVectorSubsetBuilderFactory;
  FOversize := AOversize;
  FProvider := AProvider;
  FProjectionInfo := AProjectionInfo;
end;

function TVectorTileProviderByOtherBase.GetProjectionInfo: IProjectionInfo;
begin
  Result := FProjectionInfo;
end;

{ TVectorTileProviderByOtherProjection }

constructor TVectorTileProviderByOtherProjection.Create(
  const AVectorSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const AOversize: TRect;
  const AProvider: IVectorTileProvider;
  const AProjectionInfo: IProjectionInfo
);
begin
  inherited Create(AVectorSubsetBuilderFactory, AOversize, AProvider, AProjectionInfo);
  Assert(not AProjectionInfo.GeoConverter.IsSameConverter(AProvider.ProjectionInfo.GeoConverter));
end;

procedure TileToBuilder(
  const ALonLatRect: TDoubleRect;
  const AResult: IVectorItemSubsetBuilder;
  const ASource: IVectorItemSubset
); inline;
var
  VItem: IVectorDataItem;
  i: Integer;
  VBounds: ILonLatRect;
begin
  Assert(Assigned(AResult));
  Assert(Assigned(ASource));
  for i := 0 to ASource.Count - 1 do begin
    VItem := ASource.Items[i];
    if Assigned(VItem) then begin
      VBounds := VItem.Geometry.Bounds;
      if  Assigned(VBounds) then begin
        if VBounds.IsIntersecWithRect(ALonLatRect) then begin
          AResult.Add(VItem);
        end;
      end;
    end;
  end;
end;


function TVectorTileProviderByOtherProjection.GetTile(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ATile: TPoint
): IVectorItemSubset;
var
  VTile: TPoint;
  VTargetPixelRect: TRect;
  VSourceConverter: ICoordConverter;
  VSourceZoom: Byte;
  VTargetConverter: ICoordConverter;
  VTargetZoom: Byte;
  VLonLatRectSource: TDoubleRect;
  VLonLatRectTarget: TDoubleRect;
  VSourceTileRect: TRect;
  VTileIterator: ITileIterator;
  VSourceTile: TPoint;
  VSource: IVectorItemSubset;
  VSubsetBuilder: IVectorItemSubsetBuilder;
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
  Dec(VTargetPixelRect.Left, FOversize.Left);
  Dec(VTargetPixelRect.Top, FOversize.Top);
  Inc(VTargetPixelRect.Right, FOversize.Right);
  Inc(VTargetPixelRect.Bottom, FOversize.Bottom);
  VTargetConverter.ValidatePixelRect(VTargetPixelRect, VTargetZoom);
  VLonLatRectTarget := VTargetConverter.PixelRect2LonLatRect(VTargetPixelRect, VTargetZoom);
  VLonLatRectSource := VLonLatRectTarget;
  VSourceConverter.ValidateLonLatRect(VLonLatRectSource);
  VSourceTileRect := RectFromDoubleRect(VSourceConverter.LonLatRect2TileRectFloat(VLonLatRectSource, VSourceZoom), rrOutside);
  Assert(VSourceTileRect.Right > VSourceTileRect.Left);
  Assert(VSourceTileRect.Bottom > VSourceTileRect.Top);
  if (VSourceTileRect.Right - VSourceTileRect.Left = 1) and (VSourceTileRect.Bottom - VSourceTileRect.Top = 1) then begin
    VSourceTile := VSourceTileRect.TopLeft;
    VSource := FProvider.GetTile(AOperationID, ACancelNotifier, VSourceTile);
    if Assigned(VSource) then begin
      VSubsetBuilder := FVectorSubsetBuilderFactory.Build;
      TileToBuilder(VLonLatRectTarget, VSubsetBuilder, VSource);
      Result := VSubsetBuilder.MakeStaticAndClear;
    end;
  end else begin
    VSubsetBuilder := FVectorSubsetBuilderFactory.Build;
    VTileIterator := TTileIteratorByRect.Create(VSourceTileRect);
    while VTileIterator.Next(VSourceTile) do begin
      VSource := FProvider.GetTile(AOperationID, ACancelNotifier, VSourceTile);
      if Assigned(VSource) then begin
        TileToBuilder(VLonLatRectTarget, VSubsetBuilder, VSource);
      end;
    end;
    Result := VSubsetBuilder.MakeStaticAndClear;
  end;
end;

{ TVectorTileProviderBySameProjection }

constructor TVectorTileProviderBySameProjection.Create(
  const AVectorSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const AOversize: TRect;
  const AProvider: IVectorTileProvider;
  const AProjectionInfo: IProjectionInfo
);
begin
  inherited Create(AVectorSubsetBuilderFactory, AOversize, AProvider, AProjectionInfo);
  Assert(AProjectionInfo.GeoConverter.IsSameConverter(AProvider.ProjectionInfo.GeoConverter));
end;

function TVectorTileProviderBySameProjection.GetTile(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ATile: TPoint
): IVectorItemSubset;
var
  VTile: TPoint;
  VTargetPixelRect: TRect;
  VConverter: ICoordConverter;
  VSourceZoom: Byte;
  VTargetZoom: Byte;
  VRelativeRect: TDoubleRect;
  VSourceTileRect: TRect;
  VTileIterator: ITileIterator;
  VSourceTile: TPoint;
  VSource: IVectorItemSubset;
  VLonLatRectTarget: TDoubleRect;
  VSubsetBuilder: IVectorItemSubsetBuilder;
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
  Dec(VTargetPixelRect.Left, FOversize.Left);
  Dec(VTargetPixelRect.Top, FOversize.Top);
  Inc(VTargetPixelRect.Right, FOversize.Right);
  Inc(VTargetPixelRect.Bottom, FOversize.Bottom);
  VConverter.ValidatePixelRect(VTargetPixelRect, VTargetZoom);
  VLonLatRectTarget := VConverter.PixelRect2LonLatRect(VTargetPixelRect, VTargetZoom);
  VRelativeRect := VConverter.PixelRect2RelativeRect(VTargetPixelRect, VTargetZoom);
  VSourceTileRect := RectFromDoubleRect(VConverter.RelativeRect2TileRectFloat(VRelativeRect, VSourceZoom), rrOutside);
  Assert(VSourceTileRect.Right > VSourceTileRect.Left);
  Assert(VSourceTileRect.Bottom > VSourceTileRect.Top);
  if (VSourceTileRect.Right - VSourceTileRect.Left = 1) and (VSourceTileRect.Bottom - VSourceTileRect.Top = 1) then begin
    VSourceTile := VSourceTileRect.TopLeft;
    VSource := FProvider.GetTile(AOperationID, ACancelNotifier, VSourceTile);
    if Assigned(VSource) then begin
      VSubsetBuilder := FVectorSubsetBuilderFactory.Build;
      TileToBuilder(VLonLatRectTarget, VSubsetBuilder, VSource);
      Result := VSubsetBuilder.MakeStaticAndClear;
    end;
  end else begin
    VSubsetBuilder := FVectorSubsetBuilderFactory.Build;
    VTileIterator := TTileIteratorByRect.Create(VSourceTileRect);
    while VTileIterator.Next(VSourceTile) do begin
      VSource := FProvider.GetTile(AOperationID, ACancelNotifier, VSourceTile);
      if Assigned(VSource) then begin
        TileToBuilder(VLonLatRectTarget, VSubsetBuilder, VSource);
      end;
    end;
    Result := VSubsetBuilder.MakeStaticAndClear;
  end;
end;

end.

