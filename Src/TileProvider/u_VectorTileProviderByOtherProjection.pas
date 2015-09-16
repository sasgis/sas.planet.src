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
    FProjectionInfo: IProjection;
    FOversize: TRect;
    FVectorSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  private
    function GetProjectionInfo: IProjection;
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
      const AProjectionInfo: IProjection
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
      const AProjectionInfo: IProjection
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
      const AProjectionInfo: IProjection
    );
  end;

implementation

uses
  Math,
  t_GeoTypes,
  i_VectorDataItemSimple,
  i_LonLatRect,
  u_TileIteratorByRect,
  u_GeoFunc;

{ TVectorTileProviderByOtherBase }

constructor TVectorTileProviderByOtherBase.Create(
  const AVectorSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const AOversize: TRect;
  const AProvider: IVectorTileProvider;
  const AProjectionInfo: IProjection
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

function TVectorTileProviderByOtherBase.GetProjectionInfo: IProjection;
begin
  Result := FProjectionInfo;
end;

{ TVectorTileProviderByOtherProjection }

constructor TVectorTileProviderByOtherProjection.Create(
  const AVectorSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const AOversize: TRect;
  const AProvider: IVectorTileProvider;
  const AProjectionInfo: IProjection
);
begin
  inherited Create(AVectorSubsetBuilderFactory, AOversize, AProvider, AProjectionInfo);
  Assert(not AProjectionInfo.ProjectionType.IsSame(AProvider.ProjectionInfo.ProjectionType));
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
  VProjectionSource: IProjection;
  VProjectionTarget: IProjection;
  VLonLatRectSource: TDoubleRect;
  VLonLatRectTarget: TDoubleRect;
  VSourceTileRect: TRect;
  VTileIterator: TTileIteratorByRectRecord;
  VSourceTile: TPoint;
  VSource: IVectorItemSubset;
  VSubsetBuilder: IVectorItemSubsetBuilder;
begin
  Result := nil;
  VTile := ATile;
  VProjectionSource := FProvider.ProjectionInfo;
  VProjectionTarget := FProjectionInfo;

  if not VProjectionTarget.CheckTilePosStrict(VTile) then begin
    Exit;
  end;
  VTargetPixelRect := VProjectionTarget.TilePos2PixelRect(VTile);
  Dec(VTargetPixelRect.Left, FOversize.Left);
  Dec(VTargetPixelRect.Top, FOversize.Top);
  Inc(VTargetPixelRect.Right, FOversize.Right);
  Inc(VTargetPixelRect.Bottom, FOversize.Bottom);
  VProjectionTarget.ValidatePixelRect(VTargetPixelRect);
  VLonLatRectTarget := VProjectionTarget.PixelRect2LonLatRect(VTargetPixelRect);
  VLonLatRectSource := VLonLatRectTarget;
  VProjectionSource.ProjectionType.ValidateLonLatRect(VLonLatRectSource);
  VSourceTileRect := RectFromDoubleRect(VProjectionSource.LonLatRect2TileRectFloat(VLonLatRectSource), rrOutside);
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
    VTileIterator.Init(VSourceTileRect);
    while VTileIterator.Next(VSourceTile) do begin
      VSource := FProvider.GetTile(AOperationID, ACancelNotifier, VSourceTile);
      if Assigned(VSource) then begin
        TileToBuilder(VLonLatRectTarget, VSubsetBuilder, VSource);
      end;
    end;
    VSubsetBuilder.RemoveDuplicates;
    Result := VSubsetBuilder.MakeStaticAndClear;
  end;
end;

{ TVectorTileProviderBySameProjection }

constructor TVectorTileProviderBySameProjection.Create(
  const AVectorSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const AOversize: TRect;
  const AProvider: IVectorTileProvider;
  const AProjectionInfo: IProjection
);
begin
  inherited Create(AVectorSubsetBuilderFactory, AOversize, AProvider, AProjectionInfo);
  Assert(AProjectionInfo.ProjectionType.IsSame(AProvider.ProjectionInfo.ProjectionType));
end;

function TVectorTileProviderBySameProjection.GetTile(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ATile: TPoint
): IVectorItemSubset;
var
  VTile: TPoint;
  VTargetPixelRect: TRect;
  VProjectionSource: IProjection;
  VProjectionTarget: IProjection;
  VRelativeRect: TDoubleRect;
  VSourceTileRect: TRect;
  VTileIterator: TTileIteratorByRectRecord;
  VSourceTile: TPoint;
  VSource: IVectorItemSubset;
  VLonLatRectTarget: TDoubleRect;
  VSubsetBuilder: IVectorItemSubsetBuilder;
begin
  Result := nil;
  VTile := ATile;
  VProjectionTarget := FProjectionInfo;
  VProjectionSource := FProvider.ProjectionInfo;

  if not VProjectionTarget.CheckTilePosStrict(VTile) then begin
    Exit;
  end;
  VTargetPixelRect := VProjectionTarget.TilePos2PixelRect(VTile);
  Dec(VTargetPixelRect.Left, FOversize.Left);
  Dec(VTargetPixelRect.Top, FOversize.Top);
  Inc(VTargetPixelRect.Right, FOversize.Right);
  Inc(VTargetPixelRect.Bottom, FOversize.Bottom);
  VProjectionTarget.ValidatePixelRect(VTargetPixelRect);
  VLonLatRectTarget := VProjectionTarget.PixelRect2LonLatRect(VTargetPixelRect);
  VRelativeRect := VProjectionTarget.PixelRect2RelativeRect(VTargetPixelRect);
  VSourceTileRect := RectFromDoubleRect(VProjectionSource.RelativeRect2TileRectFloat(VRelativeRect), rrOutside);
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
    VTileIterator.Init(VSourceTileRect);
    while VTileIterator.Next(VSourceTile) do begin
      VSource := FProvider.GetTile(AOperationID, ACancelNotifier, VSourceTile);
      if Assigned(VSource) then begin
        TileToBuilder(VLonLatRectTarget, VSubsetBuilder, VSource);
      end;
    end;
    VSubsetBuilder.RemoveDuplicates;
    Result := VSubsetBuilder.MakeStaticAndClear;
  end;
end;

end.

