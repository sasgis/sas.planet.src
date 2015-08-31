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

unit u_VectorTileProviderByFixedSubset;

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
  TVectorTileProviderByFixedSubset = class(TBaseInterfacedObject, IVectorTileUniProvider)
  private
    FSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
    FItemSelectOversize: TRect;
    FSource: IVectorItemSubset;
  private
    function GetTile(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const AProjectionInfo: IProjectionInfo;
      const ATile: TPoint
    ): IVectorItemSubset;
  public
    constructor Create(
      const ASubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
      const AItemSelectOversize: TRect;
      const ASource: IVectorItemSubset
    );
  end;

implementation

uses
  t_GeoTypes,
  i_LonLatRect,
  i_VectorDataItemSimple;

{ TVectorTileProviderByFixedSubset }

constructor TVectorTileProviderByFixedSubset.Create(
  const ASubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const AItemSelectOversize: TRect;
  const ASource: IVectorItemSubset
);
begin
  Assert(Assigned(ASubsetBuilderFactory));
  Assert(Assigned(ASource));
  Assert(AItemSelectOversize.Left >= 0);
  Assert(AItemSelectOversize.Left < 4096);
  Assert(AItemSelectOversize.Top >= 0);
  Assert(AItemSelectOversize.Top < 4096);
  Assert(AItemSelectOversize.Right >= 0);
  Assert(AItemSelectOversize.Right < 4096);
  Assert(AItemSelectOversize.Bottom >= 0);
  Assert(AItemSelectOversize.Bottom < 4096);
  inherited Create;
  FSubsetBuilderFactory := ASubsetBuilderFactory;
  FItemSelectOversize := AItemSelectOversize;
  FSource := ASource;
end;

function TVectorTileProviderByFixedSubset.GetTile(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const AProjectionInfo: IProjectionInfo;
  const ATile: TPoint
): IVectorItemSubset;
var
  VItemSelectPixelRect: TDoubleRect;
  VItemSelectLonLatRect: TDoubleRect;
  VElements: IVectorItemSubsetBuilder;
  i: Integer;
  VItem: IVectorDataItem;
  VBounds: ILonLatRect;
begin
  Assert(Assigned(AProjectionInfo));
  Assert(AProjectionInfo.CheckTilePosStrict(ATile));
  VItemSelectPixelRect := AProjectionInfo.TilePos2PixelRectFloat(ATile);
  VItemSelectPixelRect.Left := VItemSelectPixelRect.Left - FItemSelectOversize.Left;
  VItemSelectPixelRect.Top := VItemSelectPixelRect.Top - FItemSelectOversize.Top;
  VItemSelectPixelRect.Right := VItemSelectPixelRect.Right + FItemSelectOversize.Right;
  VItemSelectPixelRect.Bottom := VItemSelectPixelRect.Bottom + FItemSelectOversize.Bottom;

  AProjectionInfo.ValidatePixelRectFloat(VItemSelectPixelRect);
  VItemSelectLonLatRect := AProjectionInfo.PixelRectFloat2LonLatRect(VItemSelectPixelRect);
  VElements := FSubsetBuilderFactory.Build;
  for i := 0 to FSource.Count - 1 do begin
    VItem := FSource.Items[i];
    if Assigned(VItem) then begin
      VBounds := VItem.Geometry.Bounds;
      if Assigned(VBounds) and VBounds.IsIntersecWithRect(VItemSelectLonLatRect) then begin
        VElements.Add(VItem);
      end;
    end;
  end;

  Result := VElements.MakeStaticAndClear;
end;

end.
