{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
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

unit u_GeoCoderByVectorLayers;

interface

uses
  i_InterfaceListSimple,
  i_GeoCoder,
  i_NotifierOperation,
  i_VectorDataItemSimple,
  i_VectorItemSubsetBuilder,
  i_LocalCoordConverter,
  i_MapTypeSetChangeable,
  u_GeoCoderLocalBasic;

type
  TGeoCoderByVectorLayers = class(TGeoCoderLocalBasic)
  private
    FKmlLayersSet: IMapTypeSetChangeable;
    FSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  protected
    function DoSearch(
      const ACancelNotifier: INotifierOperation;
      AOperationID: Integer;
      const ASearch: string;
      const ALocalConverter: ILocalCoordConverter
    ): IInterfaceListSimple; override;
  public
    constructor Create(
      const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
      const APlacemarkFactory: IGeoCodePlacemarkFactory;
      const AKmlLayersSet: IMapTypeSetChangeable
    );
  end;

implementation

uses
  Types,
  SysUtils,
  StrUtils,
  Math,
  t_GeoTypes,
  i_MapTypeSet,
  i_Projection,
  i_TileError,
  i_VectorItemSubset,
  i_VectorTileProvider,
  u_GeoFunc,
  u_InterfaceListSimple,
  u_TileErrorLogProviedrStuped,
  u_TileIteratorByRect,
  u_VectorTileProviderForVectorLayers;

function IsItemMatchText(const AItem: IVectorDataItem; const ASearchText: string): Boolean; inline;
begin
  Result :=
    (AItem <> nil) and (ASearchText <> '') and
    (AnsiContainsText(AItem.Name, ASearchText) or AnsiContainsText(AItem.Desc, ASearchText));
end;

{ TGeoCoderByVectorLayers }

constructor TGeoCoderByVectorLayers.Create(
  const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const APlacemarkFactory: IGeoCodePlacemarkFactory;
  const AKmlLayersSet: IMapTypeSetChangeable
);
begin
  Assert(Assigned(AKmlLayersSet));
  inherited Create(AVectorItemSubsetBuilderFactory, APlacemarkFactory);
  FKmlLayersSet := AKmlLayersSet;
  FSubsetBuilderFactory := AVectorItemSubsetBuilderFactory;
end;

function TGeoCoderByVectorLayers.DoSearch(
  const ACancelNotifier: INotifierOperation;
  AOperationID: Integer;
  const ASearch: string;
  const ALocalConverter: ILocalCoordConverter
): IInterfaceListSimple;
var
  I: Integer;
  VKmlLayers: IMapTypeSet;
  VProvider: IVectorTileUniProvider;
  VProjection: IProjection;
  VMapRect, VLonLatRect: TDoubleRect;
  VTileRect: TRect;
  VTileIterator: TTileIteratorByRectRecord;
  VTile: TPoint;
  VFound: IVectorItemSubsetBuilder;
  VItems: IVectorItemSubset;
  VItem: IVectorDataItem;
  VSubset: IVectorItemSubset;
  VList: IInterfaceListSimple;
begin
  Result := nil;
  if ASearch = '' then begin
    Exit;
  end;

  // snapshot of the currently active vector layers
  VKmlLayers := FKmlLayersSet.GetStatic;
  if (VKmlLayers = nil) or (VKmlLayers.Count = 0) then begin
    Exit;
  end;

  VProjection := ALocalConverter.Projection;

  // visible area of the view at the current zoom
  VMapRect := ALocalConverter.LocalRect2MapRectFloat(ALocalConverter.GetLocalRect);
  VProjection.ValidatePixelRectFloat(VMapRect);
  VLonLatRect := VProjection.PixelRectFloat2LonLatRect(VMapRect);
  VProjection.ProjectionType.ValidateLonLatRect(VLonLatRect);
  VTileRect := RectFromDoubleRect(VProjection.LonLatRect2TileRectFloat(VLonLatRect), rrOutside);

  // read tiles from the cache only (no network)
  VProvider :=
    TVectorTileProviderForVectorLayers.Create(
      FSubsetBuilderFactory,
      VKmlLayers,
      True, // AUsePre - allow tiles from the prev zoom, same as the layer draws
      True, // AUseCache
      TTileErrorLogProviedrStuped.Create as ITileErrorLogger,
      Rect(0, 0, 0, 0), // ATileSelectOversize - no oversize: visible area only
      Rect(0, 0, 0, 0)  // AItemSelectOversize
    );

  VFound := FSubsetBuilderFactory.Build;

  VTileIterator.Init(VTileRect);
  while VTileIterator.Next(VTile) do begin
    if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
      Exit;
    end;
    VItems := VProvider.GetTile(AOperationID, ACancelNotifier, VProjection, VTile);
    if VItems <> nil then begin
      for I := 0 to VItems.Count - 1 do begin
        VItem := VItems.GetItem(I);
        if IsItemMatchText(VItem, ASearch) then begin
          VFound.Add(VItem);
        end;
      end;
    end;
  end;

  // an item intersecting several tiles is read more than once
  VFound.RemoveDuplicates;

  VSubset := VFound.MakeStaticAndClear;
  if (VSubset <> nil) and (VSubset.Count > 0) then begin
    VList := TInterfaceListSimple.Create;
    for I := 0 to VSubset.Count - 1 do begin
      VList.Add(VSubset.GetItem(I));
    end;
    Result := VList;
  end;
end;

end.
