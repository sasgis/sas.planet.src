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
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

unit u_VectorTileProviderForVectorLayers;

interface

uses
  Types,
  t_GeoTypes,
  i_NotifierOperation,
  i_MapType,
  i_MapTypeSet,
  i_TileError,
  i_ProjectionInfo,
  i_VectorItemSubset,
  i_VectorItemSubsetBuilder,
  i_VectorTileProvider,
  u_BaseInterfacedObject;

type
  TVectorTileProviderForVectorLayers = class(TBaseInterfacedObject, IVectorTileUniProvider)
  private
    FLayersSet: IMapTypeSet;
    FSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
    FUseCache: Boolean;
    FErrorLogger: ITileErrorLogger;
    FOversize: TRect;

    procedure AddElementsFromMap(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const AElments: IVectorItemSubsetBuilder;
      const AAlayer: IMapType;
      const AZoom: Byte;
      const ALonLatRect: TDoubleRect
    );

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
      const ALayersSet: IMapTypeSet;
      AUseCache: Boolean;
      const AErrorLogger: ITileErrorLogger;
      const AOversize: TRect
    );
  end;

implementation

uses
  SysUtils,
  i_CoordConverter,
  i_TileIterator,
  i_VectorDataItemSimple,
  i_LonLatRect,
  i_MapVersionRequest,
  u_GeoFunc,
  u_TileIteratorByRect,
  u_TileErrorInfo,
  u_ResStrings;

{ TVectorTileProviderForVectorLayers }

constructor TVectorTileProviderForVectorLayers.Create(
  const ASubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const ALayersSet: IMapTypeSet;
  AUseCache: Boolean;
  const AErrorLogger: ITileErrorLogger;
  const AOversize: TRect
);
begin
  Assert(Assigned(ASubsetBuilderFactory));
  Assert(Assigned(ALayersSet));
  Assert(AOversize.Left >= 0);
  Assert(AOversize.Left < 4096);
  Assert(AOversize.Top >= 0);
  Assert(AOversize.Top < 4096);
  Assert(AOversize.Right >= 0);
  Assert(AOversize.Right < 4096);
  Assert(AOversize.Bottom >= 0);
  Assert(AOversize.Bottom < 4096);
  inherited Create;
  FSubsetBuilderFactory := ASubsetBuilderFactory;
  FLayersSet := ALayersSet;
  FUseCache := AUseCache;
  FErrorLogger := AErrorLogger;
  FOversize := AOversize;
end;

procedure TVectorTileProviderForVectorLayers.AddElementsFromMap(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const AElments: IVectorItemSubsetBuilder;
  const AAlayer: IMapType;
  const AZoom: Byte;
  const ALonLatRect: TDoubleRect
);
var
  VSourceGeoConvert: ICoordConverter;
  VTileSourceRect: TRect;
  VTileIterator: ITileIterator;
  VVersion: IMapVersionRequest;
  VTile: TPoint;
  VErrorString: string;
  VError: ITileErrorInfo;
  VItems: IVectorItemSubset;
  i: Integer;
  VItem: IVectorDataItem;
  VBounds: ILonLatRect;
begin
  VSourceGeoConvert := AAlayer.GeoConvert;
  VVersion := AAlayer.VersionRequestConfig.GetStatic;
  VTileSourceRect :=
    RectFromDoubleRect(
      VSourceGeoConvert.LonLatRect2TileRectFloat(ALonLatRect, AZoom),
      rrOutside
    );
  VTileIterator := TTileIteratorByRect.Create(VTileSourceRect);

  while VTileIterator.Next(VTile) do begin
    VErrorString := '';
    try
      VItems := AAlayer.LoadTileVector(VTile, AZoom, VVersion, False, AAlayer.CacheVector);
      if VItems <> nil then begin
        if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
          Break;
        end else begin
          for i := 0 to VItems.Count - 1 do begin
            VItem := VItems.GetItem(i);
            if Assigned(VItem) then begin
              VBounds := VItem.Geometry.Bounds;
              if Assigned(VBounds) and VBounds.IsIntersecWithRect(ALonLatRect) then begin
                AElments.Add(VItem);
              end;
            end;
          end;
        end;
      end;
    except
      on E: Exception do begin
        VErrorString := E.Message;
      end;
      else
        VErrorString := SAS_ERR_TileDownloadUnexpectedError;
    end;
    if VErrorString <> '' then begin
      VError :=
        TTileErrorInfo.Create(
          AAlayer.Zmp.GUID,
          AZoom,
          VTile,
          VErrorString
        );
      FErrorLogger.LogError(VError);
    end;
    VItems := nil;
  end;
end;

function TVectorTileProviderForVectorLayers.GetTile(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const AProjectionInfo: IProjectionInfo;
  const ATile: TPoint
): IVectorItemSubset;
var
  VElements: IVectorItemSubsetBuilder;
  i: Integer;
  VMapType: IMapType;
  VZoom: Byte;
  VConverter: ICoordConverter;
  VMapRect: TDoubleRect;
  VLonLatRect: TDoubleRect;
begin
  Result := nil;
  if FLayersSet <> nil then begin
    VZoom := AProjectionInfo.Zoom;
    VConverter := AProjectionInfo.GeoConverter;
    Assert(VConverter.CheckTilePosStrict(ATile, VZoom));
    VMapRect := VConverter.TilePos2PixelRectFloat(ATile, VZoom);

    VMapRect.Left := VMapRect.Left - FOversize.Left;
    VMapRect.Top := VMapRect.Top - FOversize.Top;
    VMapRect.Right := VMapRect.Right + FOversize.Right;
    VMapRect.Bottom := VMapRect.Bottom + FOversize.Bottom;

    VConverter.ValidatePixelRectFloat(VMapRect, VZoom);
    VLonLatRect := VConverter.PixelRectFloat2LonLatRect(VMapRect, VZoom);
    
    VElements := FSubsetBuilderFactory.Build;
    for i := 0 to FLayersSet.Count - 1 do begin
      VMapType := FLayersSet.Items[i];
      if VMapType.IsKmlTiles then begin
        AddElementsFromMap(
          AOperationID,
          ACancelNotifier,
          VElements,
          VMapType,
          VZoom,
          VLonLatRect
        );
        if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
          Break;
        end;
      end;
    end;
    VElements.RemoveDuplicates;
    Result := VElements.MakeStaticAndClear;
  end;
end;

end.
