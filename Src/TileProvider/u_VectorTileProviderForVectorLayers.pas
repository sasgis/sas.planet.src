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
    FTileSelectOversize: TRect;
    FItemSelectOversize: TRect;

    procedure AddElementsFromMap(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const AElements: IVectorItemSubsetBuilder;
      const AAlayer: IMapType;
      const AZoom: Byte;
      const ATileSelectLonLatRect: TDoubleRect;
      const AItemSelectLonLatRect: TDoubleRect
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
      const ATileSelectOversize: TRect;
      const AItemSelectOversize: TRect
    );
  end;

implementation

uses
  SysUtils,
  Math,
  i_CoordConverter,
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
  const ATileSelectOversize: TRect;
  const AItemSelectOversize: TRect
);
begin
  Assert(Assigned(ASubsetBuilderFactory));
  Assert(Assigned(ALayersSet));
  Assert(ATileSelectOversize.Left >= 0);
  Assert(ATileSelectOversize.Left < 4096);
  Assert(ATileSelectOversize.Top >= 0);
  Assert(ATileSelectOversize.Top < 4096);
  Assert(ATileSelectOversize.Right >= 0);
  Assert(ATileSelectOversize.Right < 4096);
  Assert(ATileSelectOversize.Bottom >= 0);
  Assert(ATileSelectOversize.Bottom < 4096);
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
  FLayersSet := ALayersSet;
  FUseCache := AUseCache;
  FErrorLogger := AErrorLogger;
  FTileSelectOversize := ATileSelectOversize;
  FItemSelectOversize := AItemSelectOversize;
end;

procedure TVectorTileProviderForVectorLayers.AddElementsFromMap(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const AElements: IVectorItemSubsetBuilder;
  const AAlayer: IMapType;
  const AZoom: Byte;
  const ATileSelectLonLatRect: TDoubleRect;
  const AItemSelectLonLatRect: TDoubleRect
);
var
  VSourceGeoConvert: ICoordConverter;
  VTileSelectLonLatRect: TDoubleRect;
  VTileSourceRect: TRect;
  VTileIterator: TTileIteratorByRectRecord;
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
  VTileSelectLonLatRect := ATileSelectLonLatRect;
  VSourceGeoConvert.ValidateLonLatRect(VTileSelectLonLatRect);
  VTileSourceRect :=
    RectFromDoubleRect(
      VSourceGeoConvert.LonLatRect2TileRectFloat(VTileSelectLonLatRect, AZoom),
      rrOutside
    );
  VTileIterator.Init(VTileSourceRect);

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
              if Assigned(VBounds) and VBounds.IsIntersecWithRect(AItemSelectLonLatRect) then begin
                AElements.Add(VItem);
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
  VTileSelectPixelRect: TDoubleRect;
  VItemSelectPixelRect: TDoubleRect;
  VTileSelectLonLatRect: TDoubleRect;
  VItemSelectLonLatRect: TDoubleRect;
begin
  Result := nil;
  if FLayersSet <> nil then begin
    Assert(AProjectionInfo.CheckTilePosStrict(ATile));
    VTileSelectPixelRect := AProjectionInfo.TilePos2PixelRectFloat(ATile);
    VItemSelectPixelRect := VTileSelectPixelRect;

    VTileSelectPixelRect.Left := VTileSelectPixelRect.Left - FTileSelectOversize.Left;
    VTileSelectPixelRect.Top := VTileSelectPixelRect.Top - FTileSelectOversize.Top;
    VTileSelectPixelRect.Right := VTileSelectPixelRect.Right + FTileSelectOversize.Right;
    VTileSelectPixelRect.Bottom := VTileSelectPixelRect.Bottom + FTileSelectOversize.Bottom;

    AProjectionInfo.ValidatePixelRectFloat(VTileSelectPixelRect);
    VTileSelectLonLatRect := AProjectionInfo.PixelRectFloat2LonLatRect(VTileSelectPixelRect);

    VItemSelectPixelRect.Left := VItemSelectPixelRect.Left - FItemSelectOversize.Left;
    VItemSelectPixelRect.Top := VItemSelectPixelRect.Top - FItemSelectOversize.Top;
    VItemSelectPixelRect.Right := VItemSelectPixelRect.Right + FItemSelectOversize.Right;
    VItemSelectPixelRect.Bottom := VItemSelectPixelRect.Bottom + FItemSelectOversize.Bottom;

    AProjectionInfo.ValidatePixelRectFloat(VItemSelectPixelRect);
    VItemSelectLonLatRect := AProjectionInfo.PixelRectFloat2LonLatRect(VItemSelectPixelRect);
    VZoom := AProjectionInfo.Zoom;
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
          VTileSelectLonLatRect,
          VItemSelectLonLatRect
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
