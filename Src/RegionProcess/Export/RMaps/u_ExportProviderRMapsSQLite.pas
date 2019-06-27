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

unit u_ExportProviderRMapsSQLite;

interface

uses
  Forms,
  i_GeometryLonLat,
  i_TileIteratorFactory,
  i_LanguageManager,
  i_RegionProcessTask,
  i_RegionProcessProgressInfo,
  i_RegionProcessProgressInfoInternalFactory,
  i_ProjectionSetFactory,
  i_Bitmap32BufferFactory,
  i_BitmapTileSaveLoadFactory,
  i_MapTypeListChangeable,
  u_ExportProviderAbstract,
  fr_MapSelect,
  fr_ExportRMapsSQLite;

type
  TExportProviderRMapsSQLite = class(TExportProviderBase)
  private
    FActiveMapsList: IMapTypeListChangeable;
    FBitmap32StaticFactory: IBitmap32StaticFactory;
    FBitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
    FProjectionSetFactory: IProjectionSetFactory;
  protected
    function CreateFrame: TFrame; override;
  protected
    function GetCaption: string; override;
    function PrepareTask(
      const APolygon: IGeometryLonLatPolygon;
      const AProgressInfo: IRegionProcessProgressInfoInternal
    ): IRegionProcessTask; override;
  public
    constructor Create(
      const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
      const ALanguageManager: ILanguageManager;
      const AMapSelectFrameBuilder: IMapSelectFrameBuilder;
      const AActiveMapsList: IMapTypeListChangeable;
      const ATileIteratorFactory: ITileIteratorFactory;
      const ABitmap32StaticFactory: IBitmap32StaticFactory;
      const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
      const AProjectionSetFactory: IProjectionSetFactory
    );
  end;


implementation

uses
  Types,
  Classes,
  SysUtils,
  i_BitmapTileSaveLoad,
  i_BitmapLayerProvider,
  i_RegionProcessParamsFrame,
  i_TileStorage,
  i_MapVersionRequest,
  i_MapType,
  u_ThreadExportToRMapsSQLite,
  u_ResStrings;

{ TExportProviderRMapsSQLite }

constructor TExportProviderRMapsSQLite.Create(
  const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
  const ALanguageManager: ILanguageManager;
  const AMapSelectFrameBuilder: IMapSelectFrameBuilder;
  const AActiveMapsList: IMapTypeListChangeable;
  const ATileIteratorFactory: ITileIteratorFactory;
  const ABitmap32StaticFactory: IBitmap32StaticFactory;
  const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
  const AProjectionSetFactory: IProjectionSetFactory
);
begin
  Assert(Assigned(ABitmap32StaticFactory));
  inherited Create(
    AProgressFactory,
    ALanguageManager,
    AMapSelectFrameBuilder,
    ATileIteratorFactory
  );
  FActiveMapsList := AActiveMapsList;
  FBitmap32StaticFactory := ABitmap32StaticFactory;
  FBitmapTileSaveLoadFactory := ABitmapTileSaveLoadFactory;
  FProjectionSetFactory := AProjectionSetFactory;
end;

function TExportProviderRMapsSQLite.CreateFrame: TFrame;
begin
  Result :=
    TfrExportRMapsSQLite.Create(
      Self.LanguageManager,
      Self.MapSelectFrameBuilder,
      FActiveMapsList,
      FBitmap32StaticFactory,
      FBitmapTileSaveLoadFactory
    );

  Assert(Supports(Result, IRegionProcessParamsFrameZoomArray));
  Assert(Supports(Result, IRegionProcessParamsFrameTargetPath));
  Assert(Supports(Result, IRegionProcessParamsFrameOneMap));
  Assert(Supports(Result, IRegionProcessParamsFrameImageProvider));
  Assert(Supports(Result, IRegionProcessParamsFrameRMapsSQLiteExport));
end;

function TExportProviderRMapsSQLite.GetCaption: string;
begin
  Result := SAS_STR_ExportRMapsSQLiteExportCaption;
end;

function TExportProviderRMapsSQLite.PrepareTask(
  const APolygon: IGeometryLonLatPolygon;
  const AProgressInfo: IRegionProcessProgressInfoInternal
): IRegionProcessTask;
var
  VPath: string;
  VZoomArr: TByteDynArray;
  VForceDropTarget: Boolean;
  VReplaceExistingTiles: Boolean;
  VDirectTilesCopy: Boolean;
  VBitmapTileSaver: IBitmapTileSaver;
  VBitmapProvider: IBitmapTileUniProvider;
  VMapType: IMapType;
  VMapVersion: IMapVersionRequest;
  VTileStorage: ITileStorage;
begin
  inherited;

  VZoomArr := (ParamsFrame as IRegionProcessParamsFrameZoomArray).ZoomArray;
  VPath := (ParamsFrame as IRegionProcessParamsFrameTargetPath).Path;
  VMapType := (ParamsFrame as IRegionProcessParamsFrameOneMap).MapType;
  VBitmapProvider := (ParamsFrame as IRegionProcessParamsFrameImageProvider).Provider;

  VTileStorage := nil;
  VMapVersion := nil;
  if Assigned(VMapType) then begin
    VMapVersion := VMapType.VersionRequest.GetStatic;
    VTileStorage := VMapType.TileStorage;
  end;

  with (ParamsFrame as IRegionProcessParamsFrameRMapsSQLiteExport) do begin
    VForceDropTarget := ForceDropTarget;
    VReplaceExistingTiles := ReplaceExistingTiles;
    VDirectTilesCopy := DirectTilesCopy;
    VBitmapTileSaver := BitmapTileSaver;
  end;

  Result :=
    TExportTaskToRMapsSQLite.Create(
      AProgressInfo,
      VPath,
      Self.TileIteratorFactory,
      FProjectionSetFactory,
      APolygon,
      VZoomArr,
      VTileStorage,
      VMapVersion,
      VBitmapTileSaver,
      VBitmapProvider,
      VForceDropTarget,
      VReplaceExistingTiles,
      VDirectTilesCopy
    );
end;

end.
