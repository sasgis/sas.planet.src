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
  i_GeometryProjectedFactory,
  i_LanguageManager,
  i_MapTypeSet,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  i_RegionProcessProgressInfoInternalFactory,
  i_CoordConverterFactory,
  i_LocalCoordConverterFactorySimpe,
  i_Bitmap32StaticFactory,
  i_BitmapTileSaveLoadFactory,
  u_ExportProviderAbstract,
  fr_ExportRMapsSQLite;

type
  TExportProviderRMapsSQLite = class(TExportProviderAbstract)
  private
    FProjectionFactory: IProjectionInfoFactory;
    FVectorGeometryProjectedFactory: IGeometryProjectedFactory;
    FBitmapFactory: IBitmap32StaticFactory;
    FBitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
    FCoordConverterFactory: ICoordConverterFactory;
    FLocalConverterFactory: ILocalCoordConverterFactorySimpe;
  protected
    function CreateFrame: TFrame; override;
  public
    constructor Create(
      const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
      const ALanguageManager: ILanguageManager;
      const AMainMapsConfig: IMainMapsConfig;
      const AFullMapsSet: IMapTypeSet;
      const AGUIConfigList: IMapTypeGUIConfigList;
      const AProjectionFactory: IProjectionInfoFactory;
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
      const ABitmapFactory: IBitmap32StaticFactory;
      const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
      const ACoordConverterFactory: ICoordConverterFactory;
      const ALocalConverterFactory: ILocalCoordConverterFactorySimpe
    );
    function GetCaption: string; override;
    procedure StartProcess(const APolygon: IGeometryLonLatMultiPolygon); override;
  end;


implementation

uses
  Types,
  Classes,
  SysUtils,
  i_BitmapTileSaveLoad,
  i_BitmapLayerProvider,
  i_RegionProcessParamsFrame,
  i_RegionProcessProgressInfo,
  i_TileStorage,
  i_MapVersionRequest,
  i_MapTypes,
  u_ThreadExportToRMapsSQLite,
  u_ResStrings;

{ TExportProviderRMapsSQLite }

constructor TExportProviderRMapsSQLite.Create(
  const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
  const ALanguageManager: ILanguageManager;
  const AMainMapsConfig: IMainMapsConfig;
  const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList;
  const AProjectionFactory: IProjectionInfoFactory;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  const ABitmapFactory: IBitmap32StaticFactory;
  const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
  const ACoordConverterFactory: ICoordConverterFactory;
  const ALocalConverterFactory: ILocalCoordConverterFactorySimpe
);
begin
  inherited Create(
    AProgressFactory,
    ALanguageManager,
    AMainMapsConfig,
    AFullMapsSet,
    AGUIConfigList
  );
  FProjectionFactory := AProjectionFactory;
  FVectorGeometryProjectedFactory := AVectorGeometryProjectedFactory;
  FBitmapFactory := ABitmapFactory;
  FBitmapTileSaveLoadFactory := ABitmapTileSaveLoadFactory;
  FCoordConverterFactory := ACoordConverterFactory;
  FLocalConverterFactory := ALocalConverterFactory;
end;

function TExportProviderRMapsSQLite.CreateFrame: TFrame;
begin
  Result :=
    TfrExportRMapsSQLite.Create(
      Self.LanguageManager,
      Self.MainMapsConfig,
      Self.FullMapsSet,
      Self.GUIConfigList,
      FBitmapFactory,
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

procedure TExportProviderRMapsSQLite.StartProcess(const APolygon: IGeometryLonLatMultiPolygon);
var
  VPath: string;
  VZoomArr: TByteDynArray;
  VForceDropTarget: Boolean;
  VReplaceExistingTiles: Boolean;
  VDirectTilesCopy: Boolean;
  VBitmapTileSaver: IBitmapTileSaver;
  VBitmapProvider: IBitmapLayerProvider;
  VMapType: IMapType;
  VProgressInfo: IRegionProcessProgressInfoInternal;
  VThread: TThread;
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
    VMapVersion := VMapType.VersionRequestConfig.GetStatic;
    VTileStorage := VMapType.TileStorage;
  end;

  with (ParamsFrame as IRegionProcessParamsFrameRMapsSQLiteExport) do begin
    VForceDropTarget := ForceDropTarget;
    VReplaceExistingTiles := ReplaceExistingTiles;
    VDirectTilesCopy := DirectTilesCopy;
    VBitmapTileSaver := BitmapTileSaver;
  end;

  VProgressInfo := ProgressFactory.Build(APolygon);

  VThread :=
    TThreadExportToRMapsSQLite.Create(
      VProgressInfo,
      VPath,
      FProjectionFactory,
      FVectorGeometryProjectedFactory,
      FCoordConverterFactory,
      FLocalConverterFactory,
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
  VThread.Resume;
end;

end.


