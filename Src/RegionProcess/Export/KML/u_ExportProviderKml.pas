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

unit u_ExportProviderKml;

interface

uses
  Forms,
  i_LanguageManager,
  i_TileIteratorFactory,
  i_TileStorageTypeList,
  i_TileFileNameGeneratorsList,
  i_RegionProcessProgressInfoInternalFactory,
  i_GeometryLonLat,
  i_RegionProcessTask,
  i_RegionProcessProgressInfo,
  u_ExportProviderAbstract,
  fr_MapSelect,
  fr_ExportKml;

type
  TExportProviderKml = class(TExportProviderBase)
  private
    FTileStorageTypeList: ITileStorageTypeListStatic;
    FTileNameGenerator: ITileFileNameGeneratorsList;
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
      const ATileIteratorFactory: ITileIteratorFactory;
      const ATileStorageTypeList: ITileStorageTypeListStatic;
      const ATileNameGenerator: ITileFileNameGeneratorsList
    );
  end;


implementation

uses
  Types,
  Classes,
  SysUtils,
  i_MapType,
  i_RegionProcessParamsFrame,
  u_ExportTaskToKML,
  u_ResStrings;

{ TExportProviderKml }

constructor TExportProviderKml.Create(
  const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
  const ALanguageManager: ILanguageManager;
  const AMapSelectFrameBuilder: IMapSelectFrameBuilder;
  const ATileIteratorFactory: ITileIteratorFactory;
  const ATileStorageTypeList: ITileStorageTypeListStatic;
  const ATileNameGenerator: ITileFileNameGeneratorsList
);
begin
  inherited Create(
    AProgressFactory,
    ALanguageManager,
    AMapSelectFrameBuilder,
    ATileIteratorFactory
  );
  FTileStorageTypeList := ATileStorageTypeList;
  FTileNameGenerator := ATileNameGenerator;
end;

function TExportProviderKml.CreateFrame: TFrame;
begin
  Result :=
    TfrExportKml.Create(
      Self.LanguageManager,
      Self.MapSelectFrameBuilder,
      FTileStorageTypeList,
      FTileNameGenerator
    );
  Assert(Supports(Result, IRegionProcessParamsFrameZoomArray));
  Assert(Supports(Result, IRegionProcessParamsFrameOneMap));
  Assert(Supports(Result, IRegionProcessParamsFrameTargetPath));
  Assert(Supports(Result, IRegionProcessParamsFrameKmlExport));
end;

function TExportProviderKml.GetCaption: string;
begin
  Result := SAS_STR_ExportGEKmlExportCaption;
end;

function TExportProviderKml.PrepareTask(
  const APolygon: IGeometryLonLatPolygon;
  const AProgressInfo: IRegionProcessProgressInfoInternal
): IRegionProcessTask;
var
  VPath: string;
  VZoomArr: TByteDynArray;
  VMapType: IMapType;
  NotSaveNotExists: boolean;
  RelativePath: Boolean;
begin
  inherited;
  VZoomArr := (ParamsFrame as IRegionProcessParamsFrameZoomArray).ZoomArray;
  VPath := (ParamsFrame as IRegionProcessParamsFrameTargetPath).Path;
  VMapType := (ParamsFrame as IRegionProcessParamsFrameOneMap).MapType;
  RelativePath := (ParamsFrame as IRegionProcessParamsFrameKmlExport).RelativePath;
  NotSaveNotExists := (ParamsFrame as IRegionProcessParamsFrameKmlExport).NotSaveNotExists;

  Result :=
    TExportTaskToKML.Create(
      AProgressInfo,
      VPath,
      Self.TileIteratorFactory,
      APolygon,
      VZoomArr,
      VMapType.TileStorage,
      VMapType.VersionRequest.GetStatic.BaseVersion,
      NotSaveNotExists,
      RelativePath,
      (ParamsFrame as IRegionProcessParamsFrameKmlExport).ExtractTilesFromStorage,
      (ParamsFrame as IRegionProcessParamsFrameKmlExport).TileFileNameGenerator
    );
end;

end.
