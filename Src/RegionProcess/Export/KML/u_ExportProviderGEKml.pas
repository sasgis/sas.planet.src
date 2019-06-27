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

unit u_ExportProviderGEKml;

interface

uses
  Forms,
  i_GeometryLonLat,
  i_RegionProcessTask,
  i_RegionProcessProgressInfo,
  u_ExportProviderAbstract,
  fr_ExportGEKml;

type
  TExportProviderKml = class(TExportProviderBase)
  protected
    function CreateFrame: TFrame; override;
  protected
    function GetCaption: string; override;
    function PrepareTask(
      const APolygon: IGeometryLonLatPolygon;
      const AProgressInfo: IRegionProcessProgressInfoInternal
    ): IRegionProcessTask; override;
  end;


implementation

uses
  Types,
  Classes,
  SysUtils,
  i_MapType,
  i_RegionProcessParamsFrame,
  u_ThreadExportKML,
  u_ResStrings;

{ TExportProviderKml }

function TExportProviderKml.CreateFrame: TFrame;
begin
  Result :=
    TfrExportKml.Create(
      Self.LanguageManager,
      Self.MapSelectFrameBuilder
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
      RelativePath
    );
end;

end.
