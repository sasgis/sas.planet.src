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

unit u_ExportProviderCE;

interface

uses
  Classes,
  Forms,
  i_GeometryLonLat,
  i_TileIteratorFactory,
  i_LanguageManager,
  i_RegionProcessTask,
  i_RegionProcessProgressInfo,
  i_RegionProcessProgressInfoInternalFactory,
  fr_MapSelect,
  u_ExportProviderAbstract;

type
  TExportProviderCE = class(TExportProviderBase)
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
  SysUtils,
  i_MapType,
  i_RegionProcessParamsFrame,
  u_ThreadExportToCE,
  u_ResStrings,
  fr_ExportToCE;

{ TExportProviderCE }

function TExportProviderCE.CreateFrame: TFrame;
begin
  Result :=
    TfrExportToCE.Create(
      Self.LanguageManager,
      Self.MapSelectFrameBuilder,
      'd00 |*.d00',
      'd00'
    );
  Assert(Supports(Result, IRegionProcessParamsFrameZoomArray));
  Assert(Supports(Result, IRegionProcessParamsFrameOneMap));
  Assert(Supports(Result, IRegionProcessParamsFrameExportToCE));
  Assert(Supports(Result, IRegionProcessParamsFrameTargetPath));
end;

function TExportProviderCE.GetCaption: string;
begin
  Result := SAS_STR_ExportCEPackCaption;
end;

function TExportProviderCE.PrepareTask(
  const APolygon: IGeometryLonLatPolygon;
  const AProgressInfo: IRegionProcessProgressInfoInternal
): IRegionProcessTask;
var
  VPath: string;
  Zoomarr: TByteDynArray;
  VMapType: IMapType;
  VMaxSize: integer;
  VComent: string;
  VRecoverInfo: boolean;
begin
  Zoomarr := (ParamsFrame as IRegionProcessParamsFrameZoomArray).ZoomArray;
  VMapType := (ParamsFrame as IRegionProcessParamsFrameOneMap).MapType;
  VPath := (ParamsFrame as IRegionProcessParamsFrameTargetPath).Path;
  VMaxSize := (ParamsFrame as IRegionProcessParamsFrameExportToCE).MaxSize;
  VComent := (ParamsFrame as IRegionProcessParamsFrameExportToCE).Coment;
  VRecoverInfo := (ParamsFrame as IRegionProcessParamsFrameExportToCE).IsAddRecoverInfo;

  Result :=
    TExportTaskToCE.Create(
      AProgressInfo,
      Self.TileIteratorFactory,
      VPath,
      APolygon,
      Zoomarr,
      VMapType.TileStorage,
      VMapType.VersionRequest.GetStatic,
      VMaxSize,
      VComent,
      VRecoverInfo
    );
end;

end.
