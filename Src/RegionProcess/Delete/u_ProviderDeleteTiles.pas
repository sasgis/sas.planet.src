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

unit u_ProviderDeleteTiles;

interface

uses
  Windows,
  Forms,
  i_LanguageManager,
  i_GeometryProjectedFactory,
  i_GeometryLonLat,
  i_RegionProcessTask,
  i_RegionProcessProgressInfo,
  i_RegionProcessProgressInfoInternalFactory,
  fr_MapSelect,
  u_ExportProviderAbstract;

type
  TProviderDeleteTiles = class(TExportProviderBase)
  private
    FVectorGeometryProjectedFactory: IGeometryProjectedFactory;
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
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory
    );
  end;

implementation

uses
  Classes,
  SysUtils,
  gnugettext,
  i_MapType,
  i_RegionProcessParamsFrame,
  i_PredicateByTileInfo,
  i_Projection,
  i_GeometryProjected,
  u_ThreadDeleteTiles,
  u_ResStrings,
  fr_DeleteTiles;

{ TProviderTilesDelete }

constructor TProviderDeleteTiles.Create(
  const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
  const ALanguageManager: ILanguageManager;
  const AMapSelectFrameBuilder: IMapSelectFrameBuilder;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory
);
begin
  inherited Create(
    AProgressFactory,
    ALanguageManager,
    AMapSelectFrameBuilder
  );
  FVectorGeometryProjectedFactory := AVectorGeometryProjectedFactory;
end;

function TProviderDeleteTiles.CreateFrame: TFrame;
begin
  Result :=
    TfrDeleteTiles.Create(
      Self.LanguageManager,
      Self.MapSelectFrameBuilder
    );
  Assert(Supports(Result, IRegionProcessParamsFrameOneMap));
  Assert(Supports(Result, IRegionProcessParamsFrameOneZoom));
  Assert(Supports(Result, IRegionProcessParamsFrameProcessPredicate));
end;

function TProviderDeleteTiles.GetCaption: string;
begin
  Result := _('Tiles');
end;

function TProviderDeleteTiles.PrepareTask(
  const APolygon: IGeometryLonLatPolygon;
  const AProgressInfo: IRegionProcessProgressInfoInternal
): IRegionProcessTask;
var
  VMapType: IMapType;
  VZoom: byte;
  VProjection: IProjection;
  VProjectedPolygon: IGeometryProjectedPolygon;
  VPredicate: IPredicateByTileInfo;
begin
  inherited;
  if (Application.MessageBox(pchar(SAS_MSG_DeleteTilesInRegionAsk), pchar(SAS_MSG_coution), 36) <> IDYES) then begin
    Exit;
  end;

  VMapType := (ParamsFrame as IRegionProcessParamsFrameOneMap).MapType;
  VZoom := (ParamsFrame as IRegionProcessParamsFrameOneZoom).Zoom;
  VPredicate := (ParamsFrame as IRegionProcessParamsFrameProcessPredicate).Predicate;
  VProjection := VMapType.ProjectionSet[VZoom];
  VProjectedPolygon :=
    FVectorGeometryProjectedFactory.CreateProjectedPolygonByLonLatPolygon(
      VProjection,
      APolygon
    );

  Result :=
    TThreadDeleteTiles.Create(
      AProgressInfo,
      APolygon,
      VProjectedPolygon,
      VProjection,
      VMapType.TileStorage,
      VMapType.VersionRequest.GetStatic,
      VPredicate
    );
end;

end.
