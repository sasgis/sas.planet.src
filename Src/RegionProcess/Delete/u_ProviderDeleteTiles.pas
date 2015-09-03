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
  i_CoordConverterFactory,
  i_GeometryProjectedFactory,
  i_GeometryLonLat,
  i_RegionProcessProgressInfoInternalFactory,
  fr_MapSelect,
  u_ExportProviderAbstract;

type
  TProviderDeleteTiles = class(TExportProviderAbstract)
  private
    FProjectionFactory: IProjectionInfoFactory;
    FVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  protected
    function CreateFrame: TFrame; override;
  protected
    function GetCaption: string; override;
    procedure StartProcess(const APolygon: IGeometryLonLatPolygon); override;
  public
    constructor Create(
      const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
      const ALanguageManager: ILanguageManager;
      const AMapSelectFrameBuilder: IMapSelectFrameBuilder;
      const AProjectionFactory: IProjectionInfoFactory;
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory
    );
  end;

implementation

uses
  Classes,
  SysUtils,
  i_MapType,
  i_RegionProcessParamsFrame,
  i_RegionProcessProgressInfo,
  i_PredicateByTileInfo,
  i_ProjectionInfo,
  i_GeometryProjected,
  u_ThreadDeleteTiles,
  u_ResStrings,
  fr_DeleteTiles;

{ TProviderTilesDelete }

constructor TProviderDeleteTiles.Create(
  const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
  const ALanguageManager: ILanguageManager;
  const AMapSelectFrameBuilder: IMapSelectFrameBuilder;
  const AProjectionFactory: IProjectionInfoFactory;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory
);
begin
  inherited Create(
    AProgressFactory,
    ALanguageManager,
    AMapSelectFrameBuilder
  );
  FProjectionFactory := AProjectionFactory;
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
  Result := SAS_STR_OperationDeleteCaption;
end;

procedure TProviderDeleteTiles.StartProcess(const APolygon: IGeometryLonLatPolygon);
var
  VMapType: IMapType;
  VZoom: byte;
  VProjection: IProjectionInfo;
  VProjectedPolygon: IGeometryProjectedPolygon;
  VProgressInfo: IRegionProcessProgressInfoInternal;
  VPredicate: IPredicateByTileInfo;
  VThread: TThread;
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
  VProgressInfo := ProgressFactory.Build(APolygon);
  VThread :=
    TThreadDeleteTiles.Create(
      VProgressInfo,
      APolygon,
      VProjectedPolygon,
      VProjection,
      VMapType.TileStorage,
      VMapType.VersionRequestConfig.GetStatic,
      VPredicate
    );
  VThread.Resume;
end;

end.
