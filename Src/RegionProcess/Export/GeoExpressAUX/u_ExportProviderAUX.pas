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

unit u_ExportProviderAUX;

interface

uses
  Forms,
  i_LanguageManager,
  i_GeometryProjectedFactory,
  i_GeometryLonLat,
  i_RegionProcessProgressInfoInternalFactory,
  fr_MapSelect,
  u_ExportProviderAbstract;

type
  TExportProviderAUX = class(TExportProviderAbstract)
  private
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
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory
    );
  end;


implementation

uses
  Classes,
  SysUtils,
  i_RegionProcessParamsFrame,
  i_RegionProcessProgressInfo,
  i_MapType,
  i_Projection,
  i_GeometryProjected,
  u_ThreadExportToAUX,
  u_ResStrings,
  fr_ExportAUX;

{ TExportProviderKml }

constructor TExportProviderAUX.Create(
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

function TExportProviderAUX.CreateFrame: TFrame;
begin
  Result :=
    TfrExportAUX.Create(
      Self.LanguageManager,
      Self.MapSelectFrameBuilder
    );
  Assert(Supports(Result, IRegionProcessParamsFrameOneMap));
  Assert(Supports(Result, IRegionProcessParamsFrameOneZoom));
  Assert(Supports(Result, IRegionProcessParamsFrameTargetPath));
end;

function TExportProviderAUX.GetCaption: string;
begin
  Result := SAS_STR_ExportAUXGeoServerCaption;
end;

procedure TExportProviderAUX.StartProcess(const APolygon: IGeometryLonLatPolygon);
var
  VPath: string;
  VMapType: IMapType;
  VZoom: byte;
  VProjection: IProjection;
  VProjectedPolygon: IGeometryProjectedPolygon;
  VProgressInfo: IRegionProcessProgressInfoInternal;
  VThread: TThread;
begin
  inherited;
  VMapType := (ParamsFrame as IRegionProcessParamsFrameOneMap).MapType;
  VZoom := (ParamsFrame as IRegionProcessParamsFrameOneZoom).Zoom;
  VPath := (ParamsFrame as IRegionProcessParamsFrameTargetPath).Path;

  VProjection := VMapType.ProjectionSet[VZoom];
  VProjectedPolygon :=
    FVectorGeometryProjectedFactory.CreateProjectedPolygonByLonLatPolygon(
      VProjection,
      APolygon
    );

  VProgressInfo := ProgressFactory.Build(APolygon);

  VThread :=
    TThreadExportToAUX.Create(
      VProgressInfo,
      APolygon,
      VProjectedPolygon,
      VProjection,
      VMapType.TileStorage,
      VMapType.VersionRequestConfig.GetStatic.BaseVersion,
      VPath
    );
  VThread.Resume;
end;

end.
