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

unit u_ProviderDeleteTiles;

interface

uses
  Windows,
  Forms,
  i_GeometryLonLat,
  i_RegionProcessTask,
  i_RegionProcessProgressInfo,
  u_ExportProviderAbstract;

type
  TProviderDeleteTiles = class(TExportProviderBase)
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
  Classes,
  SysUtils,
  gnugettext,
  i_MapType,
  i_RegionProcessParamsFrame,
  i_PredicateByTileInfo,
  i_Projection,
  u_Dialogs,
  u_ThreadDeleteTiles,
  u_ResStrings,
  fr_DeleteTiles;

{ TProviderTilesDelete }

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
  Result := _('Delete');
end;

function TProviderDeleteTiles.PrepareTask(
  const APolygon: IGeometryLonLatPolygon;
  const AProgressInfo: IRegionProcessProgressInfoInternal
): IRegionProcessTask;
var
  VZoom: Byte;
  VMapType: IMapType;
  VProjection: IProjection;
  VPredicate: IPredicateByTileInfo;
begin
  inherited;

  if ShowQuestionMessage(SAS_MSG_DeleteTilesInRegionAsk, MB_YESNO) <> ID_YES then begin
    AProgressInfo.Finish;
    Exit;
  end;

  VMapType := (ParamsFrame as IRegionProcessParamsFrameOneMap).MapType;
  VZoom := (ParamsFrame as IRegionProcessParamsFrameOneZoom).Zoom;
  VPredicate := (ParamsFrame as IRegionProcessParamsFrameProcessPredicate).Predicate;
  VProjection := VMapType.ProjectionSet[VZoom];

  Result :=
    TThreadDeleteTiles.Create(
      AProgressInfo,
      Self.TileIteratorFactory,
      APolygon,
      VProjection,
      VMapType.TileStorage,
      VMapType.VersionRequest.GetStatic,
      VPredicate
    );
end;

end.
