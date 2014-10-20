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

unit u_ProviderTilesDelete;

interface

uses
  Windows,
  Forms,
  i_LanguageManager,
  i_MarkSystem,
  i_MapTypeSet,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  i_CoordConverterFactory,
  i_GeometryProjectedFactory,
  i_GeometryLonLat,
  i_RegionProcessProgressInfoInternalFactory,
  u_ExportProviderAbstract,
  fr_TilesDelete;

type
  TProviderTilesDelete = class(TExportProviderAbstract)
  private
    FProjectionFactory: IProjectionInfoFactory;
    FVectorGeometryProjectedFactory: IGeometryProjectedFactory;
    FMarkSystem: IMarkSystem;
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
      const AMarkSystem: IMarkSystem
    );
    function GetCaption: string; override;
    procedure StartProcess(const APolygon: IGeometryLonLatPolygon); override;
  end;

implementation

uses
  Classes,
  SysUtils,
  i_MapTypes,
  i_RegionProcessParamsFrame,
  i_RegionProcessProgressInfo,
  i_PredicateByTileInfo,
  i_ProjectionInfo,
  i_GeometryProjected,
  u_ThreadDeleteTiles,
  u_ThreadDeleteMarks,
  u_ResStrings;

{ TProviderTilesDelete }

constructor TProviderTilesDelete.Create(
  const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
  const ALanguageManager: ILanguageManager;
  const AMainMapsConfig: IMainMapsConfig;
  const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList;
  const AProjectionFactory: IProjectionInfoFactory;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  const AMarkSystem: IMarkSystem
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
  FMarkSystem := AMarkSystem;
end;

function TProviderTilesDelete.CreateFrame: TFrame;
begin
  Result :=
    TfrTilesDelete.Create(
      Self.LanguageManager,
      Self.MainMapsConfig,
      Self.FullMapsSet,
      Self.GUIConfigList
    );
  Assert(Supports(Result, IRegionProcessParamsFrameOneMap));
  Assert(Supports(Result, IRegionProcessParamsFrameOneZoom));
  Assert(Supports(Result, IRegionProcessParamsFrameProcessPredicate));
  Assert(Supports(Result, IRegionProcessParamsFrameMarksState));
end;

function TProviderTilesDelete.GetCaption: string;
begin
  Result := SAS_STR_OperationDeleteCaption;
end;

procedure TProviderTilesDelete.StartProcess(const APolygon: IGeometryLonLatPolygon);
var
  VMapType: IMapType;
  VZoom: byte;
  VProjection: IProjectionInfo;
  VProjectedPolygon: IGeometryProjectedPolygon;
  VProgressInfo: IRegionProcessProgressInfoInternal;
  VPredicate: IPredicateByTileInfo;
  VThread: TThread;
  VMarkState: Byte;
  VDelHiddenMarks: Boolean;
  VMode: TDeleteSrc;
begin
  inherited;
  VMode := dmNone; // Cancel
  VMarkState := (ParamsFrame as IRegionProcessParamsFrameMarksState).GetMarksState;

  case (ParamsFrame as IRegionProcessParamsFrameMarksState).GetDeleteMode of
  dmTiles: begin
      if (Application.MessageBox(pchar(SAS_MSG_DeleteTilesInRegionAsk), pchar(SAS_MSG_coution), 36) = IDYES) then begin
       VMode := dmTiles; // Tiles
      end
    end;
  dmMarks: begin
      if VMarkState <> 0 then begin
        if (Application.MessageBox(pchar(SAS_MSG_DeleteMarksInRegionAsk), pchar(SAS_MSG_coution), 36) = IDYES) then begin
          VMode := dmMarks; // Marks
        end
      end;
    end;
  end;

  if VMode <> dmNone then begin
    VMapType := (ParamsFrame as IRegionProcessParamsFrameOneMap).MapType;
    VZoom := (ParamsFrame as IRegionProcessParamsFrameOneZoom).Zoom;
    VPredicate := (ParamsFrame as IRegionProcessParamsFrameProcessPredicate).Predicate;
    VThread := nil;
    VProjection := FProjectionFactory.GetByConverterAndZoom(VMapType.GeoConvert, VZoom);
    VProjectedPolygon :=
      FVectorGeometryProjectedFactory.CreateProjectedPolygonByLonLatPolygon(
        VProjection,
        APolygon
      );
    VProgressInfo := ProgressFactory.Build(APolygon);
    VDelHiddenMarks := (ParamsFrame as IRegionProcessParamsFrameMarksState).GetDeleteHiddenMarks;
    case VMode of
    dmTiles:
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
    dmMarks:
      VThread :=
      TThreadDeleteMarks.Create(
        VProgressInfo,
        APolygon,
        VProjectedPolygon,
        VProjection,
        FMarkSystem,
        VMarkState,
        VDelHiddenMarks
      );
    end;
    VThread.Resume;
  end;
end;

end.


