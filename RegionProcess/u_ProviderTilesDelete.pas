{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_ProviderTilesDelete;

interface

uses
  Windows,
  Forms,
  i_LanguageManager,
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
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory
    );
    function GetCaption: string; override;
    procedure StartProcess(const APolygon: IGeometryLonLatMultiPolygon); override;

  end;


implementation

uses
  Classes,
  SysUtils,
  i_RegionProcessParamsFrame,
  i_RegionProcessProgressInfo,
  i_PredicateByTileInfo,
  i_GeometryProjected,
  u_ThreadDeleteTiles,
  u_ResStrings,
  u_MapType;

{ TProviderTilesDelete }

constructor TProviderTilesDelete.Create(
  const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
  const ALanguageManager: ILanguageManager;
  const AMainMapsConfig: IMainMapsConfig;
  const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList;
  const AProjectionFactory: IProjectionInfoFactory;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory
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
end;

function TProviderTilesDelete.GetCaption: string;
begin
  Result := SAS_STR_OperationDeleteCaption;
end;

procedure TProviderTilesDelete.StartProcess(const APolygon: IGeometryLonLatMultiPolygon);
var
  VMapType: TMapType;
  VZoom: byte;
  VProjectedPolygon: IGeometryProjectedMultiPolygon;
  VProgressInfo: IRegionProcessProgressInfoInternal;
  VPredicate: IPredicateByTileInfo;
  VThread: TThread;
begin
  inherited;
  if (Application.MessageBox(pchar(SAS_MSG_DeleteTilesInRegionAsk), pchar(SAS_MSG_coution), 36) = IDYES) then begin
    VMapType := (ParamsFrame as IRegionProcessParamsFrameOneMap).MapType;
    VZoom := (ParamsFrame as IRegionProcessParamsFrameOneZoom).Zoom;
    VPredicate := (ParamsFrame as IRegionProcessParamsFrameProcessPredicate).Predicate;

    VProjectedPolygon :=
      FVectorGeometryProjectedFactory.CreateProjectedPolygonByLonLatPolygon(
        FProjectionFactory.GetByConverterAndZoom(VMapType.GeoConvert, VZoom),
        APolygon
      );

    VProgressInfo := ProgressFactory.Build(APolygon);

    VThread :=
      TThreadDeleteTiles.Create(
        VProgressInfo,
        APolygon,
        VProjectedPolygon,
        VZoom,
        VMapType.TileStorage,
        VMapType.VersionRequestConfig.GetStatic,
        VPredicate
      );
    VThread.Resume;
  end;
end;

end.


