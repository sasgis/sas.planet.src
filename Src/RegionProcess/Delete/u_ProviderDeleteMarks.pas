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

unit u_ProviderDeleteMarks;

interface

uses
  Windows,
  Forms,
  i_LanguageManager,
  i_MarkSystem,
  i_MapTypeSet,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  i_GeometryProjectedFactory,
  i_GeometryLonLat,
  i_LocalCoordConverterChangeable,
  i_RegionProcessProgressInfoInternalFactory,
  u_ExportProviderAbstract;

type
  TProviderDeleteMarks = class(TExportProviderAbstract)
  private
    FVectorGeometryProjectedFactory: IGeometryProjectedFactory;
    FMarkSystem: IMarkSystem;
    FPosition: ILocalCoordConverterChangeable;
  protected
    function CreateFrame: TFrame; override;
  protected
    function GetCaption: string; override;
    procedure StartProcess(const APolygon: IGeometryLonLatPolygon); override;
  public
    constructor Create(
      const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
      const ALanguageManager: ILanguageManager;
      const AMainMapsConfig: IMainMapsConfig;
      const AFullMapsSet: IMapTypeSet;
      const AGUIConfigList: IMapTypeGUIConfigList;
      const APosition: ILocalCoordConverterChangeable;
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
      const AMarkSystem: IMarkSystem
    );
  end;

implementation

uses
  Classes,
  SysUtils,
  i_RegionProcessParamsFrame,
  i_RegionProcessProgressInfo,
  i_ProjectionInfo,
  i_GeometryProjected,
  u_ThreadDeleteMarks,
  u_ResStrings,
  fr_DeleteMarks;

{ TProviderDeleteMarks }

constructor TProviderDeleteMarks.Create(
  const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
  const ALanguageManager: ILanguageManager;
  const AMainMapsConfig: IMainMapsConfig;
  const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList;
  const APosition: ILocalCoordConverterChangeable;
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
  FPosition := APosition;
  FVectorGeometryProjectedFactory := AVectorGeometryProjectedFactory;
  FMarkSystem := AMarkSystem;
end;

function TProviderDeleteMarks.CreateFrame: TFrame;
begin
  Result := TfrDeleteMarks.Create(Self.LanguageManager);
  Assert(Supports(Result, IRegionProcessParamsFrameMarksState));
end;

function TProviderDeleteMarks.GetCaption: string;
begin
  Result := SAS_STR_OperationDeleteCaption;
end;

procedure TProviderDeleteMarks.StartProcess(const APolygon: IGeometryLonLatPolygon);
var
  VProjection: IProjectionInfo;
  VProjectedPolygon: IGeometryProjectedPolygon;
  VProgressInfo: IRegionProcessProgressInfoInternal;
  VThread: TThread;
  VMarkState: Byte;
  VDelHiddenMarks: Boolean;
begin
  inherited;
  VMarkState := (ParamsFrame as IRegionProcessParamsFrameMarksState).GetMarksState;

  if VMarkState <> 0 then begin
    if (Application.MessageBox(pchar(SAS_MSG_DeleteMarksInRegionAsk), pchar(SAS_MSG_coution), 36) <> IDYES) then begin
      Exit;
    end;
  end;

  VProjection := FPosition.GetStatic.ProjectionInfo;
  VProjectedPolygon :=
    FVectorGeometryProjectedFactory.CreateProjectedPolygonByLonLatPolygon(
      VProjection,
      APolygon
    );
  VProgressInfo := ProgressFactory.Build(APolygon);
  VDelHiddenMarks := (ParamsFrame as IRegionProcessParamsFrameMarksState).GetDeleteHiddenMarks;
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
  VThread.Resume;
end;

end.
