{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
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

unit u_ProviderMarksProcess;

interface

uses
  Forms,
  i_LanguageManager,
  i_MarkSystem,
  i_GeometryProjectedFactory,
  i_GeometryLonLat,
  i_LocalCoordConverterChangeable,
  i_RegionProcessTask,
  i_RegionProcessProgressInfo,
  i_RegionProcessProgressInfoInternalFactory,
  u_MarkDbGUIHelper,
  u_ExportProviderAbstract,
  fr_MapSelect;

type
  TProviderMarksProcess = class(TExportProviderBase)
  private
    FMarkSystem: IMarkSystem;
    FMarkDBGUI: TMarkDbGUIHelper;
    FPosition: ILocalCoordConverterChangeable;
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
      const APosition: ILocalCoordConverterChangeable;
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
      const AMarkSystem: IMarkSystem;
      const AMarkDBGUI: TMarkDbGUIHelper
    );
  end;

implementation

uses
  Windows,
  SysUtils,
  gnugettext,
  t_MarksProcess,
  i_RegionProcessParamsFrame,
  i_Projection,
  i_GeometryProjected,
  u_Dialogs,
  u_ResStrings,
  u_ThreadMarksProcess,
  fr_MarksProcess;

{ TProviderMarksProcess }

constructor TProviderMarksProcess.Create(
  const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
  const ALanguageManager: ILanguageManager;
  const AMapSelectFrameBuilder: IMapSelectFrameBuilder;
  const APosition: ILocalCoordConverterChangeable;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  const AMarkSystem: IMarkSystem;
  const AMarkDBGUI: TMarkDbGUIHelper
);
begin
  inherited Create(
    AProgressFactory,
    ALanguageManager,
    AMapSelectFrameBuilder,
    nil
  );
  FPosition := APosition;
  FVectorGeometryProjectedFactory := AVectorGeometryProjectedFactory;
  FMarkSystem := AMarkSystem;
  FMarkDBGUI := AMarkDBGUI;
end;

function TProviderMarksProcess.CreateFrame: TFrame;
begin
  Result := TfrMarksProcess.Create(Self.LanguageManager, FMarkSystem.CategoryDB);
  Assert(Supports(Result, IRegionProcessParamsFrameMarks));
end;

function TProviderMarksProcess.GetCaption: string;
begin
  Result := _('Placemarks');
end;

function TProviderMarksProcess.PrepareTask(
  const APolygon: IGeometryLonLatPolygon;
  const AProgressInfo: IRegionProcessProgressInfoInternal
): IRegionProcessTask;
var
  VProjection: IProjection;
  VProjectedPolygon: IGeometryProjectedPolygon;
  VParams: TMarksProcessTaskParams;
begin
  inherited;

  VParams := (ParamsFrame as IRegionProcessParamsFrameMarks).TaskParams;

  if VParams.MarksTypes = [] then begin
    Assert(False);
    Exit;
  end;

  with FMarkSystem.State.GetStatic do begin
    if not ReadAccess then begin
      ShowErrorMessage(_('There is no read access to the placemarks DB!'));
      AProgressInfo.Finish;
      Exit;
    end;
    if not WriteAccess and (VParams.Operation in [mpoCopy, mpoMove, mpoDelete]) then begin
      ShowErrorMessage(_('There is no write access to the placemarks DB!'));
      AProgressInfo.Finish;
      Exit;
    end;
  end;

  if (VParams.Operation = mpoDelete) and
     (ShowQuestionMessage(SAS_MSG_DeleteMarksInRegionAsk, MB_YESNO) <> ID_YES)
  then begin
    AProgressInfo.Finish;
    Exit;
  end;

  VProjection := FPosition.GetStatic.Projection;
  VProjectedPolygon :=
    FVectorGeometryProjectedFactory.CreateProjectedPolygonByLonLatPolygon(
      VProjection,
      APolygon
    );

  Result :=
    TThreadMarksProcess.Create(
      FMarkDBGUI,
      AProgressInfo,
      APolygon,
      VProjectedPolygon,
      VProjection,
      FMarkSystem,
      VParams
    );
end;

end.
