{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2017, SAS.Planet development team.                      *}
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

unit u_ExportProviderIMG;

interface

uses
  Forms,
  i_GeometryLonLat,
  i_GeometryProjectedFactory,
  i_LanguageManager,
  i_BitmapTileSaveLoadFactory,
  i_BitmapPostProcessing,
  i_RegionProcessTask,
  i_RegionProcessProgressInfo,
  i_RegionProcessProgressInfoInternalFactory,
  u_ExportProviderAbstract,
  fr_MapSelect,
  fr_ExportToIMG;

type
  TExportProviderIMG = class(TExportProviderBase)
  private
    FVectorGeometryProjectedFactory: IGeometryProjectedFactory;
    FBitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
    FBitmapPostProcessing: IBitmapPostProcessingChangeable;
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
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
      const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
      const ABitmapPostProcessing: IBitmapPostProcessingChangeable
    );
  end;

implementation

uses
  Classes,
  SysUtils,
  i_RegionProcessParamsFrame,
  u_ExportToIMGTask,
  u_ThreadExportToIMG,
  u_ResStrings;

{ TExportProviderIMG }

constructor TExportProviderIMG.Create(
  const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
  const ALanguageManager: ILanguageManager;
  const AMapSelectFrameBuilder: IMapSelectFrameBuilder;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
  const ABitmapPostProcessing: IBitmapPostProcessingChangeable
);
begin
  inherited Create(
    AProgressFactory,
    ALanguageManager,
    AMapSelectFrameBuilder
  );
  FVectorGeometryProjectedFactory := AVectorGeometryProjectedFactory;
  FBitmapTileSaveLoadFactory := ABitmapTileSaveLoadFactory;
  FBitmapPostProcessing := ABitmapPostProcessing;
end;

function TExportProviderIMG.CreateFrame: TFrame;
begin
  Result :=
    TfrExportToIMG.Create(
      Self.LanguageManager,
      FBitmapTileSaveLoadFactory,
      Self.MapSelectFrameBuilder,
      'IMG |*.IMG',
      'IMG'
    );
  Assert(Supports(Result, IRegionProcessParamsFrameTargetPath));
  Assert(Supports(Result, IRegionProcessParamsFrameExportToIMG));
end;

function TExportProviderIMG.GetCaption: string;
begin
  Result := SAS_STR_ExportIMGPackCaption;
end;

function TExportProviderIMG.PrepareTask(
  const APolygon: IGeometryLonLatPolygon;
  const AProgressInfo: IRegionProcessProgressInfoInternal
): IRegionProcessTask;
var
  VProcessParams: IRegionProcessParamsFrameExportToIMG;
  VBitmapPostProcessing: IBitmapPostProcessing;
  VTask: TExportToIMGTask;
begin
  inherited;

  VProcessParams := ParamsFrame as IRegionProcessParamsFrameExportToIMG;

  VTask := VProcessParams.Task;
  if VTask.FUseRecolor then begin
    VBitmapPostProcessing := FBitmapPostProcessing.GetStatic
  end else begin
    VBitmapPostProcessing := nil;
  end;

  Result :=
    TThreadExportToIMG.Create(
      AProgressInfo,
      FVectorGeometryProjectedFactory,
      VProcessParams.Path,
      APolygon,
      VTask,
      VBitmapPostProcessing
    );
end;

end.
