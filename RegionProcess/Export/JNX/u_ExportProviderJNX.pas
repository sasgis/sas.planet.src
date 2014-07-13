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

unit u_ExportProviderJNX;

interface

uses
  Forms,
  i_GeometryLonLat,
  i_CoordConverterFactory,
  i_GeometryProjectedFactory,
  i_LanguageManager,
  i_MapTypeSet,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  i_BitmapTileSaveLoadFactory,
  i_BitmapPostProcessing,
  i_RegionProcessProgressInfoInternalFactory,
  u_ExportProviderAbstract,
  fr_ExportToJNX;

type
  TExportProviderJNX = class(TExportProviderAbstract)
  private
    FCoordConverterFactory: ICoordConverterFactory;
    FProjectionFactory: IProjectionInfoFactory;
    FVectorGeometryProjectedFactory: IGeometryProjectedFactory;
    FBitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
    FBitmapPostProcessing: IBitmapPostProcessingChangeable;
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
      const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
      const ABitmapPostProcessing: IBitmapPostProcessingChangeable;
      const ACoordConverterFactory: ICoordConverterFactory
    );
    function GetCaption: string; override;
    procedure StartProcess(const APolygon: IGeometryLonLatPolygon); override;
  end;

implementation

uses
  Classes,
  SysUtils,
  i_RegionProcessParamsFrame,
  i_RegionProcessProgressInfo,
  u_ExportToJnxTask,
  u_ThreadExportToJNX,
  u_ResStrings;

{ TExportProviderJNX }

constructor TExportProviderJNX.Create(
  const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
  const ALanguageManager: ILanguageManager;
  const AMainMapsConfig: IMainMapsConfig;
  const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList;
  const AProjectionFactory: IProjectionInfoFactory;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
  const ABitmapPostProcessing: IBitmapPostProcessingChangeable;
  const ACoordConverterFactory: ICoordConverterFactory
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
  FBitmapTileSaveLoadFactory := ABitmapTileSaveLoadFactory;
  FCoordConverterFactory := ACoordConverterFactory;
  FBitmapPostProcessing := ABitmapPostProcessing;
end;

function TExportProviderJNX.CreateFrame: TFrame;
begin
  Result :=
    TfrExportToJNX.Create(
      Self.LanguageManager,
      FBitmapTileSaveLoadFactory,
      Self.MainMapsConfig,
      Self.FullMapsSet,
      Self.GUIConfigList,
      'JNX |*.jnx',
      'jnx'
    );
  Assert(Supports(Result, IRegionProcessParamsFrameTargetPath));
  Assert(Supports(Result, IRegionProcessParamsFrameExportToJNX));
end;

function TExportProviderJNX.GetCaption: string;
begin
  Result := SAS_STR_ExportJNXPackCaption;
end;

procedure TExportProviderJNX.StartProcess(const APolygon: IGeometryLonLatPolygon);
var
  VPath: string;
  VProductName: string;
  VMapName: string;
  VJNXVersion: integer;
  VZorder: integer;
  VProductID: integer;
  VProgressInfo: IRegionProcessProgressInfoInternal;
  VTasks: TExportTaskJnxArray;
  VThread: TThread;
  VUseRecolor: Boolean;
  VBitmapPostProcessing: IBitmapPostProcessing;
begin
  inherited;

  VPath := (ParamsFrame as IRegionProcessParamsFrameTargetPath).Path;
  VProductName := (ParamsFrame as IRegionProcessParamsFrameExportToJNX).ProductName;
  VMapName := (ParamsFrame as IRegionProcessParamsFrameExportToJNX).MapName;
  VJNXVersion := (ParamsFrame as IRegionProcessParamsFrameExportToJNX).JNXVersion;
  VZorder := (ParamsFrame as IRegionProcessParamsFrameExportToJNX).ZOrder;
  VProductID := (ParamsFrame as IRegionProcessParamsFrameExportToJNX).ProductID;
  VTasks := (ParamsFrame as IRegionProcessParamsFrameExportToJNX).Tasks;
  VUseRecolor := (ParamsFrame as IRegionProcessParamsFrameExportToJNX).UseRecolor;
  VProgressInfo := ProgressFactory.Build(APolygon);
  VBitmapPostProcessing := nil;
  if VUseRecolor then VBitmapPostProcessing := FBitmapPostProcessing.GetStatic;

  VThread :=
    TThreadExportToJnx.Create(
      VProgressInfo,
      FProjectionFactory,
      FVectorGeometryProjectedFactory,
      VPath,
      APolygon,
      VTasks,
      VProductName,
      VMapName,
      VBitmapPostProcessing,
      VJNXVersion,
      VZorder,
      VProductID
    );
  VThread.Resume;
end;

end.


