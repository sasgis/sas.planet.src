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

unit u_ProviderMapCombine;

interface

uses
  Windows,
  Forms,
  t_GeoTypes,
  i_LanguageManager,
  i_ProjectionSetList,
  i_BitmapTileProvider,
  i_BitmapTileProviderBuilder,
  i_Projection,
  i_GeometryProjected,
  i_GeometryLonLat,
  i_UseTilePrevZoomConfig,
  i_Bitmap32BufferFactory,
  i_MapCalibration,
  i_GeometryProjectedFactory,
  i_GlobalViewMainConfig,
  i_ViewProjectionConfig,
  i_MapTypeListChangeable,
  i_BitmapMapCombiner,
  i_RegionProcessTask,
  i_RegionProcessProgressInfo,
  i_RegionProcessProgressInfoInternalFactory,
  i_RegionProcessParamsFrame,
  u_ExportProviderAbstract,
  fr_MapSelect,
  fr_MapCombine;

type
  TProviderMapCombine = class(TExportProviderBase)
  private
    FCombinerFactory: IBitmapMapCombinerFactory;
    FBitmapTileProviderBuilder: IBitmapTileProviderBuilder;
    FViewConfig: IGlobalViewMainConfig;
    FViewProjectionConfig: IViewProjectionConfig;
    FUseTilePrevZoomConfig: IUseTilePrevZoomConfig;
    FBitmapFactory: IBitmap32StaticFactory;
    FProjectionSetList: IProjectionSetList;
    FVectorGeometryProjectedFactory: IGeometryProjectedFactory;
    FActiveMapsSet: IMapTypeListChangeable;
    FMapCalibrationList: IMapCalibrationList;
  protected
    function PrepareTargetFileName: string;
    function PrepareTargetRect(
      const AProjection: IProjection;
      const APolygon: IGeometryProjectedPolygon
    ): TRect;
    function PrepareImageProvider(
      const APolygon: IGeometryLonLatPolygon;
      const AProjection: IProjection;
      const AProjectedPolygon: IGeometryProjectedPolygon
    ): IBitmapTileProvider;
    function PrepareProjection: IProjection;
    function PreparePolygon(
      const AProjection: IProjection;
      const APolygon: IGeometryLonLatPolygon
    ): IGeometryProjectedPolygon;
    function PrepareCombineProgressUpdate(
      const AProgressInfo: IRegionProcessProgressInfoInternal
    ): IBitmapCombineProgressUpdate;
  protected
    function Validate(const APolygon: IGeometryLonLatPolygon): Boolean; override;
    function CreateFrame: TFrame; override;
  protected
    function GetCaption: string; override;
    function PrepareTask(
      const APolygon: IGeometryLonLatPolygon;
      const AProgressInfo: IRegionProcessProgressInfoInternal
    ): IRegionProcessTask; override;
  public
    constructor Create(
      const ACombinerFactory: IBitmapMapCombinerFactory;
      const ABitmapTileProviderBuilder: IBitmapTileProviderBuilder;
      const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
      const ALanguageManager: ILanguageManager;
      const AMapSelectFrameBuilder: IMapSelectFrameBuilder;
      const AActiveMapsSet: IMapTypeListChangeable;
      const AViewConfig: IGlobalViewMainConfig;
      const AViewProjectionConfig: IViewProjectionConfig;
      const AUseTilePrevZoomConfig: IUseTilePrevZoomConfig;
      const AProjectionSetList: IProjectionSetList;
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
      const ABitmapFactory: IBitmap32StaticFactory;
      const AMapCalibrationList: IMapCalibrationList
    );
  end;

implementation

uses
  Classes,
  SysUtils,
  Types,
  Math,
  gnugettext,
  u_GeoFunc,
  u_ResStrings,
  u_RegionProcessTaskCombine,
  u_BaseInterfacedObject;

type
  TBitmapCombineProgressUpdate = class(TBaseInterfacedObject, IBitmapCombineProgressUpdate)
  private
    FProgressInfo: IRegionProcessProgressInfoInternal;
  private
    procedure Update(AProgress: Double);
  public
    constructor Create(
      const AProgressInfo: IRegionProcessProgressInfoInternal
    );
  end;

{ TBitmapCombineProgressUpdate }

constructor TBitmapCombineProgressUpdate.Create(
  const AProgressInfo: IRegionProcessProgressInfoInternal
);
begin
  inherited Create;
  FProgressInfo := AProgressInfo;
end;

procedure TBitmapCombineProgressUpdate.Update(AProgress: Double);
begin
  FProgressInfo.SetProcessedRatio(AProgress);
  FProgressInfo.SetSecondLine(SAS_STR_Processed + ': ' + IntToStr(Trunc(AProgress * 100)) + '%');
end;

{ TProviderMapCombineBase }

constructor TProviderMapCombine.Create(
  const ACombinerFactory: IBitmapMapCombinerFactory;
  const ABitmapTileProviderBuilder: IBitmapTileProviderBuilder;
  const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
  const ALanguageManager: ILanguageManager;
  const AMapSelectFrameBuilder: IMapSelectFrameBuilder;
  const AActiveMapsSet: IMapTypeListChangeable;
  const AViewConfig: IGlobalViewMainConfig;
  const AViewProjectionConfig: IViewProjectionConfig;
  const AUseTilePrevZoomConfig: IUseTilePrevZoomConfig;
  const AProjectionSetList: IProjectionSetList;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  const ABitmapFactory: IBitmap32StaticFactory;
  const AMapCalibrationList: IMapCalibrationList
);
begin
  Assert(Assigned(ACombinerFactory));
  Assert(Assigned(ABitmapTileProviderBuilder));
  inherited Create(
    AProgressFactory,
    ALanguageManager,
    AMapSelectFrameBuilder,
    nil
  );
  FCombinerFactory := ACombinerFactory;
  FBitmapTileProviderBuilder := ABitmapTileProviderBuilder;
  FMapCalibrationList := AMapCalibrationList;
  FViewConfig := AViewConfig;
  FViewProjectionConfig := AViewProjectionConfig;
  FUseTilePrevZoomConfig := AUseTilePrevZoomConfig;
  FActiveMapsSet := AActiveMapsSet;
  FBitmapFactory := ABitmapFactory;
  FProjectionSetList := AProjectionSetList;
  FVectorGeometryProjectedFactory := AVectorGeometryProjectedFactory;
end;

function TProviderMapCombine.CreateFrame: TFrame;
begin
  Result :=
    TfrMapCombine.Create(
      Self.LanguageManager,
      FProjectionSetList,
      FVectorGeometryProjectedFactory,
      FBitmapFactory,
      Self.MapSelectFrameBuilder,
      FActiveMapsSet,
      FViewConfig,
      FViewProjectionConfig,
      FUseTilePrevZoomConfig,
      FMapCalibrationList,
      FCombinerFactory.MinPartSize,
      FCombinerFactory.MaxPartSize,
      FCombinerFactory.OptionsSet,
      FCombinerFactory.CombinePathStringTypeSupport,
      FCombinerFactory.DefaultExt,
      FCombinerFactory.FormatName
    );
  Assert(Supports(Result, IRegionProcessParamsFrameImageProvider));
  Assert(Supports(Result, IRegionProcessParamsFrameMapCalibrationList));
  Assert(Supports(Result, IRegionProcessParamsFrameTargetProjection));
  Assert(Supports(Result, IRegionProcessParamsFrameTargetPath));
  Assert(Supports(Result, IRegionProcessParamsFrameMapCombine));
end;

function TProviderMapCombine.GetCaption: string;
begin
  Result := _(FCombinerFactory.FormatName);
end;

function TProviderMapCombine.PrepareCombineProgressUpdate(
  const AProgressInfo: IRegionProcessProgressInfoInternal): IBitmapCombineProgressUpdate;
begin
  Result := TBitmapCombineProgressUpdate.Create(AProgressInfo);
end;

function TProviderMapCombine.PreparePolygon(
  const AProjection: IProjection;
  const APolygon: IGeometryLonLatPolygon
): IGeometryProjectedPolygon;
begin
  Result :=
    FVectorGeometryProjectedFactory.CreateProjectedPolygonByLonLatPolygon(
      AProjection,
      APolygon
    );
end;

function TProviderMapCombine.PrepareProjection: IProjection;
begin
  Result := (ParamsFrame as IRegionProcessParamsFrameTargetProjection).Projection;
end;

function TProviderMapCombine.PrepareTargetRect(
  const AProjection: IProjection;
  const APolygon: IGeometryProjectedPolygon
): TRect;
begin
  Result := RectFromDoubleRect(APolygon.Bounds, rrOutside);
end;

function TProviderMapCombine.PrepareImageProvider(
  const APolygon: IGeometryLonLatPolygon;
  const AProjection: IProjection;
  const AProjectedPolygon: IGeometryProjectedPolygon
): IBitmapTileProvider;
begin
  Result :=
    FBitmapTileProviderBuilder.Build(
      (ParamsFrame as IRegionProcessParamsFrameMapCombine).UseMarks,
      (ParamsFrame as IRegionProcessParamsFrameMapCombine).UseRecolor,
      (ParamsFrame as IRegionProcessParamsFrameMapCombine).UseFillingMap,
      (ParamsFrame as IRegionProcessParamsFrameMapCombine).UseGrids,
      (ParamsFrame as IRegionProcessParamsFrameMapCombine).UsePreciseCropping,
      (ParamsFrame as IRegionProcessParamsFrameMapCombine).BGColor,
      (ParamsFrame as IRegionProcessParamsFrameMapCombine).BGColor,
      (ParamsFrame as IRegionProcessParamsFrameImageProvider).Provider,
      APolygon,
      AProjection,
      AProjectedPolygon
    );
end;

function TProviderMapCombine.PrepareTask(
  const APolygon: IGeometryLonLatPolygon;
  const AProgressInfo: IRegionProcessProgressInfoInternal
): IRegionProcessTask;
var
  VMapCalibrations: IMapCalibrationList;
  VFileName: string;
  VSplitCount: TPoint;
  VSkipExistingFiles: Boolean;
  VRoundToTileRect: Boolean;
  VProjection: IProjection;
  VProjectedPolygon: IGeometryProjectedPolygon;
  VImageProvider: IBitmapTileProvider;
  VProgressUpdate: IBitmapCombineProgressUpdate;
  VCombiner: IBitmapMapCombiner;
  VMapRect: TRect;
begin
  VProjection := PrepareProjection;
  VProjectedPolygon := PreparePolygon(VProjection, APolygon);
  VImageProvider := PrepareImageProvider(APolygon, VProjection, VProjectedPolygon);
  VMapCalibrations := (ParamsFrame as IRegionProcessParamsFrameMapCalibrationList).MapCalibrationList;
  VFileName := PrepareTargetFileName;
  VSplitCount := (ParamsFrame as IRegionProcessParamsFrameMapCombine).SplitCount;
  VSkipExistingFiles := (ParamsFrame as IRegionProcessParamsFrameMapCombine).SkipExistingFiles;
  VRoundToTileRect := (ParamsFrame as IRegionProcessParamsFrameMapCombine).CustomOptions.RoundToTileRect;
  VProgressUpdate := PrepareCombineProgressUpdate(AProgressInfo);
  VCombiner := FCombinerFactory.PrepareMapCombiner(ParamsFrame as IRegionProcessParamsFrameMapCombine, VProgressUpdate);
  VMapRect := PrepareTargetRect(VProjection, VProjectedPolygon);

  Result :=
    TRegionProcessTaskCombine.Create(
      AProgressInfo,
      APolygon,
      VMapRect,
      VCombiner,
      VImageProvider,
      VMapCalibrations,
      VFileName,
      VSplitCount,
      VSkipExistingFiles,
      VRoundToTileRect
    );
end;

function TProviderMapCombine.Validate(
  const APolygon: IGeometryLonLatPolygon
): Boolean;
begin
  Result := inherited Validate(APolygon);
  if Result then begin
    Result := FCombinerFactory.Validate(ParamsFrame as IRegionProcessParamsFrameMapCombine, APolygon);
  end;
end;

function TProviderMapCombine.PrepareTargetFileName: string;
begin
  Result := (ParamsFrame as IRegionProcessParamsFrameTargetPath).Path;
  if Result = '' then begin
    raise Exception.Create(_('Please, select output file first!'));
  end;
end;

end.
