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

unit u_ExportProviderYaMobileV4;

interface

uses
  Forms,
  i_LanguageManager,
  i_GeometryLonLat,
  i_Bitmap32BufferFactory,
  i_GeometryProjectedFactory,
  i_CoordConverterFactory,
  i_BitmapTileSaveLoadFactory,
  i_LocalCoordConverterFactorySimpe,
  i_RegionProcessProgressInfoInternalFactory,
  u_ExportProviderAbstract,
  fr_MapSelect,
  fr_ExportYaMobileV4;

type
  TExportProviderYaMobileV4 = class(TExportProviderAbstract)
  private
    FFrame: TfrExportYaMobileV4;
    FCoordConverterFactory: ICoordConverterFactory;
    FLocalConverterFactory: ILocalCoordConverterFactorySimpe;
    FProjectionFactory: IProjectionInfoFactory;
    FVectorGeometryProjectedFactory: IGeometryProjectedFactory;
    FBitmap32StaticFactory: IBitmap32StaticFactory;
    FBitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
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
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
      const ABitmap32StaticFactory: IBitmap32StaticFactory;
      const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
      const ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
      const ACoordConverterFactory: ICoordConverterFactory
    );
  end;

implementation

uses
  Types,
  Classes,
  SysUtils,
  i_MapVersionRequest,
  i_RegionProcessParamsFrame,
  i_RegionProcessProgressInfo,
  u_ThreadExportYaMobileV4,
  u_BitmapLayerProviderMapWithLayer,
  u_ResStrings;

{ TExportProviderYaMaps }

constructor TExportProviderYaMobileV4.Create(
  const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
  const ALanguageManager: ILanguageManager;
  const AMapSelectFrameBuilder: IMapSelectFrameBuilder;
  const AProjectionFactory: IProjectionInfoFactory;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  const ABitmap32StaticFactory: IBitmap32StaticFactory;
  const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
  const ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
  const ACoordConverterFactory: ICoordConverterFactory
);
begin
  Assert(Assigned(ABitmap32StaticFactory));
  inherited Create(
    AProgressFactory,
    ALanguageManager,
    AMapSelectFrameBuilder
  );
  FCoordConverterFactory := ACoordConverterFactory;
  FLocalConverterFactory := ALocalConverterFactory;
  FProjectionFactory := AProjectionFactory;
  FVectorGeometryProjectedFactory := AVectorGeometryProjectedFactory;
  FBitmap32StaticFactory := ABitmap32StaticFactory;
  FBitmapTileSaveLoadFactory := ABitmapTileSaveLoadFactory;
end;

function TExportProviderYaMobileV4.CreateFrame: TFrame;
begin
  FFrame :=
    TfrExportYaMobileV4.Create(
      Self.LanguageManager,
      Self.MapSelectFrameBuilder
    );
  Result := FFrame;
  Assert(Supports(Result, IRegionProcessParamsFrameZoomArray));
  Assert(Supports(Result, IRegionProcessParamsFrameTargetPath));
end;

function TExportProviderYaMobileV4.GetCaption: string;
begin
  Result := SAS_STR_ExportYaMobileV4Caption;
end;

procedure TExportProviderYaMobileV4.StartProcess(const APolygon: IGeometryLonLatPolygon);
var
  VPath: string;
  VZoomArr: TByteDynArray;
  comprSat, comprMap: byte;
  VProgressInfo: IRegionProcessProgressInfoInternal;
  VThread: TThread;
  VTasks: TExportTaskYaMobileV4Array;
  VTaskIndex: Integer;
  VMapVersion: IMapVersionRequest;
  VLayerVersion: IMapVersionRequest;
begin
  inherited;
  VZoomArr := (ParamsFrame as IRegionProcessParamsFrameZoomArray).ZoomArray;
  VPath := (ParamsFrame as IRegionProcessParamsFrameTargetPath).Path;
  comprSat := FFrame.seSatCompress.Value;
  comprMap := FFrame.seMapCompress.Value;

  VProgressInfo := ProgressFactory.Build(APolygon);

  VTaskIndex := -1;
  if (FFrame.GetSat.GetSelectedMapType <> nil) or (FFrame.GetHyb.GetSelectedMapType <> nil) then begin
    Inc(VTaskIndex);
    SetLength(VTasks, VTaskIndex + 1);
    if FFrame.GetHyb.GetSelectedMapType <> nil then begin
      VTasks[VTaskIndex].FMapId := 12;
      VTasks[VTaskIndex].FMapName := FFrame.GetHyb.GetSelectedMapType.GUIConfig.Name.Value;
    end else begin
      VTasks[VTaskIndex].FMapId := 10;
      VTasks[VTaskIndex].FMapName := FFrame.GetSat.GetSelectedMapType.GUIConfig.Name.Value;
    end;
    VTasks[VTaskIndex].FSaver := FBitmapTileSaveLoadFactory.CreateJpegSaver(comprSat);
    VMapVersion := nil;
    if FFrame.GetSat.GetSelectedMapType <> nil then begin
      VMapVersion := FFrame.GetSat.GetSelectedMapType.VersionRequestConfig.GetStatic;
    end;
    VLayerVersion := nil;
    if FFrame.GetHyb.GetSelectedMapType <> nil then begin
      VLayerVersion := FFrame.GetHyb.GetSelectedMapType.VersionRequestConfig.GetStatic;
    end;
    VTasks[VTaskIndex].FImageProvider :=
      TBitmapLayerProviderMapWithLayer.Create(
        FBitmap32StaticFactory,
        FFrame.GetSat.GetSelectedMapType,
        VMapVersion,
        FFrame.GetHyb.GetSelectedMapType,
        VLayerVersion,
        nil,
        False,
        False
      );
  end;
  if FFrame.GetMap.GetSelectedMapType <> nil then begin
    Inc(VTaskIndex);
    SetLength(VTasks, VTaskIndex + 1);
    VTasks[VTaskIndex].FMapId := 11;
    VTasks[VTaskIndex].FMapName := FFrame.GetMap.GetSelectedMapType.GUIConfig.Name.Value;
    VTasks[VTaskIndex].FSaver := FBitmapTileSaveLoadFactory.CreatePngSaver(i8bpp, comprMap);
    VTasks[VTaskIndex].FImageProvider :=
      TBitmapLayerProviderMapWithLayer.Create(
        FBitmap32StaticFactory,
        FFrame.GetMap.GetSelectedMapType,
        FFrame.GetMap.GetSelectedMapType.VersionRequestConfig.GetStatic,
        nil,
        nil,
        nil,
        False,
        False
      );
  end;

  VThread :=
    TThreadExportYaMobileV4.Create(
      VProgressInfo,
      FCoordConverterFactory,
      FLocalConverterFactory,
      FProjectionFactory,
      FVectorGeometryProjectedFactory,
      FBitmap32StaticFactory,
      VPath,
      APolygon,
      VTasks,
      VZoomArr,
      FFrame.chkReplaseTiles.Checked,
      TYaMobileV4TileSize(FFrame.rgTileSize.ItemIndex)
    );
  VThread.Resume;
end;

end.
