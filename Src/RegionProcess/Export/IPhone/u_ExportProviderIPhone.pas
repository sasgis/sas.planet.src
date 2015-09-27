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

unit u_ExportProviderIPhone;

interface

uses
  Forms,
  i_LanguageManager,
  i_GeometryLonLat,
  i_Bitmap32BufferFactory,
  i_ProjectionSetFactory,
  i_GeometryProjectedFactory,
  i_BitmapTileSaveLoadFactory,
  i_RegionProcessProgressInfoInternalFactory,
  u_ExportProviderAbstract,
  fr_MapSelect,
  fr_ExportIPhone;

type
  TExportProviderIPhone = class(TExportProviderAbstract)
  private
    FFrame: TfrExportIPhone;
    FProjectionSetFactory: IProjectionSetFactory;
    FBitmap32StaticFactory: IBitmap32StaticFactory;
    FVectorGeometryProjectedFactory: IGeometryProjectedFactory;
    FBitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
    FNewFormat: Boolean;
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
      const AProjectionSetFactory: IProjectionSetFactory;
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
      const ABitmap32StaticFactory: IBitmap32StaticFactory;
      const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
      ANewFormat: Boolean
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
  u_ThreadExportIPhone,
  u_BitmapLayerProviderMapWithLayer,
  u_ResStrings;

{ TExportProviderIPhone }

constructor TExportProviderIPhone.Create(
  const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
  const ALanguageManager: ILanguageManager;
  const AMapSelectFrameBuilder: IMapSelectFrameBuilder;
  const AProjectionSetFactory: IProjectionSetFactory;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  const ABitmap32StaticFactory: IBitmap32StaticFactory;
  const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
  ANewFormat: Boolean
);
begin
  Assert(Assigned(ABitmap32StaticFactory));
  inherited Create(
    AProgressFactory,
    ALanguageManager,
    AMapSelectFrameBuilder
  );
  FProjectionSetFactory := AProjectionSetFactory;
  FVectorGeometryProjectedFactory := AVectorGeometryProjectedFactory;
  FBitmapTileSaveLoadFactory := ABitmapTileSaveLoadFactory;
  FBitmap32StaticFactory := ABitmap32StaticFactory;
  FNewFormat := ANewFormat;
end;

function TExportProviderIPhone.CreateFrame: TFrame;
begin
  FFrame :=
    TfrExportIPhone.Create(
      Self.LanguageManager,
      Self.MapSelectFrameBuilder
    );
  Result := FFrame;
  Assert(Supports(Result, IRegionProcessParamsFrameZoomArray));
  Assert(Supports(Result, IRegionProcessParamsFrameTargetPath));
end;

function TExportProviderIPhone.GetCaption: string;
begin
  if FNewFormat then begin
    Result := SAS_STR_ExportIPhone128Caption;
  end else begin
    Result := SAS_STR_ExportIPhone64Caption;
  end;
end;

procedure TExportProviderIPhone.StartProcess(const APolygon: IGeometryLonLatPolygon);
var
  VPath: string;
  VZoomArr: TByteDynArray;
  comprSat, comprMap, comprHyb: byte;
  Replace: boolean;
  VActiveMapIndex: Integer;
  VActiveTaskIndex: Integer;
  VProgressInfo: IRegionProcessProgressInfoInternal;
  VThread: TThread;
  VTasks: TExportTaskIPhoneArray;
  VTaskIndex: Integer;
  VMapVersion: IMapVersionRequest;
  VLayerVersion: IMapVersionRequest;
begin
  inherited;
  VZoomArr := (ParamsFrame as IRegionProcessParamsFrameZoomArray).ZoomArray;
  VPath := (ParamsFrame as IRegionProcessParamsFrameTargetPath).Path;
  VActiveMapIndex := 0;
  if FFrame.GetMap <> nil then begin
    if FFrame.rbSat.Checked then begin
      VActiveMapIndex := 0;
    end;
  end;
  if FFrame.GetSat <> nil then begin
    if FFrame.rbMap.Checked then begin
      VActiveMapIndex := 1;
    end;
  end;
  if FFrame.GetHyb <> nil then begin
    if FFrame.rbHybr.Checked then begin
      VActiveMapIndex := 2;
    end;
  end;
  comprSat := FFrame.seSatCompress.Value;
  comprMap := FFrame.seMapCompress.Value;
  comprHyb := FFrame.seHybrCompress.Value;
  Replace := FFrame.chkAppendTilse.Checked;

  VProgressInfo := ProgressFactory.Build(APolygon);

  VTaskIndex := -1;
  VActiveTaskIndex := VTaskIndex;
  SetLength(VTasks, 0);

  if FFrame.GetSat <> nil then begin
    Inc(VTaskIndex);
    SetLength(VTasks, VTaskIndex + 1);
    if VActiveMapIndex = 0 then begin
      VActiveTaskIndex := VTaskIndex;
    end;
    VTasks[VTaskIndex].FFlag := 3;
    VTasks[VTaskIndex].FSaver := FBitmapTileSaveLoadFactory.CreateJpegSaver(comprSat);
    VTasks[VTaskIndex].FImageProvider :=
      TBitmapLayerProviderMapWithLayer.Create(
        FBitmap32StaticFactory,
        FFrame.GetSat,
        FFrame.GetSat.VersionRequest.GetStatic,
        nil,
        nil,
        nil,
        False,
        False
      );
  end;
  if FFrame.GetMap <> nil then begin
    Inc(VTaskIndex);
    SetLength(VTasks, VTaskIndex + 1);
    if VActiveMapIndex = 1 then begin
      VActiveTaskIndex := VTaskIndex;
    end;
    VTasks[VTaskIndex].FFlag := 2;
    VTasks[VTaskIndex].FSaver := FBitmapTileSaveLoadFactory.CreatePngSaver(i24bpp, comprMap);
    VTasks[VTaskIndex].FImageProvider :=
      TBitmapLayerProviderMapWithLayer.Create(
        FBitmap32StaticFactory,
        FFrame.GetMap,
        FFrame.GetMap.VersionRequest.GetStatic,
        nil,
        nil,
        nil,
        False,
        False
      );
  end;
  if FFrame.GetHyb <> nil then begin
    Inc(VTaskIndex);
    SetLength(VTasks, VTaskIndex + 1);
    if VActiveMapIndex = 2 then begin
      VActiveTaskIndex := VTaskIndex;
    end;
    VTasks[VTaskIndex].FFlag := 6;
    VTasks[VTaskIndex].FSaver := FBitmapTileSaveLoadFactory.CreateJpegSaver(comprHyb);
    VMapVersion := nil;
    if FFrame.GetSat <> nil then begin
      VMapVersion := FFrame.GetSat.VersionRequest.GetStatic;
    end;
    VLayerVersion := nil;
    if FFrame.GetHyb <> nil then begin
      VLayerVersion := FFrame.GetHyb.VersionRequest.GetStatic;
    end;
    VTasks[VTaskIndex].FImageProvider :=
      TBitmapLayerProviderMapWithLayer.Create(
        FBitmap32StaticFactory,
        FFrame.GetSat,
        VMapVersion,
        FFrame.GetHyb,
        VLayerVersion,
        nil,
        False,
        False
      );
  end;
  VThread :=
    TThreadExportIPhone.Create(
      VProgressInfo,
      FProjectionSetFactory,
      FVectorGeometryProjectedFactory,
      FBitmap32StaticFactory,
      VPath,
      APolygon,
      VTasks,
      VZoomArr,
      VActiveTaskIndex,
      Replace,
      FNewFormat
    );
  VThread.Resume;
end;

end.
