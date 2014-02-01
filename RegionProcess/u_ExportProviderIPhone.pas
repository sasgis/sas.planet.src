unit u_ExportProviderIPhone;

interface

uses
  Forms,
  i_LanguageManager,
  i_GeometryLonLat,
  i_MapTypeSet,
  i_ActiveMapsConfig,
  i_Bitmap32StaticFactory,
  i_CoordConverterFactory,
  i_LocalCoordConverterFactorySimpe,
  i_GeometryProjectedFactory,
  i_MapTypeGUIConfigList,
  i_BitmapTileSaveLoadFactory,
  i_RegionProcessProgressInfoInternalFactory,
  u_ExportProviderAbstract,
  fr_ExportIPhone;

type
  TExportProviderIPhone = class(TExportProviderAbstract)
  private
    FFrame: TfrExportIPhone;
    FCoordConverterFactory: ICoordConverterFactory;
    FLocalConverterFactory: ILocalCoordConverterFactorySimpe;
    FProjectionFactory: IProjectionInfoFactory;
    FBitmapFactory: IBitmap32StaticFactory;
    FVectorGeometryProjectedFactory: IGeometryProjectedFactory;
    FBitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
    FNewFormat: Boolean;
  protected
    function CreateFrame: TFrame; override;
  public
    constructor Create(
      const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
      const ALanguageManager: ILanguageManager;
      const AMainMapsConfig: IMainMapsConfig;
      const AFullMapsSet: IMapTypeSet;
      const AGUIConfigList: IMapTypeGUIConfigList;
      const ACoordConverterFactory: ICoordConverterFactory;
      const ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
      const AProjectionFactory: IProjectionInfoFactory;
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
      const ABitmapFactory: IBitmap32StaticFactory;
      const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
      ANewFormat: Boolean
    );
    function GetCaption: string; override;
    procedure StartProcess(const APolygon: IGeometryLonLatMultiPolygon); override;
  end;


implementation

uses
  Types,
  Classes,
  SysUtils,
  i_MapVersionInfo,
  i_RegionProcessParamsFrame,
  i_RegionProcessProgressInfo,
  u_ThreadExportIPhone,
  u_BitmapLayerProviderMapWithLayer,
  u_ResStrings;

{ TExportProviderIPhone }

constructor TExportProviderIPhone.Create(
  const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
  const ALanguageManager: ILanguageManager;
  const AMainMapsConfig: IMainMapsConfig;
  const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList;
  const ACoordConverterFactory: ICoordConverterFactory;
  const ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
  const AProjectionFactory: IProjectionInfoFactory;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  const ABitmapFactory: IBitmap32StaticFactory;
  const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
  ANewFormat: Boolean
);
begin
  inherited Create(
    AProgressFactory,
    ALanguageManager,
    AMainMapsConfig,
    AFullMapsSet,
    AGUIConfigList
  );
  FCoordConverterFactory := ACoordConverterFactory;
  FLocalConverterFactory := ALocalConverterFactory;
  FProjectionFactory := AProjectionFactory;
  FVectorGeometryProjectedFactory := AVectorGeometryProjectedFactory;
  FBitmapTileSaveLoadFactory := ABitmapTileSaveLoadFactory;
  FBitmapFactory := ABitmapFactory;
  FNewFormat := ANewFormat;
end;

function TExportProviderIPhone.CreateFrame: TFrame;
begin
  FFrame :=
    TfrExportIPhone.Create(
      Self.LanguageManager,
      Self.MainMapsConfig,
      Self.FullMapsSet,
      Self.GUIConfigList
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

procedure TExportProviderIPhone.StartProcess(const APolygon: IGeometryLonLatMultiPolygon);
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
  VMapVersion: IMapVersionInfo;
  VLayerVersion: IMapVersionInfo;
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
        FBitmapFactory,
        FFrame.GetSat,
        FFrame.GetSat.VersionConfig.Version,
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
        FBitmapFactory,
        FFrame.GetMap,
        FFrame.GetMap.VersionConfig.Version,
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
      VMapVersion := FFrame.GetSat.VersionConfig.Version;
    end;
    VLayerVersion := nil;
    if FFrame.GetHyb <> nil then begin
      VLayerVersion := FFrame.GetHyb.VersionConfig.Version;
    end;
    VTasks[VTaskIndex].FImageProvider :=
      TBitmapLayerProviderMapWithLayer.Create(
        FBitmapFactory,
        FFrame.GetSat,
        VMapVersion,
        FFrame.GetHyb,
        VLayerVersion,
        False,
        False
      );
  end;
  VThread :=
    TThreadExportIPhone.Create(
      VProgressInfo,
      FCoordConverterFactory,
      FLocalConverterFactory,
      FProjectionFactory,
      FVectorGeometryProjectedFactory,
      FBitmapFactory,
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


