unit u_ProviderMapCombine;

interface

uses
  Windows,
  Controls,
  t_GeoTypes,
  i_LanguageManager,
  i_CoordConverterFactory,
  i_VectorItemLonLat,
  i_MapTypes,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  i_LocalCoordConverterFactorySimpe,
  i_BitmapPostProcessingConfig,
  i_UsedMarksConfig,
  i_MarksDrawConfig,
  i_MapCalibration,
  i_VectorItmesFactory,
  i_GlobalViewMainConfig,
  u_ExportProviderAbstract,
  u_MarksSystem,
  fr_MapCombine;

type
  TProviderMapCombine = class(TExportProviderAbstract)
  private
    FFrame: TfrMapCombine;
    FViewConfig: IGlobalViewMainConfig;
    FProjectionFactory: IProjectionInfoFactory;
    FVectorItmesFactory: IVectorItmesFactory;
    FMarksDB: TMarksSystem;
    FMarksShowConfig: IUsedMarksConfig;
    FMarksDrawConfig: IMarksDrawConfig;
    FLocalConverterFactory: ILocalCoordConverterFactorySimpe;
    FBitmapPostProcessingConfig: IBitmapPostProcessingConfig;
    FMapCalibrationList: IMapCalibrationList;
  public
    constructor Create(
      AParent: TWinControl;
      ALanguageManager: ILanguageManager;
      AMainMapsConfig: IMainMapsConfig;
      AFullMapsSet: IMapTypeSet;
      AGUIConfigList: IMapTypeGUIConfigList;
      AViewConfig: IGlobalViewMainConfig;
      AProjectionFactory: IProjectionInfoFactory;
      AVectorItmesFactory: IVectorItmesFactory;
      AMarksShowConfig: IUsedMarksConfig;
      AMarksDrawConfig: IMarksDrawConfig;
      AMarksDB: TMarksSystem;
      ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
      ABitmapPostProcessingConfig: IBitmapPostProcessingConfig;
      AMapCalibrationList: IMapCalibrationList
    );
    destructor Destroy; override;
    function GetCaption: string; override;
    procedure InitFrame(Azoom: byte; APolygon: ILonLatPolygon); override;
    procedure Show; override;
    procedure Hide; override;
    procedure RefreshTranslation; override;
    procedure StartProcess(APolygon: ILonLatPolygon); override;
  end;


implementation

uses
  Classes,
  SysUtils,
  gnugettext,
  i_MarksSimple,
  i_CoordConverter,
  i_VectorItemProjected,
  i_BitmapLayerProvider,
  i_ProjectionInfo,
  u_ProjectionInfo,
  u_GeoFun,
  u_IdCacheSimpleThreadSafe,
  u_MapMarksBitmapLayerProviderByMarksSubset,
  u_ThreadMapCombineBMP,
  u_ThreadMapCombineECW,
  u_ThreadMapCombineJPG,
  u_ThreadMapCombineKMZ,
  u_ThreadMapCombinePNG,
  u_ResStrings,
  u_MapType;

{ TProviderTilesDelete }

constructor TProviderMapCombine.Create(
  AParent: TWinControl;
  ALanguageManager: ILanguageManager;
  AMainMapsConfig: IMainMapsConfig;
  AFullMapsSet: IMapTypeSet;
  AGUIConfigList: IMapTypeGUIConfigList;
  AViewConfig: IGlobalViewMainConfig;
  AProjectionFactory: IProjectionInfoFactory;
  AVectorItmesFactory: IVectorItmesFactory;
  AMarksShowConfig: IUsedMarksConfig;
  AMarksDrawConfig: IMarksDrawConfig;
  AMarksDB: TMarksSystem;
  ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
  ABitmapPostProcessingConfig: IBitmapPostProcessingConfig;
  AMapCalibrationList: IMapCalibrationList
);
begin
  inherited Create(AParent, ALanguageManager, AMainMapsConfig, AFullMapsSet, AGUIConfigList);
  FMapCalibrationList := AMapCalibrationList;
  FViewConfig := AViewConfig;
  FMarksShowConfig := AMarksShowConfig;
  FMarksDrawConfig := AMarksDrawConfig;
  FMarksDB := AMarksDB;
  FLocalConverterFactory := ALocalConverterFactory;
  FBitmapPostProcessingConfig := ABitmapPostProcessingConfig;
  FProjectionFactory := AProjectionFactory;
  FVectorItmesFactory := AVectorItmesFactory;
end;

destructor TProviderMapCombine.Destroy;
begin
  FreeAndNil(FFrame);
  inherited;
end;

function TProviderMapCombine.GetCaption: string;
begin
  Result := SAS_STR_OperationMapCombineCaption;
end;

procedure TProviderMapCombine.InitFrame(Azoom: byte; APolygon: ILonLatPolygon);
begin
  if FFrame = nil then begin
    FFrame := TfrMapCombine.Create(
      nil,
      FProjectionFactory,
      FVectorItmesFactory,
      Self.MainMapsConfig,
      Self.FullMapsSet,
      Self.GUIConfigList,
      FMapCalibrationList
    );
    FFrame.Visible := False;
    FFrame.Parent := Self.Parent;
  end;
  FFrame.Init(Azoom, APolygon);
end;

procedure TProviderMapCombine.RefreshTranslation;
begin
  inherited;
  if FFrame <> nil then begin
    FFrame.RefreshTranslation;
  end;
end;

procedure TProviderMapCombine.Hide;
begin
  inherited;
  if FFrame <> nil then begin
    if FFrame.Visible then begin
      FFrame.Hide;
    end;
  end;
end;

procedure TProviderMapCombine.Show;
begin
  inherited;
  if FFrame <> nil then begin
    if not FFrame.Visible then begin
      FFrame.Show;
    end;
  end;
end;

procedure TProviderMapCombine.StartProcess(APolygon: ILonLatPolygon);
var
  Amt,Hmt:TMapType;
  i:integer;
  VPrTypes: IInterfaceList;
  VFileName: string;
  VSplitCount: TPoint;
  VFileExt: string;
  VMarksSubset: IMarksSubset;
  VLonLatRect: TDoubleRect;
  VMarksConfigStatic: IUsedMarksConfigStatic;
  VZoom: Byte;
  VList: IInterfaceList;
  VMarksImageProvider: IBitmapLayerProvider;
  VMainMapType: TMapType;
  VGeoConverter: ICoordConverter;
  VProjection: IProjectionInfo;
  VProjectedPolygon: IProjectedPolygon;
  VPolygonLine: IProjectedPolygonLine;
  VMapRect: TDoubleRect;
  VLineClipRect: TDoubleRect;
begin
  Amt:=TMapType(FFrame.cbbMap.Items.Objects[FFrame.cbbMap.ItemIndex]);
  Hmt:=TMapType(FFrame.cbbHybr.Items.Objects[FFrame.cbbHybr.ItemIndex]);

  if Amt <> nil then begin
    VMainMapType := Amt;
  end else if Hmt <> nil then begin
    VMainMapType := Hmt;
  end else begin
    raise Exception.Create( _('No one Map or Layer are selected!') );
  end;

  VFileName := FFrame.edtTargetFile.Text;
  if VFileName = '' then begin
    raise Exception.Create( _('Please, select output file first!') );
  end;

  VGeoConverter := VMainMapType.GeoConvert;

  VZoom := FFrame.cbbZoom.ItemIndex;
  VGeoConverter.CheckZoom(VZoom);

  VProjection := TProjectionInfo.Create(VGeoConverter, VZoom);

  VLonLatRect := APolygon.Item[0].Bounds;
  VGeoConverter.CheckLonLatRect(VLonLatRect);

  VProjectedPolygon :=
    FVectorItmesFactory.CreateProjectedPolygonByLonLatPolygon(
      VProjection,
      APolygon
    );
  VPolygonLine := VProjectedPolygon.Item[0];
  GetMinMax(VMapRect, VPolygonLine.Points, VPolygonLine.Count);

  VPrTypes := TInterfaceList.Create;
  for i:=0 to FFrame.chklstPrTypes.Items.Count-1 do begin
    if FFrame.chklstPrTypes.Checked[i] then begin
      VPrTypes.Add(IInterface(Pointer(FFrame.chklstPrTypes.Items.Objects[i])));
    end;
  end;
  VSplitCount.X := FFrame.seSplitHor.Value;
  VSplitCount.Y := FFrame.seSplitVert.Value;
  VFileExt := UpperCase(ExtractFileExt(VFileName));
  VMarksSubset := nil;
  if FFrame.chkUseMapMarks.Checked then begin
    VMarksConfigStatic := FMarksShowConfig.GetStatic;
    if VMarksConfigStatic.IsUseMarks then begin
      VList := nil;
      if not VMarksConfigStatic.IgnoreCategoriesVisible then begin
        VList := FMarksDB.GetVisibleCategories(VZoom);
      end;
      try
        if (VList <> nil) and (VList.Count = 0) then begin
          VMarksSubset := nil;
        end else begin
          VMarksSubset := FMarksDB.MarksDb.GetMarksSubset(VLonLatRect, VList, VMarksConfigStatic.IgnoreMarksVisible);
        end;
      finally
        VList := nil;
      end;
    end;
  end else begin
    VMarksSubset := nil;
  end;
  VMarksImageProvider := nil;
  if VMarksSubset <> nil then begin
    VLineClipRect.Left := VMapRect.Left - 10;
    VLineClipRect.Top := VMapRect.Top - 10;
    VLineClipRect.Right := VMapRect.Right + 10;
    VLineClipRect.Bottom := VMapRect.Bottom + 10;
    VMarksImageProvider :=
      TMapMarksBitmapLayerProviderByMarksSubset.Create(
        FMarksDrawConfig.GetStatic,
        FVectorItmesFactory,
        VProjection,
        TIdCacheSimpleThreadSafe.Create,
        VLineClipRect,
        VMarksSubset
      );
  end;
  if (VFileExt='.ECW')or(VFileExt='.JP2') then begin
    TThreadMapCombineECW.Create(
      FViewConfig,
      VMarksImageProvider,
      FLocalConverterFactory,
      VPrTypes,
      VFileName,
      APolygon.Item[0],
      VPolygonLine,
      VSplitCount,
      VZoom,
      Amt,Hmt,
      FFrame.chkUseRecolor.Checked,
      FBitmapPostProcessingConfig.GetStatic,
      FFrame.seJpgQuality.Value
    );
  end else if (VFileExt='.BMP') then begin
    TThreadMapCombineBMP.Create(
      FViewConfig,
      VMarksImageProvider,
      FLocalConverterFactory,
      VPrTypes,
      VFileName,
      APolygon.Item[0],
      VPolygonLine,
      VSplitCount,
      VZoom,
      Amt,Hmt,
      FFrame.chkUseRecolor.Checked,
      FBitmapPostProcessingConfig.GetStatic
    );
  end else if (VFileExt='.KMZ') then begin
    TThreadMapCombineKMZ.Create(
      FViewConfig,
      VMarksImageProvider,
      FLocalConverterFactory,
      VPrTypes,
      VFileName,
      APolygon.Item[0],
      VPolygonLine,
      VSplitCount,
      VZoom,
      Amt,Hmt,
      FFrame.chkUseRecolor.Checked,
      FBitmapPostProcessingConfig.GetStatic,
      FFrame.seJpgQuality.Value
    );
  end else if (VFileExt='.JPG') then begin
    TThreadMapCombineJPG.Create(
      FViewConfig,
      VMarksImageProvider,
      FLocalConverterFactory,
      VPrTypes,
      VFileName,
      APolygon.Item[0],
      VPolygonLine,
      VSplitCount,
      VZoom,
      Amt,Hmt,
      FFrame.chkUseRecolor.Checked,
      FBitmapPostProcessingConfig.GetStatic,
      FFrame.seJpgQuality.Value
    );
  end else if (VFileExt='.PNG') then begin
    TThreadMapCombinePNG.Create(
      FViewConfig,
      VMarksImageProvider,
      FLocalConverterFactory,
      VPrTypes,
      VFileName,
      APolygon.Item[0],
      VPolygonLine,
      VSplitCount,
      VZoom,
      Amt,Hmt,
      FFrame.chkUseRecolor.Checked,
      FBitmapPostProcessingConfig.GetStatic,
      FFrame.chkPngWithAlpha.Checked
    );
  end;
end;

end.

