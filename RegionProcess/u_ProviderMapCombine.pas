unit u_ProviderMapCombine;

interface

uses
  Windows,
  Controls,
  t_GeoTypes,
  i_JclNotify,
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
    FAppClosingNotifier: IJclNotifier;
    FTimerNoifier: IJclNotifier;
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
      AAppClosingNotifier: IJclNotifier;
      ATimerNoifier: IJclNotifier;
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
  Forms,
  Classes,
  Dialogs,
  SysUtils,
  gnugettext,
  i_MarksSimple,
  i_CoordConverter,
  i_LocalCoordConverter,
  
  i_RegionProcessProgressInfo,
  i_VectorItemProjected,
  i_BitmapLayerProvider,
  i_ProjectionInfo,
  u_ProjectionInfo,
  u_GeoFun,
  u_IdCacheSimpleThreadSafe,
  u_BitmapLayerProviderByMarksSubset,
  u_BitmapLayerProviderSimpleForCombine,
  u_BitmapLayerProviderInPolygon,
  u_BitmapLayerProviderWithBGColor,
  u_ThreadMapCombineBMP,
  u_ThreadMapCombineECW,
  u_ThreadMapCombineJPG,
  u_ThreadMapCombineKMZ,
  u_ThreadMapCombinePNG,
  u_OperationNotifier,
  u_RegionProcessProgressInfo,
  u_ResStrings,
  u_MapType,
  frm_ProgressSimple;

{ TProviderTilesDelete }

constructor TProviderMapCombine.Create(
  AParent: TWinControl;
  ALanguageManager: ILanguageManager;
  AMainMapsConfig: IMainMapsConfig;
  AFullMapsSet: IMapTypeSet;
  AGUIConfigList: IMapTypeGUIConfigList;
  AViewConfig: IGlobalViewMainConfig;
  AAppClosingNotifier: IJclNotifier;
  ATimerNoifier: IJclNotifier;
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
  FAppClosingNotifier := AAppClosingNotifier;
  FTimerNoifier := ATimerNoifier;
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
  VMapRect: TRect;
  VLineClipRect: TDoubleRect;
  VTargetConverter: ILocalCoordConverter;
  VImageProvider: IBitmapLayerProvider;
  VRecolorConfig: IBitmapPostProcessingConfigStatic;
  VMapSize: TPoint;
  VMapPieceSize: TPoint;
  VKmzImgesCount: TPoint;
  VCancelNotifierInternal: IOperationNotifierInternal;
  VOperationID: Integer;
  VProgressInfo: IRegionProcessProgressInfo;
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

  VMapRect := RectFromDoubleRect(VProjectedPolygon.Bounds, rrOutside);
  VMapSize.X := VMapRect.Right - VMapRect.Left;
  VMapSize.Y := VMapRect.Bottom - VMapRect.Top;

  VTargetConverter :=
    FLocalConverterFactory.CreateConverterNoScale(
      Rect(0, 0, VMapSize.X, VMapSize.Y),
      VZoom,
      VGeoConverter,
      VMapRect.TopLeft
    );

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
      TBitmapLayerProviderByMarksSubset.Create(
        FMarksDrawConfig.GetStatic,
        FVectorItmesFactory,
        VProjection,
        TIdCacheSimpleThreadSafe.Create,
        VLineClipRect,
        VMarksSubset
      );
  end;

  VCancelNotifierInternal := TOperationNotifier.Create;
  VOperationID := VCancelNotifierInternal.CurrentOperation;
  VProgressInfo := TRegionProcessProgressInfo.Create;

  TfrmProgressSimple.Create(
    Application,
    FAppClosingNotifier,
    FTimerNoifier,
    VCancelNotifierInternal,
    VProgressInfo
  );

  VRecolorConfig := nil;
  if FFrame.chkUseRecolor.Checked then begin
    VRecolorConfig := FBitmapPostProcessingConfig.GetStatic;
  end;
  VImageProvider :=
    TBitmapLayerProviderWithBGColor.Create(
      FViewConfig.BackGroundColor,
      TBitmapLayerProviderInPolygon.Create(
        VProjectedPolygon,
        TBitmapLayerProviderSimpleForCombine.Create(
          VRecolorConfig,
          Amt,
          Hmt,
          VMarksImageProvider,
          True,
          True
        )
      )
    );
  if (VFileExt='.ECW')or(VFileExt='.JP2') then begin
    TThreadMapCombineECW.Create(
      VCancelNotifierInternal,
      VOperationID,
      VProgressInfo,
      APolygon,
      VTargetConverter,
      VImageProvider,
      FLocalConverterFactory,
      VPrTypes,
      VFileName,
      VSplitCount,
      FFrame.seJpgQuality.Value
    );
  end else if (VFileExt='.BMP') then begin
    TThreadMapCombineBMP.Create(
      VCancelNotifierInternal,
      VOperationID,
      VProgressInfo,
      APolygon,
      VTargetConverter,
      VImageProvider,
      FLocalConverterFactory,
      VPrTypes,
      VFileName,
      VSplitCount
    );
  end else if (VFileExt='.KMZ') then begin
    VMapPieceSize.X := VMapSize.X div VSplitCount.X;
    VMapPieceSize.Y := VMapSize.Y div VSplitCount.Y;
    VKmzImgesCount.X := ((VMapPieceSize.X-1) div 1024) + 1;
    VKmzImgesCount.Y := ((VMapPieceSize.Y-1) div 1024) + 1;
    if ((VKmzImgesCount.X * VKmzImgesCount.Y) > 100) then begin
      ShowMessage(SAS_MSG_GarminMax1Mp);
    end;

    TThreadMapCombineKMZ.Create(
      VCancelNotifierInternal,
      VOperationID,
      VProgressInfo,
      APolygon,
      VTargetConverter,
      VImageProvider,
      FLocalConverterFactory,
      VPrTypes,
      VFileName,
      VSplitCount,
      FFrame.seJpgQuality.Value
    );
  end else if (VFileExt='.JPG') then begin
    TThreadMapCombineJPG.Create(
      VCancelNotifierInternal,
      VOperationID,
      VProgressInfo,
      APolygon,
      VTargetConverter,
      VImageProvider,
      FLocalConverterFactory,
      VPrTypes,
      VFileName,
      VSplitCount,
      FFrame.seJpgQuality.Value
    );
  end else if (VFileExt='.PNG') then begin
    TThreadMapCombinePNG.Create(
      VCancelNotifierInternal,
      VOperationID,
      VProgressInfo,
      APolygon,
      VTargetConverter,
      VImageProvider,
      FLocalConverterFactory,
      VPrTypes,
      VFileName,
      VSplitCount,
      FFrame.chkPngWithAlpha.Checked
    );
  end;
end;

end.

