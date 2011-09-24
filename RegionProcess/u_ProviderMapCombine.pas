unit u_ProviderMapCombine;

interface

uses
  Windows,
  Controls,
  Forms,
  t_GeoTypes,
  i_MapTypes,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  i_LocalCoordConverterFactorySimpe,
  i_BitmapPostProcessingConfig,
  i_UsedMarksConfig,
  i_MarksDrawConfig,
  i_MapCalibration,
  i_GlobalViewMainConfig,
  u_ExportProviderAbstract,
  u_MarksSystem,
  fr_MapCombine;

type
  TProviderMapCombine = class(TExportProviderAbstract)
  private
    FFrame: TfrMapCombine;
    FViewConfig: IGlobalViewMainConfig;
    FMarksDB: TMarksSystem;
    FMarksShowConfig: IUsedMarksConfig;
    FMarksDrawConfig: IMarksDrawConfig;
    FLocalConverterFactory: ILocalCoordConverterFactorySimpe;
    FBitmapPostProcessingConfig: IBitmapPostProcessingConfig;
    FMapCalibrationList: IMapCalibrationList;
  public
    constructor Create(
      AParent: TWinControl;
      AMainMapsConfig: IMainMapsConfig;
      AFullMapsSet: IMapTypeSet;
      AGUIConfigList: IMapTypeGUIConfigList;
      AViewConfig: IGlobalViewMainConfig;
      AMarksShowConfig: IUsedMarksConfig;
      AMarksDrawConfig: IMarksDrawConfig;
      AMarksDB: TMarksSystem;
      ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
      ABitmapPostProcessingConfig: IBitmapPostProcessingConfig;
      AMapCalibrationList: IMapCalibrationList
    );
    destructor Destroy; override;
    function GetCaption: string; override;
    procedure InitFrame(Azoom: byte; APolygon: TArrayOfDoublePoint); override;
    procedure Show; override;
    procedure Hide; override;
    procedure RefreshTranslation; override;
    procedure StartProcess(APolygon: TArrayOfDoublePoint); override;
  end;


implementation

uses
  Classes,
  SysUtils,
  i_MarksSimple,
  i_BitmapLayerProvider,
  u_MapMarksBitmapLayerProviderByMarksSubset,
  u_GlobalState,
  u_ThreadMapCombineBMP,
  u_ThreadMapCombineECW,
  u_ThreadMapCombineJPG,
  u_ThreadMapCombineKMZ,
  u_ResStrings,
  u_MapType;

{ TProviderTilesDelete }

constructor TProviderMapCombine.Create(
  AParent: TWinControl;
  AMainMapsConfig: IMainMapsConfig;
  AFullMapsSet: IMapTypeSet;
  AGUIConfigList: IMapTypeGUIConfigList;
  AViewConfig: IGlobalViewMainConfig;
  AMarksShowConfig: IUsedMarksConfig;
  AMarksDrawConfig: IMarksDrawConfig;
  AMarksDB: TMarksSystem;
  ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
  ABitmapPostProcessingConfig: IBitmapPostProcessingConfig;
  AMapCalibrationList: IMapCalibrationList
);
begin
  inherited Create(AParent, AMainMapsConfig, AFullMapsSet, AGUIConfigList);
  FMapCalibrationList := AMapCalibrationList;
  FViewConfig := AViewConfig;
  FMarksShowConfig := AMarksShowConfig;
  FMarksDrawConfig := AMarksDrawConfig;
  FMarksDB := AMarksDB;
  FLocalConverterFactory := ALocalConverterFactory;
  FBitmapPostProcessingConfig := ABitmapPostProcessingConfig;
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

procedure TProviderMapCombine.InitFrame(Azoom: byte; APolygon: TArrayOfDoublePoint);
begin
  if FFrame = nil then begin
    FFrame := TfrMapCombine.Create(
      nil,
      FMainMapsConfig,
      FFullMapsSet,
      FGUIConfigList,
      FMapCalibrationList
    );
    FFrame.Visible := False;
    FFrame.Parent := FParent;
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

procedure TProviderMapCombine.StartProcess(APolygon: TArrayOfDoublePoint);
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
begin
  Amt:=TMapType(FFrame.cbbMap.Items.Objects[FFrame.cbbMap.ItemIndex]);
  Hmt:=TMapType(FFrame.cbbHybr.Items.Objects[FFrame.cbbHybr.ItemIndex]);
  VFileName := FFrame.edtTargetFile.Text;
  VPrTypes := TInterfaceList.Create;
  for i:=0 to FFrame.chklstPrTypes.Items.Count-1 do begin
    if FFrame.chklstPrTypes.Checked[i] then begin
      VPrTypes.Add(IInterface(Pointer(FFrame.chklstPrTypes.Items.Objects[i])));
    end;
  end;
  VSplitCount.X := FFrame.seSplitHor.Value;
  VSplitCount.Y := FFrame.seSplitVert.Value;
  VFileExt := UpperCase(ExtractFileExt(VFileName));
  VZoom := FFrame.cbbZoom.ItemIndex+1;
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
          VLonLatRect.TopLeft := APolygon[0];
          VLonLatRect.BottomRight := APolygon[0];
          for i := 1 to Length(APolygon) - 1 do begin
            if VLonLatRect.Left > APolygon[i].X then begin
              VLonLatRect.Left := APolygon[i].X;
            end;
            if VLonLatRect.Top < APolygon[i].Y then begin
              VLonLatRect.Top := APolygon[i].Y;
            end;
            if VLonLatRect.Right < APolygon[i].X then begin
              VLonLatRect.Right := APolygon[i].X;
            end;
            if VLonLatRect.Bottom > APolygon[i].Y then begin
              VLonLatRect.Bottom := APolygon[i].Y;
            end;
          end;
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
    VMarksImageProvider :=
      TMapMarksBitmapLayerProviderByMarksSubset.Create(
        FMarksDrawConfig.GetStatic,
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
      APolygon,
      VSplitCount,
      VZoom,
      Amt,Hmt,
      FFrame.chkUseRecolor.Checked,
      FBitmapPostProcessingConfig.GetStatic,
      GState.EcwDll,
      FFrame.seJpgQuality.Value
    );
  end else if (VFileExt='.BMP') then begin
    TThreadMapCombineBMP.Create(
      FViewConfig,
      VMarksImageProvider,
      GState.LocalConverterFactory,
      VPrTypes,
      VFileName,
      APolygon,
      VSplitCount,
      VZoom,
      Amt,Hmt,
      FFrame.chkUseRecolor.Checked,
      GState.BitmapPostProcessingConfig.GetStatic
    );
  end else if (VFileExt='.KMZ') then begin
    TThreadMapCombineKMZ.Create(
      FViewConfig,
      VMarksImageProvider,
      GState.LocalConverterFactory,
      VPrTypes,
      VFileName,
      APolygon,
      VSplitCount,
      VZoom,
      Amt,Hmt,
      FFrame.chkUseRecolor.Checked,
      GState.BitmapPostProcessingConfig.GetStatic,
      FFrame.seJpgQuality.Value
    );
  end else begin
    TThreadMapCombineJPG.Create(
      FViewConfig,
      VMarksImageProvider,
      GState.LocalConverterFactory,
      VPrTypes,
      VFileName,
      APolygon,
      VSplitCount,
      VZoom,
      Amt,Hmt,
      FFrame.chkUseRecolor.Checked,
      GState.BitmapPostProcessingConfig.GetStatic,
      FFrame.seJpgQuality.Value
    );
  end;
end;

end.

