unit fr_MapCombine;

interface

uses
  Windows,
  SysUtils,
  Classes,
  Controls,
  Forms,
  Dialogs,
  ExtCtrls,
  StdCtrls,
  CheckLst,
  Spin,
  i_LanguageManager,
  i_MapTypes,
  i_CoordConverterFactory,
  i_VectorItemLonLat,
  i_VectorItmesFactory,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  i_MapCalibration,
  i_RegionProcessParamsFrame,
  i_ProjectionInfo,
  i_BitmapLayerProvider,
  u_CommonFormAndFrameParents;

type
  IRegionProcessParamsFrameMapCombine = interface(IRegionProcessParamsFrameBase)
    ['{6771DEDD-F33C-4152-B4AB-47E6A0B032E1}']
    function GetUseMarks: Boolean;
    property UseMarks: Boolean read GetUseMarks;

    function GetUseRecolor: Boolean;
    property UseRecolor: Boolean read GetUseRecolor;

    function GetSplitCount: TPoint;
    property SplitCount: TPoint read GetSplitCount;
  end;

  IRegionProcessParamsFrameMapCombineJpg = interface(IRegionProcessParamsFrameBase)
    ['{C9A0712B-4456-4F34-8A06-9BA57F21D40A}']
    function GetQuality: Integer;
    property Quality: Integer read GetQuality;
  end;

  IRegionProcessParamsFrameMapCombineWithAlfa = interface(IRegionProcessParamsFrameBase)
    ['{45043DE9-9950-40C9-92E2-0D28FEC341C7}']
    function GetIsSaveAlfa: Boolean;
    property IsSaveAlfa: Boolean read GetIsSaveAlfa;
  end;

type
  TfrMapCombine = class(
      TFrame,
      IRegionProcessParamsFrameBase,
      IRegionProcessParamsFrameImageProvider,
      IRegionProcessParamsFrameMapCalibrationList,
      IRegionProcessParamsFrameTargetProjection,
      IRegionProcessParamsFrameTargetPath,
      IRegionProcessParamsFrameMapCombine,
      IRegionProcessParamsFrameMapCombineJpg,
      IRegionProcessParamsFrameMapCombineWithAlfa
    )
    pnlTop: TPanel;
    pnlTargetFile: TPanel;
    lblTargetFile: TLabel;
    edtTargetFile: TEdit;
    btnSelectTargetFile: TButton;
    pnlOutputFormat: TPanel;
    cbbOutputFormat: TComboBox;
    lblOutputFormat: TLabel;
    dlgSaveTargetFile: TSaveDialog;
    pnlSplit: TPanel;
    grpSplit: TGroupBox;
    lblSplitHor: TLabel;
    lblSplitVert: TLabel;
    seSplitHor: TSpinEdit;
    seSplitVert: TSpinEdit;
    pnlCenter: TPanel;
    pnlMapSource: TPanel;
    lblMap: TLabel;
    cbbMap: TComboBox;
    pnlZoom: TPanel;
    lblZoom: TLabel;
    cbbZoom: TComboBox;
    lblHybr: TLabel;
    cbbHybr: TComboBox;
    pnlOptions: TPanel;
    chkUseMapMarks: TCheckBox;
    chkUseRecolor: TCheckBox;
    flwpnlJpegQuality: TFlowPanel;
    lblJpgQulity: TLabel;
    pnlPrTypes: TPanel;
    lblPrTypes: TLabel;
    chklstPrTypes: TCheckListBox;
    seJpgQuality: TSpinEdit;
    lblStat: TLabel;
    pnlBottom: TPanel;
    pnlCenterMain: TPanel;
    chkPngWithAlpha: TCheckBox;
    procedure cbbOutputFormatChange(Sender: TObject);
    procedure cbbZoomChange(Sender: TObject);
    procedure btnSelectTargetFileClick(Sender: TObject);
  private
    FVectorFactory: IVectorItmesFactory;
    FProjectionFactory: IProjectionInfoFactory;
    FMainMapsConfig: IMainMapsConfig;
    FFullMapsSet: IMapTypeSet;
    FGUIConfigList: IMapTypeGUIConfigList;
    FMapCalibrationList: IMapCalibrationList;
    FPolygLL: ILonLatPolygon;
    procedure UpdatePanelSizes;
  private
    procedure Init(
      const AZoom: byte;
      const APolygon: ILonLatPolygon
    );
  private
    function GetProvider: IBitmapLayerProvider;
    function GetPath: string;
    function GetProjection: IProjectionInfo;
    function GetMapCalibrationList: IMapCalibrationList;
  private
    function GetUseMarks: Boolean;
    function GetUseRecolor: Boolean;
    function GetSplitCount: TPoint;
    function GetQuality: Integer;
    function GetIsSaveAlfa: Boolean;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AProjectionFactory: IProjectionInfoFactory;
      const AVectorFactory: IVectorItmesFactory;
      const AMainMapsConfig: IMainMapsConfig;
      const AFullMapsSet: IMapTypeSet;
      const AGUIConfigList: IMapTypeGUIConfigList;
      const AMapCalibrationList: IMapCalibrationList
    ); reintroduce;
    procedure RefreshTranslation; override;
  end;

implementation

uses
  t_GeoTypes,
  i_GUIDListStatic,
  i_VectorItemProjected,
  i_CoordConverter,
  u_GeoFun,
  u_BitmapLayerProviderMapWithLayer,
  u_MapCalibrationListBasic,
  u_ResStrings,
  u_MapType;

{$R *.dfm}

{ TfrMapCombine }

procedure TfrMapCombine.btnSelectTargetFileClick(Sender: TObject);
begin
  if dlgSaveTargetFile.Execute then begin
    edtTargetFile.Text := dlgSaveTargetFile.FileName;
  end;
end;

procedure TfrMapCombine.cbbOutputFormatChange(Sender: TObject);
var
  VNewExt: string;
  VFileName: string;
begin
  case cbbOutputFormat.ItemIndex of
    0: VNewExt := 'ecw';
    1: VNewExt := 'bmp';
    2: VNewExt := 'kmz';
    3: VNewExt := 'jpg';
    4: VNewExt := 'jp2';
    5: VNewExt := 'png';
  else
    VNewExt := '';
  end;
  VFileName := edtTargetFile.Text;
  if VFileName <> '' then begin
    VFileName := ChangeFileExt(VFileName, '.' + VNewExt);
  end;
  edtTargetFile.Text := VFileName;
  dlgSaveTargetFile.DefaultExt := VNewExt;
  dlgSaveTargetFile.Filter := cbbOutputFormat.Items[cbbOutputFormat.ItemIndex] + ' | *.' + VNewExt;
end;

procedure TfrMapCombine.cbbZoomChange(Sender: TObject);
var
  numd:int64 ;
  Vmt: TMapType;
  VZoom: byte;
  VPolyLL: ILonLatPolygon;
  VProjected: IProjectedPolygon;
  VLine: IProjectedPolygonLine;
  VBounds: TDoubleRect;
  VPixelRect: TRect;
  VTileRect: TRect;
begin
  if cbbMap.ItemIndex >= 0 then begin
    Vmt := TMapType(cbbMap.Items.Objects[cbbMap.ItemIndex]);
  end else begin
    Vmt := nil;
  end;

  if Vmt <> nil then begin
    VZoom := cbbZoom.ItemIndex;
    Vmt.GeoConvert.CheckZoom(VZoom);
    VPolyLL := FPolygLL;
    if VPolyLL <> nil then begin
      VProjected :=
        FVectorFactory.CreateProjectedPolygonByLonLatPolygon(
          FProjectionFactory.GetByConverterAndZoom(Vmt.GeoConvert, VZoom),
          VPolyLL
        );
      if VProjected.Count > 0 then begin
        VLine := VProjected.Item[0];
        VBounds := VLine.Bounds;
        VPixelRect := RectFromDoubleRect(VBounds, rrOutside);
        VTileRect := Vmt.GeoConvert.PixelRect2TileRect(VPixelRect, VZoom);
        numd := (VTileRect.Right - VTileRect.Left);
        numd := numd * (VTileRect.Bottom - VTileRect.Top);
        lblStat.Caption :=
          SAS_STR_filesnum+': '+
          inttostr(VTileRect.Right - VTileRect.Left)+'x'+
          inttostr(VTileRect.Bottom - VTileRect.Top)+
          '('+inttostr(numd)+')' +
          ', '+SAS_STR_Resolution + ' ' +
          inttostr(VPixelRect.Right - VPixelRect.Left)+'x'+
          inttostr(VPixelRect.Bottom - VPixelRect.Top);
      end;
    end;
  end;
end;

constructor TfrMapCombine.Create(
  const ALanguageManager: ILanguageManager;
  const AProjectionFactory: IProjectionInfoFactory;
  const AVectorFactory: IVectorItmesFactory;
  const AMainMapsConfig: IMainMapsConfig;
  const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList;
  const AMapCalibrationList: IMapCalibrationList
);
begin
  inherited Create(ALanguageManager);
  FProjectionFactory := AProjectionFactory;
  FVectorFactory := AVectorFactory;
  FMainMapsConfig := AMainMapsConfig;
  FFullMapsSet := AFullMapsSet;
  FGUIConfigList := AGUIConfigList;
  FMapCalibrationList := AMapCalibrationList;
  cbbOutputFormat.ItemIndex := 0;
  UpdatePanelSizes;
end;

function TfrMapCombine.GetIsSaveAlfa: Boolean;
begin
  Result := chkPngWithAlpha.Checked;
end;

function TfrMapCombine.GetMapCalibrationList: IMapCalibrationList;
var
  i: Integer;
  VList: IInterfaceList;
begin
  VList := TInterfaceList.Create;
  for i := 0 to chklstPrTypes.Items.Count - 1 do begin
    if chklstPrTypes.Checked[i] then begin
      VList.Add(IMapCalibration(Pointer(chklstPrTypes.Items.Objects[i])));
    end;
  end;
  Result := TMapCalibrationListByInterfaceList.Create(VList);
end;

function TfrMapCombine.GetPath: string;
begin
  Result := edtTargetFile.Text;
end;

function TfrMapCombine.GetProjection: IProjectionInfo;
var
  VMap: TMapType;
  VLayer: TMapType;
  VMainMapType: TMapType;
  VZoom: Byte;
  VGeoConverter: ICoordConverter;
begin
  Result := nil;
  VMap := nil;
  VMainMapType := nil;
  if cbbMap.ItemIndex >= 0 then begin
    VMap := TMapType(cbbMap.Items.Objects[cbbMap.ItemIndex]);
  end;
  VLayer := nil;
  if cbbHybr.ItemIndex >= 0 then begin
    VLayer := TMapType(cbbHybr.Items.Objects[cbbHybr.ItemIndex]);
  end;

  if VMap <> nil then begin
    VMainMapType := VMap;
  end else if VLayer <> nil then begin
    VMainMapType := VLayer;
  end;
  if VMainMapType <> nil then begin
    VGeoConverter := VMainMapType.GeoConvert;
    VZoom := cbbZoom.ItemIndex;
    Result := FProjectionFactory.GetByConverterAndZoom(VGeoConverter, VZoom);
  end;
end;

function TfrMapCombine.GetProvider: IBitmapLayerProvider;
var
  VMap: TMapType;
  VLayer: TMapType;
begin
  VMap := TMapType(cbbMap.Items.Objects[cbbMap.ItemIndex]);
  VLayer := TMapType(cbbHybr.Items.Objects[cbbHybr.ItemIndex]);

  Result :=
    TBitmapLayerProviderMapWithLayer.Create(
      VMap,
      VLayer,
      True,
      True
    );
end;

function TfrMapCombine.GetQuality: Integer;
begin
  Result := seJpgQuality.Value;
end;

function TfrMapCombine.GetSplitCount: TPoint;
begin
  Result.X := seSplitHor.Value;
  Result.Y := seSplitVert.Value;
end;

function TfrMapCombine.GetUseMarks: Boolean;
begin
  Result := chkUseMapMarks.Checked;
end;

function TfrMapCombine.GetUseRecolor: Boolean;
begin
  Result := chkUseRecolor.Checked;
end;

procedure TfrMapCombine.Init(
  const AZoom: byte;
  const APolygon: ILonLatPolygon
);
var
  i: Integer;
  VMapType: TMapType;
  VActiveMapGUID: TGUID;
  VAddedIndex: Integer;
  VMapCalibration: IMapCalibration;
  VGUIDList: IGUIDListStatic;
  VGUID: TGUID;
begin
  FPolygLL := APolygon;
  cbbZoom.Items.Clear;
  for i:=1 to 24 do begin
    cbbZoom.Items.Add(inttostr(i));
  end;
  cbbZoom.ItemIndex := AZoom;

  VActiveMapGUID := FMainMapsConfig.GetActiveMap.GetSelectedGUID;
  cbbMap.Items.Clear;
  cbbHybr.Items.Clear;
  cbbMap.Items.Add(SAS_STR_No);
  cbbHybr.Items.Add(SAS_STR_No);
  VGUIDList := FGUIConfigList.OrderedMapGUIDList;
  For i := 0 to VGUIDList.Count-1 do begin
    VGUID := VGUIDList.Items[i];
    VMapType := FFullMapsSet.GetMapTypeByGUID(VGUID).MapType;
    if (VMapType.Abilities.IsUseStick)and(VMapType.IsBitmapTiles)and(VMapType.GUIConfig.Enabled) then begin
      if not VMapType.Abilities.IsLayer then begin
        VAddedIndex := cbbMap.Items.AddObject(VMapType.GUIConfig.Name.Value, VMapType);
        if IsEqualGUID(VMapType.Zmp.GUID, VActiveMapGUID) then begin
          cbbMap.ItemIndex:=VAddedIndex;
        end;
      end else begin
        VAddedIndex := cbbHybr.Items.AddObject(VMapType.GUIConfig.Name.Value, VMapType);
        if (cbbHybr.ItemIndex=-1) then begin
          if FMainMapsConfig.GetActiveLayersSet.IsGUIDSelected(VGUID) then begin
            cbbHybr.ItemIndex:=VAddedIndex;
          end;
        end;
      end;
    end;
  end;
  if (cbbMap.Items.Count > 0) and (cbbMap.ItemIndex < 0) then begin
    cbbMap.ItemIndex := 0;
  end;
  if (cbbHybr.Items.Count > 0) and (cbbHybr.ItemIndex < 0) then begin
    cbbHybr.ItemIndex := 0;
  end;

  chklstPrTypes.Clear;
  for i := 0 to FMapCalibrationList.Count - 1 do begin
    VMapCalibration := FMapCalibrationList.Get(i);
    chklstPrTypes.AddItem(VMapCalibration.GetName, Pointer(VMapCalibration));
  end;
  cbbOutputFormatChange(cbbOutputFormat);
  cbbZoomChange(nil);
  UpdatePanelSizes;
end;

procedure TfrMapCombine.RefreshTranslation;
var
  i: Integer;
begin
  i := cbbOutputFormat.ItemIndex;
  inherited;
  cbbOutputFormat.ItemIndex := i;
  UpdatePanelSizes;
end;

procedure TfrMapCombine.UpdatePanelSizes;
begin
  pnlCenter.ClientHeight := pnlMapSource.Height;
end;

end.
