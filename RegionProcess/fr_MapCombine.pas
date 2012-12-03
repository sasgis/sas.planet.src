unit fr_MapCombine;

interface

uses
  SysUtils,
  Classes,
  Controls,
  Forms,
  Dialogs,
  ExtCtrls,
  StdCtrls,
  CheckLst,
  Spin,
  GR32,
  i_LanguageManager,
  i_MapTypes,
  i_CoordConverterFactory,
  i_CoordConverterList,
  i_VectorItemLonLat,
  i_VectorItmesFactory,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  i_MapCalibration,
  i_UseTilePrevZoomConfig,
  i_GlobalViewMainConfig,
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

    function GetBGColor: TColor32;
    property BGColor: TColor32 read GetBGColor;
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
    pnlTargetFile: TPanel;
    lblTargetFile: TLabel;
    edtTargetFile: TEdit;
    btnSelectTargetFile: TButton;
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
    pnlProjection: TPanel;
    lblProjection: TLabel;
    cbbProjection: TComboBox;
    procedure cbbZoomChange(Sender: TObject);
    procedure btnSelectTargetFileClick(Sender: TObject);
  private
    FVectorFactory: IVectorItemsFactory;
    FProjectionFactory: IProjectionInfoFactory;
    FCoordConverterList: ICoordConverterList;
    FMainMapsConfig: IMainMapsConfig;
    FFullMapsSet: IMapTypeSet;
    FGUIConfigList: IMapTypeGUIConfigList;
    FMapCalibrationList: IMapCalibrationList;
    FUseTilePrevZoomConfig: IUseTilePrevZoomConfig;
    FPolygLL: ILonLatPolygon;
    FViewConfig: IGlobalViewMainConfig;
    FUseQuality: Boolean;
    FUseAlfa: Boolean;
    FDefaultExt: string;
    FFormatName: string;
    procedure UpdatePanelSizes;
    procedure UpdateProjectionsList;
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
    function GetBGColor: TColor32;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AProjectionFactory: IProjectionInfoFactory;
      const ACoordConverterList: ICoordConverterList;
      const AVectorFactory: IVectorItemsFactory;
      const AMainMapsConfig: IMainMapsConfig;
      const AFullMapsSet: IMapTypeSet;
      const AGUIConfigList: IMapTypeGUIConfigList;
      const AViewConfig: IGlobalViewMainConfig;
      const AUseTilePrevZoomConfig: IUseTilePrevZoomConfig;
      const AMapCalibrationList: IMapCalibrationList;
      const AUseQuality: Boolean;
      const AUseAlfa: Boolean;
      const ADefaultExt: string;
      const AFormatName: string
    ); reintroduce;
    procedure RefreshTranslation; override;
  end;

implementation

uses
  gnugettext,
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

constructor TfrMapCombine.Create(
  const ALanguageManager: ILanguageManager;
  const AProjectionFactory: IProjectionInfoFactory;
  const ACoordConverterList: ICoordConverterList;
  const AVectorFactory: IVectorItemsFactory;
  const AMainMapsConfig: IMainMapsConfig;
  const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList;
  const AViewConfig: IGlobalViewMainConfig;
  const AUseTilePrevZoomConfig: IUseTilePrevZoomConfig;
  const AMapCalibrationList: IMapCalibrationList;
  const AUseQuality: Boolean;
  const AUseAlfa: Boolean;
  const ADefaultExt: string;
  const AFormatName: string
);
begin
  inherited Create(ALanguageManager);
  FProjectionFactory := AProjectionFactory;
  FCoordConverterList := ACoordConverterList;
  FVectorFactory := AVectorFactory;
  FMainMapsConfig := AMainMapsConfig;
  FFullMapsSet := AFullMapsSet;
  FGUIConfigList := AGUIConfigList;
  FMapCalibrationList := AMapCalibrationList;
  FViewConfig := AViewConfig;
  FUseTilePrevZoomConfig := AUseTilePrevZoomConfig;
  FUseQuality := AUseQuality;
  FUseAlfa := AUseAlfa;
  FDefaultExt := ADefaultExt;
  FFormatName := AFormatName;
  chkPngWithAlpha.Visible := FUseAlfa;
  flwpnlJpegQuality.Visible := FUseQuality;
  UpdateProjectionsList;
  UpdatePanelSizes;
end;

procedure TfrMapCombine.btnSelectTargetFileClick(Sender: TObject);
begin
  dlgSaveTargetFile.DefaultExt := FDefaultExt;
  dlgSaveTargetFile.Filter := _(FFormatName) + ' | *.' + FDefaultExt;
  if dlgSaveTargetFile.Execute then begin
    edtTargetFile.Text := dlgSaveTargetFile.FileName;
  end;
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

function TfrMapCombine.GetBGColor: TColor32;
var
  VMap: TMapType;
begin
  VMap := nil;
  if cbbMap.ItemIndex >= 0 then begin
    VMap := TMapType(cbbMap.Items.Objects[cbbMap.ItemIndex]);
  end;
  if VMap = nil then begin
    Result := SetAlpha(FViewConfig.BackGroundColor, 0);
  end else begin
    Result := SetAlpha(FViewConfig.BackGroundColor, 255);
  end;
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
  VIndex: Integer;
begin
  Result := nil;
  VGeoConverter := nil;
  VIndex := cbbProjection.ItemIndex;
  if VIndex < 0 then begin
    VIndex := 0;
  end;
  if VIndex >= 2 then begin
    VIndex := VIndex - 2;
    if VIndex < FCoordConverterList.Count then begin
      VGeoConverter := FCoordConverterList.Items[VIndex];
    end;
    VIndex := 0;
  end;
  if VGeoConverter = nil then begin
    VMainMapType := nil;

    VMap := nil;
    if cbbMap.ItemIndex >= 0 then begin
      VMap := TMapType(cbbMap.Items.Objects[cbbMap.ItemIndex]);
    end;
    VLayer := nil;
    if cbbHybr.ItemIndex >= 0 then begin
      VLayer := TMapType(cbbHybr.Items.Objects[cbbHybr.ItemIndex]);
    end;

    if VIndex = 0 then begin
      if VMap <> nil then begin
        VMainMapType := VMap;
      end else if VLayer <> nil then begin
        VMainMapType := VLayer;
      end;
    end else begin
      if VLayer <> nil then begin
        VMainMapType := VLayer;
      end else if VMap <> nil then begin
        VMainMapType := VMap;
      end;
    end;
    if VMainMapType <> nil then begin
      VGeoConverter := VMainMapType.GeoConvert;
    end;
  end;
  VZoom := cbbZoom.ItemIndex;
  if VGeoConverter <> nil then begin
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
      FUseTilePrevZoomConfig.UsePrevZoomAtMap,
      FUseTilePrevZoomConfig.UsePrevZoomAtLayer
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
  VActiveLayers: IMapTypeSet;
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

  VActiveMapGUID := FMainMapsConfig.GetActiveMap.GetStatic.GUID;
  VActiveLayers := FMainMapsConfig.GetActiveLayersSet.GetStatic;
  cbbMap.Items.Clear;
  cbbHybr.Items.Clear;
  cbbMap.Items.Add(SAS_STR_No);
  cbbHybr.Items.Add(SAS_STR_No);
  VGUIDList := FGUIConfigList.OrderedMapGUIDList;
  For i := 0 to VGUIDList.Count-1 do begin
    VGUID := VGUIDList.Items[i];
    VMapType := FFullMapsSet.GetMapTypeByGUID(VGUID).MapType;
    if (VMapType.IsBitmapTiles)and(VMapType.GUIConfig.Enabled) then begin
      if not VMapType.Abilities.IsLayer then begin
        VAddedIndex := cbbMap.Items.AddObject(VMapType.GUIConfig.Name.Value, VMapType);
        if IsEqualGUID(VMapType.Zmp.GUID, VActiveMapGUID) then begin
          cbbMap.ItemIndex:=VAddedIndex;
        end;
      end else begin
        VAddedIndex := cbbHybr.Items.AddObject(VMapType.GUIConfig.Name.Value, VMapType);
        if (cbbHybr.ItemIndex=-1) then begin
          if VActiveLayers.GetMapTypeByGUID(VGUID) <> nil then begin
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
  cbbZoomChange(nil);
  UpdatePanelSizes;
end;

procedure TfrMapCombine.RefreshTranslation;
var
  VProjectionIndex: Integer;
begin
  VProjectionIndex := cbbProjection.ItemIndex;
  inherited;
  UpdateProjectionsList;
  if VProjectionIndex >= 0 then begin
    cbbProjection.ItemIndex := VProjectionIndex;
  end;
  UpdatePanelSizes;
end;

procedure TfrMapCombine.UpdatePanelSizes;
begin
  pnlCenter.ClientHeight := pnlMapSource.Height;
end;

procedure TfrMapCombine.UpdateProjectionsList;
var
  i: Integer;
begin
  cbbProjection.Items.Clear;
  cbbProjection.Items.Add(_('Projection of map'));
  cbbProjection.Items.Add(_('Projection of layer'));
  for i := 0 to FCoordConverterList.Count - 1 do begin
    cbbProjection.Items.Add(_(FCoordConverterList.Captions[i]));
  end;
  cbbProjection.ItemIndex := 0;
end;

end.
