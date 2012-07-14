unit fr_ExportToOgf2;

interface

uses
  Types,
  SysUtils,
  Classes,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  Spin,
  i_LanguageManager,
  i_MapTypes,
  i_BitmapTileSaveLoad,
  i_CoordConverterFactory,
  i_VectorItemLonLat,
  i_VectorItmesFactory,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  i_BitmapLayerProvider,
  i_BitmapTileSaveLoadFactory,
  i_RegionProcessParamsFrame,
  u_CommonFormAndFrameParents;

type
  IRegionProcessParamsFrameExportToOgf2 = interface(IRegionProcessParamsFrameBase)
    ['{CDF84DFB-9DD8-4F4D-B0B3-6D0D35B082F0}']
    function GetSaver: IBitmapTileSaver;
    property Saver: IBitmapTileSaver read GetSaver;

    function GetTileSize: TPoint;
    property TileSize: TPoint read GetTileSize;
  end;

type
  TfrExportToOgf2 = class(
      TFrame,
      IRegionProcessParamsFrameBase,
      IRegionProcessParamsFrameOneZoom,
      IRegionProcessParamsFrameImageProvider,
      IRegionProcessParamsFrameTargetPath,
      IRegionProcessParamsFrameExportToOgf2
    )
    pnlCenter: TPanel;
    lblMap: TLabel;
    cbbMap: TComboBox;
    pnlTop: TPanel;
    lblTargetFile: TLabel;
    edtTargetFile: TEdit;
    btnSelectTargetFile: TButton;
    dlgSaveTargetFile: TSaveDialog;
    pnlZoom: TPanel;
    lblZoom: TLabel;
    cbbZoom: TComboBox;
    lblStat: TLabel;
    lblHyb: TLabel;
    cbbHyb: TComboBox;
    cbbImageFormat: TComboBox;
    lblImageFormat: TLabel;
    lblTileRes: TLabel;
    cbbTileRes: TComboBox;
    chkUsePrevZoom: TCheckBox;
    lblJpgQulity: TLabel;
    seJpgQuality: TSpinEdit;
    pnlBottom: TPanel;
    procedure btnSelectTargetFileClick(Sender: TObject);
    procedure cbbZoomChange(Sender: TObject);
    procedure cbbTileResChange(Sender: TObject);
  private
    FVectorFactory: IVectorItmesFactory;
    FProjectionFactory: IProjectionInfoFactory;
    FBitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
    FMainMapsConfig: IMainMapsConfig;
    FFullMapsSet: IMapTypeSet;
    FGUIConfigList: IMapTypeGUIConfigList;
    FPolygLL: ILonLatPolygon;
  private
    procedure Init(
      const AZoom: byte;
      const APolygon: ILonLatPolygon
    );
  private
    function GetProvider: IBitmapLayerProvider;
    function GetZoom: Byte;
    function GetPath: string;
  private
    function GetSaver: IBitmapTileSaver;
    function GetTileSize: TPoint;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AProjectionFactory: IProjectionInfoFactory;
      const AVectorFactory: IVectorItmesFactory;
      const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
      const AMainMapsConfig: IMainMapsConfig;
      const AFullMapsSet: IMapTypeSet;
      const AGUIConfigList: IMapTypeGUIConfigList;
      const AFileFilters: string;
      const AFileExtDefault: string
    );
  end;

implementation

uses
  t_GeoTypes,
  i_GUIDListStatic,
  i_VectorItemProjected,
  u_GeoFun,
  u_BitmapLayerProviderMapWithLayer,
  u_MapType,
  u_ResStrings;

{$R *.dfm}

{ TfrExportToOgf2 }

procedure TfrExportToOgf2.btnSelectTargetFileClick(Sender: TObject);
begin
  if dlgSaveTargetFile.Execute then begin
    edtTargetFile.Text := dlgSaveTargetFile.FileName;
  end;
end;

procedure TfrExportToOgf2.cbbTileResChange(Sender: TObject);
begin
  cbbZoomChange(Sender);
end;

procedure TfrExportToOgf2.cbbZoomChange(Sender: TObject);
var
  VTilesCountRow: Int64;
  VTilesCountCol: Int64;
  VTilesCountTotal: Int64;
  VMapType: TMapType;
  VZoom: byte;
  VPolyLL: ILonLatPolygon;
  VProjected: IProjectedPolygon;
  VLine: IProjectedPolygonLine;
  VBounds: TDoubleRect;
  VPixelRect: TRect;
  VTileRect: TRect;
  VTileSize: Integer;
begin
  if cbbMap.ItemIndex >= 0 then begin
    VMapType := TMapType(cbbMap.Items.Objects[cbbMap.ItemIndex]);
  end else begin
    VMapType := nil;
  end;

  if cbbTileRes.ItemIndex > 0 then begin
    VTileSize := 256;
  end else begin
    VTileSize := 128;
  end;

  if VMapType <> nil then begin
    VZoom := cbbZoom.ItemIndex;
    VMapType.GeoConvert.CheckZoom(VZoom);
    VPolyLL := FPolygLL;
    if VPolyLL <> nil then begin
      VProjected :=
        FVectorFactory.CreateProjectedPolygonByLonLatPolygon(
          FProjectionFactory.GetByConverterAndZoom(VMapType.GeoConvert, VZoom),
          VPolyLL
        );
      if VProjected.Count > 0 then begin
        VLine := VProjected.Item[0];
        VBounds := VLine.Bounds;
        VPixelRect := RectFromDoubleRect(VBounds, rrOutside);
        VTileRect := VMapType.GeoConvert.PixelRect2TileRect(VPixelRect, VZoom);

        VTilesCountRow := (VTileRect.Right - VTileRect.Left) * (256 div VTileSize);
        VTilesCountCol := (VTileRect.Bottom - VTileRect.Top) * (256 div VTileSize);
        VTilesCountTotal := VTilesCountRow * VTilesCountCol;

        lblStat.Caption :=
          SAS_STR_filesnum + ': ' +
          IntToStr(VTilesCountRow) + 'x' +
          IntToStr(VTilesCountCol) +
          '(' + FloatToStrF(VTilesCountTotal, ffNumber, 12, 0) + ')' +
          ', ' + SAS_STR_Resolution + ' ' +
          IntToStr(VTilesCountRow * VTileSize) + 'x' +
          IntToStr(VTilesCountCol * VTileSize) + ' pix';
      end;
    end;
  end;
end;

constructor TfrExportToOgf2.Create(
  const ALanguageManager: ILanguageManager;
  const AProjectionFactory: IProjectionInfoFactory;
  const AVectorFactory: IVectorItmesFactory;
  const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
  const AMainMapsConfig: IMainMapsConfig;
  const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList;
  const AFileFilters: string;
  const AFileExtDefault: string
);
begin
  inherited Create(ALanguageManager);
  FProjectionFactory := AProjectionFactory;
  FVectorFactory := AVectorFactory;
  FBitmapTileSaveLoadFactory := ABitmapTileSaveLoadFactory;
  FMainMapsConfig := AMainMapsConfig;
  FFullMapsSet := AFullMapsSet;
  FGUIConfigList := AGUIConfigList;
  dlgSaveTargetFile.Filter := AFileFilters;
  dlgSaveTargetFile.DefaultExt := AFileExtDefault;
end;

function TfrExportToOgf2.GetPath: string;
begin
  Result := edtTargetFile.Text;
end;

function TfrExportToOgf2.GetProvider: IBitmapLayerProvider;
var
  VMap: TMapType;
  VLayer: TMapType;
  VUsePrevZoom: Boolean;
begin
  VMap := nil;
  if cbbMap.ItemIndex >= 0 then begin
    VMap := TMapType(cbbMap.Items.Objects[cbbMap.ItemIndex]);
  end;
  VLayer := nil;
  if cbbHyb.ItemIndex >= 0 then begin
    VLayer := TMapType(cbbHyb.Items.Objects[cbbHyb.ItemIndex]);
  end;

  VUsePrevZoom := chkUsePrevZoom.Checked;

  Result :=
    TBitmapLayerProviderMapWithLayer.Create(
      VMap,
      VLayer,
      VUsePrevZoom,
      VUsePrevZoom
    );
end;

function TfrExportToOgf2.GetSaver: IBitmapTileSaver;
var
  VJpegQuality: Byte;
begin
  case cbbImageFormat.ItemIndex of
    0: begin
      Result := FBitmapTileSaveLoadFactory.CreateBmpSaver;
    end;

    1: begin
      Result := FBitmapTileSaveLoadFactory.CreatePngSaver(i24bpp)
    end;
  else begin
    VJpegQuality := seJpgQuality.Value;
    Result := FBitmapTileSaveLoadFactory.CreateJpegSaver(VJpegQuality);
  end;
  end;
end;

function TfrExportToOgf2.GetTileSize: TPoint;
var
  VTileSize: Integer;
begin
  if cbbTileRes.ItemIndex > 0 then begin
    VTileSize := 256;
  end else begin
    VTileSize := 128;
  end;
  Result.X := VTileSize;
  Result.Y := VTileSize;
end;

function TfrExportToOgf2.GetZoom: Byte;
begin
  if cbbZoom.ItemIndex < 0 then begin
    cbbZoom.ItemIndex := 0;
  end;
  Result := cbbZoom.ItemIndex;
end;

procedure TfrExportToOgf2.Init(
  const AZoom: byte;
  const APolygon: ILonLatPolygon
);
var
  I: Integer;
  VMapType: TMapType;
  VActiveMapGUID: TGUID;
  VAddedIndex: Integer;
  VGUIDList: IGUIDListStatic;
  VGUID: TGUID;
begin
  FPolygLL := APolygon;

  cbbZoom.Items.Clear;
  cbbMap.Items.Clear;
  cbbHyb.Items.Clear;

  for I := 1 to 24 do begin
    cbbZoom.Items.Add(IntToStr(I));
  end;
  cbbZoom.ItemIndex := AZoom;

  cbbTileRes.ItemIndex := 0; // 128*128 pix
  cbbImageFormat.ItemIndex := 2; // JPEG

  cbbHyb.Items.AddObject(SAS_STR_No, nil);   
  VActiveMapGUID := FMainMapsConfig.GetActiveMap.GetSelectedGUID;
  VGUIDList := FGUIConfigList.OrderedMapGUIDList;
  for I := 0 to VGUIDList.Count-1 do begin
    VGUID := VGUIDList.Items[I];
    VMapType := FFullMapsSet.GetMapTypeByGUID(VGUID).MapType;
    if (VMapType.IsBitmapTiles) and (VMapType.GUIConfig.Enabled) then begin
      if (not(VMapType.Abilities.IsLayer)) then begin
        VAddedIndex := cbbMap.Items.AddObject(VMapType.GUIConfig.Name.Value,VMapType);
        if IsEqualGUID(VMapType.Zmp.GUID, VActiveMapGUID) then begin
          cbbMap.ItemIndex := VAddedIndex;
        end;
      end else if(VMapType.IsHybridLayer) then begin
        VAddedIndex := cbbHyb.Items.AddObject(VMapType.GUIConfig.Name.Value,VMapType);
        if (cbbHyb.ItemIndex = -1) then begin
          if FMainMapsConfig.GetActiveLayersSet.IsGUIDSelected(VGUID) then begin
            cbbHyb.ItemIndex := VAddedIndex;
          end;
        end;
      end;
    end;
  end;
  if (cbbMap.Items.Count > 0) and (cbbMap.ItemIndex < 0) then begin
    cbbMap.ItemIndex := 0;
  end;
  if cbbHyb.ItemIndex = -1 then begin
    cbbHyb.ItemIndex := 0;
  end;

  cbbZoomChange(nil);
end;

end.
