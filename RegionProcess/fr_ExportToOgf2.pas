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
  CheckLst,
  ExtCtrls,
  i_MapTypes,
  i_CoordConverterFactory,
  i_VectorItemLonLat,
  i_VectorItmesFactory,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  u_CommonFormAndFrameParents,
  t_GeoTypes;

type
  TfrExportToOgf2 = class(TFrame)
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
    procedure btnSelectTargetFileClick(Sender: TObject);
    procedure cbbZoomChange(Sender: TObject);
  private
    FVectorFactory: IVectorItmesFactory;
    FProjectionFactory: IProjectionInfoFactory;
    FMainMapsConfig: IMainMapsConfig;
    FFullMapsSet: IMapTypeSet;
    FGUIConfigList: IMapTypeGUIConfigList;
    FPolygLL: ILonLatPolygon;
  public
    constructor CreateForFileType(
      AOwner : TComponent;
      const AProjectionFactory: IProjectionInfoFactory;
      const AVectorFactory: IVectorItmesFactory;
      const AMainMapsConfig: IMainMapsConfig;
      const AFullMapsSet: IMapTypeSet;
      const AGUIConfigList: IMapTypeGUIConfigList;
      const AFileFilters: string;
      const AFileExtDefault: string
    );
    procedure Init(const AZoom: Byte; const APolygLL: ILonLatPolygon);
    procedure RefreshTranslation; override;
  end;

implementation

uses
  i_GUIDListStatic,
  i_VectorItemProjected,
  u_GeoFun,
  u_ResStrings,
  u_MapType;

{$R *.dfm}

{ TfrExportToOgf2 }

procedure TfrExportToOgf2.btnSelectTargetFileClick(Sender: TObject);
begin
  if dlgSaveTargetFile.Execute then begin
    edtTargetFile.Text := dlgSaveTargetFile.FileName;
  end;
end;

procedure TfrExportToOgf2.cbbZoomChange(Sender: TObject);
var
  numd:int64 ;
  VMapType: TMapType;
  VZoom: byte;
  VPolyLL: ILonLatPolygon;
  VProjected: IProjectedPolygon;
  VLine: IProjectedPolygonLine;
  VBounds: TDoubleRect;
  VPixelRect: TRect;
  VTileRect: TRect;
begin
  if cbbMap.ItemIndex >= 0 then begin
    VMapType := TMapType(cbbMap.Items.Objects[cbbMap.ItemIndex]);
  end else begin
    VMapType := nil;
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
        numd := (VTileRect.Right - VTileRect.Left);
        numd := numd * (VTileRect.Bottom - VTileRect.Top);
        lblStat.Caption :=
          SAS_STR_filesnum+': '+
          inttostr(VTileRect.Right - VTileRect.Left)+'x'+
          inttostr(VTileRect.Bottom - VTileRect.Top)+
          '('+inttostr(numd)+')' +
          ', '+SAS_STR_Resolution + ' ' +
          inttostr( (VTileRect.Right - VTileRect.Left) * 256 )+'x'+
          inttostr( (VTileRect.Bottom - VTileRect.Top) * 256 );
      end;
    end;
  end;
end;

constructor TfrExportToOgf2.CreateForFileType(
  AOwner : TComponent;
  const AProjectionFactory: IProjectionInfoFactory;
  const AVectorFactory: IVectorItmesFactory;
  const AMainMapsConfig: IMainMapsConfig;
  const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList;
  const AFileFilters: string;
  const AFileExtDefault: string
);
begin
  inherited Create(AOwner);
  FProjectionFactory := AProjectionFactory;
  FVectorFactory := AVectorFactory;
  FMainMapsConfig := AMainMapsConfig;
  FFullMapsSet := AFullMapsSet;
  FGUIConfigList := AGUIConfigList;
  dlgSaveTargetFile.Filter := AFileFilters;
  dlgSaveTargetFile.DefaultExt := AFileExtDefault;
end;

procedure TfrExportToOgf2.Init(const AZoom: Byte; const APolygLL: ILonLatPolygon);
var
  i: integer;
  VMapType: TMapType;
  VActiveMapGUID: TGUID;
  VAddedIndex: Integer;
  VGUIDList: IGUIDListStatic;
  VGUID: TGUID;
begin
  FPolygLL := APolygLL;
  cbbZoom.Items.Clear;
  for i:=1 to 24 do begin
    cbbZoom.Items.Add(inttostr(i));
  end;
  cbbZoom.ItemIndex := AZoom;
  
  VActiveMapGUID := FMainMapsConfig.GetActiveMap.GetSelectedGUID;
  cbbMap.items.Clear;
  VGUIDList := FGUIConfigList.OrderedMapGUIDList;
  For i := 0 to VGUIDList.Count-1 do begin
    VGUID := VGUIDList.Items[i];
    VMapType := FFullMapsSet.GetMapTypeByGUID(VGUID).MapType;
    if (VMapType.GUIConfig.Enabled) then begin
      VAddedIndex := cbbMap.Items.AddObject(VMapType.GUIConfig.Name.Value,VMapType);
      if IsEqualGUID(VMapType.Zmp.GUID, VActiveMapGUID) then begin
        cbbMap.ItemIndex:=VAddedIndex;
      end;
    end;
  end;
  if (cbbMap.Items.Count > 0) and (cbbMap.ItemIndex < 0) then begin
    cbbMap.ItemIndex := 0;
  end;

  cbbZoomChange(nil);
end;

procedure TfrExportToOgf2.RefreshTranslation;
begin
  inherited;
end;

end.
