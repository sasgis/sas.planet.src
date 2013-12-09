unit fr_TilesDownload;

interface

uses
  Classes,
  Controls,
  ComCtrls,
  ExtCtrls,
  Forms,
  SysUtils,
  StdCtrls,
  Windows,
  i_MapTypeSet,
  i_CoordConverterFactory,
  i_LanguageManager,
  i_GeometryLonLat,
  i_VectorGeometryProjectedFactory,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  i_RegionProcessParamsFrame,
  u_MapType,
  fr_MapSelect,
  u_CommonFormAndFrameParents;

type
  IRegionProcessParamsFrameTilesDownload = interface(IRegionProcessParamsFrameBase)
    ['{70B48431-5383-4CD2-A1EF-AF9291F6ABB0}']
    function GetIsStartPaused: Boolean;
    property IsStartPaused: Boolean read GetIsStartPaused;

    function GetIsIgnoreTne: Boolean;
    property IsIgnoreTne: Boolean read GetIsIgnoreTne;

    function GetIsReplace: Boolean;
    property IsReplace: Boolean read GetIsReplace;

    function GetIsReplaceIfDifSize: Boolean;
    property IsReplaceIfDifSize: Boolean read GetIsReplaceIfDifSize;

    function GetIsReplaceIfOlder: Boolean;
    property IsReplaceIfOlder: Boolean read GetIsReplaceIfOlder;

    function GetReplaceDate: TDateTime;
    property ReplaceDate: TDateTime read GetReplaceDate;
  end;

type
  TfrTilesDownload = class(
      TFrame,
      IRegionProcessParamsFrameBase,
      IRegionProcessParamsFrameOneMap,
      IRegionProcessParamsFrameOneZoom,
      IRegionProcessParamsFrameTilesDownload
    )
    lblZoom: TLabel;
    lblStat: TLabel;
    chkReplace: TCheckBox;
    chkReplaceIfDifSize: TCheckBox;
    chkReplaceOlder: TCheckBox;
    dtpReplaceOlderDate: TDateTimePicker;
    cbbZoom: TComboBox;
    chkTryLoadIfTNE: TCheckBox;
    pnlTop: TPanel;
    pnlBottom: TPanel;
    pnlMain: TPanel;
    pnlTileReplaceCondition: TPanel;
    pnlReplaceOlder: TPanel;
    lblReplaceOlder: TLabel;
    chkStartPaused: TCheckBox;
    pnlMapSelect: TPanel;
    pnlZoom: TPanel;
    lblMapCaption: TLabel;
    pnlFrame: TPanel;
    procedure chkReplaceClick(Sender: TObject);
    procedure chkReplaceOlderClick(Sender: TObject);
    procedure cbbZoomChange(Sender: TObject);
  private
    FVectorGeometryProjectedFactory: IVectorGeometryProjectedFactory;
    FProjectionFactory: IProjectionInfoFactory;
    FPolygLL: IGeometryLonLatMultiPolygon;
    FMainMapsConfig: IMainMapsConfig;
    FFullMapsSet: IMapTypeSet;
    FGUIConfigList: IMapTypeGUIConfigList;
    FfrMapSelect: TfrMapSelect;

  private
    procedure Init(
      const AZoom: byte;
      const APolygon: IGeometryLonLatMultiPolygon
    );
    function Validate: Boolean;
  private
    function GetMapType: TMapType;
    function GetZoom: Byte;
  private
    function GetIsStartPaused: Boolean;
    function GetIsIgnoreTne: Boolean;
    function GetIsReplace: Boolean;
    function GetIsReplaceIfDifSize: Boolean;
    function GetIsReplaceIfOlder: Boolean;
    function GetReplaceDate: TDateTime;
    function GetAllowDownload(AMapType: TMapType): boolean; // чисто для проверки
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AProjectionFactory: IProjectionInfoFactory;
      const AVectorGeometryProjectedFactory: IVectorGeometryProjectedFactory;
      const AMainMapsConfig: IMainMapsConfig;
      const AFullMapsSet: IMapTypeSet;
      const AGUIConfigList: IMapTypeGUIConfigList
    ); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  t_GeoTypes,
  i_VectorItemProjected,
  u_GeoFun,
  u_ResStrings;

{$R *.dfm}

procedure TfrTilesDownload.cbbZoomChange(Sender: TObject);
var
  numd:int64 ;
  Vmt: TMapType;
  VZoom: byte;
  VPolyLL: IGeometryLonLatMultiPolygon;
  VProjected: IProjectedPolygon;
  VLine: IProjectedPolygonLine;
  VBounds: TDoubleRect;
  VPixelRect: TRect;
  VTileRect: TRect;
begin
  Vmt := FfrMapSelect.GetSelectedMapType;
  if Vmt <> nil then begin
    VZoom := cbbZoom.ItemIndex;
    Vmt.GeoConvert.CheckZoom(VZoom);
    VPolyLL := FPolygLL;
    if VPolyLL <> nil then begin
      VProjected :=
        FVectorGeometryProjectedFactory.CreateProjectedPolygonByLonLatPolygon(
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

destructor TfrTilesDownload.Destroy;
begin
  FreeAndNil(FfrMapSelect);
  inherited;
end;

procedure TfrTilesDownload.chkReplaceClick(Sender: TObject);
var
  VEnabled: Boolean;
begin
  VEnabled := chkReplace.Checked;
  chkReplaceIfDifSize.Enabled := VEnabled;
  chkReplaceOlder.Enabled := VEnabled;
  chkReplaceOlderClick(chkReplaceOlder);
end;

procedure TfrTilesDownload.chkReplaceOlderClick(Sender: TObject);
begin
  dtpReplaceOlderDate.Enabled := chkReplaceOlder.Enabled and chkReplaceOlder.Checked;
end;

constructor TfrTilesDownload.Create(
  const ALanguageManager: ILanguageManager;
  const AProjectionFactory: IProjectionInfoFactory;
  const AVectorGeometryProjectedFactory: IVectorGeometryProjectedFactory;
  const AMainMapsConfig: IMainMapsConfig;
  const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList
);
begin
  inherited Create(ALanguageManager);
  FProjectionFactory := AProjectionFactory;
  FVectorGeometryProjectedFactory := AVectorGeometryProjectedFactory;
  FMainMapsConfig := AMainMapsConfig;
  FFullMapsSet := AFullMapsSet;
  FGUIConfigList := AGUIConfigList;
  FfrMapSelect :=
    TfrMapSelect.Create(
      ALanguageManager,
      AMainMapsConfig,
      AGUIConfigList,
      AFullMapsSet,
      mfAll, // show maps and layers
      false,  // add -NO- to combobox
      false,  // show disabled map
      GetAllowDownload
    );
end;

function TfrTilesDownload.GetAllowDownload(AMapType: TMapType): boolean; // чисто для проверки
begin
   Result := (AMapType.StorageConfig.GetAllowAdd) and (AMapType.TileDownloadSubsystem.State.GetStatic.Enabled);
end;

function TfrTilesDownload.GetIsIgnoreTne: Boolean;
begin
  Result := chkTryLoadIfTNE.Checked
end;

function TfrTilesDownload.GetIsReplace: Boolean;
begin
  Result := chkReplace.Checked;
end;

function TfrTilesDownload.GetIsReplaceIfDifSize: Boolean;
begin
  Result := chkReplaceIfDifSize.Checked;
end;

function TfrTilesDownload.GetIsReplaceIfOlder: Boolean;
begin
  Result := chkReplaceOlder.Checked;
end;

function TfrTilesDownload.GetIsStartPaused: Boolean;
begin
  Result := chkStartPaused.Checked;
end;

function TfrTilesDownload.GetMapType: TMapType;
begin
  Result := FfrMapSelect.GetSelectedMapType;
end;

function TfrTilesDownload.GetReplaceDate: TDateTime;
begin
  Result := dtpReplaceOlderDate.DateTime;
end;

function TfrTilesDownload.GetZoom: Byte;
begin
  if cbbZoom.ItemIndex < 0 then begin
    cbbZoom.ItemIndex := 0;
  end;
  Result := cbbZoom.ItemIndex;
end;

procedure TfrTilesDownload.Init(const AZoom: Byte; const APolygon: IGeometryLonLatMultiPolygon);
var
  i: integer;
begin
  FPolygLL := APolygon;
  cbbZoom.Items.Clear;
  for i:=1 to 24 do begin
    cbbZoom.Items.Add(inttostr(i));
  end;
  cbbZoom.ItemIndex := AZoom;
  dtpReplaceOlderDate.Date:=now;
  cbbZoomChange(nil);
  FfrMapSelect.Show(pnlFrame);
  cbbZoomChange(cbbzoom);

end;

function TfrTilesDownload.Validate: Boolean;
begin
  Result := True;
end;

end.
