unit fr_ExportIPhone;

interface

uses
  Types,
  SysUtils,
  Classes,
  Controls,
  Forms,
  StdCtrls,
  CheckLst,
  Spin,
  ExtCtrls,
  i_LanguageManager,
  i_MapTypeSet,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  i_VectorItemLonLat,
  i_RegionProcessParamsFrame,
  u_MapType,
  fr_MapSelect,
  fr_ZoomsSelect,
  u_CommonFormAndFrameParents;

type
  TfrExportIPhone = class(
      TFrame,
      IRegionProcessParamsFrameBase,
      IRegionProcessParamsFrameZoomArray,
      IRegionProcessParamsFrameTargetPath
    )
    pnlMaps: TPanel;
    lblMaps: TLabel;
    lblSat: TLabel;
    lblMap: TLabel;
    lblHybr: TLabel;
    lblCompress: TLabel;
    lblSatCompress: TLabel;
    lblMapCompress: TLabel;
    lblHybrCompress: TLabel;
    rbSat: TRadioButton;
    rbMap: TRadioButton;
    rbHybr: TRadioButton;
    seSatCompress: TSpinEdit;
    seMapCompress: TSpinEdit;
    chkAppendTilse: TCheckBox;
    seHybrCompress: TSpinEdit;
    pnlTop: TPanel;
    btnSelectTargetPath: TButton;
    edtTargetPath: TEdit;
    lblTargetPath: TLabel;
    pnlBottom: TPanel;
    pnlZoom: TPanel;
    grdpnlMaps: TGridPanel;
    pnlSat: TPanel;
    pnlMap: TPanel;
    pnlHyb: TPanel;
    procedure btnSelectTargetPathClick(Sender: TObject);
  private
    FMainMapsConfig: IMainMapsConfig;
    FFullMapsSet: IMapTypeSet;
    FGUIConfigList: IMapTypeGUIConfigList;
    FfrSatSelect: TfrMapSelect;
    FfrMapSelect: TfrMapSelect;
    FfrHybSelect: TfrMapSelect;
    FfrZoomsSelect: TfrZoomsSelect;
  private
    procedure Init(
      const AZoom: byte;
      const APolygon: ILonLatPolygon
    );
    function Validate: Boolean;
  private
    function GetZoomArray: TByteDynArray;
    function GetPath: string;
    function GetAllowExport(AMapType: TMapType): boolean;
  public
    function GetSat(): TMapType;
    function GetMap(): TMapType;
    function GetHyb(): TMapType;
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AMainMapsConfig: IMainMapsConfig;
      const AFullMapsSet: IMapTypeSet;
      const AGUIConfigList: IMapTypeGUIConfigList
    ); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  Dialogs,
  gnugettext,
  {$WARN UNIT_PLATFORM OFF}
  FileCtrl;
  {$WARN UNIT_PLATFORM ON}

{$R *.dfm}

constructor TfrExportIPhone.Create(
  const ALanguageManager: ILanguageManager;
  const AMainMapsConfig: IMainMapsConfig;
  const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList
);
begin
  inherited Create(ALanguageManager);
  FMainMapsConfig := AMainMapsConfig;
  FFullMapsSet := AFullMapsSet;
  FGUIConfigList := AGUIConfigList;
  FfrSatSelect :=
    TfrMapSelect.Create(
      ALanguageManager,
      AMainMapsConfig,
      AGUIConfigList,
      AFullMapsSet,
      mfMaps, // show maps and layers
      True,  // add -NO- to combobox
      False,  // show disabled map
      GetAllowExport
    );
  FfrMapSelect :=
    TfrMapSelect.Create(
      ALanguageManager,
      AMainMapsConfig,
      AGUIConfigList,
      AFullMapsSet,
      mfMaps, // show maps and layers
      True,  // add -NO- to combobox
      False,  // show disabled map
      GetAllowExport
    );
  FfrHybSelect :=
    TfrMapSelect.Create(
      ALanguageManager,
      AMainMapsConfig,
      AGUIConfigList,
      AFullMapsSet,
      mfLayers, // show maps and layers
      True,  // add -NO- to combobox
      False,  // show disabled map
      GetAllowExport
    );
  FfrZoomsSelect :=
    TfrZoomsSelect.Create(
      ALanguageManager
    );
  FfrZoomsSelect.Init(0, 23);
end;

destructor TfrExportIPhone.Destroy;
begin
  FreeAndNil(FfrSatSelect);
  FreeAndNil(FfrMapSelect);
  FreeAndNil(FfrHybSelect);
  FreeAndNil(FfrZoomsSelect);
  inherited;
end;

function TfrExportIPhone.GetAllowExport(AMapType: TMapType): boolean;
begin
  Result := AMapType.IsBitmapTiles;
end;

procedure TfrExportIPhone.btnSelectTargetPathClick(Sender: TObject);
var
  TempPath: string;
begin
  if SelectDirectory('', '', TempPath) then begin
    edtTargetPath.Text := IncludeTrailingPathDelimiter(TempPath);
  end;
end;

function TfrExportIPhone.GetPath: string;
begin
  Result := IncludeTrailingPathDelimiter(edtTargetPath.Text);
end;

function TfrExportIPhone.GetZoomArray: TByteDynArray;
begin
  Result := FfrZoomsSelect.GetZoomList;
end;

function TfrExportIPhone.GetSat: TMapType;
begin
  Result := FfrSatSelect.GetSelectedMapType;
end;

function TfrExportIPhone.GetMap: TMapType;
begin
  Result := FfrMapSelect.GetSelectedMapType;
end;

function TfrExportIPhone.GetHyb: TMapType;
begin
  Result := FfrHybSelect.GetSelectedMapType;
end;

procedure TfrExportIPhone.Init;
begin
  FfrSatSelect.Show(pnlSat);
  FfrMapSelect.Show(pnlMap);
  FfrHybSelect.Show(pnlHyb);
  FfrZoomsSelect.Show(pnlZoom);
end;

function TfrExportIPhone.Validate: Boolean;
begin
  Result := FfrZoomsSelect.Validate;
  if not Result then begin
    ShowMessage(_('Please select at least one zoom'));
  end;
end;

end.
