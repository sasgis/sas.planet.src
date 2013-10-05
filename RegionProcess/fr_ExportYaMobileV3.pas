unit fr_ExportYaMobileV3;

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
  u_CommonFormAndFrameParents;

type
  TfrExportYaMobileV3 = class(
      TFrame,
      IRegionProcessParamsFrameBase,
      IRegionProcessParamsFrameZoomArray,
      IRegionProcessParamsFrameTargetPath
    )
    pnlCenter: TPanel;
    pnlTop: TPanel;
    lblTargetPath: TLabel;
    edtTargetPath: TEdit;
    btnSelectTargetPath: TButton;
    chkReplaseTiles: TCheckBox;
    pnlRight: TPanel;
    lblZooms: TLabel;
    chklstZooms: TCheckListBox;
    pnlMapsSelect: TPanel;
    grdpnlMaps: TGridPanel;
    lblMapCompress: TLabel;
    seMapCompress: TSpinEdit;
    seSatCompress: TSpinEdit;
    lblSatCompress: TLabel;
    lblCompress: TLabel;
    lblHybr: TLabel;
    lblMap: TLabel;
    lblSat: TLabel;
    lblMaps: TLabel;
    pnlHyb: TPanel;
    pnlMap: TPanel;
    pnlSat: TPanel;
    procedure btnSelectTargetPathClick(Sender: TObject);
  private
    FMainMapsConfig: IMainMapsConfig;
    FFullMapsSet: IMapTypeSet;
    FGUIConfigList: IMapTypeGUIConfigList;
    FfrSatSelect: TfrMapSelect;
    FfrMapSelect: TfrMapSelect;
    FfrHybSelect: TfrMapSelect;
  private
    procedure Init(
      const AZoom: byte;
      const APolygon: ILonLatPolygon
    );
    function GetAllowExport(AMapType: TMapType): boolean;
    function GetZoomArray: TByteDynArray;
    function GetPath: string;
  public
    function GetSat(): TfrMapSelect;
    function GetMap(): TfrMapSelect;
    function GetHyb(): TfrMapSelect;
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
  {$WARN UNIT_PLATFORM OFF}
  FileCtrl;
  {$WARN UNIT_PLATFORM ON}

{$R *.dfm}

constructor TfrExportYaMobileV3.Create(
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
end;

destructor TfrExportYaMobileV3.Destroy;
begin
  FreeAndNil(FfrSatSelect);
  FreeAndNil(FfrMapSelect);
  FreeAndNil(FfrHybSelect);
  inherited;
end;

function TfrExportYaMobileV3.GetAllowExport(AMapType: TMapType): boolean;
begin
  Result := AMapType.IsBitmapTiles;
end;

procedure TfrExportYaMobileV3.btnSelectTargetPathClick(Sender: TObject);
var
  TempPath: string;
begin
  if SelectDirectory('', '', TempPath) then begin
    edtTargetPath.Text := IncludeTrailingPathDelimiter(TempPath);
  end;
end;

function TfrExportYaMobileV3.GetSat: TfrMapSelect;
begin
  Result := FfrSatSelect;
end;

function TfrExportYaMobileV3.GetMap: TfrMapSelect;
begin
  Result := FfrMapSelect;
end;

function TfrExportYaMobileV3.GetHyb: TfrMapSelect;
begin
  Result := FfrHybSelect;
end;

function TfrExportYaMobileV3.GetPath: string;
begin
  Result := IncludeTrailingPathDelimiter(edtTargetPath.Text);
end;

function TfrExportYaMobileV3.GetZoomArray: TByteDynArray;
var
  i: Integer;
  VCount: Integer;
begin
  Result := nil;
  VCount := 0;
  for i := 0 to 23 do begin
    if chklstZooms.Checked[i] then begin
      SetLength(Result, VCount + 1);
      Result[VCount] := i;
      Inc(VCount);
    end;
  end;
end;

procedure TfrExportYaMobileV3.Init;
var
  i: integer;
begin
  chklstZooms.Items.Clear;
  for i:=1 to 24 do begin
    chklstZooms.Items.Add(inttostr(i));
  end;
  FfrSatSelect.Show(pnlSat);
  FfrMapSelect.Show(pnlMap);
  FfrHybSelect.Show(pnlHyb);
end;
end.
