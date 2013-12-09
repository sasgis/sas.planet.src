unit fr_ExportAUX;

interface

uses
  SysUtils,
  Classes,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
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
  TfrExportAUX = class(
      TFrame,
      IRegionProcessParamsFrameBase,
      IRegionProcessParamsFrameOneMap,
      IRegionProcessParamsFrameOneZoom,
      IRegionProcessParamsFrameTargetPath
    )
    pnlCenter: TPanel;
    pnlTop: TPanel;
    lblTargetFile: TLabel;
    edtTargetFile: TEdit;
    btnSelectTargetFile: TButton;
    dlgTargetFileSelect: TSaveDialog;
    pnlMapSelect: TPanel;
    pnlZoom: TPanel;
    lblZoom: TLabel;
    cbbZoom: TComboBox;
    pnlFrame: TPanel;
    lblMapCaption: TLabel;
    procedure btnSelectTargetFileClick(Sender: TObject);
  private
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
    function GetPath: string;
    function GetAllowExport(AMapType: TMapType): boolean;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AMainMapsConfig: IMainMapsConfig;
      const AFullMapsSet: IMapTypeSet;
      const AGUIConfigList: IMapTypeGUIConfigList
    ); reintroduce;
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

{ TfrExportAUX }

constructor TfrExportAUX.Create(
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
  FfrMapSelect :=
    TfrMapSelect.Create(
      ALanguageManager,
      AMainMapsConfig,
      AGUIConfigList,
      AFullMapsSet,
      mfAll, // show maps and layers
      False,  // add -NO- to combobox
      False,  // show disabled map
      GetAllowExport
    );
end;

destructor TfrExportAUX.Destroy;
begin
  FreeAndNil(FfrMapSelect);
  inherited;
end;

procedure TfrExportAUX.btnSelectTargetFileClick(Sender: TObject);
begin
  if dlgTargetFileSelect.Execute then begin
    edtTargetFile.Text := dlgTargetFileSelect.FileName;
  end;
end;

function TfrExportAUX.GetMapType: TMapType;
begin
  Result := FfrMapSelect.GetSelectedMapType;
  Assert(Result <> nil);
end;

function TfrExportAUX.GetPath: string;
begin
  Result := edtTargetFile.Text;
end;

function TfrExportAUX.GetZoom: Byte;
begin
  if cbbZoom.ItemIndex < 0 then begin
    cbbZoom.ItemIndex := 0;
  end;
  Result := cbbZoom.ItemIndex;
end;

function TfrExportAUX.GetAllowExport(AMapType: TMapType): boolean;
begin
  Result := (AMapType.IsBitmapTiles) and (AMapType.TileStorage.IsFileCache);
end;

procedure TfrExportAUX.Init(
  const AZoom: byte;
  const APolygon: IGeometryLonLatMultiPolygon
);
var
  i: integer;
begin
  cbbZoom.Items.Clear;
  for i:=1 to 24 do begin
    cbbZoom.Items.Add(inttostr(i));
  end;
  cbbZoom.ItemIndex := AZoom;
  FfrMapSelect.Show(pnlFrame);
end;

function TfrExportAUX.Validate: Boolean;
begin
  Result := True;
end;

end.
