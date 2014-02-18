unit fr_ExportGEKml;

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
  i_LanguageManager,
  i_MapTypeSet,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  i_GeometryLonLat,
  i_MapTypes,
  i_RegionProcessParamsFrame,
  fr_MapSelect,
  fr_ZoomsSelect,
  u_CommonFormAndFrameParents;

type
  IRegionProcessParamsFrameKmlExport = interface(IRegionProcessParamsFrameBase)
    ['{B2DFB5AD-EAD9-4F36-81F1-87A3D2F1A5B0}']
    function GetNotSaveNotExists: Boolean;
    property NotSaveNotExists: Boolean read GetNotSaveNotExists;

    function GetRelativePath: Boolean;
    property RelativePath: Boolean read GetRelativePath;
  end;

type
  TfrExportGEKml = class(
      TFrame,
      IRegionProcessParamsFrameBase,
      IRegionProcessParamsFrameOneMap,
      IRegionProcessParamsFrameZoomArray,
      IRegionProcessParamsFrameTargetPath,
      IRegionProcessParamsFrameKmlExport
    )
    pnlCenter: TPanel;
    pnlTop: TPanel;
    lblTargetFile: TLabel;
    edtTargetFile: TEdit;
    btnSelectTargetFile: TButton;
    pnlZoom: TPanel;
    pnlMain: TPanel;
    chkNotSaveNotExists: TCheckBox;
    chkUseRelativePath: TCheckBox;
    lblMap: TLabel;
    dlgSaveKML: TSaveDialog;
    pnlMap: TPanel;
    procedure btnSelectTargetFileClick(Sender: TObject);
  private
    FMainMapsConfig: IMainMapsConfig;
    FFullMapsSet: IMapTypeSet;
    FGUIConfigList: IMapTypeGUIConfigList;
    FfrMapSelect: TfrMapSelect;
    FfrZoomsSelect: TfrZoomsSelect;
  private
    procedure Init(
      const AZoom: byte;
      const APolygon: IGeometryLonLatMultiPolygon
    );
    function Validate: Boolean;
  private
    function GetMapType: IMapType;
    function GetZoomArray: TByteDynArray;
    function GetPath: string;
    function GetNotSaveNotExists: Boolean;
    function GetRelativePath: Boolean;
    function GetAllowExport(const AMapType: IMapType): boolean;
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

uses
  gnugettext;
  
{$R *.dfm}

constructor TfrExportGEKml.Create(
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
  FfrZoomsSelect :=
    TfrZoomsSelect.Create(
      ALanguageManager
    );
  FfrZoomsSelect.Init(0, 23);
end;

destructor TfrExportGEKml.Destroy;
begin
  FreeAndNil(FfrMapSelect);
  FreeAndNil(FfrZoomsSelect);
  inherited;
end;

function TfrExportGEKml.GetAllowExport(const AMapType: IMapType): boolean;
begin
  Result := (AMapType.IsBitmapTiles) and (AMapType.TileStorage.IsFileCache);
end;
procedure TfrExportGEKml.btnSelectTargetFileClick(Sender: TObject);
begin
 if dlgSaveKML.Execute then
  edtTargetFile.Text:=dlgSaveKML.FileName;
end;

function TfrExportGEKml.GetMapType: IMapType;
begin
  Result := FfrMapSelect.GetSelectedMapType;
end;

function TfrExportGEKml.GetNotSaveNotExists: Boolean;
begin
  Result := chkNotSaveNotExists.Checked;
end;

function TfrExportGEKml.GetPath: string;
begin
  Result := edtTargetFile.Text;
end;

function TfrExportGEKml.GetRelativePath: Boolean;
begin
  Result := chkUseRelativePath.Checked;
end;

function TfrExportGEKml.GetZoomArray: TByteDynArray;
begin
  Result := FfrZoomsSelect.GetZoomList;
end;

procedure TfrExportGEKml.Init;
begin
  FfrMapSelect.Show(pnlMap);
  FfrZoomsSelect.Show(pnlZoom);
end;
function TfrExportGEKml.Validate: Boolean;
begin
  Result := FfrZoomsSelect.Validate;
  if not Result then begin
    ShowMessage(_('Please select at least one zoom'));
  end;
end;

end.
