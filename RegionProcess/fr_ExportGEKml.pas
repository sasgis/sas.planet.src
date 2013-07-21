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
  CheckLst,
  ExtCtrls,
  i_LanguageManager,
  i_MapTypes,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  i_VectorItemLonLat,
  i_RegionProcessParamsFrame,
  u_MapType,
  fr_MapSelect,
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
    lblZooms: TLabel;
    chkAllZooms: TCheckBox;
    chklstZooms: TCheckListBox;
    pnlTop: TPanel;
    lblTargetFile: TLabel;
    edtTargetFile: TEdit;
    btnSelectTargetFile: TButton;
    pnlRight: TPanel;
    pnlMain: TPanel;
    chkNotSaveNotExists: TCheckBox;
    chkUseRelativePath: TCheckBox;
    lblMap: TLabel;
    dlgSaveKML: TSaveDialog;
    pnlMap: TPanel;
    procedure btnSelectTargetFileClick(Sender: TObject);
    procedure chkAllZoomsClick(Sender: TObject);
  private
    FMainMapsConfig: IMainMapsConfig;
    FFullMapsSet: IMapTypeSet;
    FGUIConfigList: IMapTypeGUIConfigList;
    FfrMapSelect: TfrMapSelect;
  private
    procedure Init(
      const AZoom: byte;
      const APolygon: ILonLatPolygon
    );
  private
    function GetMapType: TMapType;
    function GetZoomArray: TByteDynArray;
    function GetPath: string;
    function GetNotSaveNotExists: Boolean;
    function GetRelativePath: Boolean;
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
end;

destructor TfrExportGEKml.Destroy;
begin
  FreeAndNil(FfrMapSelect);
  inherited;
end;

function TfrExportGEKml.GetAllowExport(AMapType: TMapType): boolean;
begin
  Result := (AMapType.IsBitmapTiles) and (AMapType.TileStorage.IsFileCache);
end;
procedure TfrExportGEKml.btnSelectTargetFileClick(Sender: TObject);
begin
 if dlgSaveKML.Execute then
  edtTargetFile.Text:=dlgSaveKML.FileName;
end;

procedure TfrExportGEKml.chkAllZoomsClick(Sender: TObject);
var
  i: byte;
begin
  for i:=0 to chklstZooms.Count-1 do begin
    chklstZooms.Checked[i] := TCheckBox(Sender).Checked;
  end;
end;

function TfrExportGEKml.GetMapType: TMapType;
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

procedure TfrExportGEKml.Init;
var
  i: integer;
begin
  chklstZooms.Items.Clear;
  for i:=1 to 24 do begin
    chklstZooms.Items.Add(inttostr(i));
  end;
  FfrMapSelect.Show(pnlMap);
end;
end.
