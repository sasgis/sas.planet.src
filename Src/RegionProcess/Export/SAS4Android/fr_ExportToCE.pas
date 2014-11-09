{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
{* This program is free software: you can redistribute it and/or modify       *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* This program is distributed in the hope that it will be useful,            *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *}
{*                                                                            *}
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

unit fr_ExportToCE;

interface

uses
  Types,
  SysUtils,
  Classes,
  Controls,
  Forms,
  StdCtrls,
  ExtCtrls,
  Spin,
  Dialogs,
  i_LanguageManager,
  i_MapType,
  i_MapTypeSet,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  i_GeometryLonLat,
  i_RegionProcessParamsFrame,
  fr_MapSelect,
  fr_ZoomsSelect,
  u_CommonFormAndFrameParents;

type
  IRegionProcessParamsFrameExportToCE = interface(IRegionProcessParamsFrameBase)
    ['{00A64FCB-EFC5-4E88-B4CF-0FCCDB096FAE}']
    function GetComent: string;
    property Coment: string read GetComent;
    function GetIsAddRecoverInfo: boolean;
    property IsAddRecoverInfo: boolean read GetIsAddRecoverInfo;
    function GetMaxSize: integer;
    property MaxSize: integer read GetMaxSize;
  end;

type
  TfrExportToCE = class(
      TFrame,
      IRegionProcessParamsFrameBase,
      IRegionProcessParamsFrameZoomArray,
      IRegionProcessParamsFrameTargetPath,
      IRegionProcessParamsFrameOneMap,
      IRegionProcessParamsFrameExportToCE
    )
    pnlCenter: TPanel;
    pnlZoom: TPanel;
    lblMap: TLabel;
    pnlTop: TPanel;
    lblTargetFile: TLabel;
    edtTargetFile: TEdit;
    btnSelectTargetFile: TButton;
    EMapName: TEdit;
    EComent: TEdit;
    SaveRecoverInfo: TCheckBox;
    lVolSize: TLabel;
    dlgSaveTargetFile: TSaveDialog;
    CComment: TCheckBox;
    CMapName: TCheckBox;
    TempPath: TEdit;
    cbbMaxVolSize: TSpinEdit;
    pnlMap: TPanel;
    procedure btnSelectTargetFileClick(Sender: TObject);
    procedure MapChange(Sender: TObject);
    procedure CMapNameClick(Sender: TObject);
    procedure CCommentClick(Sender: TObject);
  private
    FMainMapsConfig: IMainMapsConfig;
    FFullMapsSet: IMapTypeSet;
    FGUIConfigList: IMapTypeGUIConfigList;
    FfrMapSelect: TfrMapSelect;
    FfrZoomsSelect: TfrZoomsSelect;
  private
    procedure Init(
      const AZoom: byte;
      const APolygon: IGeometryLonLatPolygon
    );
    function Validate: Boolean;
  private
    function GetMapType: IMapType;
    function GetZoomArray: TByteDynArray;
    function GetPath: string;
    function GetAllowExport(const AMapType: IMapType): boolean;
    function GetComent: string;
    function GetIsAddRecoverInfo: boolean;
    function GetMaxSize: integer;
    procedure SetMapName();
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AMainMapsConfig: IMainMapsConfig;
      const AFullMapsSet: IMapTypeSet;
      const AGUIConfigList: IMapTypeGUIConfigList;
      const AFileFilters: string;
      const AFileExtDefault: string
    ); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  gnugettext,
  {$WARN UNIT_PLATFORM OFF}
  FileCtrl;
  {$WARN UNIT_PLATFORM ON}

{$R *.dfm}

constructor TfrExportToCe.Create(
  const ALanguageManager: ILanguageManager;
  const AMainMapsConfig: IMainMapsConfig;
  const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList;
  const AFileFilters: string;
  const AFileExtDefault: string
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
  FfrMapSelect.OnMapChange := MapChange;
  FfrZoomsSelect :=
    TfrZoomsSelect.Create(
      ALanguageManager
    );
  FfrZoomsSelect.Init(0, 23);
end;

destructor TfrExportToCE.Destroy;
begin
  FreeAndNil(FfrMapSelect);
  FreeAndNil(FfrZoomsSelect);
  inherited;
end;

function TfrExportToCE.GetAllowExport(const AMapType: IMapType): boolean;
begin
  Result := AMapType.IsBitmapTiles;
end;

procedure TfrExportToCE.btnSelectTargetFileClick(Sender: TObject);
var
  TempString: string;
begin
  if FfrMapSelect.GetSelectedMapType <> nil then begin
    if SelectDirectory('', '', TempString) then begin
      TempPath.text := TempString;
      edtTargetFile.Text := IncludeTrailingPathDelimiter(TempPath.text) + FfrMapSelect.GetSelectedMapType.GetShortFolderName;
    end;
  end;
end;

procedure TfrExportToCE.SetMapName();
begin
  if CMapName.checked then begin
    EMapName.enabled := true;
    if FfrMapSelect.GetSelectedMapType <> nil then begin
      EMapName.text := FfrMapSelect.GetSelectedMapType.GUIConfig.Name.Value;
    end else begin
      EMapName.text := '';
    end;
  end else begin
    EMapName.Enabled := false;
    EMapName.text := '';
  end;
end;

function TfrExportToCE.Validate: Boolean;
begin
  Result := FfrZoomsSelect.Validate;
  if not Result then begin
    ShowMessage(_('Please select at least one zoom'));
  end;
end;

procedure TfrExportToCE.MapChange(Sender: TObject);
begin
  SetMapName();
  if (TempPath.text = '') and (edtTargetFile.Text <> '') then begin
    TempPath.text := edtTargetFile.Text;
  end;
  if (TempPath.text <> '') then begin
    if FfrMapSelect.GetSelectedMapType <> nil then begin
      edtTargetFile.Text := IncludeTrailingPathDelimiter(TempPath.text) + FfrMapSelect.GetSelectedMapType.GetShortFolderName;
    end else begin
      edtTargetFile.Text := '';
    end;
  end;
end;

procedure TfrExportToCE.CCommentClick(Sender: TObject);
begin
  if CComment.checked then begin
    EComent.enabled := true;
  end else begin
    EComent.Enabled := false;
    EComent.text := '';
  end;
end;

procedure TfrExportToCE.CMapNameClick(Sender: TObject);
begin
  SetMapName();
end;

function TfrExportToCE.GetComent: string;
var
  VMapType: IMapType;
begin
  Result := EMapName.Text;
  if Result <> '' then begin
    VMapType := GetMapType;
    Result := Guidtostring(VMapType.Zmp.GUID) + #13#10 + Result;
  end;
  if EComent.Text <> '' then begin
    if Result <> '' then begin
      Result := Result + #13#10;
    end;
    Result := Result + EComent.Text;
  end;
end;

function TfrExportToCE.GetIsAddRecoverInfo: boolean;
begin
  Result := SaveRecoverInfo.Checked;
end;

function TfrExportToCE.GetMapType: IMapType;
begin
  Result := FfrMapSelect.GetSelectedMapType;
end;

function TfrExportToCE.GetMaxSize: integer;
begin
  Result := cbbMaxVolSize.value;
end;

function TfrExportToCE.GetPath: string;
var
  VMapType: IMapType;
begin
  Result := '';
  if TempPath.Text <> '' then begin
    Result := edtTargetFile.Text;
  end else if copy(edtTargetFile.Text, length(edtTargetFile.Text), 1) <> '\' then begin
    Result := edtTargetFile.Text;
  end else begin
    VMapType := GetMapType;
    if VMapType <> nil then begin
      Result := IncludeTrailingPathDelimiter(edtTargetFile.Text) + VMapType.GetShortFolderName;
    end;
  end;
end;

function TfrExportToCE.GetZoomArray: TByteDynArray;
begin
  Result := FfrZoomsSelect.GetZoomList;
end;

procedure TfrExportToCE.Init;
begin
  if CComment.checked then begin
    EComent.enabled := true;
  end else begin
    EComent.Enabled := false;
    EComent.text := '';
  end;
  FfrMapSelect.Show(pnlMap);
  SetMapName();
  FfrZoomsSelect.Show(pnlZoom);
end;

end.
