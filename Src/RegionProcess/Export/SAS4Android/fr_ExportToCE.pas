{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2021, SAS.Planet development team.                      *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
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
    lblTargetPath: TLabel;
    edtTargetPath: TEdit;
    btnSelectTargetPath: TButton;
    EMapName: TEdit;
    EComent: TEdit;
    SaveRecoverInfo: TCheckBox;
    lVolSize: TLabel;
    CComment: TCheckBox;
    CMapName: TCheckBox;
    cbbMaxVolSize: TSpinEdit;
    pnlMap: TPanel;
    procedure btnSelectTargetPathClick(Sender: TObject);
    procedure MapChange(Sender: TObject);
    procedure CMapNameClick(Sender: TObject);
    procedure CCommentClick(Sender: TObject);
  private
    FfrMapSelect: TfrMapSelect;
    FfrZoomsSelect: TfrZoomsSelect;
    TempPath: string;
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
      const AMapSelectFrameBuilder: IMapSelectFrameBuilder;
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
  const AMapSelectFrameBuilder: IMapSelectFrameBuilder;
  const AFileFilters: string;
  const AFileExtDefault: string
);
begin
  inherited Create(ALanguageManager);
  FfrMapSelect :=
    AMapSelectFrameBuilder.Build(
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

procedure TfrExportToCE.btnSelectTargetPathClick(Sender: TObject);
var
  TempString: string;
begin
  if FfrMapSelect.GetSelectedMapType <> nil then begin
    if SelectDirectory('', '', TempString) then begin
      TempPath := TempString;
      edtTargetPath.Text := IncludeTrailingPathDelimiter(TempPath) + FfrMapSelect.GetSelectedMapType.GetShortFolderName;
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
  Result := False;

  if Trim(edtTargetPath.Text) = '' then begin
    ShowMessage(_('Please select output folder'));
    Exit;
  end;

  if not FfrZoomsSelect.Validate then begin
    ShowMessage(_('Please select at least one zoom'));
    Exit;
  end;

  if FfrMapSelect.GetSelectedMapType = nil then begin
    ShowMessage(_('Please select the map first!'));
    Exit;
  end;

  Result := True;
end;

procedure TfrExportToCE.MapChange(Sender: TObject);
begin
  SetMapName();
  if (TempPath = '') and (edtTargetPath.Text <> '') then begin
    TempPath := edtTargetPath.Text;
  end;
  if (TempPath <> '') then begin
    if FfrMapSelect.GetSelectedMapType <> nil then begin
      edtTargetPath.Text := IncludeTrailingPathDelimiter(TempPath) + FfrMapSelect.GetSelectedMapType.GetShortFolderName;
    end else begin
      edtTargetPath.Text := '';
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
  if TempPath <> '' then begin
    Result := edtTargetPath.Text;
  end else if copy(edtTargetPath.Text, length(edtTargetPath.Text), 1) <> '\' then begin
    Result := edtTargetPath.Text;
  end else begin
    VMapType := GetMapType;
    if VMapType <> nil then begin
      Result := IncludeTrailingPathDelimiter(edtTargetPath.Text) + VMapType.GetShortFolderName;
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
  if TempPath <> '' then begin
    edtTargetPath.Text := IncludeTrailingPathDelimiter(TempPath) + FfrMapSelect.GetSelectedMapType.GetShortFolderName;
  end;
end;

end.
