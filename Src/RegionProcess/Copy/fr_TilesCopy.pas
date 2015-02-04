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

unit fr_TilesCopy;

interface

uses
  Types,
  SysUtils,
  Classes,
  Controls,
  Forms,
  StdCtrls,
  CheckLst,
  ExtCtrls,
  fr_ZoomsSelect,
  fr_CacheTypeList,
  i_LanguageManager,
  i_MapTypeSet,
  i_MapTypeListStatic,
  i_MapTypeListBuilder,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  i_GeometryLonLat,
  i_TileStorageTypeList,
  i_RegionProcessParamsFrame,
  u_CommonFormAndFrameParents;

type
  IRegionProcessParamsFrameTilesCopy = interface(IRegionProcessParamsFrameBase)
    ['{71851148-93F1-42A9-ADAC-757928C5C85A}']
    function GetReplaseTarget: Boolean;
    property ReplaseTarget: Boolean read GetReplaseTarget;

    function GetDeleteSource: Boolean;
    property DeleteSource: Boolean read GetDeleteSource;

    function GetTargetCacheType: Byte;
    property TargetCacheType: Byte read GetTargetCacheType;

    function GetMapTypeList: IMapTypeListStatic;
    property MapTypeList: IMapTypeListStatic read GetMapTypeList;

    function GetPlaceInNameSubFolder: Boolean;
    property PlaceInNameSubFolder: Boolean read GetPlaceInNameSubFolder;

    function GetSetTargetVersionEnabled: Boolean;
    property SetTargetVersionEnabled: Boolean read GetSetTargetVersionEnabled;

    function GetSetTargetVersionValue: String;
    property SetTargetVersionValue: String read GetSetTargetVersionValue;
  end;

type
  TfrTilesCopy = class(
      TFrame,
      IRegionProcessParamsFrameBase,
      IRegionProcessParamsFrameZoomArray,
      IRegionProcessParamsFrameTargetPath,
      IRegionProcessParamsFrameTilesCopy
    )
    pnlCenter: TPanel;
    pnlZoom: TPanel;
    pnlMain: TPanel;
    lblNamesType: TLabel;
    pnlCacheTypes: TPanel;
    pnlTop: TPanel;
    lblTargetPath: TLabel;
    edtTargetPath: TEdit;
    btnSelectTargetPath: TButton;
    chkDeleteSource: TCheckBox;
    chkReplaseTarget: TCheckBox;
    chkAllMaps: TCheckBox;
    chklstMaps: TCheckListBox;
    Panel1: TPanel;
    chkPlaceInNameSubFolder: TCheckBox;
    chkSetTargetVersionTo: TCheckBox;
    edSetTargetVersionValue: TEdit;
    pnSetTargetVersionOptions: TPanel;
    procedure btnSelectTargetPathClick(Sender: TObject);
    procedure OnCacheTypeChange(Sender: TObject);
    procedure chkSetTargetVersionToClick(Sender: TObject);
  private
    FMapTypeListBuilderFactory: IMapTypeListBuilderFactory;
    FMainMapsConfig: IMainMapsConfig;
    FFullMapsSet: IMapTypeSet;
    FGUIConfigList: IMapTypeGUIConfigList;
    FfrZoomsSelect: TfrZoomsSelect;
    FfrCacheTypeList: TfrCacheTypeList;
  private
    procedure Init(
      const AZoom: byte;
      const APolygon: IGeometryLonLatPolygon
    );
    function Validate: Boolean;
    procedure UpdateSetTargetVersionState;
  private
    function GetZoomArray: TByteDynArray;
    function GetPath: string;
  private
    { IRegionProcessParamsFrameTilesCopy }
    function GetReplaseTarget: Boolean;
    function GetDeleteSource: Boolean;
    function GetTargetCacheType: Byte;
    function GetMapTypeList: IMapTypeListStatic;
    function GetPlaceInNameSubFolder: Boolean;
    function GetSetTargetVersionEnabled: Boolean;
    function GetSetTargetVersionValue: String;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AMapTypeListBuilderFactory: IMapTypeListBuilderFactory;
      const AMainMapsConfig: IMainMapsConfig;
      const AFullMapsSet: IMapTypeSet;
      const AGUIConfigList: IMapTypeGUIConfigList;
      const ATileStorageTypeList: ITileStorageTypeListStatic
    ); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  Dialogs,
  gnugettext,
  {$WARN UNIT_PLATFORM OFF}
  FileCtrl,
  {$WARN UNIT_PLATFORM ON}
  c_CacheTypeCodes, // for cache types
  i_GUIDListStatic,
  i_MapType;

{$R *.dfm}

constructor TfrTilesCopy.Create(
  const ALanguageManager: ILanguageManager;
  const AMapTypeListBuilderFactory: IMapTypeListBuilderFactory;
  const AMainMapsConfig: IMainMapsConfig;
  const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList;
  const ATileStorageTypeList: ITileStorageTypeListStatic
);
begin
  inherited Create(ALanguageManager);
  FMainMapsConfig := AMainMapsConfig;
  FMapTypeListBuilderFactory := AMapTypeListBuilderFactory;
  FFullMapsSet := AFullMapsSet;
  FGUIConfigList := AGUIConfigList;
  FfrCacheTypeList :=
    TfrCacheTypeList.Create(
      ALanguageManager,
      ATileStorageTypeList,
      False,
      True,
      Self.OnCacheTypeChange
    );
  FfrZoomsSelect :=
    TfrZoomsSelect.Create(
      ALanguageManager
    );
  FfrZoomsSelect.Init(0, 23);
end;

destructor TfrTilesCopy.Destroy;
begin
  FreeAndNil(FfrZoomsSelect);
  FreeAndNil(FfrCacheTypeList);
  inherited;
end;

procedure TfrTilesCopy.btnSelectTargetPathClick(Sender: TObject);
var
  TempPath: string;
begin
  if SelectDirectory('', '', TempPath) then begin
    edtTargetPath.Text := IncludeTrailingPathDelimiter(TempPath);
  end;
end;

procedure TfrTilesCopy.OnCacheTypeChange(Sender: TObject);
var
  VIntCode: Byte;
  VAllowSetVersion: Boolean;
begin
  VIntCode := GetTargetCacheType;
  VAllowSetVersion := (VIntCode in [c_File_Cache_Id_DBMS, c_File_Cache_Id_BDB_Versioned]);
  chkSetTargetVersionTo.Enabled := VAllowSetVersion;
  UpdateSetTargetVersionState;
end;

procedure TfrTilesCopy.chkSetTargetVersionToClick(Sender: TObject);
begin
  UpdateSetTargetVersionState;
end;

function TfrTilesCopy.GetDeleteSource: Boolean;
begin
  Result := chkDeleteSource.Checked;
end;

function TfrTilesCopy.GetMapTypeList: IMapTypeListStatic;
var
  VMaps: IMapTypeListBuilder;
  VMapType: IMapType;
  i: Integer;
begin
  VMaps := FMapTypeListBuilderFactory.Build;
  for i := 0 to chklstMaps.Items.Count - 1 do begin
    if chklstMaps.Checked[i] then begin
      VMapType := IMapType(Pointer(chklstMaps.Items.Objects[i]));
      if VMapType <> nil then begin
        VMaps.Add(VMapType);
      end;
    end;
  end;
  Result := VMaps.MakeAndClear;
end;

function TfrTilesCopy.GetPath: string;
begin
  Result := IncludeTrailingPathDelimiter(edtTargetPath.Text);
end;

function TfrTilesCopy.GetPlaceInNameSubFolder: Boolean;
begin
  Result := chkPlaceInNameSubFolder.Checked;
end;

function TfrTilesCopy.GetReplaseTarget: Boolean;
begin
  Result := chkReplaseTarget.Checked;
end;

function TfrTilesCopy.GetSetTargetVersionEnabled: Boolean;
begin
  Result := chkSetTargetVersionTo.Enabled and chkSetTargetVersionTo.Checked;
end;

function TfrTilesCopy.GetSetTargetVersionValue: String;
begin
  Result := Trim(edSetTargetVersionValue.Text);
end;

function TfrTilesCopy.GetTargetCacheType: Byte;
begin
  Result := Byte(FfrCacheTypeList.IntCode);
end;

function TfrTilesCopy.GetZoomArray: TByteDynArray;
begin
  Result := FfrZoomsSelect.GetZoomList;
end;

procedure TfrTilesCopy.Init;
var
  i: integer;
  VMapType: IMapType;
  VActiveMapGUID: TGUID;
  VAddedIndex: Integer;
  VGUIDList: IGUIDListStatic;
  VGUID: TGUID;
begin
  FfrZoomsSelect.Show(pnlZoom);
  FfrCacheTypeList.Show(pnlCacheTypes);
  FfrCacheTypeList.IntCode := c_File_Cache_Id_SAS;
  VActiveMapGUID := FMainMapsConfig.GetActiveMap.GetStatic.GUID;
  chklstMaps.Items.Clear;
  VGUIDList := FGUIConfigList.OrderedMapGUIDList;
  For i := 0 to VGUIDList.Count - 1 do begin
    VGUID := VGUIDList.Items[i];
    VMapType := FFullMapsSet.GetMapTypeByGUID(VGUID);
    if (VMapType.GUIConfig.Enabled) then begin
      VAddedIndex := chklstMaps.Items.AddObject(VMapType.GUIConfig.Name.Value, TObject(Pointer(VMapType)));
      if IsEqualGUID(VMapType.Zmp.GUID, VActiveMapGUID) then begin
        chklstMaps.ItemIndex := VAddedIndex;
        chklstMaps.Checked[VAddedIndex] := True;
      end;
    end;
  end;
end;

procedure TfrTilesCopy.UpdateSetTargetVersionState;
begin
  edSetTargetVersionValue.Enabled := chkSetTargetVersionTo.Enabled and chkSetTargetVersionTo.Checked;
end;

function TfrTilesCopy.Validate: Boolean;
var
  VMaps: IMapTypeListStatic;
begin
  Result := FfrZoomsSelect.Validate;
  if not Result then begin
    ShowMessage(_('Please select at least one zoom'));
    Exit;
  end;
  VMaps := GetMapTypeList;
  Result := Assigned(VMaps) and (VMaps.Count > 0);
  if not Result then begin
    ShowMessage(_('Please select at least one map'));
    Exit;
  end;
end;

end.
