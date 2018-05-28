{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2015, SAS.Planet development team.                      *}
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

unit frm_FavoriteMapSetEditor;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  CheckLst,
  ComCtrls,
  ExtCtrls,
  UITypes,
  fr_MapSelect,
  fr_LonLat,
  i_ActiveMapsConfig,
  i_MapType,
  i_MapTypeSet,
  i_MapTypeGUIConfigList,
  i_LanguageManager,
  i_ProjectionSetChangeable,
  i_CoordFromStringParser,
  i_CoordToStringConverter,
  i_CoordRepresentationConfig,
  i_LocalCoordConverterChangeable,
  i_FavoriteMapSetConfig,
  i_FavoriteMapSetItemStatic,
  u_CommonFormAndFrameParents;

type
  TfrmFavoriteMapSetEditor = class(TFormWitghLanguageManager)
    pnlMain: TPanel;
    lblName: TLabel;
    edtName: TEdit;
    EditHotKey: THotKey;
    btnResetHotKey: TButton;
    lblHotKey: TLabel;
    chklstMaps: TCheckListBox;
    pnlMap: TPanel;
    pnlBottom: TPanel;
    btnOk: TButton;
    btnCancel: TButton;
    chkMergeLayers: TCheckBox;
    pnlZoom: TPanel;
    chkZoom: TCheckBox;
    cbbZoom: TComboBox;
    lblLayersCount: TLabel;
    pnlLayers: TPanel;
    chkAll: TCheckBox;
    chkLayers: TCheckBox;
    chkMap: TCheckBox;
    grdpnlHotkey1: TGridPanel;
    pnlCoords: TPanel;
    chkCoords: TCheckBox;
    procedure btnCancelClick(Sender: TObject);
    procedure chkMapClick(Sender: TObject);
    procedure chkLayersClick(Sender: TObject);
    procedure chkZoomClick(Sender: TObject);
    procedure chklstMapsClickCheck(Sender: TObject);
    procedure chkAllClick(Sender: TObject);
    procedure btnResetHotKeyClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure chkCoordsClick(Sender: TObject);
  private
    FMapSetItem: IFavoriteMapSetItemStatic;
    FfrMapSelect: TfrMapSelect;
    FfrLonLat: TfrLonLat;
    FFavoriteMapSetConfig: IFavoriteMapSetConfig;
    FViewPortState: ILocalCoordConverterChangeable;
    FMainMapConfig: IActiveMapConfig;
    FMainLayersConfig: IActiveLayersConfig;
    FFullMapsSet: IMapTypeSet;
    FGUIConfigList: IMapTypeGUIConfigList;
    function GetAllowWrite(const AMapType: IMapType): Boolean;
    procedure Init;
  public
    function DoAdd: Boolean;
    function DoUpdate(const AItemGUID: TGUID): Boolean;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AFavoriteMapSetConfig: IFavoriteMapSetConfig;
      const AViewPortState: ILocalCoordConverterChangeable;
      const AProjectionSet: IProjectionSetChangeable;
      const ACoordRepresentationConfig: ICoordRepresentationConfig;
      const ACoordFromStringParser: ICoordFromStringParser;
      const ACoordToStringConverter: ICoordToStringConverterChangeable;
      const AMainMapConfig: IActiveMapConfig;
      const AMainLayersConfig: IActiveLayersConfig;
      const AFullMapsSet: IMapTypeSet;
      const AGUIConfigList: IMapTypeGUIConfigList
    );
    destructor Destroy; override;
  end;

implementation

uses
  gnugettext,
  Math,
  c_ZeroGUID,
  t_GeoTypes,
  i_LocalCoordConverter,
  i_GUIDListStatic,
  u_GeoFunc,
  u_GUIDListStatic;

{$R *.dfm}

{ TfrmFavoriteMapSetEditor }

constructor TfrmFavoriteMapSetEditor.Create(
  const ALanguageManager: ILanguageManager;
  const AFavoriteMapSetConfig: IFavoriteMapSetConfig;
  const AViewPortState: ILocalCoordConverterChangeable;
  const AProjectionSet: IProjectionSetChangeable;
  const ACoordRepresentationConfig: ICoordRepresentationConfig;
  const ACoordFromStringParser: ICoordFromStringParser;
  const ACoordToStringConverter: ICoordToStringConverterChangeable;
  const AMainMapConfig: IActiveMapConfig;
  const AMainLayersConfig: IActiveLayersConfig;
  const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList
);
var
  VMapSelectFrameBuilder: IMapSelectFrameBuilder;
begin
  inherited Create(ALanguageManager);
  FFavoriteMapSetConfig := AFavoriteMapSetConfig;
  FViewPortState := AViewPortState;
  FMainMapConfig := AMainMapConfig;
  FMainLayersConfig := AMainLayersConfig;
  FFullMapsSet := AFullMapsSet;
  FGUIConfigList := AGUIConfigList;

  VMapSelectFrameBuilder :=
    TMapSelectFrameBuilder.Create(
      ALanguageManager,
      AMainMapConfig,
      AMainLayersConfig,
      AGUIConfigList,
      AFullMapsSet
    );

  FfrMapSelect :=
    VMapSelectFrameBuilder.Build(
      mfMaps,
      False,
      False,
      GetAllowWrite
    );

  FfrLonLat :=
    TfrLonLat.Create(
      ALanguageManager,
      AProjectionSet,
      AViewPortState,
      ACoordRepresentationConfig,
      ACoordFromStringParser,
      ACoordToStringConverter,
      tssCenter
    );

  FMapSetItem := nil;
end;

destructor TfrmFavoriteMapSetEditor.Destroy;
begin
  FreeAndNil(FfrMapSelect);
  FreeAndNil(FfrLonLat);
  inherited Destroy;
end;

procedure TfrmFavoriteMapSetEditor.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmFavoriteMapSetEditor.Init;
var
  I: Integer;
  VMapType: IMapType;
  VAddedIndex: Integer;
  VActiveLayers: IGUIDSetStatic;
  VGUIDList: IGUIDListStatic;
  VGUID: TGUID;
  VCoordConverter: ILocalCoordConverter;
begin
  VCoordConverter := FViewPortState.GetStatic;

  FfrMapSelect.Show(pnlMap);
  FfrLonLat.Parent := pnlCoords;

  if Assigned(FMapSetItem) then begin
    edtName.Text := FMapSetItem.Name;
    chkMap.Checked := False;
    if not IsEqualGUID(FMapSetItem.BaseMap, CGUID_Zero) then begin
      if FfrMapSelect.TrySelectMapType(FMapSetItem.BaseMap) then begin
        chkMap.Checked := True;
      end else begin
        MessageDlg(_('Can''t bound the Map - GUID not found!'), mtError, [mbOK], 0);
      end;
    end;
    VActiveLayers := FMapSetItem.Layers;
    chkMergeLayers.Checked := not FMapSetItem.MergeLayers;
    EditHotKey.HotKey := FMapSetItem.HotKey;
  end else begin
    edtName.Text := '';
    EditHotKey.HotKey := 0;
    VActiveLayers := FMainLayersConfig.LayerGuids;
  end;

  chkAll.Checked := False;
  chkLayers.Checked := (VActiveLayers <> nil);

  chklstMaps.Items.Clear;
  chklstMaps.ItemIndex := -1;
  VGUIDList := FGUIConfigList.OrderedMapGUIDList;
  for I := 0 to VGUIDList.Count - 1 do begin
    VGUID := VGUIDList.Items[I];
    VMapType := FFullMapsSet.GetMapTypeByGUID(VGUID);
    if VMapType.GUIConfig.Enabled and VMapType.Zmp.IsLayer then begin
      VAddedIndex := chklstMaps.Items.AddObject(VMapType.GUIConfig.Name.Value, TObject(Pointer(VMapType)));
      if Assigned(VActiveLayers) and VActiveLayers.IsExists(VMapType.Zmp.GUID) then begin
        if chklstMaps.ItemIndex = -1 then begin
          chklstMaps.ItemIndex := VAddedIndex;
        end;
        chklstMaps.Checked[VAddedIndex] := True;
      end;
    end;
  end;

  cbbZoom.Items.Clear;
  for I := 1 to 24 do begin
    cbbZoom.Items.Add(IntToStr(I));
  end;
  if (FMapSetItem <> nil) and (FMapSetItem.Zoom >= 0) then begin
    cbbZoom.ItemIndex := FMapSetItem.Zoom;
    chkZoom.Checked := True;
  end else begin
    cbbZoom.ItemIndex := VCoordConverter.Projection.Zoom;
  end;

  if (FMapSetItem <> nil) and not PointIsEmpty(FMapSetItem.LonLat) then begin
    FfrLonLat.LonLat := FMapSetItem.LonLat;
    chkCoords.Checked := True;
  end else begin
    FfrLonLat.LonLat := VCoordConverter.GetCenterLonLat;
  end;

  chkMapClick(nil);
  chkLayersClick(nil);
  chklstMapsClickCheck(nil);
  chkZoomClick(nil);
  chkCoordsClick(nil);
end;

procedure TfrmFavoriteMapSetEditor.chkMapClick(Sender: TObject);
begin
  FfrMapSelect.cbbMap.Enabled := chkMap.Checked;
end;

procedure TfrmFavoriteMapSetEditor.btnResetHotKeyClick(Sender: TObject);
begin
  EditHotKey.HotKey := 0;
end;

procedure TfrmFavoriteMapSetEditor.chkAllClick(Sender: TObject);
var
  I: Integer;
  VChkCount: Integer;
begin
  if chkAll.Checked then begin
    VChkCount := chklstMaps.Count;
  end else begin
    VChkCount := 0;
  end;
  for I := 0 to chklstMaps.Count - 1 do begin
    chklstMaps.Checked[I] := chkAll.Checked
  end;
  lblLayersCount.Caption := Format('(%d of %d)', [VChkCount, chklstMaps.Count]);
end;

procedure TfrmFavoriteMapSetEditor.chkCoordsClick(Sender: TObject);
begin
  FfrLonLat.Enabled := chkCoords.Checked;
end;

procedure TfrmFavoriteMapSetEditor.chklstMapsClickCheck(Sender: TObject);
var
  I: Integer;
  VChkCount: Integer;
begin
  VChkCount := 0;
  for I := 0 to chklstMaps.Count - 1 do begin
    if chklstMaps.Checked[I] then begin
      Inc(VChkCount);
    end;
  end;
  lblLayersCount.Caption := Format('(%d of %d)', [VChkCount, chklstMaps.Count]);
end;

procedure TfrmFavoriteMapSetEditor.chkLayersClick(Sender: TObject);
begin
  chklstMaps.Enabled := chkLayers.Checked;
  chkAll.Enabled := chkLayers.Checked;
  lblLayersCount.Enabled := chkLayers.Checked;
end;

procedure TfrmFavoriteMapSetEditor.chkZoomClick(Sender: TObject);
begin
  cbbZoom.Enabled := chkZoom.Checked;
end;

function TfrmFavoriteMapSetEditor.GetAllowWrite(const AMapType: IMapType): Boolean;
begin
  Result := True;
end;

function TfrmFavoriteMapSetEditor.DoAdd: Boolean;
begin
  FMapSetItem := nil;
  Init;
  Caption := _('Add to Favorites');
  btnOk.Caption := _('Add');
  Result := ShowModal = mrOk;;
end;

function TfrmFavoriteMapSetEditor.DoUpdate(const AItemGUID: TGUID): Boolean;
begin
  FMapSetItem := FFavoriteMapSetConfig.GetByID(AItemGUID);
  Assert(FMapSetItem <> nil);
  Init;
  Caption := _('Edit');
  btnOk.Caption := _('Save');
  Result := ShowModal = mrOk;;
end;

procedure TfrmFavoriteMapSetEditor.btnOkClick(Sender: TObject);

  function _GetLayers: IGUIDSetStatic;
  var
    I, J: Integer;
    VList: array of TGUID;
    VMapType: IMapType;
  begin
    Result := nil;
    J := 0;
    SetLength(VList, chklstMaps.Count);
    for I := 0 to chklstMaps.Count - 1 do begin
      if chklstMaps.Checked[I] then begin
        VMapType := IMapType(Pointer(chklstMaps.Items.Objects[I]));
        if VMapType <> nil then begin
          VList[J] := VMapType.GUID;
          Inc(J);
        end;
      end;
    end;
    if J > 0 then begin
      SetLength(VList, J);
      Result := TGUIDSetStatic.CreateAndSort(VList, J);
    end;
  end;

var
  VBaseMap: TGUID;
  VLayers: IGUIDSetStatic;
  VZoom: Integer;
  VName: string;
  VLonLat: TDoublePoint;
begin
  VName := Trim(edtName.Text);

  if VName = '' then begin
    MessageDlg(_('The Name can''t be empty!'), mtError, [mbOK], 0);
    Exit;
  end;

  if chkMap.Checked then begin
    VBaseMap := FfrMapSelect.GetSelectedMapType.GUID;
  end else begin
    VBaseMap := CGUID_Zero;
  end;

  if chkLayers.Checked then begin
    VLayers := _GetLayers;
  end else begin
    VLayers := nil;
  end;

  if chkZoom.Checked then begin
    VZoom := cbbZoom.ItemIndex;
  end else begin
    VZoom := -1;
  end;

  VLonLat := CEmptyDoublePoint;
  if chkCoords.Checked then begin
    if FfrLonLat.Validate then begin
      VLonLat := FfrLonLat.LonLat;
    end else begin
      Exit;
    end;
  end;

  if (VZoom = -1) and PointIsEmpty(VLonLat) and
    IsEqualGUID(VBaseMap, CGUID_Zero) and (VLayers = nil) then begin
    MessageDlg(
      _('Please, select zoom/coordinates or at least one Layer or Map first!'),
      mtError, [mbOK], 0
    );
    Exit;
  end;

  if FMapSetItem = nil then begin
    FFavoriteMapSetConfig.Add(
      VBaseMap,
      VLayers,
      not chkMergeLayers.Checked,
      VZoom,
      VLonLat,
      VName,
      EditHotKey.HotKey
    );
  end else begin
    FFavoriteMapSetConfig.Update(
      FMapSetItem.ID,
      VBaseMap,
      VLayers,
      not chkMergeLayers.Checked,
      VZoom,
      VLonLat,
      VName,
      EditHotKey.HotKey
    );
  end;

  ModalResult := mrOk;
end;

end.
