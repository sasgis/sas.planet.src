{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
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

unit fr_ExportToIMG;

interface

uses
  SysUtils,
  Classes,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  Spin,
  ComCtrls,
  ExtCtrls,
  Mask,
  CheckLst,
  TB2Item,
  TB2Toolbar,
  TB2Dock,
  TBX,
  i_LanguageManager,
  i_MapType,
  i_GeometryLonLat,
  i_ExportToIMGConfig,
  i_BitmapTileSaveLoadFactory,
  i_RegionProcessParamsFrame,
  t_ExportToIMGTask,
  u_CommonFormAndFrameParents,
  fr_MapSelect;

type
  IRegionProcessParamsFrameExportToIMG = interface(IRegionProcessParamsFrameTargetPath)
    ['{7E35F9DF-27C6-43D7-B14D-604BFE3A8616}']
    function GetTask: TExportToIMGTask;
    property Task: TExportToIMGTask read GetTask;
  end;

type
  TfrExportToIMG = class(
      TFrame,
      IRegionProcessParamsFrameBase,
      IRegionProcessParamsFrameTargetPath,
      IRegionProcessParamsFrameExportToIMG
    )
    pnlTop: TPanel;
    lblTargetFile: TLabel;
    edtTargetFile: TEdit;
    btnSelectTargetFile: TButton;
    dlgSaveTargetFile: TSaveDialog;
    pgcMain: TPageControl;
    tsMap: TTabSheet;
    tsSettings: TTabSheet;
    edtDrawOrder: TSpinEdit;
    lblMapFormat: TLabel;
    lblDrawOrder: TLabel;
    lblMapSeries: TLabel;
    edtMapID: TEdit;
    lblMapID: TLabel;
    lblMapName: TLabel;
    edtMapName: TEdit;
    lblMap: TLabel;
    pnlSettings: TPanel;
    cbbMapFormat: TComboBox;
    chkUseRecolor: TCheckBox;
    edtMapCompilerPath: TEdit;
    edtMapSeries: TMaskEdit;
    lblMapCompilerPath: TLabel;
    btnSetMapCompilerPath: TButton;
    dlgSetMapCompilerPath: TOpenDialog;
    dlgSetMapCompilerLicensePath: TOpenDialog;
    dlgSetGMTPath: TOpenDialog;
    ZoomGarmin: TCheckListBox;
    btnAddLayer: TButton;
    btnRemoveLayer: TButton;
    MapList: TListView;
    pnlListMaps: TPanel;
    pnlSasZoom: TPanel;
    pnlZooms: TPanel;
    lstSasZooms: TListBox;
    lblGarmin: TLabel;
    lblsas: TLabel;
    pnlMapselect: TPanel;
    pnlMaps: TPanel;
    pnlButtons: TPanel;
    TBXSettings: TTBXToolbar;
    tbSettings: TTBItem;
    TBXEdit: TTBXToolbar;
    TBEdit: TTBItem;
    TBReset: TTBItem;
    tbxtlbrGenerateId: TTBXToolbar;
    tbtmGenerateId: TTBItem;
    pnlGMT: TPanel;
    lblGMTPath: TLabel;
    edtGMTPath: TEdit;
    btnSetGMTPath: TButton;
    pnlLicense: TPanel;
    btnSetMapCompilerLicensePath: TButton;
    edtMapCompilerLicensePath: TEdit;
    lblMapCompilerLicensePath: TLabel;
    pnlCompiler: TPanel;
    pnlMapName: TPanel;
    lblVolumeSize: TLabel;
    lblCodePage: TLabel;
    cbbCodePage: TComboBox;
    chkKeepTempFiles: TCheckBox;
    pnlGMTTop: TPanel;
    lblWebSite: TLabel;
    pnlCompilerPath: TPanel;
    pnlLicensePath: TPanel;
    seVolumeSize: TSpinEdit;
    seJpegQuality: TSpinEdit;
    lblCompression: TLabel;
    procedure btnSelectTargetFileClick(Sender: TObject);
    procedure edtMapCompilePathChange(Sender: TObject);
    procedure edtMapCompilerLicensePathChange(Sender: TObject);
    procedure edtGMTPathChange(Sender: TObject);
    procedure btnSetMapCompilerPathClick(Sender: TObject);
    procedure btnSetMapCompilerLicensePathClick(Sender: TObject);
    procedure btnSetGMTPathClick(Sender: TObject);
    procedure btnAddLayerClick(Sender: TObject);
    procedure btnRemoveLayerClick(Sender: TObject);
    procedure ZoomGarminClick(Sender: TObject);
    procedure lstSasZoomsDblClick(Sender: TObject);
    procedure lstSasZoomsClick(Sender: TObject);
    procedure tbSettingsClick(Sender: TObject);
    procedure ZoomGarminClickCheck(Sender: TObject);
    procedure MapListSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure TBResetClick(Sender: TObject);
    procedure MapListCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure MapListCustomDrawItem(Sender: TCustomListView; Item: TListItem;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure tbtmGenerateIdClick(Sender: TObject);
    procedure ZoomGarminDblClick(Sender: TObject);
    procedure lblWebSiteClick(Sender: TObject);
  private
    FExportToIMGConfig: IExportToIMGConfig;
    FBitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
    FfrMapSelect: TfrMapSelect;

    function GetAllowExport(const AMapType: IMapType): boolean;
    procedure SetSASZooms(const AStr: String);
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
      const AMapSelectFrameBuilder: IMapSelectFrameBuilder;
      const AFileFilters: string;
      const AFileExtDefault: string
    ); reintroduce;
    destructor Destroy; override;

    // IRegionProcessParamsFrameBase
    procedure Init(
      const AZoom: byte;
      const APolygon: IGeometryLonLatPolygon
    );
    function Validate: Boolean;

    // IRegionProcessParamsFrameTargetPath
    function GetPath: string;

    // IRegionProcessParamsFrameExportToIMG
    function GetTask: TExportToIMGTask;
  end;

implementation

uses
  Windows,
  Graphics,
  gnugettext,
  u_Dialogs,
  u_InetFunc,
  u_FileSystemFunc,
  u_GlobalState;

{$R *.dfm}

const
  CDefaultSasZooms = '6,7,8,9,10,11,12,13,14,15,16,17,18';
  CGMapToolHomepage = 'https://www.gmaptool.eu/en/content/windows-setup';

function GenerateMapId: LongWord;
const
  CLimit = $0A00;
begin
  Result := Random(CLimit) shl 16 + Random(CLimit - 1);
end;

function GetUserDefaultUILanguage: LANGID; stdcall;
  external 'kernel32.dll' name 'GetUserDefaultUILanguage';

const
  LOCALE_RETURN_NUMBER = $20000000;


function FindSubstringInList(const AList: TStrings; const AStr: String): Integer;
var
  I: Integer;
begin
  for I := 0 to AList.Count - 1 do begin
    if Pos(AStr, AList[I]) > 0 then begin
      Result := I;
      Exit;
    end;
  end;
  Result := -1;
end;

constructor TfrExportToIMG.Create(
  const ALanguageManager: ILanguageManager;
  const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
  const AMapSelectFrameBuilder: IMapSelectFrameBuilder;
  const AFileFilters: string;
  const AFileExtDefault: string
);
var
  VCodePage: DWORD;
  VIndex: Integer;
begin
  inherited Create(ALanguageManager);

  FExportToIMGConfig := GState.Config.ExportToIMGConfig;

  FBitmapTileSaveLoadFactory := ABitmapTileSaveLoadFactory;
  dlgSaveTargetFile.Filter := AFileFilters;
  dlgSaveTargetFile.DefaultExt := AFileExtDefault;

  FfrMapSelect :=
    AMapSelectFrameBuilder.Build(
      mfMaps, // show maps
      False,  // don't add -NO- to combobox
      False,  // don't show disabled map
      GetAllowExport
    );

  cbbMapFormat.ItemIndex := 2;
  lblWebSite.Caption := 'GMapTool';
  lblWebSite.Hint := CGMapToolHomepage;

  // Trying to autodetect the code page.
  VIndex := -1;
  if GetLocaleInfo(GetUserDefaultUILanguage, LOCALE_IDEFAULTANSICODEPAGE or LOCALE_RETURN_NUMBER, @VCodePage, sizeof(VCodePage)) <> 0 then begin
    VIndex := FindSubstringInList(cbbCodePage.Items, 'CP' + IntToStr(VCodePage));
  end;
  if (VIndex = -1) and (GetLocaleInfo(LOCALE_USER_DEFAULT, LOCALE_IDEFAULTANSICODEPAGE or LOCALE_RETURN_NUMBER, @VCodePage, sizeof(VCodePage)) <> 0) then begin
    VIndex := FindSubstringInList(cbbCodePage.Items, 'CP' + IntToStr(VCodePage));
  end;
  if VIndex = -1 then begin
    VIndex := FindSubstringInList(cbbCodePage.Items, 'CP1252');
  end;
  cbbCodePage.ItemIndex := VIndex;

  // Restore paths saved into the config file.
  edtMapCompilerPath.Text := FExportToIMGConfig.MapCompilerPath;
  edtMapCompilerLicensePath.Text := FExportToIMGConfig.MapCompilerLicensePath;
  edtGMTPath.Text := FExportToIMGConfig.GMTPath;

  // Restore form preferences.
  tbSettings.Checked := FExportToIMGConfig.ZoomOptionsVisible;
  tbSettingsClick(Self);

  SetSASZooms(CDefaultSasZooms);
  SetSASZooms(FExportToIMGConfig.SASZoomList);

  FPropertyState := CreateComponentPropertyState(
    Self, [pnlTop, tsMap, pnlCompiler, pnlLicense, pnlGMT, edtMapID, tbtmGenerateId], [], True, False, True, True
  );
end;

destructor TfrExportToIMG.Destroy;
begin
  FreeAndNil(FfrMapSelect);
  inherited;
end;

procedure TfrExportToIMG.SetSASZooms(const AStr: String);
var
  VStrList: TStringList;
  I, VOldIndex: Integer;
begin
  VStrList := TStringList.Create;
  try
    VStrList.CommaText := AStr;
    if VStrList.Count = 13 then begin
      for I := 0 to VStrList.Count - 1 do begin
        VStrList.Objects[I] := Pointer(StrToIntDef(VStrList[I], I + 6));
      end;

      VOldIndex := lstSasZooms.ItemIndex;
      lstSasZooms.Items.Assign(VStrList);
      lstSasZooms.ItemIndex := VOldIndex;

      TBReset.Enabled := AStr <> CDefaultSasZooms;
    end;
  finally
    VStrList.Free;
  end;
end;

procedure TfrExportToIMG.tbtmGenerateIdClick(Sender: TObject);
begin
  edtMapId.Text := IntToHex(GenerateMapId, 8);
end;

procedure TfrExportToIMG.TBResetClick(Sender: TObject);
begin
  SetSasZooms(CDefaultSasZooms);
  FExportToIMGConfig.SASZoomList := CDefaultSasZooms;
end;

procedure TfrExportToIMG.edtMapCompilePathChange(Sender: TObject);
begin
  FExportToIMGConfig.MapCompilerPath := edtMapCompilerPath.Text;
end;

procedure TfrExportToIMG.edtMapCompilerLicensePathChange(Sender: TObject);
begin
  FExportToIMGConfig.MapCompilerLicensePath := edtMapCompilerLicensePath.Text;
end;

procedure TfrExportToIMG.edtGMTPathChange(Sender: TObject);
begin
  FExportToIMGConfig.GMTPath := edtGMTPath.Text;
end;

procedure TfrExportToIMG.btnSelectTargetFileClick(Sender: TObject);
begin
  if dlgSaveTargetFile.Execute then begin
    dlgSaveTargetFile.InitialDir := ExtractFileDir(dlgSaveTargetFile.FileName);
    edtTargetFile.Text := dlgSaveTargetFile.FileName;
  end;
end;

procedure TfrExportToIMG.btnSetGMTPathClick(Sender: TObject);
begin
  if dlgSetGMTPath.Execute then begin
    dlgSetGMTPath.InitialDir := ExtractFileDir(dlgSetGMTPath.FileName);
    edtGMTPath.Text := dlgSetGMTPath.FileName;
  end;
end;

procedure TfrExportToIMG.btnSetMapCompilerLicensePathClick(Sender: TObject);
begin
  if dlgSetMapCompilerLicensePath.Execute then begin
    dlgSetMapCompilerLicensePath.InitialDir := ExtractFileDir(dlgSetMapCompilerLicensePath.FileName);
    edtMapCompilerLicensePath.Text := dlgSetMapCompilerLicensePath.FileName;
  end;
end;

procedure TfrExportToIMG.btnSetMapCompilerPathClick(Sender: TObject);
begin
  if dlgSetMapCompilerPath.Execute then begin
    dlgSetMapCompilerPath.InitialDir := ExtractFileDir(dlgSetMapCompilerPath.FileName);
    edtMapCompilerPath.Text := dlgSetMapCompilerPath.FileName;
  end;
end;

procedure TfrExportToIMG.btnAddLayerClick(Sender: TObject);
var
  I: Integer;
  VItem: TListItem;
begin
  ZoomGarmin.Items.BeginUpdate;
  for I := 0 to ZoomGarmin.Items.Count - 1 do begin
    if ZoomGarmin.Checked[I] then begin
      ZoomGarmin.State[I] := cbGrayed;
      ZoomGarmin.ItemEnabled[I] := false;
      MapList.Items.BeginUpdate;
      try
        MapList.AddItem(FfrMapSelect.Text, nil);
        VItem := MapList.Items[MapList.Items.Count - 1];
        VItem.SubItems.AddObject(lstSasZooms.Items[I], lstSasZooms.Items.Objects[I]);
        VItem.SubItems.AddObject(ZoomGarmin.Items[I],  Pointer(I));
        VItem.Data := Pointer(FfrMapSelect.GetSelectedMapType);
      finally
        MapList.Items.EndUpdate;
      end;
    end;
  end;
  ZoomGarmin.Items.EndUpdate;

  if (MapList.Items.Count = 1) and (edtMapName.Text = '') then begin
    edtMapName.Text := IMapType(MapList.Items[0].Data).GUIConfig.Name.Value;
  end;

  btnAddLayer.Enabled := False;
end;

procedure TfrExportToIMG.btnRemoveLayerClick(Sender: TObject);
var
  I, VZoomIndex: Integer;
begin
  if MapList.ItemIndex = -1 then Exit;
  ZoomGarmin.Items.BeginUpdate;
  for I := MapList.Items.Count - 1 downto 0 do begin
    if MapList.Items[I].Selected then begin
      VZoomIndex := Integer(MapList.Items[I].SubItems.Objects[1]);
      if (VZoomIndex >= 0) and (VZoomIndex <= ZoomGarmin.Items.Count) then begin
        ZoomGarmin.ItemEnabled[VZoomIndex] := True;
        ZoomGarmin.State[VZoomIndex] := cbUnchecked;
      end;
      MapList.Items.Delete(I);
    end;
  end;
  ZoomGarmin.Items.EndUpdate;
end;

procedure TfrExportToIMG.MapListCompare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
begin
  Compare := Integer(Item1.SubItems.Objects[1]) - Integer(Item2.SubItems.Objects[1]);
end;

procedure TfrExportToIMG.MapListCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
const
  CDrawColor: array [0..1] of TColor = (clWindow, cl3DLight);
begin
  if not Assigned(Item) then begin
    Exit;
  end;

  Sender.Canvas.Brush.Color := CDrawColor[Item.Index mod 2];
end;

procedure TfrExportToIMG.MapListSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  btnRemoveLayer.Enabled := Selected;
end;

procedure TfrExportToIMG.Init(const AZoom: byte; const APolygon: IGeometryLonLatPolygon);
begin
  if not Assigned(FfrMapSelect.Parent) then begin
    FfrMapSelect.Show(pnlMapSelect);
  end;

  edtMapID.Text := IntToHex(GenerateMapId, 8);
end;

procedure TfrExportToIMG.lblWebSiteClick(Sender: TObject);
begin
  OpenUrlInBrowser(CGMapToolHomepage);
end;

procedure TfrExportToIMG.lstSasZoomsClick(Sender: TObject);
begin
  ZoomGarmin.ItemIndex := lstSasZooms.ItemIndex;
  TBEdit.Enabled := True;
end;

procedure TfrExportToIMG.lstSasZoomsDblClick(Sender: TObject);
var
  I: Integer;
  VStr: String;
begin
  try
    I := StrToInt(InputBox(_('Change source zoom'), _('Zoom'), lstSasZooms.Items[lstSasZooms.ItemIndex]));
    if I in [1..24] then begin
      lstSasZooms.Items[lstSasZooms.ItemIndex] := IntToStr(I);
      lstSasZooms.Items.Objects[lstSasZooms.ItemIndex] := TObject(I);
      VStr := lstSasZooms.Items.CommaText;
      FExportToIMGConfig.SASZoomList := lstSasZooms.Items.CommaText;
      TBReset.Enabled := VStr <> CDefaultSasZooms;
    end;
  except
  end;
end;

function TfrExportToIMG.GetAllowExport(const AMapType: IMapType): boolean;
begin
  Result := AMapType.IsBitmapTiles;
end;

procedure TfrExportToIMG.tbSettingsClick(Sender: TObject);
begin
  pnlSasZoom.Visible := tbSettings.Checked;
  //pnlSasZoom.Left := pnlZooms.Left + pnlZooms.Width;
  FExportToIMGConfig.ZoomOptionsVisible := tbSettings.Checked;
end;

function TfrExportToIMG.Validate: Boolean;
begin
  Result := False;

  if MapList.Items.Count = 0 then begin
    ShowErrorMessage(_('Empty map list. Please add the layers to export!'));
    pgcMain.ActivePage := tsMap;
    Exit;
  end;

  edtTargetFile.Text := Trim(edtTargetFile.Text);

  if not IsValidFileName(edtTargetFile.Text) then begin
    ShowErrorMessage(_('Output file name is not set or incorrect!'));
    edtTargetFile.SetFocus;
    Exit;
  end;

  if IsRelativePath(ExtractFilePath(edtTargetFile.Text)) then begin
    ShowErrorMessage(_('Specify the full (absolute) path to the output file!'));
    edtTargetFile.SetFocus;
    Exit;
  end;

  if not FileExists(edtMapCompilerPath.Text) then begin
    ShowErrorMessage(_('MPC compiler path is not set or incorrect!'));
    pgcMain.ActivePage := tsSettings;
    edtMapCompilerPath.SetFocus;
    Exit;
  end;

  if not FileExists(edtGMTPath.Text) then begin
    ShowErrorMessage(_('GMT tool path is not set or incorrect!'));
    pgcMain.ActivePage := tsSettings;
    edtGMTPath.SetFocus;
    Exit;
  end;

  Result := True;
end;

procedure TfrExportToIMG.ZoomGarminClick(Sender: TObject);
begin
  lstSasZooms.ItemIndex := ZoomGarmin.ItemIndex;
  TBEdit.Enabled := True;
end;

procedure TfrExportToIMG.ZoomGarminClickCheck(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to ZoomGarmin.Items.Count - 1 do begin
    if ZoomGarmin.Checked[I] then begin
      btnAddLayer.Enabled := True;
      Exit;
    end;
  end;

  btnAddLayer.Enabled := False;
end;

procedure TfrExportToIMG.ZoomGarminDblClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to ZoomGarmin.Items.Count - 1 do begin
    if ZoomGarmin.Selected[I] and (ZoomGarmin.State[I] <> cbGrayed) then begin
      ZoomGarmin.Checked[I] := True;
      btnAddLayerClick(Self);
      Break;
    end;
  end;
end;

function TfrExportToIMG.GetPath: string;
begin
  Result := Trim(edtTargetFile.Text);
end;

function TfrExportToIMG.GetTask: TExportToIMGTask;
var
  I: Integer;
  VMapListItem: TListItem;
  VMap: IMapType;
  VSourceScale: Integer;
  VDeviceZoom: Integer;
  VPrevMap: Pointer;
  VPrevSourceScale: Integer;
  VItemCount: Integer;
  VItem: PExportToIMGTaskItem;
begin
  Result.FCodePageIndex := cbbCodePage.ItemIndex;
  Result.FMapName := edtMapName.Text;
  Result.FIMGMapFormat := TIMGMapFormat(cbbMapFormat.ItemIndex);
  Result.FDrawOrder := edtDrawOrder.Value;
  Result.FMapSeries := StrToInt(edtMapSeries.Text);
  Result.FMapID := StrToInt('$' + edtMapID.Text);
  Result.FUseRecolor := chkUseRecolor.Checked;
  Result.FBitmapTileSaver := FBitmapTileSaveLoadFactory.CreateJpegSaver(seJpegQuality.Value);
  Result.FVolumeSize := Cardinal(seVolumeSize.Value) * 1024 * 1024; // in bytes
  Result.FKeepTempFiles := chkKeepTempFiles.Checked;

  Result.FMapCompilerPath := edtMapCompilerPath.Text;
  Result.FMapCompilerLicensePath := edtMapCompilerLicensePath.Text;
  Result.FGMTPath := edtGMTPath.Text;

  VPrevMap := Nil;
  VPrevSourceScale := 0;
  VItemCount := 0;
  Result.FItems := Nil;

  for I := 0 to MapList.Items.Count - 1 do begin
    VMapListItem := MapList.Items[I];
    VSourceScale := Integer(VMapListItem.SubItems.Objects[0]) - 1;   // 1..24 => 0..23
    VDeviceZoom  := 12 - Integer(VMapListItem.SubItems.Objects[1]);  // 0 for the most detailed level, 12 for the least detailed one.

    // If the same source layer is used more than once in consequent device zooms, combine them to reduce the IMG file size.
    if (VItemCount > 0) and (VPrevMap = VMapListItem.Data) and (VPrevSourceScale = VSourceScale) and (Result.FItems[VItemCount - 1].FDeviceZoomStart = VDeviceZoom + 1) then begin
      Result.FItems[VItemCount - 1].FDeviceZoomStart := VDeviceZoom;
      continue;
    end;

    SetLength(Result.FItems, VItemCount + 1);
    VItem := @Result.FItems[VItemCount];
    Inc(VItemCount);

    VMap := IMapType(VMapListItem.Data);
    VItem.FSourceTileStorage := VMap.TileStorage;
    VItem.FSourceMapVersion := VMap.VersionRequest.GetStatic;
    VItem.FSourceScale := VSourceScale;
    VItem.FDeviceZoomStart := VDeviceZoom;
    VItem.FDeviceZoomEnd := VDeviceZoom;

    VPrevMap := VMapListItem.Data;
    VPrevSourceScale := VSourceScale;
  end;
end;

end.
