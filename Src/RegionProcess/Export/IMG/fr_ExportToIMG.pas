{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2017, SAS.Planet development team.                      *}
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
  TBXControls,
  i_LanguageManager,
  i_MapType,
  i_GeometryLonLat,
  i_BitmapTileSaveLoadFactory,
  i_RegionProcessParamsFrame,
  u_ExportToIMGTask,
  fr_MapSelect,
  u_CommonFormAndFrameParents,
  i_ExportToIMGConfig;

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
    PageControl1: TPageControl;
    Map: TTabSheet;
    Settings: TTabSheet;
    edtDrawOrder: TSpinEdit;
    lblMapFormat: TLabel;
    lblDrawOrder: TLabel;
    lblMapSeries: TLabel;
    edtMapID: TEdit;
    lblMapID: TLabel;
    lblMapName: TLabel;
    edtMapName: TEdit;
    lblMap: TLabel;
    PnlSettings: TPanel;
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
    TBXGenerateId: TTBXToolbar;
    TBGenerateId: TTBItem;
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
    edtVolumeSize: TEdit;
    lblCodePage: TLabel;
    cbbCodePage: TComboBox;
    chkKeepTempFiles: TCheckBox;
    pnlGMTTop: TPanel;
    lblWebSite: TLabel;
    LMapCompilerPath: TLabel;
    LLicenseFile: TLabel;
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
    procedure TBGenerateIdClick(Sender: TObject);
    procedure ZoomGarminDblClick(Sender: TObject);
    procedure lblWebSiteClick(Sender: TObject);
  private
    FExportToIMGConfig: IExportToIMGConfig;
    FBitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
    FfrMapSelect: TfrMapSelect;

    function GetAllowExport(const AMapType: IMapType): boolean;
    procedure SetSASZooms(const Str: String);
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
  IniFiles,
  {$IFNDef UNICODE}
  CompatibilityIniFiles,
  {$ENDIF}
  ALString,
  RegExprUtils,
  gnugettext,
  Graphics,
  i_ConfigDataProvider,
  i_PathConfig,
  u_ConfigDataProviderByIniFile,
  u_InetFunc,
  u_GlobalState;

{$R *.dfm}

const
  DefaultSASZooms    = '6,7,8,9,10,11,12,13,14,15,16,17,18';
  DefJPEGCompression = 95;
  cGMapToolHP = 'http://www.gmaptool.eu/en/content/windows-setup';

function GenerateMapId: LongWord;
const
  Limit = $0A00;
begin
  Result := Random(Limit) shl 16 + Random(Limit - 1);
end;

function GetUserDefaultUILanguage: LANGID; stdcall;
  external 'kernel32.dll' name 'GetUserDefaultUILanguage';

const
  LOCALE_RETURN_NUMBER = $20000000;


function FindSubstringInList(const List: TStrings; const Str: String): Integer;
var
  i: Integer;
begin
  for i:=0 to List.Count - 1 do begin
    if pos(Str, List[i]) > 0 then begin
      Result := i;
      exit;
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
  lblWebSite.Caption := cGMapToolHP;

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

  SetSASZooms(DefaultSASZooms);
  SetSASZooms(FExportToIMGConfig.SASZoomList);
end;

destructor TfrExportToIMG.Destroy;
begin
  FreeAndNil(FfrMapSelect);

  inherited;
end;

procedure ExchangeItems(
  lv: TListView;
  const i, j: Integer
);
var
  tempLI: TListItem;
begin
  lv.Items.BeginUpdate;
  try
    tempLI := TListItem.Create(lv.Items);
    try
      tempLI.Assign(lv.Items.Item[i]);
      lv.Items.Item[i].Assign(lv.Items.Item[j]);
      lv.Items.Item[j].Assign(tempLI);
    finally
      tempLI.Free;
    end;
  finally
    lv.Items.EndUpdate
  end;
end;

procedure TfrExportToIMG.SetSASZooms(const Str: String);
var
  StrList: TStringList;
  i, OldIndex: Integer;
begin
  StrList := TStringList.Create;
  try
    StrList.CommaText := Str;
    if StrList.Count = 13 then begin
      for i:=0 to StrList.Count - 1 do begin
        StrList.Objects[i] := Pointer(StrToIntDef(StrList[i], i + 6));
      end;

      OldIndex := lstSasZooms.ItemIndex;
      lstSasZooms.Items.Assign(StrList);
      lstSasZooms.ItemIndex := OldIndex;

      TBReset.Enabled := Str <> DefaultSASZooms;
    end;
  finally
    StrList.Free;
  end;
end;

procedure TfrExportToIMG.TBGenerateIdClick(Sender: TObject);
begin
  edtMapId.Text := IntToHex(GenerateMapId, 8);
end;

procedure TfrExportToIMG.TBResetClick(Sender: TObject);
begin
  SetSasZooms(DefaultSASZooms);
  FExportToIMGConfig.SASZoomList := DefaultSASZooms;
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
    edtTargetFile.Text := dlgSaveTargetFile.FileName;
  end;
end;

procedure TfrExportToIMG.btnSetGMTPathClick(Sender: TObject);
begin
  if dlgSetGMTPath.Execute then begin
    edtGMTPath.Text := dlgSetGMTPath.FileName;
  end;
end;

procedure TfrExportToIMG.btnSetMapCompilerLicensePathClick(Sender: TObject);
begin
  if dlgSetMapCompilerLicensePath.Execute then begin
    edtMapCompilerLicensePath.Text := dlgSetMapCompilerLicensePath.FileName;
  end;
end;

procedure TfrExportToIMG.btnSetMapCompilerPathClick(Sender: TObject);
begin
  if dlgSetMapCompilerPath.Execute then begin
    edtMapCompilerPath.Text := dlgSetMapCompilerPath.FileName;
  end;
end;

procedure TfrExportToIMG.btnAddLayerClick(Sender: TObject);
var
  i: integer;
  Item: TListItem;
begin
  ZoomGarmin.Items.BeginUpdate;
  for I := 0 to ZoomGarmin.Items.Count - 1 do begin
    if ZoomGarmin.Checked[i] then begin
      ZoomGarmin.State[i] := cbGrayed;
      ZoomGarmin.ItemEnabled[i] := false;
      MapList.Items.BeginUpdate;
      try
        MapList.AddItem(FfrMapSelect.Text, nil);
        Item := MapList.Items[MapList.Items.Count - 1];
        Item.SubItems.AddObject(lstSasZooms.Items[i], lstSasZooms.Items.Objects[i]);
        Item.SubItems.AddObject(ZoomGarmin.Items[i],  Pointer(i));
        Item.Data := Pointer(FfrMapSelect.GetSelectedMapType);
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
  i, ZoomIndex: Integer;
begin
  if MapList.ItemIndex = -1 then Exit;
  ZoomGarmin.Items.BeginUpdate;
  for i:=MapList.Items.Count - 1 downto 0 do begin
    if MapList.Items[i].Selected then begin
      ZoomIndex := Integer(MapList.Items[i].SubItems.Objects[1]);
      if (ZoomIndex >= 0) and (ZoomIndex <= ZoomGarmin.Items.Count) then begin
        ZoomGarmin.ItemEnabled[ZoomIndex] := True;
        ZoomGarmin.State[ZoomIndex] := cbUnchecked;
      end;
      MapList.Items.Delete(i);
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
  DrawColor: array [0..1] of TColor = (clWindow, cl3DLight);
begin
  if not Assigned(Item) then begin
    exit;
  end;

  Sender.Canvas.Brush.Color := DrawColor[Item.Index mod 2];
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
  OpenUrlInBrowser(cGMapToolHP);
end;

procedure TfrExportToIMG.lstSasZoomsClick(Sender: TObject);
begin
  ZoomGarmin.ItemIndex := lstSasZooms.ItemIndex;
  TBEdit.Enabled := True;
end;

procedure TfrExportToIMG.lstSasZoomsDblClick(Sender: TObject);
var
  i: integer;
  Str: String;
begin
  try
    i := StrToInt(InputBox(_('Change source zoom'), _('Zoom'), lstSasZooms.Items[lstSasZooms.ItemIndex]));
    if i in [1..24] then begin
      lstSasZooms.Items[lstSasZooms.ItemIndex] := IntToStr(i);
      lstSasZooms.Items.Objects[lstSasZooms.ItemIndex] := TObject(i);
      Str := lstSasZooms.Items.CommaText;
      FExportToIMGConfig.SASZoomList := lstSasZooms.Items.CommaText;
      TBReset.Enabled := Str <> DefaultSASZooms;
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
    ShowMessage(_('Empty map list. Please add the layers to export!'));
    PageControl1.ActivePage := Map;    
    exit;
  end;

  if edtTargetFile.Text = '' then begin
    ShowMessage(_('Please select output file first!'));
    edtTargetFile.SetFocus;
    exit;
  end;

  if not FileExists(edtMapCompilerPath.Text) then begin
    ShowMessage(_('MPC compiler path is not set or incorrect!'));
    PageControl1.ActivePage := Settings;
    edtMapCompilerPath.SetFocus;
    exit;
  end;

  if not FileExists(edtGMTPath.Text) then begin
    ShowMessage(_('GMT tool path is not set or incorrect!'));
    PageControl1.ActivePage := Settings;
    edtGMTPath.SetFocus;
    exit;
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
  i: Integer;
begin
  for i:=0 to ZoomGarmin.Items.Count - 1 do begin
    if ZoomGarmin.Checked[i] then begin
      btnAddLayer.Enabled := True;
      exit;
    end;
  end;

  btnAddLayer.Enabled := False;
end;

procedure TfrExportToIMG.ZoomGarminDblClick(Sender: TObject);
var
  i: integer;
begin
  for I := 0 to ZoomGarmin.Items.Count - 1 do begin
    if ZoomGarmin.Selected[i] and (ZoomGarmin.State[i] <> cbGrayed) then begin
      ZoomGarmin.Checked[i] := True;
      btnAddLayerClick(Self);
      break;
    end;
  end;
end;

function TfrExportToIMG.GetPath: string;
begin
  Result := edtTargetFile.Text;
end;

function TfrExportToIMG.GetTask: TExportToIMGTask;
var
  i: Integer;
  VMapListItem: TListItem;
  VMap: IMapType;
  VSourceScale: Integer;
  VDeviceZoom: Integer;
  VPrevMap: Pointer;
  VPrevSourceScale: Integer;
  VItemCount: Integer;
  VItem: ^TExportToIMGTaskItem; 
begin
  Result.FCodePageIndex := cbbCodePage.ItemIndex;
  Result.FMapName := edtMapName.Text;
  Result.FIMGMapFormat := TIMGMapFormat(cbbMapFormat.ItemIndex);
  Result.FDrawOrder := edtDrawOrder.Value;
  Result.FMapSeries := StrToInt(edtMapSeries.Text);
  Result.FMapID := StrToInt('$' + edtMapID.Text);
  Result.FUseRecolor := chkUseRecolor.Checked;
  Result.FBitmapTileSaver := FBitmapTileSaveLoadFactory.CreateJpegSaver(DefJPEGCompression);
  Result.FVolumeSize := StrToInt(edtVolumeSize.Text);
  Result.FKeepTempFiles := chkKeepTempFiles.Checked;

  Result.FMapCompilerPath := edtMapCompilerPath.Text;
  Result.FMapCompilerLicensePath := edtMapCompilerLicensePath.Text;
  Result.FGMTPath := edtGMTPath.Text;

  VPrevMap := Nil;
  VPrevSourceScale := 0;
  VItemCount := 0;
  Result.FItems := Nil; 

  for i:=0 to MapList.Items.Count - 1 do begin
    VMapListItem := MapList.Items[i];
    VSourceScale := Integer(VMapListItem.SubItems.Objects[0]) - 1;   // 1..24 => 0..23
    VDeviceZoom  := 12 - Integer(VMapListItem.SubItems.Objects[1]);  // 0 for the most detailed level, 12 for the least detailed one.

    // If the same source layer is used more than once in consequent device zooms, combine them to reduce the IMG file size.
    if (VItemCount > 0) and (VPrevMap = VMapListItem.Data) and (VPrevSourceScale = VSourceScale) and (Result.FItems[VItemCount - 1].FDeviceZoomStart = VDeviceZoom + 1) then begin
      Result.FItems[VItemCount - 1].FDeviceZoomStart := VDeviceZoom;
      continue;
    end;

    SetLength(Result.FItems, VItemCount + 1);
    VItem := @Result.FItems[VItemCount];
    inc(VItemCount);

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
