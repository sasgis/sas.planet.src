{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
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
  ComCtrls,
  Spin,
  ExtCtrls,
  i_LanguageManager,
  i_MapType,
  i_MapVersionRequest,
  i_MapTypeSet,
  i_MapTypeListStatic,
  i_MapTypeListBuilder,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  i_MapTypeListChangeable,
  i_GeometryLonLat,
  i_ContentTypeInfo,
  i_ContentTypeManager,
  i_BitmapTileSaveLoad,
  i_BitmapLayerProvider,
  i_Bitmap32BufferFactory,
  i_BitmapTileSaveLoadFactory,
  i_TileStorageTypeList,
  i_RegionProcessParamsFrame,
  fr_MapSelect,
  fr_ZoomsSelect,
  fr_CacheTypeList,
  fr_ImageFormatSelect,
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

    function GetSetTargetVersionValue: string;
    property SetTargetVersionValue: string read GetSetTargetVersionValue;

    // For modifications
    function GetMapSource: IMapType;
    property MapSource: IMapType read GetMapSource;

    function GetOverlay: IMapType;
    property Overlay: IMapType read GetOverlay;

    function GetBitmapTileSaver: IBitmapTileSaver;
    property BitmapTileSaver: IBitmapTileSaver read GetBitmapTileSaver;

    function GetContentType: IContentTypeInfoBasic;
    property ContentType: IContentTypeInfoBasic read GetContentType;

    function GetUseMarks: Boolean;
    property UseMarks: Boolean read GetUseMarks;

    function GetUseGrids: Boolean;
    property UseGrids: Boolean read GetUseGrids;

    function GetUseFillingMap: Boolean;
    property UseFillingMap: Boolean read GetUseFillingMap;

    function GetUseRecolor: Boolean;
    property UseRecolor: Boolean read GetUseRecolor;

    function GetUsePreciseCropping: Boolean;
    property UsePreciseCropping: Boolean read GetUsePreciseCropping;
  end;

type
  TfrTilesCopy = class(
      TFrame,
      IRegionProcessParamsFrameBase,
      IRegionProcessParamsFrameZoomArray,
      IRegionProcessParamsFrameTargetPath,
      IRegionProcessParamsFrameTilesCopy,
      IRegionProcessParamsFrameImageProvider
    )
    pnlCenter: TPanel;
    pnlZoom: TPanel;
    pnlMain: TPanel;
    lblNamesType: TLabel;
    pnlCacheTypes: TPanel;
    pnlTop: TPanel;
    lblTargetPath: TLabel;
    btnSelectTargetPath: TButton;
    chkReplaseTarget: TCheckBox;
    pnlHeader: TPanel;
    chkSetTargetVersionTo: TCheckBox;
    edSetTargetVersionValue: TEdit;
    pnSetTargetVersionOptions: TPanel;
    pcSource: TPageControl;
    tsDirectCopy: TTabSheet;
    tsOverlay: TTabSheet;
    chklstMaps: TCheckListBox;
    chkAllMaps: TCheckBox;
    lblOverlay: TLabel;
    pnlOverlay: TPanel;
    chkDeleteSource: TCheckBox;
    pnlImageFormat: TPanel;
    pnlMap: TPanel;
    lblMap: TLabel;
    chkPlaceInNameSubFolder: TCheckBox;
    chkAddVisibleLayers: TCheckBox;
    cbbTargetPath: TComboBox;
    chkAddVisibleOverlays: TCheckBox;
    chkUseRecolor: TCheckBox;
    procedure btnSelectTargetPathClick(Sender: TObject);
    procedure OnCacheTypeChange(Sender: TObject);
    procedure chkSetTargetVersionToClick(Sender: TObject);
    procedure chkAllMapsClick(Sender: TObject);
    procedure chkAddVisibleLayersClick(Sender: TObject);
  private
    FBitmap32StaticFactory: IBitmap32StaticFactory;
    FBitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
    FMapTypeListBuilderFactory: IMapTypeListBuilderFactory;
    FContentTypeManager: IContentTypeManager;
    FMainMapConfig: IActiveMapConfig;
    FFullMapsSet: IMapTypeSet;
    FActiveMapsList: IMapTypeListChangeable;
    FGUIConfigList: IMapTypeGUIConfigList;
    FfrMapSelect: TfrMapSelect;
    FfrOverlaySelect: TfrMapSelect;
    FfrZoomsSelect: TfrZoomsSelect;
    FfrCacheTypeList: TfrCacheTypeList;
    FfrImageFormatSelect: TfrImageFormatSelect;
  private
    procedure Init(
      const AZoom: Byte;
      const APolygon: IGeometryLonLatPolygon
    );
    function Validate: Boolean;
    procedure UpdateSetTargetVersionState;
    function GetSelectedLayer: IMapType;
  private
    function GetZoomArray: TByteDynArray;
    function GetPath: string;
  private
    function GetAllowCopy(const AMapType: IMapType): Boolean;
    { IRegionProcessParamsFrameTilesCopy }
    function GetReplaseTarget: Boolean;
    function GetDeleteSource: Boolean;
    function GetTargetCacheType: Byte;
    function GetMapTypeList: IMapTypeListStatic;
    function GetPlaceInNameSubFolder: Boolean;
    function GetSetTargetVersionEnabled: Boolean;
    function GetSetTargetVersionValue: String;

    function GetMapSource: IMapType;
    function GetOverlay: IMapType;
    function GetProvider: IBitmapTileUniProvider;
    function GetBitmapTileSaver: IBitmapTileSaver;
    function GetContentType: IContentTypeInfoBasic;
    function GetUseMarks: Boolean;
    function GetUseGrids: Boolean;
    function GetUseFillingMap: Boolean;
    function GetUseRecolor: Boolean;
    function GetUsePreciseCropping: Boolean;
  protected
    procedure OnShow(const AIsFirstTime: Boolean); override;
    procedure OnHide; override;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AMapSelectFrameBuilder: IMapSelectFrameBuilder;
      const AActiveMapsList: IMapTypeListChangeable;
      const AMapTypeListBuilderFactory: IMapTypeListBuilderFactory;
      const AMainMapConfig: IActiveMapConfig;
      const AFullMapsSet: IMapTypeSet;
      const AGUIConfigList: IMapTypeGUIConfigList;
      const ATileStorageTypeList: ITileStorageTypeListStatic;
      const ABitmap32StaticFactory: IBitmap32StaticFactory;
      const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
      const AContentTypeManager: IContentTypeManager
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
  i_TileStorageAbilities,
  u_BitmapLayerProviderMapWithLayer,
  i_GUIDListStatic;

{$R *.dfm}

constructor TfrTilesCopy.Create(
  const ALanguageManager: ILanguageManager;
  const AMapSelectFrameBuilder: IMapSelectFrameBuilder;
  const AActiveMapsList: IMapTypeListChangeable;
  const AMapTypeListBuilderFactory: IMapTypeListBuilderFactory;
  const AMainMapConfig: IActiveMapConfig;
  const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList;
  const ATileStorageTypeList: ITileStorageTypeListStatic;
  const ABitmap32StaticFactory: IBitmap32StaticFactory;
  const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
  const AContentTypeManager: IContentTypeManager
);
begin
  inherited Create(ALanguageManager);

  FActiveMapsList := AActiveMapsList;
  FMainMapConfig := AMainMapConfig;
  FMapTypeListBuilderFactory := AMapTypeListBuilderFactory;
  FFullMapsSet := AFullMapsSet;
  FGUIConfigList := AGUIConfigList;
  FBitmap32StaticFactory := ABitmap32StaticFactory;
  FBitmapTileSaveLoadFactory := ABitmapTileSaveLoadFactory;
  FContentTypeManager := AContentTypeManager;

  FfrCacheTypeList :=
    TfrCacheTypeList.Create(
      ALanguageManager,
      ATileStorageTypeList,
      False,
      CTileStorageTypeClassAll - [tstcInMemory],
      [tsacAdd],
      Self.OnCacheTypeChange
    );
  FfrCacheTypeList.IntCode := c_File_Cache_Id_SAS;

  FfrMapSelect :=
    AMapSelectFrameBuilder.Build(
      mfMaps,          // show maps
      True,            // add -NO- to combobox
      False,           // show disabled map
      GetAllowCopy
    );

  FfrOverlaySelect :=
    AMapSelectFrameBuilder.Build(
      mfLayers,        // show layers
      True,            // add -NO- to combobox
      False,           // show disabled map
      GetAllowCopy
    );

  FfrZoomsSelect :=
    TfrZoomsSelect.Create(
      ALanguageManager
    );
  FfrZoomsSelect.Init(0, 23);

  FfrImageFormatSelect :=
    TfrImageFormatSelect.Create(
      ALanguageManager,
      ABitmapTileSaveLoadFactory,
      CImageFormatAll
    );

  FPropertyState := CreateComponentPropertyState(
    Self, [pnlTop, chklstMaps, chkAllMaps], [], True, False, True, True
  );
end;

destructor TfrTilesCopy.Destroy;
begin
  FreeAndNil(FfrMapSelect);
  FreeAndNil(FfrOverlaySelect);
  FreeAndNil(FfrZoomsSelect);
  FreeAndNil(FfrCacheTypeList);
  FreeAndNil(FfrImageFormatSelect);
  inherited;
end;

procedure TfrTilesCopy.btnSelectTargetPathClick(Sender: TObject);
var
  I: Integer;
  VFound: Boolean;
  VTempPath: string;
begin
  if SelectDirectory('', '', VTempPath) then begin
    VFound := False;
    cbbTargetPath.Text := IncludeTrailingPathDelimiter(VTempPath);
    if cbbTargetPath.Items.Count > 0 then begin
      for I := 0 to cbbTargetPath.Items.Count - 1 do begin
        if cbbTargetPath.Items.ValueFromIndex[I] = cbbTargetPath.Text then begin
          VFound := True;
          Break;
        end;
      end;
    end;
    if not VFound then begin
      cbbTargetPath.Items.Add(cbbTargetPath.Text);
    end;
  end;
end;

procedure TfrTilesCopy.OnCacheTypeChange(Sender: TObject);
const
  CVersionedTileStorageId = [
    c_File_Cache_Id_DBMS,
    c_File_Cache_Id_BDB_Versioned,
    c_File_Cache_Id_SQLite
  ];
begin
  chkSetTargetVersionTo.Enabled := Self.GetTargetCacheType in CVersionedTileStorageId;
  UpdateSetTargetVersionState;
end;

procedure TfrTilesCopy.OnShow(const AIsFirstTime: Boolean);
begin
  inherited;

  if AIsFirstTime then begin
    if pcSource.ActivePageIndex < 0 then begin
      pcSource.ActivePageIndex := 0;
    end;
  end else begin
    FfrCacheTypeList.Visible := True;
    FfrImageFormatSelect.Visible := True;
  end;
end;

procedure TfrTilesCopy.OnHide;
begin
  inherited;
  FfrCacheTypeList.Hide;
  FfrImageFormatSelect.Hide;
end;

procedure TfrTilesCopy.chkAddVisibleLayersClick(Sender: TObject);
begin
  FfrOverlaySelect.SetEnabled(not chkAddVisibleLayers.Checked);
end;

procedure TfrTilesCopy.chkAllMapsClick(Sender: TObject);
var
  I: Byte;
begin
  if chkAllMaps.State <> cbGrayed then begin
    for I := 0 to chklstMaps.Count - 1 do begin
      if chklstMaps.ItemEnabled[I] or // Select only enabled items
         (not TCheckBox(Sender).Checked and chklstMaps.Checked[I]) // deselect disabled items
      then begin
        chklstMaps.Checked[I] := TCheckBox(Sender).Checked;
      end;
    end;
  end;
end;

procedure TfrTilesCopy.chkSetTargetVersionToClick(Sender: TObject);
begin
  UpdateSetTargetVersionState;
end;

function TfrTilesCopy.GetAllowCopy(const AMapType: IMapType): Boolean;
begin
  Result := AMapType.IsBitmapTiles;
end;

function TfrTilesCopy.GetSelectedLayer: IMapType;
var
  VActiveMapsSet: IMapTypeListStatic;
begin
  if chkAddVisibleLayers.Checked then begin
    VActiveMapsSet := FActiveMapsList.List;
    if Assigned(VActiveMapsSet) and (VActiveMapsSet.Count > 0) then begin
      Result := VActiveMapsSet.Items[0];
    end else begin
      Result := nil;
    end;
  end else begin
    Result := FfrOverlaySelect.GetSelectedMapType;
  end;
end;

function TfrTilesCopy.GetBitmapTileSaver: IBitmapTileSaver;
begin
  if pcSource.ActivePageIndex = 0 then begin
    Result := nil;
    Exit;
  end;

  Result :=
    FfrImageFormatSelect.GetBitmapTileSaver(
      FfrMapSelect.GetSelectedMapType,
      Self.GetSelectedLayer
    );
end;

function TfrTilesCopy.GetContentType: IContentTypeInfoBasic;
var
  VContentType: AnsiString;
begin
  Result := nil;

  if pcSource.ActivePageIndex = 0 then begin
    Exit;
  end;

  VContentType :=
    FfrImageFormatSelect.GetContentType(
      FfrMapSelect.GetSelectedMapType,
      Self.GetSelectedLayer
    );

  if VContentType <> '' then begin
    Result := FContentTypeManager.GetInfo(VContentType);
  end;
end;

function TfrTilesCopy.GetDeleteSource: Boolean;
begin
  if pcSource.ActivePageIndex = 0 then begin
    Result := chkDeleteSource.Checked;
  end else begin
    Result := False;
  end;
end;

function TfrTilesCopy.GetMapSource: IMapType;
begin
  Result := FfrMapSelect.GetSelectedMapType;
  if not Assigned(Result) then begin
    Result := GetSelectedLayer;
  end;
end;

function TfrTilesCopy.GetMapTypeList: IMapTypeListStatic;
var
  I: Integer;
  VMaps: IMapTypeListBuilder;
  VMapType: IMapType;
begin
  if pcSource.ActivePageIndex <> 0 then begin
    Result := nil;
    Exit;
  end;

  VMaps := FMapTypeListBuilderFactory.Build;
  for I := 0 to chklstMaps.Items.Count - 1 do begin
    if chklstMaps.Checked[I] then begin
      VMapType := IMapType(Pointer(chklstMaps.Items.Objects[I]));
      if VMapType <> nil then begin
        VMaps.Add(VMapType);
      end;
    end;
  end;
  Result := VMaps.MakeAndClear;
end;

function TfrTilesCopy.GetOverlay: IMapType;
begin
  Result := nil;
  if (pcSource.ActivePageIndex <> 0) and not chkAddVisibleLayers.Checked then begin
    Result := FfrOverlaySelect.GetSelectedMapType;
  end;
end;

function TfrTilesCopy.GetPath: string;
begin
  Result := cbbTargetPath.Text;
end;

function TfrTilesCopy.GetPlaceInNameSubFolder: Boolean;
begin
  Result := chkPlaceInNameSubFolder.Checked;
end;

function TfrTilesCopy.GetProvider: IBitmapTileUniProvider;
var
  VMap: IMapType;
  VMapVersion: IMapVersionRequest;
  VLayer: IMapType;
  VLayerVersion: IMapVersionRequest;
  VActiveMapsSet: IMapTypeListStatic;
begin
  if pcSource.ActivePageIndex = 0 then begin
    Result := nil;
    Exit;
  end;

  VMap := FfrMapSelect.GetSelectedMapType;
  if Assigned(VMap) then begin
    VMapVersion := VMap.VersionRequest.GetStatic;
  end else begin
    VMapVersion := nil;
  end;

  VLayer := FfrOverlaySelect.GetSelectedMapType;
  if Assigned(VLayer) then begin
    VLayerVersion := VLayer.VersionRequest.GetStatic;
  end else begin
    VLayerVersion := nil;
  end;

  if chkAddVisibleLayers.Checked then begin
    VLayer := nil;
    VLayerVersion := nil;
    VActiveMapsSet := FActiveMapsList.List;
  end else begin
    VActiveMapsSet := nil;
  end;

  Result :=
    TBitmapLayerProviderMapWithLayer.Create(
      FBitmap32StaticFactory,
      VMap,
      VMapVersion,
      VLayer,
      VLayerVersion,
      VActiveMapsSet,
      False,
      False
    );
end;

function TfrTilesCopy.GetReplaseTarget: Boolean;
begin
  Result := chkReplaseTarget.Checked;
end;

function TfrTilesCopy.GetSetTargetVersionEnabled: Boolean;
begin
  Result := chkSetTargetVersionTo.Enabled and chkSetTargetVersionTo.Checked;
end;

function TfrTilesCopy.GetSetTargetVersionValue: string;
begin
  Result := Trim(edSetTargetVersionValue.Text);
end;

function TfrTilesCopy.GetTargetCacheType: Byte;
begin
  Result := Byte(FfrCacheTypeList.IntCode);
end;

function TfrTilesCopy.GetUseFillingMap: Boolean;
begin
  Result := chkAddVisibleOverlays.Checked;
end;

function TfrTilesCopy.GetUseGrids: Boolean;
begin
  Result := chkAddVisibleOverlays.Checked;
end;

function TfrTilesCopy.GetUseMarks: Boolean;
begin
  Result := chkAddVisibleOverlays.Checked;
end;

function TfrTilesCopy.GetUseRecolor: Boolean;
begin
  Result := chkUseRecolor.Checked;
end;

function TfrTilesCopy.GetUsePreciseCropping: Boolean;
begin
  Result := False;
end;

function TfrTilesCopy.GetZoomArray: TByteDynArray;
begin
  Result := FfrZoomsSelect.GetZoomList;
end;

procedure TfrTilesCopy.Init;
var
  I: Integer;
  VMapType: IMapType;
  VActiveMapGUID: TGUID;
  VAddedIndex: Integer;
  VGUIDList: IGUIDListStatic;
  VGUID: TGUID;
begin
  FfrMapSelect.Show(pnlMap);
  FfrOverlaySelect.Show(pnlOverlay);
  FfrZoomsSelect.Show(pnlZoom);
  FfrCacheTypeList.Show(pnlCacheTypes);
  FfrImageFormatSelect.Show(pnlImageFormat);

  OnCacheTypeChange(nil);

  VActiveMapGUID := FMainMapConfig.MainMapGUID;
  chklstMaps.Items.Clear;
  VGUIDList := FGUIConfigList.OrderedMapGUIDList;
  For I := 0 to VGUIDList.Count - 1 do begin
    VGUID := VGUIDList.Items[I];
    VMapType := FFullMapsSet.GetMapTypeByGUID(VGUID);
    if (VMapType.GUIConfig.Enabled) then begin
      VAddedIndex := chklstMaps.Items.AddObject(VMapType.GUIConfig.Name.Value, TObject(Pointer(VMapType)));
      if IsEqualGUID(VMapType.Zmp.GUID, VActiveMapGUID) then begin
        chklstMaps.ItemIndex := VAddedIndex;
        chklstMaps.Checked[VAddedIndex] := True;
      end;
    end;
  end;
  FfrOverlaySelect.cbbMap.ItemIndex := 0;
end;

procedure TfrTilesCopy.UpdateSetTargetVersionState;
begin
  edSetTargetVersionValue.Enabled := chkSetTargetVersionTo.Enabled and chkSetTargetVersionTo.Checked;
end;

function TfrTilesCopy.Validate: Boolean;
var
  VMaps: IMapTypeListStatic;
begin
  Result := (cbbTargetPath.Text <> '');
  if not Result then begin
    ShowMessage(_('Please select output folder'));
    Exit;
  end;

  Result := FfrZoomsSelect.Validate;
  if not Result then begin
    ShowMessage(_('Please select at least one zoom'));
    Exit;
  end;

  if pcSource.ActivePageIndex = 0 then begin
    VMaps := GetMapTypeList;
    Result := Assigned(VMaps) and (VMaps.Count > 0);
    if not Result then begin
      ShowMessage(_('Please select at least one map'));
      Exit;
    end;
  end else begin
    Result := (FfrMapSelect.GetSelectedMapType <> nil) or (Self.GetSelectedLayer <> nil);
    if not Result then begin
      ShowMessage(_('Please select at least one map'));
      Exit;
    end;
  end;
end;

end.
