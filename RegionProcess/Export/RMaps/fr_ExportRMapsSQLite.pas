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

unit fr_ExportRMapsSQLite;

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
  Spin,
  i_LanguageManager,
  i_MapTypeSet,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  i_GeometryLonLat,
  i_RegionProcessParamsFrame,
  i_Bitmap32BufferFactory,
  i_BitmapLayerProvider,
  i_BitmapTileSaveLoad,
  i_BitmapTileSaveLoadFactory,
  i_MapTypes,
  fr_MapSelect,
  fr_ZoomsSelect,
  u_CommonFormAndFrameParents;

type
  IRegionProcessParamsFrameRMapsSQLiteExport = interface(IRegionProcessParamsFrameBase)
    ['{AF048EAA-5AE3-45CD-94FD-443DFCB580B6}']
    function GetForceDropTarget: Boolean;
    property ForceDropTarget: Boolean read GetForceDropTarget;

    function GetReplaceExistingTiles: Boolean;
    property ReplaceExistingTiles: Boolean read GetReplaceExistingTiles;

    function GetDirectTilesCopy: Boolean;
    property DirectTilesCopy: Boolean read GetDirectTilesCopy;

    function GetBitmapTileSaver: IBitmapTileSaver;
    property BitmapTileSaver: IBitmapTileSaver read GetBitmapTileSaver;
  end;

type
  TfrExportRMapsSQLite = class(
      TFrame,
      IRegionProcessParamsFrameBase,
      IRegionProcessParamsFrameZoomArray,
      IRegionProcessParamsFrameTargetPath,
      IRegionProcessParamsFrameOneMap,
      IRegionProcessParamsFrameImageProvider,
      IRegionProcessParamsFrameRMapsSQLiteExport
    )
    pnlCenter: TPanel;
    pnlTop: TPanel;
    lblTargetFile: TLabel;
    edtTargetFile: TEdit;
    btnSelectTargetFile: TButton;
    pnlMain: TPanel;
    chkReplaceExistingTiles: TCheckBox;
    chkForceDropTarget: TCheckBox;
    lblMap: TLabel;
    dlgSaveSQLite: TSaveDialog;
    pnlMap: TPanel;
    PnlZoom: TPanel;
    chkDirectTilesCopy: TCheckBox;
    lblOverlay: TLabel;
    pnlOverlay: TPanel;
    pnlImageFormat: TPanel;
    seJpgQuality: TSpinEdit;
    lblJpgQulity: TLabel;
    cbbImageFormat: TComboBox;
    lblImageFormat: TLabel;
    seCompression: TSpinEdit;
    lblCompression: TLabel;
    chkUsePrevZoom: TCheckBox;
    procedure btnSelectTargetFileClick(Sender: TObject);
    procedure chkDirectTilesCopyClick(Sender: TObject);
    procedure cbbImageFormatChange(Sender: TObject);
    procedure chkUsePrevZoomClick(Sender: TObject);
  private
    FMainMapsConfig: IMainMapsConfig;
    FFullMapsSet: IMapTypeSet;
    FGUIConfigList: IMapTypeGUIConfigList;
    FBitmapFactory: IBitmap32BufferFactory;
    FBitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
    FfrMapSelect: TfrMapSelect;
    FfrOverlaySelect: TfrMapSelect;
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
    function GetForceDropTarget: Boolean;
    function GetReplaceExistingTiles: Boolean;
    function GetDirectTilesCopy: Boolean;
    function GetAllowExport(const AMapType: IMapType): Boolean;
    function GetProvider: IBitmapLayerProvider;
    function GetBitmapTileSaver: IBitmapTileSaver;
    procedure OnDirectTilesCopyChange(const AEnableDirectCopy: Boolean);
    procedure OnMapChange(Sender: TObject);
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AMainMapsConfig: IMainMapsConfig;
      const AFullMapsSet: IMapTypeSet;
      const AGUIConfigList: IMapTypeGUIConfigList;
      const ABitmapFactory: IBitmap32BufferFactory;
      const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory
    ); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  gnugettext,
  Graphics,
  c_CoordConverter,
  i_MapVersionRequest,
  i_ContentTypeInfo,
  u_BitmapLayerProviderMapWithLayer;

{$R *.dfm}

constructor TfrExportRMapsSQLite.Create(
  const ALanguageManager: ILanguageManager;
  const AMainMapsConfig: IMainMapsConfig;
  const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList;
  const ABitmapFactory: IBitmap32BufferFactory;
  const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory
);
begin
  inherited Create(ALanguageManager);

  FMainMapsConfig := AMainMapsConfig;
  FFullMapsSet := AFullMapsSet;
  FGUIConfigList := AGUIConfigList;

  FBitmapFactory := ABitmapFactory;
  FBitmapTileSaveLoadFactory := ABitmapTileSaveLoadFactory;

  FfrMapSelect :=
    TfrMapSelect.Create(
      ALanguageManager,
      AMainMapsConfig,
      AGUIConfigList,
      AFullMapsSet,
      mfMaps,          // show maps
      True,            // add -NO- to combobox
      False,           // show disabled map
      GetAllowExport
    );
  FfrMapSelect.OnMapChange := Self.OnMapChange;

  FfrOverlaySelect :=
    TfrMapSelect.Create(
      ALanguageManager,
      AMainMapsConfig,
      AGUIConfigList,
      AFullMapsSet,
      mfLayers,        // show layers
      True,            // add -NO- to combobox
      False,           // show disabled map
      GetAllowExport
    );
  FfrOverlaySelect.OnMapChange := Self.OnMapChange;

  FfrZoomsSelect :=
    TfrZoomsSelect.Create(
      ALanguageManager
    );
  FfrZoomsSelect.Init(0, 23);
end;

destructor TfrExportRMapsSQLite.Destroy;
begin
  FreeAndNil(FfrMapSelect);
  FreeAndNil(FfrOverlaySelect);
  FreeAndNil(FfrZoomsSelect);
  inherited;
end;

procedure TfrExportRMapsSQLite.cbbImageFormatChange(Sender: TObject);
var
  VValue: Boolean;
begin
  VValue := not (cbbImageFormat.ItemIndex = 0) and cbbImageFormat.Enabled;

  lblJpgQulity.Enabled := VValue;
  seJpgQuality.Enabled := VValue;

  lblCompression.Enabled := VValue;
  seCompression.Enabled := VValue;

  if GetDirectTilesCopy then begin
    chkDirectTilesCopy.Font.Style := [fsBold];
  end else begin
    chkDirectTilesCopy.Font.Style := [];
  end;
end;

procedure TfrExportRMapsSQLite.OnDirectTilesCopyChange(const AEnableDirectCopy: Boolean);
var
  VMap: IMapType;
  VLayer: IMapType;
  VItemEnabled: Boolean;
begin
  VItemEnabled := not AEnableDirectCopy;

  if chkDirectTilesCopy.Checked then begin
    VMap := FfrMapSelect.GetSelectedMapType;
    VLayer := FfrOverlaySelect.GetSelectedMapType;

    if not Assigned(VMap) or not Assigned(VLayer) then begin
      FfrOverlaySelect.cbbMap.Enabled := not Assigned(VMap);
      lblOverlay.Enabled := not Assigned(VMap);

      FfrMapSelect.cbbMap.Enabled := not Assigned(VLayer);
      lblMap.Enabled := not Assigned(VLayer);
    end else begin
      FfrOverlaySelect.cbbMap.Enabled := VItemEnabled;
      lblOverlay.Enabled := VItemEnabled;
    end;

    chkUsePrevZoom.Enabled := VItemEnabled;

    lblImageFormat.Enabled := VItemEnabled;
    cbbImageFormat.Enabled := VItemEnabled;
  end else begin
    FfrOverlaySelect.cbbMap.Enabled := True;
    lblOverlay.Enabled := True;

    FfrMapSelect.cbbMap.Enabled := True;
    lblMap.Enabled := True;

    chkUsePrevZoom.Enabled := True;

    lblImageFormat.Enabled := VItemEnabled or (cbbImageFormat.ItemIndex = 0);
    cbbImageFormat.Enabled := VItemEnabled or (cbbImageFormat.ItemIndex = 0);
  end;

  cbbImageFormatChange(cbbImageFormat);

  if AEnableDirectCopy then begin
    chkDirectTilesCopy.Font.Style := [fsBold];
  end else begin
    chkDirectTilesCopy.Font.Style := [];
  end;
end;

procedure TfrExportRMapsSQLite.OnMapChange(Sender: TObject);
begin
  OnDirectTilesCopyChange(GetDirectTilesCopy);
end;

procedure TfrExportRMapsSQLite.chkDirectTilesCopyClick(Sender: TObject);
begin
  OnDirectTilesCopyChange(GetDirectTilesCopy);
end;

procedure TfrExportRMapsSQLite.chkUsePrevZoomClick(Sender: TObject);
begin
  OnDirectTilesCopyChange(GetDirectTilesCopy);
end;

procedure TfrExportRMapsSQLite.btnSelectTargetFileClick(Sender: TObject);
begin
  if dlgSaveSQLite.Execute then begin
    edtTargetFile.Text := dlgSaveSQLite.FileName;
  end;
end;

function TfrExportRMapsSQLite.GetAllowExport(const AMapType: IMapType): Boolean;
begin
  Result := AMapType.IsBitmapTiles;
end;

function TfrExportRMapsSQLite.GetMapType: IMapType;
var
  VMapType: IMapType;
begin
  VMapType := FfrMapSelect.GetSelectedMapType;
  if not Assigned(VMapType) then begin
    VMapType := FfrOverlaySelect.GetSelectedMapType;
  end;
  Result := VMapType;
end;

function TfrExportRMapsSQLite.GetForceDropTarget: Boolean;
begin
  Result := chkForceDropTarget.Checked;
end;

function TfrExportRMapsSQLite.GetPath: string;
begin
  Result := edtTargetFile.Text;
end;

function TfrExportRMapsSQLite.GetReplaceExistingTiles: Boolean;
begin
  Result := chkReplaceExistingTiles.Checked;
end;

function TfrExportRMapsSQLite.GetDirectTilesCopy: Boolean;
var
  VMap: IMapType;
  VLayer: IMapType;
begin
  Result := chkDirectTilesCopy.Checked;
  if not Result then begin
    VMap := FfrMapSelect.GetSelectedMapType;
    VLayer := FfrOverlaySelect.GetSelectedMapType;
    if Assigned(VMap) and not Assigned(VLayer) then begin
      if VMap.IsBitmapTiles then begin
        if VMap.GeoConvert.ProjectionEPSG = CGoogleProjectionEPSG then begin
          Result := True;
        end;
      end;
    end else if not Assigned(VMap) and Assigned(VLayer) then begin
      if VLayer.IsBitmapTiles then begin
        if VLayer.GeoConvert.ProjectionEPSG = CGoogleProjectionEPSG then begin
          Result := True;
        end;
      end;
    end;
    Result := Result and (cbbImageFormat.ItemIndex = 0) and not chkUsePrevZoom.Checked;
  end;
end;

function TfrExportRMapsSQLite.GetZoomArray: TByteDynArray;
begin
  Result := FfrZoomsSelect.GetZoomList;
end;

function TfrExportRMapsSQLite.GetProvider: IBitmapLayerProvider;
var
  VMap: IMapType;
  VMapVersion: IMapVersionRequest;
  VLayer: IMapType;
  VLayerVersion: IMapVersionRequest;
begin
  VMap := FfrMapSelect.GetSelectedMapType;
  if Assigned(VMap) then begin
    VMapVersion := VMap.VersionRequestConfig.GetStatic;
  end else begin
    VMapVersion := nil;
  end;

  VLayer := FfrOverlaySelect.GetSelectedMapType;
  if Assigned(VLayer) then begin
    VLayerVersion := VLayer.VersionRequestConfig.GetStatic;
  end else begin
    VLayerVersion := nil;
  end;

  Result :=
    TBitmapLayerProviderMapWithLayer.Create(
      FBitmapFactory,
      VMap,
      VMapVersion,
      VLayer,
      VLayerVersion,
      chkUsePrevZoom.Checked,
      chkUsePrevZoom.Checked
    );
end;

function TfrExportRMapsSQLite.GetBitmapTileSaver: IBitmapTileSaver;

  function _GetSaver(const AMap: IMapType): IBitmapTileSaver;
  var
    VContentType: IContentTypeInfoBitmap;
  begin
    Result := nil;
    if Assigned(AMap) then begin
      if Supports(AMap.ContentType, IContentTypeInfoBitmap, VContentType) then begin
        Result := VContentType.GetSaver;
      end;
    end;
  end;

begin
  if cbbImageFormat.ItemIndex = 0 then begin
    Result := _GetSaver(FfrMapSelect.GetSelectedMapType);
    if not Assigned(Result) then begin
      Result := _GetSaver(FfrOverlaySelect.GetSelectedMapType);
    end;
  end else begin
    case cbbImageFormat.ItemIndex of
      1: Result := FBitmapTileSaveLoadFactory.CreateJpegSaver(seJpgQuality.Value);
      2: Result := FBitmapTileSaveLoadFactory.CreateBmpSaver;
      3: Result := FBitmapTileSaveLoadFactory.CreateGifSaver;
      4: Result := FBitmapTileSaveLoadFactory.CreatePngSaver(i8bpp, seCompression.Value);
      5: Result := FBitmapTileSaveLoadFactory.CreatePngSaver(i24bpp, seCompression.Value);
      6: Result := FBitmapTileSaveLoadFactory.CreatePngSaver(i32bpp, seCompression.Value);
    end;
  end;
  if not Assigned(Result) then begin
    Assert(False, 'Unexpected result!');
    Result := FBitmapTileSaveLoadFactory.CreateJpegSaver;
  end;
end;

procedure TfrExportRMapsSQLite.Init;
begin
  FfrMapSelect.Show(pnlMap);
  FfrOverlaySelect.Show(pnlOverlay);
  FfrZoomsSelect.Show(pnlZoom);
  cbbImageFormat.ItemIndex := 0; // Auto
  OnDirectTilesCopyChange(GetDirectTilesCopy);
end;

function TfrExportRMapsSQLite.Validate: Boolean;
var
  VMap: IMapType;
  VLayer: IMapType;
begin
  Result := FfrZoomsSelect.Validate;
  if not Result then begin
    ShowMessage(_('Please select at least one zoom'));
  end;

  if Result then begin
    VMap := FfrMapSelect.GetSelectedMapType;
    VLayer := FfrOverlaySelect.GetSelectedMapType;
    if not Assigned(VMap) and not Assigned(VLayer) then begin
      Result := False;
      ShowMessage(_('Please select at least one map or overlay layer'));
    end;
  end;
end;

end.
