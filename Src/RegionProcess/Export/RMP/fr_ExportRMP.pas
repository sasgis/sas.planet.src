{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2016, SAS.Planet development team.                      *}
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

unit fr_ExportRMP;

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
  i_GeometryLonLat,
  i_RegionProcessParamsFrame,
  i_Bitmap32BufferFactory,
  i_BitmapLayerProvider,
  i_BitmapTileSaveLoad,
  i_BitmapTileSaveLoadFactory,
  i_MapType,
  i_MapTypeListChangeable,
  fr_MapSelect,
  fr_ZoomsSelect,
  u_CommonFormAndFrameParents;

type
  IRegionProcessParamsFrameRMPExport = interface(IRegionProcessParamsFrameBase)
    ['{4EE5A5F2-BA31-4F2A-9637-EC58F5A4A409}']
    function GetDirectTilesCopy: Boolean;
    property DirectTilesCopy: Boolean read GetDirectTilesCopy;

    function GetAlignSelection: Boolean;
    property AlignSelection: Boolean read GetAlignSelection;

    function GetProjectToLatLon: Boolean;
    property ProjectToLatLon: Boolean read GetProjectToLatLon;

    function GetRmpProduct: AnsiString;
    property RmpProduct: AnsiString read GetRmpProduct;

    function GetRmpProvider: AnsiString;
    property RmpProvider: AnsiString read GetRmpProvider;

    function GetBitmapTileSaver: IBitmapTileSaver;
    property BitmapTileSaver: IBitmapTileSaver read GetBitmapTileSaver;

    function GetBitmapUniProvider: IBitmapUniProvider;
    property BitmapUniProvider: IBitmapUniProvider read GetBitmapUniProvider;
  end;

type
  TfrExportRMP = class(
      TFrame,
      IRegionProcessParamsFrameBase,
      IRegionProcessParamsFrameZoomArray,
      IRegionProcessParamsFrameTargetPath,
      IRegionProcessParamsFrameOneMap,
      IRegionProcessParamsFrameRMPExport
    )
    pnlCenter: TPanel;
    pnlTop: TPanel;
    lblTargetFile: TLabel;
    edtTargetFile: TEdit;
    btnSelectTargetFile: TButton;
    pnlMain: TPanel;
    lblMap: TLabel;
    pnlMap: TPanel;
    PnlZoom: TPanel;
    lblOverlay: TLabel;
    pnlOverlay: TPanel;
    pnlImageFormat: TPanel;
    seJpgQuality: TSpinEdit;
    lblJpgQulity: TLabel;
    chkUsePrevZoom: TCheckBox;
    edtRmpProduct: TEdit;
    lblDesc: TLabel;
    lblAttr: TLabel;
    edtRmpProvider: TEdit;
    dlgSaveTo: TSaveDialog;
    chkAddVisibleLayers: TCheckBox;
    chkDontProjectToLatLon: TCheckBox;
    procedure btnSelectTargetFileClick(Sender: TObject);
    procedure chkAddVisibleLayersClick(Sender: TObject);
  private
    FLastPath: string;
    FBitmap32StaticFactory: IBitmap32StaticFactory;
    FBitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
    FActiveMapsList: IMapTypeListChangeable;
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
    function GetDirectTilesCopy: Boolean;
    function GetAlignSelection: Boolean;
    function GetProjectToLatLon: Boolean;
    function GetAllowExport(const AMapType: IMapType): Boolean;
    function GetBitmapUniProvider: IBitmapUniProvider;
    function GetRmpProduct: AnsiString;
    function GetRmpProvider: AnsiString;
    function GetBitmapTileSaver: IBitmapTileSaver;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AMapSelectFrameBuilder: IMapSelectFrameBuilder;
      const AActiveMapsList: IMapTypeListChangeable;
      const ABitmap32StaticFactory: IBitmap32StaticFactory;
      const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory
    ); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  gnugettext,
  ALString,
  c_CoordConverter,
  i_MapVersionRequest,
  i_ContentTypeInfo,
  i_MapTypeListStatic,
  u_StrFunc,
  u_BitmapLayerProviderMapWithLayer;

{$R *.dfm}

function IsJpegContentType(
  const AContentTypeInfo: IContentTypeInfoBasic
): Boolean;
var
  VContentType: AnsiString;
begin
  VContentType := AContentTypeInfo.GetContentType;
  Result := ALSameText(VContentType, 'image/jpg');
end;

{ TfrExportRMP }

constructor TfrExportRMP.Create(
  const ALanguageManager: ILanguageManager;
  const AMapSelectFrameBuilder: IMapSelectFrameBuilder;
  const AActiveMapsList: IMapTypeListChangeable;
  const ABitmap32StaticFactory: IBitmap32StaticFactory;
  const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory
);
begin
  Assert(Assigned(ABitmap32StaticFactory));
  inherited Create(ALanguageManager);

  FLastPath := '';

  FActiveMapsList := AActiveMapsList;
  FBitmap32StaticFactory := ABitmap32StaticFactory;
  FBitmapTileSaveLoadFactory := ABitmapTileSaveLoadFactory;

  FfrMapSelect :=
    AMapSelectFrameBuilder.Build(
      mfMaps,          // show maps
      True,            // add -NO- to combobox
      False,           // show disabled map
      GetAllowExport
    );

  FfrOverlaySelect :=
    AMapSelectFrameBuilder.Build(
      mfLayers,        // show layers
      True,            // add -NO- to combobox
      False,           // show disabled map
      GetAllowExport
    );

  FfrZoomsSelect :=
    TfrZoomsSelect.Create(
      ALanguageManager
    );
  FfrZoomsSelect.Init(0, 23);
end;

destructor TfrExportRMP.Destroy;
begin
  FreeAndNil(FfrMapSelect);
  FreeAndNil(FfrOverlaySelect);
  FreeAndNil(FfrZoomsSelect);
  inherited;
end;

procedure TfrExportRMP.chkAddVisibleLayersClick(Sender: TObject);
begin
  FfrOverlaySelect.SetEnabled(not chkAddVisibleLayers.Checked);
end;

procedure TfrExportRMP.btnSelectTargetFileClick(Sender: TObject);
begin
  if dlgSaveTo.Execute then begin
    edtTargetFile.Text := dlgSaveTo.FileName;
  end;
end;

function TfrExportRMP.GetAlignSelection: Boolean;
begin
  Result := False;
end;

function TfrExportRMP.GetAllowExport(const AMapType: IMapType): Boolean;
begin
  Result := AMapType.IsBitmapTiles;
end;

function TfrExportRMP.GetRmpProduct: AnsiString;
begin
  Result := StringToAsciiSafe(Trim(edtRmpProduct.Text));
end;

function TfrExportRMP.GetRmpProvider: AnsiString;
begin
  Result := StringToAsciiSafe(Trim(edtRmpProvider.Text));
end;

function TfrExportRMP.GetMapType: IMapType;
var
  VMapType: IMapType;
begin
  VMapType := FfrMapSelect.GetSelectedMapType;
  if not Assigned(VMapType) then begin
    VMapType := FfrOverlaySelect.GetSelectedMapType;
  end;
  Result := VMapType;
end;

function TfrExportRMP.GetPath: string;
begin
  Result := Trim(edtTargetFile.Text);
end;

function TfrExportRMP.GetProjectToLatLon: Boolean;
begin
  Result := not chkDontProjectToLatLon.Checked;
end;

function TfrExportRMP.GetDirectTilesCopy: Boolean;

  function _IsValidMap(const AMapType: IMapType): Boolean;
  begin
    Result :=
      AMapType.IsBitmapTiles and
      IsJpegContentType(AMapType.ContentType) and
      (AMapType.ProjectionSet.Zooms[0].ProjectionType.ProjectionEPSG = CGELonLatProjectionEPSG);
  end;

var
  VMap: IMapType;
  VLayer: IMapType;
begin
  Result := False;
  if chkAddVisibleLayers.Checked or chkUsePrevZoom.Checked then begin
    Exit;
  end;
  VMap := FfrMapSelect.GetSelectedMapType;
  VLayer := FfrOverlaySelect.GetSelectedMapType;
  if Assigned(VMap) and not Assigned(VLayer) then begin
    Result := _IsValidMap(VMap);
  end else if not Assigned(VMap) and Assigned(VLayer) then begin
    Result := _IsValidMap(VLayer);
  end;
end;

function TfrExportRMP.GetZoomArray: TByteDynArray;
begin
  Result := FfrZoomsSelect.GetZoomList;
end;

function TfrExportRMP.GetBitmapUniProvider: IBitmapUniProvider;
var
  VMap: IMapType;
  VMapVersion: IMapVersionRequest;
  VLayer: IMapType;
  VLayerVersion: IMapVersionRequest;
  VActiveMapsSet: IMapTypeListStatic;
begin
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
      chkUsePrevZoom.Checked,
      chkUsePrevZoom.Checked
    );
end;

function TfrExportRMP.GetBitmapTileSaver: IBitmapTileSaver;
begin
  Result := FBitmapTileSaveLoadFactory.CreateJpegSaver(seJpgQuality.Value);
end;

procedure TfrExportRMP.Init;
begin
  FfrMapSelect.Show(pnlMap);
  FfrOverlaySelect.Show(pnlOverlay);
  FfrZoomsSelect.Show(pnlZoom);
end;

function TfrExportRMP.Validate: Boolean;
begin
  Result := (Trim(edtTargetFile.Text) <> '');
  if not Result then begin
    ShowMessage(_('Please select output file'));
    Exit;
  end;

  Result := FfrZoomsSelect.Validate;
  if not Result then begin
    ShowMessage(_('Please select at least one zoom'));
  end;

  if Result then begin
    if Self.GetMapType = nil then begin
      Result := False;
      ShowMessage(_('Please select at least one map or overlay layer'));
    end;
  end;
end;

end.
