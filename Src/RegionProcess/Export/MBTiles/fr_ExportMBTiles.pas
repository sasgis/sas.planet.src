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

unit fr_ExportMBTiles;

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
  fr_MapSelect,
  fr_ZoomsSelect,
  u_CommonFormAndFrameParents;

type
  IRegionProcessParamsFrameMBTilesExport = interface(IRegionProcessParamsFrameBase)
    ['{AF048EAA-5AE3-45CD-94FD-443DFCB580B6}']
    function GetDirectTilesCopy: Boolean;
    property DirectTilesCopy: Boolean read GetDirectTilesCopy;

    function GetUseXYZScheme: Boolean;
    property UseXYZScheme: Boolean read GetUseXYZScheme;

    function GetName: string;
    property Name: string read GetName;

    function GetDescription: string;
    property Description: string read GetDescription;

    function GetAttribution: string;
    property Attribution: string read GetAttribution;

    function GetIsLayer: Boolean;
    property IsLayer: Boolean read GetIsLayer;

    procedure GetBitmapTileSaver(out ASaver: IBitmapTileSaver; out AFormat: string);
  end;

type
  TfrExportMBTiles = class(
      TFrame,
      IRegionProcessParamsFrameBase,
      IRegionProcessParamsFrameZoomArray,
      IRegionProcessParamsFrameTargetPath,
      IRegionProcessParamsFrameOneMap,
      IRegionProcessParamsFrameImageProvider,
      IRegionProcessParamsFrameMBTilesExport
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
    cbbImageFormat: TComboBox;
    lblImageFormat: TLabel;
    seCompression: TSpinEdit;
    lblCompression: TLabel;
    chkUsePrevZoom: TCheckBox;
    chkUseXYZScheme: TCheckBox;
    lblName: TLabel;
    edtName: TEdit;
    edtDesc: TEdit;
    lblDesc: TLabel;
    lblAttr: TLabel;
    edtAttr: TEdit;
    dlgSaveTo: TSaveDialog;
    procedure btnSelectTargetFileClick(Sender: TObject);
    procedure cbbImageFormatChange(Sender: TObject);
  private
    FLastPath: string;
    FBitmap32StaticFactory: IBitmap32StaticFactory;
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
    function GetDirectTilesCopy: Boolean;
    function GetAllowExport(const AMapType: IMapType): Boolean;
    function GetProvider: IBitmapTileUniProvider;
    function GetUseXYZScheme: Boolean;
    function GetName: string;
    function GetDescription: string;
    function GetAttribution: string;
    function GetIsLayer: Boolean;
    procedure GetBitmapTileSaver(out ASaver: IBitmapTileSaver; out AFormat: string);
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AMapSelectFrameBuilder: IMapSelectFrameBuilder;
      const ABitmap32StaticFactory: IBitmap32StaticFactory;
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

type
  TMBTilesContentType = (ctUnk, ctPNG, ctJPG);

function GetMBTilesContentType(
  const AContentTypeInfo: IContentTypeInfoBasic
): TMBTilesContentType;
var
  VContentType: AnsiString;
begin
  VContentType := AContentTypeInfo.GetContentType;
  if SameText(VContentType, 'image/jpg') then begin
    Result := ctJPG;
  end else if SameText(VContentType, 'image/png') then begin
    Result := ctPNG;
  end else begin
    Result := ctUnk;
  end;
end;

{ TfrExportMBTiles }

constructor TfrExportMBTiles.Create(
  const ALanguageManager: ILanguageManager;
  const AMapSelectFrameBuilder: IMapSelectFrameBuilder;
  const ABitmap32StaticFactory: IBitmap32StaticFactory;
  const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory
);
begin
  Assert(Assigned(ABitmap32StaticFactory));
  inherited Create(ALanguageManager);

  FLastPath := '';

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

destructor TfrExportMBTiles.Destroy;
begin
  FreeAndNil(FfrMapSelect);
  FreeAndNil(FfrOverlaySelect);
  FreeAndNil(FfrZoomsSelect);
  inherited;
end;

procedure TfrExportMBTiles.cbbImageFormatChange(Sender: TObject);
var
  VValue: Boolean;
begin
  VValue := not (cbbImageFormat.ItemIndex = 0) and cbbImageFormat.Enabled;

  lblJpgQulity.Enabled := VValue;
  seJpgQuality.Enabled := VValue;

  lblCompression.Enabled := VValue;
  seCompression.Enabled := VValue;
end;

procedure TfrExportMBTiles.btnSelectTargetFileClick(Sender: TObject);
begin
  if dlgSaveTo.Execute then begin
    edtTargetFile.Text := dlgSaveTo.FileName;
  end;
end;

function TfrExportMBTiles.GetAllowExport(const AMapType: IMapType): Boolean;
begin
  Result := AMapType.IsBitmapTiles;
end;

function TfrExportMBTiles.GetUseXYZScheme: Boolean;
begin
  Result := chkUseXYZScheme.Checked;
end;

function TfrExportMBTiles.GetName: string;
begin
  Result := Trim(edtName.Text);
end;

function TfrExportMBTiles.GetDescription: string;
begin
  Result := Trim(edtDesc.Text);
end;

function TfrExportMBTiles.GetAttribution: string;
begin
  Result := Trim(edtAttr.Text);
end;

function TfrExportMBTiles.GetIsLayer: Boolean;
var
  VMapType: IMapType;
begin
  VMapType := FfrMapSelect.GetSelectedMapType;
  Result := not Assigned(VMapType);
end;

function TfrExportMBTiles.GetMapType: IMapType;
var
  VMapType: IMapType;
begin
  VMapType := FfrMapSelect.GetSelectedMapType;
  if not Assigned(VMapType) then begin
    VMapType := FfrOverlaySelect.GetSelectedMapType;
  end;
  Result := VMapType;
end;

function TfrExportMBTiles.GetPath: string;
begin
  Result := Trim(edtTargetFile.Text);
end;

function TfrExportMBTiles.GetDirectTilesCopy: Boolean;

  function _IsValidMap(const AMapType: IMapType): Boolean;
  var
    VContentType: TMBTilesContentType;
  begin
    VContentType := GetMBTilesContentType(AMapType.ContentType);
    Result :=
      AMapType.IsBitmapTiles and
      (VContentType in [ctPNG, ctJPG]) and
      (AMapType.ProjectionSet.Zooms[0].ProjectionType.ProjectionEPSG = CGoogleProjectionEPSG);
  end;

var
  VMap: IMapType;
  VLayer: IMapType;
begin
  Result := False;
  VMap := FfrMapSelect.GetSelectedMapType;
  VLayer := FfrOverlaySelect.GetSelectedMapType;
  if Assigned(VMap) and not Assigned(VLayer) then begin
    Result := _IsValidMap(VMap);
  end else if not Assigned(VMap) and Assigned(VLayer) then begin
    Result := _IsValidMap(VLayer);
  end;
  Result := Result and (cbbImageFormat.ItemIndex = 0) and not chkUsePrevZoom.Checked;
end;

function TfrExportMBTiles.GetZoomArray: TByteDynArray;
begin
  Result := FfrZoomsSelect.GetZoomList;
end;

function TfrExportMBTiles.GetProvider: IBitmapTileUniProvider;
var
  VMap: IMapType;
  VMapVersion: IMapVersionRequest;
  VLayer: IMapType;
  VLayerVersion: IMapVersionRequest;
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

  Result :=
    TBitmapLayerProviderMapWithLayer.Create(
      FBitmap32StaticFactory,
      VMap,
      VMapVersion,
      VLayer,
      VLayerVersion,
      nil,
      chkUsePrevZoom.Checked,
      chkUsePrevZoom.Checked
    );
end;

procedure TfrExportMBTiles.GetBitmapTileSaver(out ASaver: IBitmapTileSaver; out AFormat: string);

const
  cJPG = 'jpg';
  cPNG = 'png';

  function _GetSaver(const AMap: IMapType; const AIsLayer: Boolean): IBitmapTileSaver;
  var
    VContentType: TMBTilesContentType;
    VContentTypeInfo: IContentTypeInfoBitmap;
  begin
    Result := nil;
    if Assigned(AMap) then begin
      if Supports(AMap.ContentType, IContentTypeInfoBitmap, VContentTypeInfo) then begin
        VContentType := GetMBTilesContentType(VContentTypeInfo);
        if (VContentType in [ctPNG, ctJPG]) then begin
          Result := VContentTypeInfo.GetSaver;
          if VContentType = ctPNG then begin
            AFormat := cPNG;
          end else begin
            AFormat := cJPG;
          end;
        end else begin
          if AIsLayer then begin
            Result := FBitmapTileSaveLoadFactory.CreatePngSaver(i32bpp, seCompression.Value);
            AFormat := cPNG;
          end else begin
            Result := FBitmapTileSaveLoadFactory.CreateJpegSaver(seJpgQuality.Value);
            AFormat := cJPG;
          end;
        end;
      end;
    end;
  end;

begin
  ASaver := nil;
  AFormat := '';
  if cbbImageFormat.ItemIndex = 0 then begin
    ASaver := _GetSaver(FfrMapSelect.GetSelectedMapType, False);
    if not Assigned(ASaver) then begin
      ASaver := _GetSaver(FfrOverlaySelect.GetSelectedMapType, True);
    end;
  end else begin
    case cbbImageFormat.ItemIndex of
      1: ASaver := FBitmapTileSaveLoadFactory.CreateJpegSaver(seJpgQuality.Value);
      2: ASaver := FBitmapTileSaveLoadFactory.CreatePngSaver(i8bpp, seCompression.Value);
      3: ASaver := FBitmapTileSaveLoadFactory.CreatePngSaver(i24bpp, seCompression.Value);
      4: ASaver := FBitmapTileSaveLoadFactory.CreatePngSaver(i32bpp, seCompression.Value);
    end;
    case cbbImageFormat.ItemIndex of
      1: AFormat := cJPG;
      2..4: AFormat := cPNG;
    end;
  end;
  if not Assigned(ASaver) then begin
    Assert(False, 'Unexpected result!');
    ASaver := FBitmapTileSaveLoadFactory.CreateJpegSaver;
    AFormat := cJPG;
  end;
end;

procedure TfrExportMBTiles.Init;
begin
  FfrMapSelect.Show(pnlMap);
  FfrOverlaySelect.Show(pnlOverlay);
  FfrZoomsSelect.Show(pnlZoom);
  cbbImageFormat.ItemIndex := 0; // Auto
  cbbImageFormatChange(Self);
end;

function TfrExportMBTiles.Validate: Boolean;
var
  VMap: IMapType;
  VLayer: IMapType;
begin
  Result := (edtTargetFile.Text <> '');
  if not Result then begin
    ShowMessage(_('Please select output file'));
    Exit;
  end;

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
