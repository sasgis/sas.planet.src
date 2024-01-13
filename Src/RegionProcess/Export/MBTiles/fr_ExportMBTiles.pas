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
  i_MapTypeListChangeable,
  fr_MapSelect,
  fr_ZoomsSelect,
  fr_ImageFormatSelect,
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

    function GetMakeTileMillCompatibility: Boolean;
    property MakeTileMillCompatibility: Boolean read GetMakeTileMillCompatibility;

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
    chkUsePrevZoom: TCheckBox;
    chkUseXYZScheme: TCheckBox;
    lblName: TLabel;
    edtName: TEdit;
    edtDesc: TEdit;
    lblDesc: TLabel;
    lblAttr: TLabel;
    edtAttr: TEdit;
    dlgSaveTo: TSaveDialog;
    chkAddVisibleLayers: TCheckBox;
    chkMakeTileMillStruct: TCheckBox;
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
    FfrImageFormatSelect: TfrImageFormatSelect;
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
    function GetMakeTileMillCompatibility: Boolean;
    procedure GetBitmapTileSaver(out ASaver: IBitmapTileSaver; out AFormat: string);
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
  Graphics,
  c_CoordConverter,
  i_MapVersionRequest,
  i_ContentTypeInfo,
  i_MapTypeListStatic,
  u_FileSystemFunc,
  u_ContentTypeFunc,
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
  if IsJpegContentType(VContentType) then begin
    Result := ctJPG;
  end else
  if IsPngContentType(VContentType) then begin
    Result := ctPNG;
  end else begin
    Result := ctUnk;
  end;
end;

{ TfrExportMBTiles }

constructor TfrExportMBTiles.Create(
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

  FfrImageFormatSelect :=
    TfrImageFormatSelect.Create(
      ALanguageManager,
      FBitmapTileSaveLoadFactory,
      [iftAuto, iftJpeg, iftPng8bpp, iftPng24bpp, iftPng32bpp]
    );
end;

destructor TfrExportMBTiles.Destroy;
begin
  FreeAndNil(FfrMapSelect);
  FreeAndNil(FfrOverlaySelect);
  FreeAndNil(FfrZoomsSelect);
  FreeAndNil(FfrImageFormatSelect);
  inherited;
end;

procedure TfrExportMBTiles.chkAddVisibleLayersClick(Sender: TObject);
begin
  FfrOverlaySelect.SetEnabled(not chkAddVisibleLayers.Checked);
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

function TfrExportMBTiles.GetMakeTileMillCompatibility: Boolean;
begin
  Result := chkMakeTileMillStruct.Checked;
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
  Result := Result and (FfrImageFormatSelect.SelectedFormat = iftAuto);
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

procedure TfrExportMBTiles.GetBitmapTileSaver(out ASaver: IBitmapTileSaver; out AFormat: string);
const
  cJPG = 'jpg';
  cPNG = 'png';
var
  VMap: IMapType;
  VContentType: AnsiString;
begin
  VMap := Self.GetMapType;

  ASaver := FfrImageFormatSelect.GetBitmapTileSaver(VMap, nil);
  AFormat := '';

  if ASaver <> nil then begin
    VContentType := FfrImageFormatSelect.GetContentType(VMap, nil);
    if IsJpegContentType(VContentType) then begin
      AFormat := cJPG;
    end else
    if IsPngContentType(VContentType) then begin
      AFormat := cPNG;
    end else begin
      raise Exception.Create('Unexpected ContentType: ' + VContentType);
    end;
  end else begin
    Assert(FfrImageFormatSelect.SelectedFormat = iftAuto);
    if FfrMapSelect.GetSelectedMapType <> nil then begin
      ASaver := FfrImageFormatSelect.GetBitmapTileSaver(iftJpeg);
      AFormat := cJPG;
    end else begin
      ASaver := FfrImageFormatSelect.GetBitmapTileSaver(iftPng32bpp);
      AFormat := cPNG;
    end;
  end;
end;

procedure TfrExportMBTiles.Init;
begin
  FfrMapSelect.Show(pnlMap);
  FfrOverlaySelect.Show(pnlOverlay);
  FfrZoomsSelect.Show(pnlZoom);
  FfrImageFormatSelect.Show(pnlImageFormat);
end;

function TfrExportMBTiles.Validate: Boolean;
begin
  Result := False;

  if not IsValidFileName(edtTargetFile.Text) then begin
    ShowMessage(_('Output file name is not set or incorrect!'));
    Exit;
  end;

  if not FfrZoomsSelect.Validate then begin
    ShowMessage(_('Please select at least one zoom'));
    Exit;
  end;

  if
    (FfrMapSelect.GetSelectedMapType = nil) and
    (FfrOverlaySelect.GetSelectedMapType = nil) then
  begin
    ShowMessage(_('Please select at least one map or overlay layer'));
    Exit;
  end;

  Result := True;
end;

end.
