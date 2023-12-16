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

unit fr_ExportKml;

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
  Math,
  t_Bitmap32,
  i_LanguageManager,
  i_GeometryLonLat,
  i_ContentTypeInfo,
  i_MapType,
  i_MapTypeListStatic,
  i_MapTypeListChangeable,
  i_BitmapLayerProvider,
  i_Bitmap32BufferFactory,
  i_BitmapTileSaveLoad,
  i_TileStorageTypeList,
  i_TileFileNameGenerator,
  i_TileFileNameGeneratorsList,
  i_RegionProcessParamsFrame,
  fr_MapSelect,
  fr_ZoomsSelect,
  fr_CacheTypeList,
  u_CommonFormAndFrameParents;

type
  IRegionProcessParamsFrameKmlExport = interface(IRegionProcessParamsFrameBase)
    ['{B2DFB5AD-EAD9-4F36-81F1-87A3D2F1A5B0}']
    function GetNotSaveNotExists: Boolean;
    property NotSaveNotExists: Boolean read GetNotSaveNotExists;

    function GetRelativePath: Boolean;
    property RelativePath: Boolean read GetRelativePath;

    function GetExtractTilesFromStorage: Boolean;
    property ExtractTilesFromStorage: Boolean read GetExtractTilesFromStorage;

    function GetTileFileNameGenerator: ITileFileNameGenerator;
    property TileFileNameGenerator: ITileFileNameGenerator read GetTileFileNameGenerator;

    function GetBitmapTileSaver: IBitmapTileSaver;
    property BitmapTileSaver: IBitmapTileSaver read GetBitmapTileSaver;

    function GetContentTypeInfo: IContentTypeInfoBasic;
    property ContentTypeInfo: IContentTypeInfoBasic read GetContentTypeInfo;

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
  TfrExportKml = class(
      TFrame,
      IRegionProcessParamsFrameBase,
      IRegionProcessParamsFrameOneMap,
      IRegionProcessParamsFrameZoomArray,
      IRegionProcessParamsFrameTargetPath,
      IRegionProcessParamsFrameImageProvider,
      IRegionProcessParamsFrameKmlExport
    )
    pnlCenter: TPanel;
    pnlTop: TPanel;
    lblTargetFile: TLabel;
    edtTargetFile: TEdit;
    btnSelectTargetFile: TButton;
    pnlZoom: TPanel;
    pnlMain: TPanel;
    chkNotSaveNotExists: TCheckBox;
    chkUseRelativePath: TCheckBox;
    lblMap: TLabel;
    dlgSaveKML: TSaveDialog;
    pnlMap: TPanel;
    chkExtractTiles: TCheckBox;
    pnlFileNameGenerator: TPanel;
    lblFileNameGenerator: TLabel;
    chkAddVisibleOverlays: TCheckBox;
    lblInfo: TLabel;
    chkUsePrevZoom: TCheckBox;
    chkPreciseCropping: TCheckBox;
    procedure btnSelectTargetFileClick(Sender: TObject);
    procedure chkExtractTilesClick(Sender: TObject);
  private
    FfrMapSelect: TfrMapSelect;
    FfrZoomsSelect: TfrZoomsSelect;
    FfrCacheTypeList: TfrCacheTypeList;
    FTileNameGeneratorList: ITileFileNameGeneratorsList;
    FBitmap32StaticFactory: IBitmap32StaticFactory;
    FActiveMapsList: IMapTypeListChangeable;
    FPolygon: IGeometryLonLatPolygon;
    function GetUseUniProvider: Boolean;
    procedure UpdateInfoText;
    procedure OnChangeNotify(Sender: TObject);
    procedure OnForceExtractTilesChange(Sender: TObject);
    function GetAllowExport(const AMapType: IMapType): Boolean;
  private
    procedure Init(
      const AZoom: Byte;
      const APolygon: IGeometryLonLatPolygon
    );
    function Validate: Boolean;
  private
    function GetMapType: IMapType;
    function GetZoomArray: TByteDynArray;
    function GetPath: string;
    function GetNotSaveNotExists: Boolean;
    function GetRelativePath: Boolean;
    function GetExtractTilesFromStorage: Boolean;
    function GetTileFileNameGenerator: ITileFileNameGenerator;
    function GetProvider: IBitmapTileUniProvider;
    function GetBitmapTileSaver: IBitmapTileSaver;
    function GetContentTypeInfo: IContentTypeInfoBasic;
    function GetUseMarks: Boolean;
    function GetUseGrids: Boolean;
    function GetUseFillingMap: Boolean;
    function GetUseRecolor: Boolean;
    function GetUsePreciseCropping: Boolean;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AMapSelectFrameBuilder: IMapSelectFrameBuilder;
      const ATileStorageTypeList: ITileStorageTypeListStatic;
      const ATileNameGeneratorList: ITileFileNameGeneratorsList;
      const ABitmap32StaticFactory: IBitmap32StaticFactory;
      const AActiveMapsList: IMapTypeListChangeable
    ); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  StrUtils,
  gnugettext,
  t_GeoTypes,
  i_Projection,
  i_MapVersionRequest,
  i_TileStorageAbilities,
  u_GeoFunc,
  u_GeometryFunc,
  u_ContentTypeFunc,
  u_BitmapLayerProviderMapWithLayer,
  u_FileSystemFunc;

{$R *.dfm}

constructor TfrExportKml.Create(
  const ALanguageManager: ILanguageManager;
  const AMapSelectFrameBuilder: IMapSelectFrameBuilder;
  const ATileStorageTypeList: ITileStorageTypeListStatic;
  const ATileNameGeneratorList: ITileFileNameGeneratorsList;
  const ABitmap32StaticFactory: IBitmap32StaticFactory;
  const AActiveMapsList: IMapTypeListChangeable
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
  FfrMapSelect.OnMapChange := Self.OnChangeNotify;

  FfrZoomsSelect :=
    TfrZoomsSelect.Create(
      ALanguageManager,
      Self.OnChangeNotify
    );
  FfrZoomsSelect.Init(0, 23);

  FfrCacheTypeList :=
    TfrCacheTypeList.Create(
      ALanguageManager,
      ATileStorageTypeList,
      False,
      [tstcInSeparateFiles],
      [tsacAdd]
    );

  FTileNameGeneratorList := ATileNameGeneratorList;
  FBitmap32StaticFactory := ABitmap32StaticFactory;
  FActiveMapsList := AActiveMapsList;

  chkAddVisibleOverlays.OnClick := Self.OnForceExtractTilesChange;
  chkPreciseCropping.OnClick := Self.OnForceExtractTilesChange;
  chkUsePrevZoom.OnClick := Self.OnForceExtractTilesChange;
end;

destructor TfrExportKml.Destroy;
begin
  FreeAndNil(FfrMapSelect);
  FreeAndNil(FfrZoomsSelect);
  FreeAndNil(FfrCacheTypeList);
  inherited Destroy;
end;

procedure TfrExportKml.OnChangeNotify(Sender: TObject);
begin
  UpdateInfoText;
end;

procedure TfrExportKml.UpdateInfoText;

  function _CalcTilesCount(out ACount: UInt64): Boolean;
  var
    I: Integer;
    VMap: IMapType;
    VRect: TRect;
    VSize: TPoint;
    VZoomArr: TByteDynArray;
    VProjection: IProjection;
  begin
    ACount := 0;
    VMap := FfrMapSelect.GetSelectedMapType;
    VZoomArr := Self.GetZoomArray;
    if VMap <> nil then begin
      for I := 0 to Length(VZoomArr) - 1 do begin
        VProjection := VMap.TileStorage.ProjectionSet.Zooms[VZoomArr[I]];
        VRect := RectFromDoubleRect(
          VProjection.LonLatRect2TileRectFloat(FPolygon.Bounds.Rect),
          rrOutside
        );
        VSize := RectSize(VRect);
        Inc(ACount, VSize.X * VSize.Y);
      end;
    end;
    Result := ACount > 0;
  end;

var
  VText: string;
  VCount: UInt64;
begin
  VText := '';

  if Self.GetExtractTilesFromStorage and _CalcTilesCount(VCount) then begin
    if IsLonLatPolygonSimpleRect(FPolygon) then begin
      VText := IfThen(VText <> '', VText + #13#10) +
        Format(_('Tiles to extract: %.0n'), [VCount + 0.0])
    end else begin
      VText := IfThen(VText <> '', VText + #13#10) +
        Format(_('Tiles to extract (no more then): %.0n'), [VCount + 0.0]);
    end;
  end;

  if Self.GetUseUniProvider then begin
    VText := IfThen(VText <> '', VText + #13#10) +
      Format(_('Tiles target format: %s') , [GetContentTypeInfo.GetContentType]);
  end;

  if VText <> '' then begin
    lblInfo.Caption := VText;
    lblInfo.Visible := True;
  end else begin
    lblInfo.Visible := False;
  end;
end;

function TfrExportKml.GetAllowExport(const AMapType: IMapType): Boolean;
begin
  Result := (AMapType.IsBitmapTiles) and (
    Self.GetExtractTilesFromStorage or
    (AMapType.TileStorage.StorageTypeAbilities.StorageClass = tstcInSeparateFiles)
  );
end;

function TfrExportKml.GetUseUniProvider: Boolean;
begin
  Result :=
    chkAddVisibleOverlays.Checked or
    chkUsePrevZoom.Checked or
    chkPreciseCropping.Checked;
end;

procedure TfrExportKml.OnForceExtractTilesChange(Sender: TObject);
begin
  if Self.GetUseUniProvider then begin
    chkExtractTiles.Checked := True;
    chkExtractTiles.Enabled := False;
  end else begin
    chkExtractTiles.Enabled := True;
  end;
  UpdateInfoText;
end;

procedure TfrExportKml.chkExtractTilesClick(Sender: TObject);
begin
  FfrMapSelect.SetEnabled(True); // force refresh maps list
  SetControlEnabled(pnlFileNameGenerator, chkExtractTiles.Checked);
  UpdateInfoText;
end;

function TfrExportKml.GetExtractTilesFromStorage: Boolean;
begin
  Result := chkExtractTiles.Checked;
end;

procedure TfrExportKml.btnSelectTargetFileClick(Sender: TObject);
begin
  if dlgSaveKML.Execute then begin
    edtTargetFile.Text := dlgSaveKML.FileName;
  end;
end;

function TfrExportKml.GetMapType: IMapType;
begin
  Result := FfrMapSelect.GetSelectedMapType;
end;

function TfrExportKml.GetNotSaveNotExists: Boolean;
begin
  Result := chkNotSaveNotExists.Checked;
end;

function TfrExportKml.GetPath: string;
begin
  Result := Trim(edtTargetFile.Text);
end;

function TfrExportKml.GetBitmapTileSaver: IBitmapTileSaver;
var
  VContentType: IContentTypeInfoBitmap;
begin
  Result := nil;
  if not Self.GetUseUniProvider then begin
    Exit;
  end;
  if Supports(Self.GetContentTypeInfo, IContentTypeInfoBitmap, VContentType) then begin
    Result := VContentType.GetSaver;
  end;
end;

function TfrExportKml.GetContentTypeInfo: IContentTypeInfoBasic;
var
  VMap: IMapType;
begin
  VMap := Self.GetMapType;
  if VMap <> nil then begin
    Result := VMap.ContentType;
  end else begin
    Result := nil;
  end;
end;

function TfrExportKml.GetProvider: IBitmapTileUniProvider;
var
  VMap: IMapType;
  VMapVersion: IMapVersionRequest;
begin
  if not Self.GetUseUniProvider then begin
    Result := nil;
    Exit;
  end;

  VMap := FfrMapSelect.GetSelectedMapType;
  VMapVersion := VMap.VersionRequest.GetStatic;

  Result :=
    TBitmapLayerProviderMapWithLayer.Create(
      FBitmap32StaticFactory,
      VMap,
      VMapVersion,
      nil, // Layer
      nil, // LayerVersion,
      FActiveMapsList.List,
      chkUsePrevZoom.Checked,
      chkUsePrevZoom.Checked
    );
end;

function TfrExportKml.GetRelativePath: Boolean;
begin
  Result := chkUseRelativePath.Checked;
end;

function TfrExportKml.GetTileFileNameGenerator: ITileFileNameGenerator;
begin
  Result := FTileNameGeneratorList.GetGenerator(FfrCacheTypeList.IntCode);
end;

function TfrExportKml.GetUseFillingMap: Boolean;
begin
  Result := chkAddVisibleOverlays.Checked;
end;

function TfrExportKml.GetUseGrids: Boolean;
begin
  Result := chkAddVisibleOverlays.Checked;
end;

function TfrExportKml.GetUseMarks: Boolean;
begin
  Result := chkAddVisibleOverlays.Checked;
end;

function TfrExportKml.GetUseRecolor: Boolean;
begin
  Result := chkAddVisibleOverlays.Checked;
end;

function TfrExportKml.GetUsePreciseCropping: Boolean;
begin
  Result := chkPreciseCropping.Checked;
end;

function TfrExportKml.GetZoomArray: TByteDynArray;
begin
  Result := FfrZoomsSelect.GetZoomList;
end;

procedure TfrExportKml.Init(
  const AZoom: Byte;
  const APolygon: IGeometryLonLatPolygon
);
begin
  FPolygon := APolygon;
  FfrMapSelect.Show(pnlMap);
  FfrZoomsSelect.Show(pnlZoom);
  FfrCacheTypeList.Show(pnlFileNameGenerator);
  SetControlEnabled(pnlFileNameGenerator, chkExtractTiles.Checked);
  OnChangeNotify(nil);
end;

function TfrExportKml.Validate: Boolean;
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

  if FfrMapSelect.GetSelectedMapType = nil then begin
    ShowMessage(_('Please select the map first!'));
    Exit;
  end;

  Result := True;
end;

end.
