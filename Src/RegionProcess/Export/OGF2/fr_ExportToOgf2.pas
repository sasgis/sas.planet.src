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

unit fr_ExportToOgf2;

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
  fr_MapSelect,
  i_LanguageManager,
  i_MapType,
  i_MapTypeSet,
  i_BitmapTileSaveLoad,
  i_CoordConverterFactory,
  i_GeometryLonLat,
  i_GeometryProjectedFactory,
  i_ActiveMapsConfig,
  i_Bitmap32BufferFactory,
  i_MapTypeGUIConfigList,
  i_BitmapLayerProvider,
  i_BitmapTileSaveLoadFactory,
  i_RegionProcessParamsFrame,
  u_CommonFormAndFrameParents;

type
  IRegionProcessParamsFrameExportToOgf2 = interface(IRegionProcessParamsFrameBase)
    ['{CDF84DFB-9DD8-4F4D-B0B3-6D0D35B082F0}']
    function GetSaver: IBitmapTileSaver;
    property Saver: IBitmapTileSaver read GetSaver;

    function GetTileSize: TPoint;
    property TileSize: TPoint read GetTileSize;
  end;

type
  TfrExportToOgf2 = class(
      TFrame,
      IRegionProcessParamsFrameBase,
      IRegionProcessParamsFrameOneZoom,
      IRegionProcessParamsFrameImageProvider,
      IRegionProcessParamsFrameTargetPath,
      IRegionProcessParamsFrameExportToOgf2
    )
    pnlCenter: TPanel;
    lblMap: TLabel;
    pnlTop: TPanel;
    lblTargetFile: TLabel;
    edtTargetFile: TEdit;
    btnSelectTargetFile: TButton;
    dlgSaveTargetFile: TSaveDialog;
    pnlZoom: TPanel;
    lblZoom: TLabel;
    cbbZoom: TComboBox;
    lblStat: TLabel;
    lblHyb: TLabel;
    cbbImageFormat: TComboBox;
    lblImageFormat: TLabel;
    lblTileRes: TLabel;
    cbbTileRes: TComboBox;
    chkUsePrevZoom: TCheckBox;
    lblJpgQulity: TLabel;
    seJpgQuality: TSpinEdit;
    pnlBottom: TPanel;
    pnlMap: TPanel;
    pnlHyb: TPanel;
    procedure btnSelectTargetFileClick(Sender: TObject);
    procedure cbbZoomChange(Sender: TObject);
    procedure cbbTileResChange(Sender: TObject);
  private
    FVectorGeometryProjectedFactory: IGeometryProjectedFactory;
    FBitmap32StaticFactory: IBitmap32StaticFactory;
    FProjectionFactory: IProjectionInfoFactory;
    FBitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
    FMainMapsConfig: IMainMapsConfig;
    FFullMapsSet: IMapTypeSet;
    FGUIConfigList: IMapTypeGUIConfigList;
    FPolygLL: IGeometryLonLatPolygon;
    FfrMapSelect: TfrMapSelect;
    FfrHybSelect: TfrMapSelect;
  private
    procedure Init(
      const AZoom: byte;
      const APolygon: IGeometryLonLatPolygon
    );
    function Validate: Boolean;
  private
    function GetProvider: IBitmapLayerProvider;
    function GetZoom: Byte;
    function GetPath: string;
  private
    function GetSaver: IBitmapTileSaver;
    function GetTileSize: TPoint;
    function GetAllowExport(const AMapType: IMapType): boolean;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AProjectionFactory: IProjectionInfoFactory;
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
      const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
      const ABitmap32StaticFactory: IBitmap32StaticFactory;
      const AMainMapsConfig: IMainMapsConfig;
      const AFullMapsSet: IMapTypeSet;
      const AGUIConfigList: IMapTypeGUIConfigList;
      const AFileFilters: string;
      const AFileExtDefault: string
    );
    destructor Destroy; override;
  end;

implementation

uses
  t_GeoTypes,
  i_MapVersionRequest,
  i_GeometryProjected,
  u_GeoFunc,
  u_GeometryFunc,
  u_BitmapLayerProviderMapWithLayer,
  u_ResStrings;

{$R *.dfm}

{ TfrExportToOgf2 }

procedure TfrExportToOgf2.btnSelectTargetFileClick(Sender: TObject);
begin
  if dlgSaveTargetFile.Execute then begin
    edtTargetFile.Text := dlgSaveTargetFile.FileName;
  end;
end;

procedure TfrExportToOgf2.cbbTileResChange(Sender: TObject);
begin
  cbbZoomChange(Sender);
end;

procedure TfrExportToOgf2.cbbZoomChange(Sender: TObject);
var
  VTilesCountRow: Int64;
  VTilesCountCol: Int64;
  VTilesCountTotal: Int64;
  VMapType: IMapType;
  VZoom: byte;
  VPolyLL: IGeometryLonLatPolygon;
  VProjected: IGeometryProjectedPolygon;
  VLine: IGeometryProjectedSinglePolygon;
  VBounds: TDoubleRect;
  VPixelRect: TRect;
  VTileRect: TRect;
  VTileSize: Integer;
begin
  VMapType := FfrMapSelect.GetSelectedMapType;

  if cbbTileRes.ItemIndex > 0 then begin
    VTileSize := 256;
  end else begin
    VTileSize := 128;
  end;

  if VMapType <> nil then begin
    VZoom := cbbZoom.ItemIndex;
    VMapType.GeoConvert.ValidateZoom(VZoom);
    VPolyLL := FPolygLL;
    if VPolyLL <> nil then begin
      VProjected :=
        FVectorGeometryProjectedFactory.CreateProjectedPolygonByLonLatPolygon(
          FProjectionFactory.GetByConverterAndZoom(VMapType.GeoConvert, VZoom),
          VPolyLL
        );
      VLine := GetProjectedSinglePolygonByProjectedPolygon(VProjected);
      if Assigned(VLine) then begin
        VBounds := VLine.Bounds;
        VPixelRect := RectFromDoubleRect(VBounds, rrOutside);
        VTileRect := VMapType.GeoConvert.PixelRect2TileRect(VPixelRect, VZoom);

        VTilesCountRow := (VTileRect.Right - VTileRect.Left) * (256 div VTileSize);
        VTilesCountCol := (VTileRect.Bottom - VTileRect.Top) * (256 div VTileSize);
        VTilesCountTotal := VTilesCountRow * VTilesCountCol;

        lblStat.Caption :=
          SAS_STR_filesnum + ': ' +
          IntToStr(VTilesCountRow) + 'x' +
          IntToStr(VTilesCountCol) +
          '(' + FloatToStrF(VTilesCountTotal, ffNumber, 12, 0) + ')' +
          ', ' + SAS_STR_Resolution + ' ' +
          IntToStr(VTilesCountRow * VTileSize) + 'x' +
          IntToStr(VTilesCountCol * VTileSize) + ' pix';
      end;
    end;
  end;
end;

constructor TfrExportToOgf2.Create(
  const ALanguageManager: ILanguageManager;
  const AProjectionFactory: IProjectionInfoFactory;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
  const ABitmap32StaticFactory: IBitmap32StaticFactory;
  const AMainMapsConfig: IMainMapsConfig;
  const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList;
  const AFileFilters: string;
  const AFileExtDefault: string
);
begin
  Assert(Assigned(ABitmap32StaticFactory));
  inherited Create(ALanguageManager);
  FProjectionFactory := AProjectionFactory;
  FVectorGeometryProjectedFactory := AVectorGeometryProjectedFactory;
  FBitmapTileSaveLoadFactory := ABitmapTileSaveLoadFactory;
  FBitmap32StaticFactory := ABitmap32StaticFactory;
  FMainMapsConfig := AMainMapsConfig;
  FFullMapsSet := AFullMapsSet;
  FGUIConfigList := AGUIConfigList;
  dlgSaveTargetFile.Filter := AFileFilters;
  dlgSaveTargetFile.DefaultExt := AFileExtDefault;
  FfrMapSelect :=
    TfrMapSelect.Create(
      ALanguageManager,
      AMainMapsConfig,
      AGUIConfigList,
      AFullMapsSet,
      mfMaps, // show maps and layers
      False,  // add -NO- to combobox
      False,  // show disabled map
      GetAllowExport
    );
  FfrHybSelect :=
    TfrMapSelect.Create(
      ALanguageManager,
      AMainMapsConfig,
      AGUIConfigList,
      AFullMapsSet,
      mfLayers, // show maps and layers
      true,  // add -NO- to combobox
      False,  // show disabled map
      GetAllowExport
    );
end;

destructor TfrExportToOgf2.Destroy;
begin
  FreeAndNil(FfrMapSelect);
  FreeAndNil(FfrHybSelect);
  inherited;
end;

function TfrExportToOgf2.GetAllowExport(const AMapType: IMapType): boolean;
begin
  Result := AMapType.IsBitmapTiles;
end;

function TfrExportToOgf2.GetPath: string;
begin
  Result := edtTargetFile.Text;
end;

function TfrExportToOgf2.GetProvider: IBitmapLayerProvider;
var
  VMap: IMapType;
  VMapVersion: IMapVersionRequest;
  VLayer: IMapType;
  VLayerVersion: IMapVersionRequest;
  VUsePrevZoom: Boolean;
begin
  VMap := FfrMapSelect.GetSelectedMapType;
  VMapVersion := nil;
  if Assigned(VMap) then begin
    VMapVersion := VMap.VersionRequestConfig.GetStatic;
  end;
  VLayer := FfrHybSelect.GetSelectedMapType;
  VLayerVersion := nil;
  if Assigned(VLayer) then begin
    VLayerVersion := VLayer.VersionRequestConfig.GetStatic;
  end;
  VUsePrevZoom := chkUsePrevZoom.Checked;

  Result :=
    TBitmapLayerProviderMapWithLayer.Create(
      FBitmap32StaticFactory,
      VMap,
      VMapVersion,
      VLayer,
      VLayerVersion,
      nil,
      VUsePrevZoom,
      VUsePrevZoom
    );
end;

function TfrExportToOgf2.GetSaver: IBitmapTileSaver;
var
  VJpegQuality: Byte;
begin
  case cbbImageFormat.ItemIndex of
    0: begin
      Result := FBitmapTileSaveLoadFactory.CreateBmpSaver;
    end;
    1: begin
      Result := FBitmapTileSaveLoadFactory.CreatePngSaver(i24bpp);
    end;
  else begin
      VJpegQuality := seJpgQuality.Value;
      Result := FBitmapTileSaveLoadFactory.CreateJpegSaver(VJpegQuality);
    end;
  end;
end;

function TfrExportToOgf2.GetTileSize: TPoint;
var
  VTileSize: Integer;
begin
  if cbbTileRes.ItemIndex > 0 then begin
    VTileSize := 256;
  end else begin
    VTileSize := 128;
  end;
  Result.X := VTileSize;
  Result.Y := VTileSize;
end;

function TfrExportToOgf2.GetZoom: Byte;
begin
  if cbbZoom.ItemIndex < 0 then begin
    cbbZoom.ItemIndex := 0;
  end;
  Result := cbbZoom.ItemIndex;
end;

procedure TfrExportToOgf2.Init(
  const AZoom: byte;
  const APolygon: IGeometryLonLatPolygon
);
var
  I: Integer;
begin
  FPolygLL := APolygon;
  cbbZoom.Items.Clear;

  for I := 1 to 24 do begin
    cbbZoom.Items.Add(IntToStr(I));
  end;
  cbbZoom.ItemIndex := AZoom;

  cbbTileRes.ItemIndex := 0; // 128*128 pix
  cbbImageFormat.ItemIndex := 2; // JPEG
  cbbZoomChange(nil);
  FfrMapSelect.Show(pnlMap);
  FfrHybSelect.Show(pnlHyb);
end;

function TfrExportToOgf2.Validate: Boolean;
begin
  Result := True;
end;

end.
