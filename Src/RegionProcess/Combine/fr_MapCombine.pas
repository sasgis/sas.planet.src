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

unit fr_MapCombine;

interface

uses
  Windows,
  SysUtils,
  Classes,
  Controls,
  Forms,
  Dialogs,
  ExtCtrls,
  StdCtrls,
  CheckLst,
  Spin,
  t_Bitmap32,
  i_LanguageManager,
  i_MapType,
  i_MapTypeListChangeable,
  i_CoordConverterFactory,
  i_CoordConverterList,
  i_GeometryLonLat,
  i_GeometryProjectedFactory,
  i_MapCalibration,
  i_UseTilePrevZoomConfig,
  i_GlobalViewMainConfig,
  i_RegionProcessParamsFrame,
  i_ProjectionInfo,
  i_BitmapLayerProvider,
  i_Bitmap32BufferFactory,
  fr_MapSelect,
  u_CommonFormAndFrameParents;

type
  IRegionProcessParamsFrameMapCombine = interface(IRegionProcessParamsFrameBase)
    ['{6771DEDD-F33C-4152-B4AB-47E6A0B032E1}']
    function GetUseMarks: Boolean;
    property UseMarks: Boolean read GetUseMarks;

    function GetUseGrids: Boolean;
    property UseGrids: Boolean read GetUseGrids;

    function GetUseRecolor: Boolean;
    property UseRecolor: Boolean read GetUseRecolor;

    function GetSplitCount: TPoint;
    property SplitCount: TPoint read GetSplitCount;

    function GetBGColor: TColor32;
    property BGColor: TColor32 read GetBGColor;
  end;

  IRegionProcessParamsFrameMapCombineJpg = interface(IRegionProcessParamsFrameBase)
    ['{C9A0712B-4456-4F34-8A06-9BA57F21D40A}']
    function GetQuality: Integer;
    property Quality: Integer read GetQuality;

    function GetIsSaveGeoRefInfoToExif: Boolean;
    property IsSaveGeoRefInfoToExif: Boolean read GetIsSaveGeoRefInfoToExif;
  end;

  IRegionProcessParamsFrameMapCombineWithAlfa = interface(IRegionProcessParamsFrameBase)
    ['{45043DE9-9950-40C9-92E2-0D28FEC341C7}']
    function GetIsSaveAlfa: Boolean;
    property IsSaveAlfa: Boolean read GetIsSaveAlfa;
  end;

type
  TfrMapCombine = class(
      TFrame,
      IRegionProcessParamsFrameBase,
      IRegionProcessParamsFrameImageProvider,
      IRegionProcessParamsFrameMapCalibrationList,
      IRegionProcessParamsFrameTargetProjection,
      IRegionProcessParamsFrameTargetPath,
      IRegionProcessParamsFrameMapCombine,
      IRegionProcessParamsFrameMapCombineJpg,
      IRegionProcessParamsFrameMapCombineWithAlfa
    )
    pnlTargetFile: TPanel;
    lblTargetFile: TLabel;
    edtTargetFile: TEdit;
    btnSelectTargetFile: TButton;
    dlgSaveTargetFile: TSaveDialog;
    pnlSplit: TPanel;
    grpSplit: TGroupBox;
    lblSplitHor: TLabel;
    lblSplitVert: TLabel;
    seSplitHor: TSpinEdit;
    seSplitVert: TSpinEdit;
    pnlOptions: TPanel;
    chkUseMapMarks: TCheckBox;
    chkUseRecolor: TCheckBox;
    flwpnlJpegQuality: TFlowPanel;
    lblJpgQulity: TLabel;
    pnlPrTypes: TPanel;
    lblPrTypes: TLabel;
    chklstPrTypes: TCheckListBox;
    seJpgQuality: TSpinEdit;
    lblStat: TLabel;
    pnlBottom: TPanel;
    chkPngWithAlpha: TCheckBox;
    pnlProjection: TPanel;
    lblProjection: TLabel;
    cbbProjection: TComboBox;
    pnlMapSelect: TPanel;
    pnlZoom: TPanel;
    Labelzoom: TLabel;
    cbbZoom: TComboBox;
    pnlMapFrame: TPanel;
    lblMapCaption: TLabel;
    pnlLayerFrame: TPanel;
    lblLayerCaption: TLabel;
    chkSaveGeoRefInfoToJpegExif: TCheckBox;
    chkUseMapGrids: TCheckBox;
    pnlMaps: TPanel;
    chkAddVisibleLayers: TCheckBox;
    procedure cbbZoomChange(Sender: TObject);
    procedure btnSelectTargetFileClick(Sender: TObject);
    procedure chkAddVisibleLayersClick(Sender: TObject);
  private
    FVectorGeometryProjectedFactory: IGeometryProjectedFactory;
    FBitmapFactory: IBitmap32StaticFactory;
    FProjectionFactory: IProjectionInfoFactory;
    FCoordConverterList: ICoordConverterList;
    FActiveMapsList: IMapTypeListChangeable;
    FMapCalibrationList: IMapCalibrationList;
    FUseTilePrevZoomConfig: IUseTilePrevZoomConfig;
    FPolygLL: IGeometryLonLatPolygon;
    FViewConfig: IGlobalViewMainConfig;
    FUseQuality: Boolean;
    FUseExif: Boolean;
    FUseAlfa: Boolean;
    FDefaultExt: string;
    FFormatName: string;
    FfrMapSelect: TfrMapSelect;
    FfrLayerSelect: TfrMapSelect;
    FMinPartSize: TPoint;
    FMaxPartSize: TPoint;
    procedure UpdateProjectionsList(Sender: TObject);
  private
    procedure Init(
      const AZoom: byte;
      const APolygon: IGeometryLonLatPolygon
    );
    function Validate: Boolean;
  private
    function GetProvider: IBitmapTileUniProvider;
    function GetPath: string;
    function GetProjection: IProjectionInfo;
    function GetMapCalibrationList: IMapCalibrationList;
  private
    function GetUseMarks: Boolean;
    function GetUseGrids: Boolean;
    function GetUseRecolor: Boolean;
    function GetSplitCount: TPoint;
    function GetQuality: Integer;
    function GetIsSaveGeoRefInfoToExif: Boolean;
    function GetIsSaveAlfa: Boolean;
    function GetBGColor: TColor32;
    function GetAllowWrite(const AMapType: IMapType): boolean;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AProjectionFactory: IProjectionInfoFactory;
      const ACoordConverterList: ICoordConverterList;
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
      const ABitmapFactory: IBitmap32StaticFactory;
      const AMapSelectFrameBuilder: IMapSelectFrameBuilder;
      const AActiveMapsList: IMapTypeListChangeable;
      const AViewConfig: IGlobalViewMainConfig;
      const AUseTilePrevZoomConfig: IUseTilePrevZoomConfig;
      const AMapCalibrationList: IMapCalibrationList;
      const AMinPartSize: TPoint;
      const AMaxPartSize: TPoint;
      const AUseQuality: Boolean;
      const AUseExif: Boolean;
      const AUseAlfa: Boolean;
      const ADefaultExt: string;
      const AFormatName: string
    ); reintroduce;
    procedure RefreshTranslation; override;
    destructor Destroy; override;
  end;

implementation

uses
  Types,
  GR32,
  gnugettext,
  t_GeoTypes,
  i_MapVersionRequest,
  i_MapTypeListStatic,
  i_InterfaceListSimple,
  i_GeometryProjected,
  i_CoordConverter,
  u_InterfaceListSimple,
  u_GeoFunc,
  u_GeometryFunc,
  u_BitmapLayerProviderMapWithLayer,
  u_MapCalibrationListBasic,
  u_ResStrings;

{$R *.dfm}

{ TfrMapCombine }

constructor TfrMapCombine.Create(
  const ALanguageManager: ILanguageManager;
  const AProjectionFactory: IProjectionInfoFactory;
  const ACoordConverterList: ICoordConverterList;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  const ABitmapFactory: IBitmap32StaticFactory;
  const AMapSelectFrameBuilder: IMapSelectFrameBuilder;
  const AActiveMapsList: IMapTypeListChangeable;
  const AViewConfig: IGlobalViewMainConfig;
  const AUseTilePrevZoomConfig: IUseTilePrevZoomConfig;
  const AMapCalibrationList: IMapCalibrationList;
  const AMinPartSize: TPoint;
  const AMaxPartSize: TPoint;
  const AUseQuality: Boolean;
  const AUseExif: Boolean;
  const AUseAlfa: Boolean;
  const ADefaultExt: string;
  const AFormatName: string
);
begin
  Assert(AMinPartSize.X <= AMaxPartSize.X);
  Assert(AMinPartSize.Y <= AMaxPartSize.Y);
  inherited Create(ALanguageManager);
  FProjectionFactory := AProjectionFactory;
  FCoordConverterList := ACoordConverterList;
  FVectorGeometryProjectedFactory := AVectorGeometryProjectedFactory;
  FBitmapFactory := ABitmapFactory;
  FActiveMapsList := AActiveMapsList;
  FMapCalibrationList := AMapCalibrationList;
  FMinPartSize := AMinPartSize;
  FMaxPartSize := AMaxPartSize;
  FViewConfig := AViewConfig;
  FUseTilePrevZoomConfig := AUseTilePrevZoomConfig;
  FUseQuality := AUseQuality;
  FUseExif := AUseExif;
  FUseAlfa := AUseAlfa;
  FDefaultExt := ADefaultExt;
  FFormatName := AFormatName;
  chkPngWithAlpha.Visible := FUseAlfa;
  flwpnlJpegQuality.Visible := FUseQuality;
  chkSaveGeoRefInfoToJpegExif.Visible := FUseExif;
  FfrMapSelect :=
    AMapSelectFrameBuilder.Build(
      mfMaps,    // show Maps
      True,      // add -NO- to combobox
      False,     // show disabled maps
      GetAllowWrite
    );
  FfrLayerSelect :=
    AMapSelectFrameBuilder.Build(
      mfLayers,  // show Layers
      True,      // add -NO- to combobox
      False,     // show disabled maps
      GetAllowWrite
    );
  UpdateProjectionsList(Self);
  FfrMapSelect.OnMapChange := Self.UpdateProjectionsList;
  FfrLayerSelect.OnMapChange := Self.UpdateProjectionsList;
end;

destructor TfrMapCombine.Destroy;
begin
  FreeAndNil(FfrMapSelect);
  FreeAndNil(FfrLayerSelect);
  inherited;
end;

function TfrMapCombine.GetAllowWrite(const AMapType: IMapType): boolean;
begin
  Result := AMapType.IsBitmapTiles;
end;

procedure TfrMapCombine.btnSelectTargetFileClick(Sender: TObject);
begin
  dlgSaveTargetFile.DefaultExt := FDefaultExt;
  dlgSaveTargetFile.Filter := _(FFormatName) + ' | *.' + FDefaultExt;
  if dlgSaveTargetFile.Execute then begin
    edtTargetFile.Text := dlgSaveTargetFile.FileName;
  end;
end;

procedure TfrMapCombine.cbbZoomChange(Sender: TObject);
var
  numd: int64;
  Vmt: IMapType;
  VZoom: byte;
  VPolyLL: IGeometryLonLatPolygon;
  VProjected: IGeometryProjectedPolygon;
  VLine: IGeometryProjectedSinglePolygon;
  VBounds: TDoubleRect;
  VPixelRect: TRect;
  VTileRect: TRect;
begin
  Vmt := FfrMapSelect.GetSelectedMapType;
  if (Vmt = nil) then begin
    Vmt := FfrLayerSelect.GetSelectedMapType;
  end; //calc for layer if map is not selected
  if Vmt <> nil then begin
    VZoom := cbbZoom.ItemIndex;
    Vmt.GeoConvert.ValidateZoom(VZoom);
    VPolyLL := FPolygLL;
    if VPolyLL <> nil then begin
      VProjected :=
        FVectorGeometryProjectedFactory.CreateProjectedPolygonByLonLatPolygon(
          FProjectionFactory.GetByConverterAndZoom(Vmt.GeoConvert, VZoom),
          VPolyLL
        );
      VLine := GetProjectedSinglePolygonByProjectedPolygon(VProjected);
      if Assigned(VLine) then begin
        VBounds := VLine.Bounds;
        VPixelRect := RectFromDoubleRect(VBounds, rrOutside);
        VTileRect := Vmt.GeoConvert.PixelRect2TileRect(VPixelRect, VZoom);
        numd := (VTileRect.Right - VTileRect.Left);
        numd := numd * (VTileRect.Bottom - VTileRect.Top);
        lblStat.Caption :=
          SAS_STR_filesnum + ': ' +
          inttostr(VTileRect.Right - VTileRect.Left) + 'x' +
          inttostr(VTileRect.Bottom - VTileRect.Top) +
          '(' + inttostr(numd) + ')' +
          ', ' + SAS_STR_Resolution + ' ' +
          inttostr(VPixelRect.Right - VPixelRect.Left) + 'x' +
          inttostr(VPixelRect.Bottom - VPixelRect.Top);
      end;
    end;
  end;
end;

procedure TfrMapCombine.chkAddVisibleLayersClick(Sender: TObject);
begin
  FfrLayerSelect.SetEnabled(not chkAddVisibleLayers.Checked);
end;

function TfrMapCombine.GetBGColor: TColor32;
var
  VMap: IMapType;
begin
  VMap := FfrMapSelect.GetSelectedMapType;
  if VMap = nil then begin
    Result := SetAlpha(FViewConfig.BackGroundColor, 0);
  end else begin
    Result := SetAlpha(FViewConfig.BackGroundColor, 255);
  end;
end;

function TfrMapCombine.GetIsSaveAlfa: Boolean;
begin
  Result := chkPngWithAlpha.Checked;
end;

function TfrMapCombine.GetMapCalibrationList: IMapCalibrationList;
var
  i: Integer;
  VList: IInterfaceListSimple;
begin
  VList := TInterfaceListSimple.Create;
  for i := 0 to chklstPrTypes.Items.Count - 1 do begin
    if chklstPrTypes.Checked[i] then begin
      VList.Add(IMapCalibration(Pointer(chklstPrTypes.Items.Objects[i])));
    end;
  end;
  Result := TMapCalibrationListByInterfaceList.Create(VList.MakeStaticAndClear);
end;

function TfrMapCombine.GetPath: string;
begin
  Result := edtTargetFile.Text;
end;

function TfrMapCombine.GetProjection: IProjectionInfo;
var
  VMap: IMapType;
  VLayer: IMapType;
  VMainMapType: IMapType;
  VZoom: Byte;
  VGeoConverter: ICoordConverter;
  VIndex: Integer;
begin
  Result := nil;
  VGeoConverter := nil;
  VIndex := cbbProjection.ItemIndex;
  if VIndex < 0 then begin
    VIndex := 0;
  end;
  if VIndex >= 2 then begin
    VIndex := VIndex - 2;
    if VIndex < FCoordConverterList.Count then begin
      VGeoConverter := FCoordConverterList.Items[VIndex];
    end;
    VIndex := 0;
  end;
  if VGeoConverter = nil then begin
    VMainMapType := nil;
    VMap := FfrMapSelect.GetSelectedMapType;
    VLayer := FfrLayerSelect.GetSelectedMapType;
    if VIndex = 0 then begin
      if VMap <> nil then begin
        VMainMapType := VMap;
      end else if VLayer <> nil then begin
        VMainMapType := VLayer;
      end;
    end else begin
      if VLayer <> nil then begin
        VMainMapType := VLayer;
      end else if VMap <> nil then begin
        VMainMapType := VMap;
      end;
    end;
    if VMainMapType <> nil then begin
      VGeoConverter := VMainMapType.GeoConvert;
    end;
  end;
  VZoom := cbbZoom.ItemIndex;
  if VGeoConverter <> nil then begin
    Result := FProjectionFactory.GetByConverterAndZoom(VGeoConverter, VZoom);
  end;
end;

function TfrMapCombine.GetProvider: IBitmapTileUniProvider;
var
  VMap: IMapType;
  VMapVersion: IMapVersionRequest;
  VLayer: IMapType;
  VLayerVersion: IMapVersionRequest;
  VActiveMapsSet: IMapTypeListStatic;
begin
  VMap := FfrMapSelect.GetSelectedMapType;
  if Assigned(VMap) then begin
    VMapVersion := VMap.VersionRequestConfig.GetStatic;
  end else begin
    VMapVersion := nil;
  end;

  VLayer := FfrLayerSelect.GetSelectedMapType;
  if Assigned(VLayer) then begin
    VLayerVersion := VLayer.VersionRequestConfig.GetStatic;
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
      FBitmapFactory,
      VMap,
      VMapVersion,
      VLayer,
      VLayerVersion,
      VActiveMapsSet,
      FUseTilePrevZoomConfig.UsePrevZoomAtMap,
      FUseTilePrevZoomConfig.UsePrevZoomAtLayer
    );
end;

function TfrMapCombine.GetQuality: Integer;
begin
  Result := seJpgQuality.Value;
end;

function TfrMapCombine.GetIsSaveGeoRefInfoToExif: Boolean;
begin
  Result := chkSaveGeoRefInfoToJpegExif.Checked;
end;

function TfrMapCombine.GetSplitCount: TPoint;
begin
  Result.X := seSplitHor.Value;
  Result.Y := seSplitVert.Value;
end;

function TfrMapCombine.GetUseGrids: Boolean;
begin
  Result := chkUseMapGrids.Checked;
end;

function TfrMapCombine.GetUseMarks: Boolean;
begin
  Result := chkUseMapMarks.Checked;
end;

function TfrMapCombine.GetUseRecolor: Boolean;
begin
  Result := chkUseRecolor.Checked;
end;

procedure TfrMapCombine.Init(
  const AZoom: byte;
  const APolygon: IGeometryLonLatPolygon
);
var
  i: Integer;
  VMapCalibration: IMapCalibration;
begin
  FPolygLL := APolygon;
  cbbZoom.Items.Clear;
  for i := 1 to 24 do begin
    cbbZoom.Items.Add(inttostr(i));
  end;
  cbbZoom.ItemIndex := AZoom;
  chklstPrTypes.Clear;
  for i := 0 to FMapCalibrationList.Count - 1 do begin
    VMapCalibration := FMapCalibrationList.Get(i);
    chklstPrTypes.AddItem(VMapCalibration.GetName, Pointer(VMapCalibration));
  end;
  cbbZoomChange(nil);
  FfrMapSelect.Show(pnlMapFrame);
  FfrLayerSelect.Show(pnlLayerFrame);
  UpdateProjectionsList(Self);
end;

procedure TfrMapCombine.RefreshTranslation;
var
  VProjectionIndex: Integer;
begin
  VProjectionIndex := cbbProjection.ItemIndex;
  inherited;
  UpdateProjectionsList(Self);
  if VProjectionIndex >= 0 then begin
    cbbProjection.ItemIndex := VProjectionIndex;
  end;
end;

procedure TfrMapCombine.UpdateProjectionsList(Sender: TObject);

  procedure AddProj(const AMapType: IMapType; const ACaption: string);
  var
    I: Integer;
    VProj: string;
    VConverter: ICoordConverter;
  begin
    VProj := ACaption;
    if Assigned(AMapType) then begin
      VConverter := AMapType.GeoConvert;
      for I := 0 to FCoordConverterList.Count - 1 do begin
        if FCoordConverterList.Items[I].IsSameConverter(VConverter) then begin
          VProj := VProj + ' - ' + FCoordConverterList.Captions[I];
          Break;
        end;
      end;
    end;
    cbbProjection.Items.Add(VProj);
  end;

var
  I: Integer;
  VPrevIndex, VPrevCount: Integer;
begin
  VPrevIndex := cbbProjection.ItemIndex;
  VPrevCount := cbbProjection.Items.Count;

  cbbProjection.Items.Clear;
  AddProj(FfrMapSelect.GetSelectedMapType, _('Projection of map'));
  AddProj(FfrLayerSelect.GetSelectedMapType, _('Projection of layer'));

  for I := 0 to FCoordConverterList.Count - 1 do begin
    cbbProjection.Items.Add(FCoordConverterList.Captions[I]);
  end;

  if (VPrevIndex >= 0) and
     (cbbProjection.Items.Count > VPrevIndex) and
     (cbbProjection.Items.Count = VPrevCount)
  then begin
    cbbProjection.ItemIndex := VPrevIndex;
  end else begin
    cbbProjection.ItemIndex := 0;
  end;
end;

function TfrMapCombine.Validate: Boolean;
var
  VPath: string;
  VMsg: string;
  VProjection: IProjectionInfo;
  VLonLatRect: TDoubleRect;
  VPixelRect: TRect;
  VSplitCount: TPoint;
  VPixelSize: TPoint;
begin
  if (FfrMapSelect.GetSelectedMapType = nil) and (FfrLayerSelect.GetSelectedMapType = nil) then begin
    ShowMessage(_('Please select map or layer'));
    Result := False;
    Exit;
  end;
  VProjection := GetProjection;
  VLonLatRect := FPolygLL.Bounds.Rect;
  VProjection.GeoConverter.ValidateLonLatRect(VLonLatRect);
  VPixelRect :=
    RectFromDoubleRect(
      VProjection.GeoConverter.LonLatRect2PixelRectFloat(VLonLatRect, VProjection.Zoom),
      rrOutside
    );
  VSplitCount := GetSplitCount;
  VPixelSize := RectSize(VPixelRect);
  VPixelSize.X := Trunc(VPixelSize.X / VSplitCount.X);
  VPixelSize.Y := Trunc(VPixelSize.Y / VSplitCount.Y);
  if VPixelSize.X < FMinPartSize.X then begin
    ShowMessageFmt(_('Every map part must have width more than %0:d but exist %1:d'), [FMinPartSize.X, VPixelSize.X]);
    Result := False;
    Exit;
  end;
  if VPixelSize.Y < FMinPartSize.Y then begin
    ShowMessageFmt(_('Every map part must have height more than %0:d but exist %1:d'), [FMinPartSize.Y, VPixelSize.Y]);
    Result := False;
    Exit;
  end;

  if VPixelSize.X > FMaxPartSize.X then begin
    ShowMessageFmt(_('Every map part must have width less than %0:d but exist %1:d'), [FMaxPartSize.X, VPixelSize.X]);
    Result := False;
    Exit;
  end;
  if VPixelSize.Y > FMaxPartSize.Y then begin
    ShowMessageFmt(_('Every map part must have height less than %0:d but exist %1:d'), [FMaxPartSize.Y, VPixelSize.Y]);
    Result := False;
    Exit;
  end;

  VPath := GetPath;
  if VPath = '' then begin
    ShowMessage(_('Please, select output file first!'));
    Result := False;
    Exit;
  end;
  if FileExists(VPath) then begin
    VMsg := Format(SAS_MSG_FileExists, [VPath]);
    if (Application.MessageBox(pchar(VMsg), pchar(SAS_MSG_coution), 36) <> IDYES) then begin
      Result := False;
      Exit;
    end;
  end;
  Result := True;
end;

end.
