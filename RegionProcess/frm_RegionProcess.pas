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

unit frm_RegionProcess;

interface

uses
  Windows,
  SysUtils,
  Forms,
  Classes,
  Controls,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  inifiles,
  ComCtrls,
  TB2Item,
  TB2Dock,
  TB2Toolbar,
  TBX,
  i_NotifierTime,
  i_NotifierOperation,
  i_MapViewGoto,
  u_CommonFormAndFrameParents,
  i_LanguageManager,
  i_LastSelectionInfo,
  i_CoordConverterFactory,
  i_CoordConverterList,
  i_ContentTypeManager,
  i_GlobalViewMainConfig,
  i_GeometryLonLat,
  i_GeometryLonLatFactory,
  i_GeometryProjectedFactory,
  i_GeometryProjectedProvider,
  i_ImageResamplerFactory,
  i_ImageResamplerConfig,
  i_Bitmap32BufferFactory,
  i_BitmapTileSaveLoadFactory,
  i_ArchiveReadWriteFactory,
  i_LocalCoordConverterFactorySimpe,
  i_BitmapPostProcessing,
  i_GlobalDownloadConfig,
  i_DownloadInfoSimple,
  i_UseTilePrevZoomConfig,
  i_UsedMarksConfig,
  i_MarksDrawConfig,
  i_MarkSystem,
  i_MapTypeSet,
  i_MapTypeListBuilder,
  i_RegionProcess,
  i_ActiveMapsConfig,
  i_MapCalibration,
  i_TileFileNameGeneratorsList,
  i_TileFileNameParsersList,
  i_TileStorageTypeList,
  i_LocalCoordConverterChangeable,
  i_ValueToStringConverter,
  i_MapTypeGUIConfigList,
  i_GlobalBerkeleyDBHelper,
  i_RegionProcessProgressInfoInternalFactory,
  u_ExportProviderAbstract,
  u_ProviderTilesDownload,
  u_MarkDbGUIHelper,
  fr_Combine,
  fr_Export;

type
  TfrmRegionProcess = class(TFormWitghLanguageManager, IRegionProcess)
    Button1: TButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    Button3: TButton;
    SaveSelDialog: TSaveDialog;
    CBCloseWithStart: TCheckBox;
    TabSheet6: TTabSheet;
    pnlBottomButtons: TPanel;
    TBXOperationsToolbar: TTBXToolbar;
    tbtmMark: TTBItem;
    tbtmZoom: TTBItem;
    tbtmSave: TTBItem;
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tbtmSaveClick(Sender: TObject);
    procedure tbtmZoomClick(Sender: TObject);
    procedure tbtmMarkClick(Sender: TObject);
  private
    FfrExport: TfrExport;
    FfrCombine: TfrCombine;
    FVectorGeometryLonLatFactory: IGeometryLonLatFactory;
    FVectorGeometryProjectedFactory: IGeometryProjectedFactory;
    FLastSelectionInfo: ILastSelectionInfo;
    FZoom_rect:byte;
    FPolygonLL: IGeometryLonLatMultiPolygon;
    FProviderTilesDelte: TExportProviderAbstract;
    FProviderTilesGenPrev: TExportProviderAbstract;
    FProviderTilesCopy: TExportProviderAbstract;
    FProviderTilesDownload: TProviderTilesDownload;
    FMapGoto: IMapViewGoto;
    FMarkDBGUI: TMarkDbGUIHelper;
    FPosition: ILocalCoordConverterChangeable;
    function LoadRegion(const APolyLL: IGeometryLonLatMultiPolygon): Boolean;
    function DelRegion(const APolyLL: IGeometryLonLatMultiPolygon): Boolean;
    function genbacksatREG(const APolyLL: IGeometryLonLatMultiPolygon): Boolean;
    function scleitRECT(const APolyLL: IGeometryLonLatMultiPolygon): Boolean;
    function savefilesREG(const APolyLL: IGeometryLonLatMultiPolygon): Boolean;
    function ExportREG(const APolyLL: IGeometryLonLatMultiPolygon): Boolean;
  private
    procedure ProcessPolygon(
      const APolygon: IGeometryLonLatMultiPolygon
    );
    procedure ProcessPolygonWithZoom(
      const AZoom: Byte;
      const APolygon: IGeometryLonLatMultiPolygon
    );
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AAppClosingNotifier: INotifierOneOperation;
      const ATimerNoifier: INotifierTime;
      const ALastSelectionInfo: ILastSelectionInfo;
      const AMainMapsConfig: IMainMapsConfig;
      const AMapTypeListBuilderFactory: IMapTypeListBuilderFactory;
      const AGlobalBerkeleyDBHelper: IGlobalBerkeleyDBHelper;
      const APosition: ILocalCoordConverterChangeable;
      const AFullMapsSet: IMapTypeSet;
      const AGUIConfigList: IMapTypeGUIConfigList;
      const AContentTypeManager: IContentTypeManager;
      const ACoordConverterFactory: ICoordConverterFactory;
      const ATileStorageTypeList: ITileStorageTypeListStatic;
      const ATileNameGenerator: ITileFileNameGeneratorsList;
      const AFileNameParsersList: ITileFileNameParsersList;
      const AViewConfig: IGlobalViewMainConfig;
      const AUseTilePrevZoomConfig: IUseTilePrevZoomConfig;
      const AImageResamplerFactoryList: IImageResamplerFactoryList;
      const AImageResamplerConfig: IImageResamplerConfig;
      const AMarksShowConfig: IUsedMarksConfig;
      const AMarksDrawConfig: IMarksDrawConfig;
      const AMarksDB: IMarkSystem;
      const ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
      const ABitmapPostProcessing: IBitmapPostProcessingChangeable;
      const AProjectionFactory: IProjectionInfoFactory;
      const ACoordConverterList: ICoordConverterList;
      const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
      const AProjectedGeometryProvider: IGeometryProjectedProvider;
      const ABitmapFactory: IBitmap32BufferFactory;
      const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
      const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
      const AMapCalibrationList: IMapCalibrationList;
      const ADownloadConfig: IGlobalDownloadConfig;
      const ADownloadInfo: IDownloadInfoSimple;
      const AValueToStringConverter: IValueToStringConverterChangeable;
      const AMapGoto: IMapViewGoto;
      const AMarkDBGUI: TMarkDbGUIHelper
    ); reintroduce;
    destructor Destroy; override;
    procedure LoadSelFromFile(const AFileName:string; out APolygon: IGeometryLonLatMultiPolygon);
    procedure StartSlsFromFile(const AFileName:string);
  end;

implementation

uses
  gnugettext,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  u_ConfigDataProviderByIniFile,
  u_ConfigDataWriteProviderByIniFile,
  u_ConfigProviderHelpers,
  u_RegionProcessProgressInfoInternalFactory,
  u_ProviderTilesDelete,
  u_ProviderTilesGenPrev,
  u_ProviderTilesCopy;

{$R *.dfm}

constructor TfrmRegionProcess.Create(
  const ALanguageManager: ILanguageManager;
  const AAppClosingNotifier: INotifierOneOperation;
  const ATimerNoifier: INotifierTime;
  const ALastSelectionInfo: ILastSelectionInfo;
  const AMainMapsConfig: IMainMapsConfig;
  const AMapTypeListBuilderFactory: IMapTypeListBuilderFactory;
  const AGlobalBerkeleyDBHelper: IGlobalBerkeleyDBHelper;
  const APosition: ILocalCoordConverterChangeable;
  const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList;
  const AContentTypeManager: IContentTypeManager;
  const ACoordConverterFactory: ICoordConverterFactory;
  const ATileStorageTypeList: ITileStorageTypeListStatic;
  const ATileNameGenerator: ITileFileNameGeneratorsList;
  const AFileNameParsersList: ITileFileNameParsersList;
  const AViewConfig: IGlobalViewMainConfig;
  const AUseTilePrevZoomConfig: IUseTilePrevZoomConfig;
  const AImageResamplerFactoryList: IImageResamplerFactoryList;
  const AImageResamplerConfig: IImageResamplerConfig;
  const AMarksShowConfig: IUsedMarksConfig;
  const AMarksDrawConfig: IMarksDrawConfig;
  const AMarksDB: IMarkSystem;
  const ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
  const ABitmapPostProcessing: IBitmapPostProcessingChangeable;
  const AProjectionFactory: IProjectionInfoFactory;
  const ACoordConverterList: ICoordConverterList;
  const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  const AProjectedGeometryProvider: IGeometryProjectedProvider;
  const ABitmapFactory: IBitmap32BufferFactory;
  const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
  const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
  const AMapCalibrationList: IMapCalibrationList;
  const ADownloadConfig: IGlobalDownloadConfig;
  const ADownloadInfo: IDownloadInfoSimple;
  const AValueToStringConverter: IValueToStringConverterChangeable;
  const AMapGoto: IMapViewGoto;
  const AMarkDBGUI: TMarkDbGUIHelper
);
var
  VProgressFactory: IRegionProcessProgressInfoInternalFactory;
begin
  inherited Create(ALanguageManager);
  FLastSelectionInfo := ALastSelectionInfo;
  FPosition := APosition;
  FVectorGeometryLonLatFactory := AVectorGeometryLonLatFactory;
  FVectorGeometryProjectedFactory := AVectorGeometryProjectedFactory;
  FMapGoto := AMapGoto;
  FMarkDBGUI:=AMarkDBGUI;
  VProgressFactory :=
    TRegionProcessProgressInfoInternalFactory.Create(
      AAppClosingNotifier,
      ATimerNoifier,
      Self,
      FMapGoto
    );
  FfrExport :=
    TfrExport.Create(
      VProgressFactory,
      ALanguageManager,
      AMainMapsConfig,
      AFullMapsSet,
      AGUIConfigList,
      AMapTypeListBuilderFactory,
      ACoordConverterFactory,
      ALocalConverterFactory,
      AProjectionFactory,
      AVectorGeometryProjectedFactory,
      ABitmapFactory,
      ABitmapPostProcessing,
      ABitmapTileSaveLoadFactory,
      AArchiveReadWriteFactory,
      ATileNameGenerator
    );

  FProviderTilesDelte :=
    TProviderTilesDelete.Create(
      VProgressFactory,
      ALanguageManager,
      AMainMapsConfig,
      AFullMapsSet,
      AGUIConfigList,
      AProjectionFactory,
      AVectorGeometryProjectedFactory
    );
  FProviderTilesGenPrev :=
    TProviderTilesGenPrev.Create(
      VProgressFactory,
      ALanguageManager,
      AMainMapsConfig,
      AFullMapsSet,
      AGUIConfigList,
      AViewConfig,
      AProjectionFactory,
      AVectorGeometryProjectedFactory,
      ABitmapFactory,
      AImageResamplerFactoryList,
      AImageResamplerConfig
    );
  FProviderTilesCopy :=
    TProviderTilesCopy.Create(
      ATimerNoifier,
      VProgressFactory,
      ALanguageManager,
      AMainMapsConfig,
      AGlobalBerkeleyDBHelper,
      AFullMapsSet,
      AGUIConfigList,
      AMapTypeListBuilderFactory,
      AContentTypeManager,
      AProjectionFactory,
      AVectorGeometryProjectedFactory,
      ATileStorageTypeList,
      AFileNameParsersList,
      ATileNameGenerator
    );
  FProviderTilesDownload :=
    TProviderTilesDownload.Create(
      AAppClosingNotifier,
      VProgressFactory,
      ALanguageManager,
      AValueToStringConverter,
      AMainMapsConfig,
      AFullMapsSet,
      AGUIConfigList,
      AProjectionFactory,
      AVectorGeometryLonLatFactory,
      AVectorGeometryProjectedFactory,
      ADownloadConfig,
      ADownloadInfo
    );
  FfrCombine :=
    TfrCombine.Create(
      VProgressFactory,
      ALanguageManager,
      AMainMapsConfig,
      AFullMapsSet,
      AGUIConfigList,
      AViewConfig,
      AUseTilePrevZoomConfig,
      AProjectionFactory,
      ACoordConverterList,
      AVectorGeometryProjectedFactory,
      AProjectedGeometryProvider,
      ABitmapTileSaveLoadFactory,
      AArchiveReadWriteFactory,
      AMarksShowConfig,
      AMarksDrawConfig,
      AMarksDB,
      ALocalConverterFactory,
      ABitmapFactory,
      ABitmapPostProcessing,
      AMapCalibrationList
    );
end;

destructor TfrmRegionProcess.Destroy;
begin
  FreeAndNil(FfrExport);
  FreeAndNil(FProviderTilesDelte);
  FreeAndNil(FProviderTilesGenPrev);
  FreeAndNil(FProviderTilesCopy);
  FreeAndNil(FProviderTilesDownload);
  FreeAndNil(FfrCombine);
  inherited;
end;

procedure TfrmRegionProcess.LoadSelFromFile(
  const AFileName: string;
  out APolygon: IGeometryLonLatMultiPolygon
);
var
  VIniFile:TMemIniFile;
  VHLGData: IConfigDataProvider;
  VPolygonSection: IConfigDataProvider;
  VZoom: Byte;
begin
  if FileExists(AFileName) then begin
    VIniFile := TMemIniFile.Create(AFileName);
    try
      VHLGData := TConfigDataProviderByIniFile.CreateWithOwn(VIniFile);
      VIniFile := nil;
    finally
      FreeAndNil(VIniFile);
    end;
    VPolygonSection := VHLGData.GetSubItem('HIGHLIGHTING');
    if VPolygonSection <> nil then begin
      APolygon := ReadPolygon(VPolygonSection, FVectorGeometryLonLatFactory);
      if (APolygon <> nil) and (APolygon.Count > 0) then begin
        VZoom := VPolygonSection.ReadInteger('zoom', 1) - 1;
        Self.ProcessPolygonWithZoom(VZoom, APolygon);
      end;
    end;
  end else begin
    ShowMessageFmt(_('Can''t open file: %s'), [AFileName]);
  end;
end;

procedure TfrmRegionProcess.ProcessPolygon(const APolygon: IGeometryLonLatMultiPolygon);
begin
  FZoom_rect := FPosition.GetStatic.Zoom;
  FPolygonLL := APolygon;
  FLastSelectionInfo.SetPolygon(APolygon, FZoom_rect);
  Self.Show;
end;

procedure TfrmRegionProcess.ProcessPolygonWithZoom(const AZoom: Byte;
  const APolygon: IGeometryLonLatMultiPolygon);
begin
  FZoom_rect := AZoom;
  FPolygonLL := APolygon;
  FLastSelectionInfo.SetPolygon(APolygon, FZoom_rect);
  Self.Show;
end;

function TfrmRegionProcess.DelRegion(const APolyLL: IGeometryLonLatMultiPolygon): Boolean;
begin
  Result := FProviderTilesDelte.Validate;
  if Result then begin
    FProviderTilesDelte.StartProcess(APolyLL);
  end;
end;

function TfrmRegionProcess.ExportREG(const APolyLL: IGeometryLonLatMultiPolygon): Boolean;
begin
  Result := FfrExport.Validate;
  if Result then begin
    FfrExport.StartProcess(APolyLL);
  end;
end;

function TfrmRegionProcess.savefilesREG(const APolyLL: IGeometryLonLatMultiPolygon): Boolean;
begin
  Result := FProviderTilesCopy.Validate;
  if Result then begin
    FProviderTilesCopy.StartProcess(APolyLL);
  end;
end;

function TfrmRegionProcess.LoadRegion(const APolyLL: IGeometryLonLatMultiPolygon): Boolean;
begin
  Result := FProviderTilesDownload.Validate;
  if Result then begin
    FProviderTilesDownload.StartProcess(APolyLL);
  end;
end;

function TfrmRegionProcess.genbacksatREG(const APolyLL: IGeometryLonLatMultiPolygon): Boolean;
begin
  Result := FProviderTilesGenPrev.Validate;
  if Result then begin
    FProviderTilesGenPrev.StartProcess(APolyLL);
  end;
end;

function TfrmRegionProcess.scleitRECT(const APolyLL: IGeometryLonLatMultiPolygon): Boolean;
begin
  Result := FfrCombine.Validate;
  if Result then begin
    FfrCombine.StartProcess(APolyLL);
  end;
end;


procedure TfrmRegionProcess.Button1Click(Sender: TObject);
var
  VResult: Boolean;
begin
  VResult := False;
  case PageControl1.ActivePage.Tag of
    0: VResult := LoadRegion(FPolygonLL);
    1: VResult := scleitRECT(FPolygonLL);
    2: VResult := genbacksatREG(FPolygonLL);
    3: VResult := DelRegion(FPolygonLL);
    4: VResult := ExportREG(FPolygonLL);
    5: VResult := savefilesREG(FPolygonLL);
  end;
  if VResult then begin
    if CBCloseWithStart.Checked then begin
      close;
    end;
  end;
end;

procedure TfrmRegionProcess.FormShow(Sender: TObject);
begin
  FfrExport.Show(TabSheet5, FZoom_rect, FPolygonLL);
  FProviderTilesDelte.Show(TabSheet4, FZoom_rect, FPolygonLL);
  FProviderTilesGenPrev.Show(TabSheet3, FZoom_rect, FPolygonLL);
  FProviderTilesCopy.Show(TabSheet6, FZoom_rect, FPolygonLL);
  FProviderTilesDownload.Show(TabSheet1, FZoom_rect, FPolygonLL);
  FfrCombine.Show(TabSheet2, FZoom_rect, FPolygonLL);

  PageControl1.ActivePageIndex:=0;
end;

procedure TfrmRegionProcess.Button3Click(Sender: TObject);
begin
  close;
end;

procedure TfrmRegionProcess.tbtmMarkClick(Sender: TObject);
var
  VIniFile: Tinifile;
  VZoom: Byte;
  VPolygon: IGeometryLonLatMultiPolygon;
  VHLGData: IConfigDataWriteProvider;
  VPolygonSection: IConfigDataWriteProvider;
begin
  if (SaveSelDialog.Execute)and(SaveSelDialog.FileName<>'') then begin
    If FileExists(SaveSelDialog.FileName) then DeleteFile(SaveSelDialog.FileName);
    VZoom := FLastSelectionInfo.Zoom;
    VPolygon := FLastSelectionInfo.Polygon;
    if VPolygon <> nil then begin
      VIniFile := TIniFile.Create(SaveSelDialog.FileName);
      try
        VHLGData := TConfigDataWriteProviderByIniFile.CreateWithOwn(VIniFile);
        VIniFile := nil;
      finally
        VIniFile.Free;
      end;
      VPolygonSection := VHLGData.GetOrCreateSubItem('HIGHLIGHTING');
      VPolygonSection.WriteInteger('zoom', VZoom + 1);
      WritePolygon(VPolygonSection, VPolygon);
    end;
  end;
end;

procedure TfrmRegionProcess.tbtmZoomClick(Sender: TObject);
var
  VPolygon: IGeometryLonLatMultiPolygon;
begin
  VPolygon := FLastSelectionInfo.Polygon;
  if (VPolygon <> nil)  then begin
    FMapGoto.FitRectToScreen(VPolygon.Bounds.Rect);
  end;
end;

procedure TfrmRegionProcess.tbtmSaveClick(Sender: TObject);
begin
  if (FLastSelectionInfo.Polygon <> nil) then FMarkDBGUI.SaveMarkModal(nil, FLastSelectionInfo.Polygon);
end;

procedure TfrmRegionProcess.StartSlsFromFile(const AFileName: string);
begin
  if FileExists(AFileName) then begin
    FProviderTilesDownload.StartBySLS(AFileName);
  end else begin
    ShowMessageFmt(_('Can''t open file: %s'), [AFileName]);
  end;
end;

end.


