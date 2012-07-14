{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit frm_RegionProcess;

interface

uses
  Windows,
  SysUtils,
  Forms,
  Buttons,
  Classes,
  Controls,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  inifiles,
  ComCtrls,
  i_Notifier,
  i_NotifierOperation,
  u_CommonFormAndFrameParents,
  i_LanguageManager,
  i_LastSelectionInfo,
  i_CoordConverterFactory,
  i_GlobalViewMainConfig,
  i_VectorItemLonLat,
  i_VectorItmesFactory,
  i_ImageResamplerConfig,
  i_BitmapTileSaveLoadFactory,
  i_LocalCoordConverterFactorySimpe,
  i_BitmapPostProcessingConfig,
  i_GlobalDownloadConfig,
  i_DownloadInfoSimple,
  i_UsedMarksConfig,
  i_MarksDrawConfig,
  i_MapTypes,
  i_ActiveMapsConfig,
  i_MapCalibration,
  i_TileFileNameGeneratorsList,
  i_ValueToStringConverter,
  i_MapTypeGUIConfigList,
  u_ExportProviderAbstract,
  u_ProviderTilesDownload,
  u_MarksSystem;

type
  TfrmRegionProcess = class(TFormWitghLanguageManager)
    Button1: TButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    Bevel5: TBevel;
    Label9: TLabel;
    CBFormat: TComboBox;
    Button3: TButton;
    SpeedButton1: TSpeedButton;
    SaveSelDialog: TSaveDialog;
    CBCloseWithStart: TCheckBox;
    TabSheet6: TTabSheet;
    pnlExport: TPanel;
    pnlBottomButtons: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure CBFormatChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FVectorItmesFactory: IVectorItmesFactory;
    FLastSelectionInfo: ILastSelectionInfo;
    FZoom_rect:byte;
    FPolygonLL: ILonLatPolygon;
    FProviderTilesDelte: TExportProviderAbstract;
    FProviderTilesGenPrev: TExportProviderAbstract;
    FProviderTilesCopy: TExportProviderAbstract;
    FProviderTilesDownload: TProviderTilesDownload;
    FProviderMapCombine: TExportProviderAbstract;
    procedure LoadRegion(const APolyLL: ILonLatPolygon);
    procedure DelRegion(const APolyLL: ILonLatPolygon);
    procedure genbacksatREG(const APolyLL: ILonLatPolygon);
    procedure scleitRECT(const APolyLL: ILonLatPolygon);
    procedure savefilesREG(const APolyLL: ILonLatPolygon);
    procedure ExportREG(const APolyLL: ILonLatPolygon);
    procedure InitExportsList(
      const ALanguageManager: ILanguageManager;
      const AAppClosingNotifier: INotifierOneOperation;
      const ATimerNoifier: INotifier;
      const AMainMapsConfig: IMainMapsConfig;
      const AFullMapsSet: IMapTypeSet;
      const AGUIConfigList: IMapTypeGUIConfigList;
      const ACoordConverterFactory: ICoordConverterFactory;
      const ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
      const AProjectionFactory: IProjectionInfoFactory;
      const AVectorItmesFactory: IVectorItmesFactory;
      const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
      const ATileNameGenerator: ITileFileNameGeneratorsList
    );
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AAppClosingNotifier: INotifierOneOperation;
      const ATimerNoifier: INotifier;
      const ALastSelectionInfo: ILastSelectionInfo;
      const AMainMapsConfig: IMainMapsConfig;
      const AFullMapsSet: IMapTypeSet;
      const AGUIConfigList: IMapTypeGUIConfigList;
      const ACoordConverterFactory: ICoordConverterFactory;
      const ATileNameGenerator: ITileFileNameGeneratorsList;
      const AViewConfig: IGlobalViewMainConfig;
      const AImageResamplerConfig: IImageResamplerConfig;
      const AMarksShowConfig: IUsedMarksConfig;
      const AMarksDrawConfig: IMarksDrawConfig;
      AMarksDB: TMarksSystem;
      const ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
      const ABitmapPostProcessingConfig: IBitmapPostProcessingConfig;
      const AProjectionFactory: IProjectionInfoFactory;
      const AVectorItmesFactory: IVectorItmesFactory;
      const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
      const AMapCalibrationList: IMapCalibrationList;
      const ADownloadConfig: IGlobalDownloadConfig;
      const ADownloadInfo: IDownloadInfoSimple;
      const AValueToStringConverterConfig: IValueToStringConverterConfig
    ); reintroduce;
    destructor Destroy; override;
    procedure LoadSelFromFile(const FileName:string);
    procedure StartSlsFromFile(const AFileName:string);
    procedure Show_(Azoom:byte; const APolygon: ILonLatPolygon);
    procedure RefreshTranslation; override;
  end;

implementation

uses
  gnugettext,
  t_GeoTypes,
  i_EnumDoublePoint,
  i_DoublePointsAggregator,
  u_DoublePointsAggregator,
  u_GeoTostr,
  u_ExportProviderYaMobileV3,
  u_ExportProviderYaMobileV4,
  u_ExportProviderGEKml,
  u_ExportProviderIPhone,
  u_ExportProviderAUX,
  u_ExportProviderZip,
  u_ExportProviderTar,
  u_ExportProviderJNX,
  u_ExportProviderOgf2,
  u_ExportProviderCE,
  u_ProviderTilesDelete,
  u_ProviderTilesGenPrev,
  u_ProviderTilesCopy,
  u_ProviderMapCombine;

{$R *.dfm}

constructor TfrmRegionProcess.Create(
  const ALanguageManager: ILanguageManager;
  const AAppClosingNotifier: INotifierOneOperation;
  const ATimerNoifier: INotifier;
  const ALastSelectionInfo: ILastSelectionInfo;
  const AMainMapsConfig: IMainMapsConfig;
  const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList;
  const ACoordConverterFactory: ICoordConverterFactory;
  const ATileNameGenerator: ITileFileNameGeneratorsList;
  const AViewConfig: IGlobalViewMainConfig;
  const AImageResamplerConfig: IImageResamplerConfig;
  const AMarksShowConfig: IUsedMarksConfig;
  const AMarksDrawConfig: IMarksDrawConfig;
  AMarksDB: TMarksSystem;
  const ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
  const ABitmapPostProcessingConfig: IBitmapPostProcessingConfig;
  const AProjectionFactory: IProjectionInfoFactory;
  const AVectorItmesFactory: IVectorItmesFactory;
  const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
  const AMapCalibrationList: IMapCalibrationList;
  const ADownloadConfig: IGlobalDownloadConfig;
  const ADownloadInfo: IDownloadInfoSimple;
  const AValueToStringConverterConfig: IValueToStringConverterConfig
);
begin
  TP_Ignore(Self, 'CBFormat.Items');
  inherited Create(ALanguageManager);
  FLastSelectionInfo := ALastSelectionInfo;
  FVectorItmesFactory := AVectorItmesFactory;
  InitExportsList(
    ALanguageManager,
    AAppClosingNotifier,
    ATimerNoifier,
    AMainMapsConfig,
    AFullMapsSet,
    AGUIConfigList,
    ACoordConverterFactory,
    ALocalConverterFactory,
    AProjectionFactory,
    AVectorItmesFactory,
    ABitmapTileSaveLoadFactory,
    ATileNameGenerator
  );

  FProviderTilesDelte :=
    TProviderTilesDelete.Create(
      ALanguageManager,
      AAppClosingNotifier,
      ATimerNoifier,
      AMainMapsConfig,
      AFullMapsSet,
      AGUIConfigList,
      AProjectionFactory,
      AVectorItmesFactory
    );
  FProviderTilesGenPrev :=
    TProviderTilesGenPrev.Create(
      ALanguageManager,
      AAppClosingNotifier,
      ATimerNoifier,
      AMainMapsConfig,
      AFullMapsSet,
      AGUIConfigList,
      AViewConfig,
      AProjectionFactory,
      AVectorItmesFactory,
      AImageResamplerConfig
    );
  FProviderTilesCopy :=
    TProviderTilesCopy.Create(
      ALanguageManager,
      AAppClosingNotifier,
      ATimerNoifier,
      AMainMapsConfig,
      AFullMapsSet,
      AGUIConfigList,
      AProjectionFactory,
      AVectorItmesFactory,
      ATileNameGenerator
    );
  FProviderTilesDownload :=
    TProviderTilesDownload.Create(
      AAppClosingNotifier,
      ALanguageManager,
      AValueToStringConverterConfig,
      AMainMapsConfig,
      AFullMapsSet,
      AGUIConfigList,
      AProjectionFactory,
      AVectorItmesFactory,
      ADownloadConfig,
      ADownloadInfo
    );
  FProviderMapCombine :=
    TProviderMapCombine.Create(
      ALanguageManager,
      AMainMapsConfig,
      AFullMapsSet,
      AGUIConfigList,
      AViewConfig,
      AAppClosingNotifier,
      ATimerNoifier,
      AProjectionFactory,
      AVectorItmesFactory,
      ABitmapTileSaveLoadFactory,
      AMarksShowConfig,
      AMarksDrawConfig,
      AMarksDB,
      ALocalConverterFactory,
      ABitmapPostProcessingConfig,
      AMapCalibrationList
    );
end;

destructor TfrmRegionProcess.Destroy;
var
  i: Integer;
begin
  for i := 0 to CBFormat.Items.Count - 1 do begin
    CBFormat.Items.Objects[i].Free;
    CBFormat.Items.Objects[i] := nil;
  end;
  FreeAndNil(FProviderTilesDelte);
  FreeAndNil(FProviderTilesGenPrev);
  FreeAndNil(FProviderTilesCopy);
  FreeAndNil(FProviderTilesDownload);
  FreeAndNil(FProviderMapCombine);
  inherited;
end;

procedure TfrmRegionProcess.LoadSelFromFile(const FileName: string);
var
  i:integer;
  VIni:TMemIniFile;
  VPointsAggregator: IDoublePointsAggregator;
  VPoint: TDoublePoint;
  VZoom: Byte;
begin
  if FileExists(FileName) then
  begin
    VIni := TMemIniFile.Create(FileName);
    try
      VPointsAggregator := TDoublePointsAggregator.Create;
      i := 1;
      while str2r( VIni.ReadString('HIGHLIGHTING','PointLon_'+inttostr(i),'2147483647') ) <> 2147483647 do
      begin
        VPoint.x := str2r(VIni.ReadString('HIGHLIGHTING','PointLon_'+inttostr(i),'2147483647'));
        VPoint.y := str2r(VIni.ReadString('HIGHLIGHTING','PointLat_'+inttostr(i),'2147483647'));
        VPointsAggregator.Add(VPoint);
        inc(i);
      end;
      if VPointsAggregator.Count > 0 then
      begin
        VZoom := VIni.Readinteger('HIGHLIGHTING','zoom',1) - 1;
        Self.Show_(VZoom, FVectorItmesFactory.CreateLonLatPolygon(VPointsAggregator.Points, VPointsAggregator.Count));
      end;
    finally
      VIni.Free;
    end;
  end
end;

procedure TfrmRegionProcess.RefreshTranslation;
var
  i: Integer;
  VProvider: TExportProviderAbstract;
  VIndex: Integer;
begin
  inherited;
  VIndex := CBFormat.ItemIndex;
  for i := 0 to CBFormat.Items.Count - 1 do begin
    VProvider := TExportProviderAbstract(CBFormat.Items.Objects[i]);
    CBFormat.Items[i] := VProvider.GetCaption;
  end;
  CBFormat.ItemIndex := VIndex;
end;

procedure TfrmRegionProcess.DelRegion(const APolyLL: ILonLatPolygon);
begin
  FProviderTilesDelte.StartProcess(APolyLL);
end;

procedure TfrmRegionProcess.ExportREG(const APolyLL: ILonLatPolygon);
var
  VExportProvider: TExportProviderAbstract;
begin
  VExportProvider := TExportProviderAbstract(CBFormat.Items.Objects[CBFormat.ItemIndex]);
  if VExportProvider <> nil then begin
    VExportProvider.StartProcess(APolyLL);
  end;
end;


procedure TfrmRegionProcess.savefilesREG(const APolyLL: ILonLatPolygon);
begin
  FProviderTilesCopy.StartProcess(APolyLL);
end;

procedure TfrmRegionProcess.LoadRegion(const APolyLL: ILonLatPolygon);
begin
  FProviderTilesDownload.StartProcess(APolyLL);
end;

procedure TfrmRegionProcess.genbacksatREG(const APolyLL: ILonLatPolygon);
begin
  FProviderTilesGenPrev.StartProcess(APolyLL);
end;

procedure TfrmRegionProcess.InitExportsList(
  const ALanguageManager: ILanguageManager;
  const AAppClosingNotifier: INotifierOneOperation;
  const ATimerNoifier: INotifier;
  const AMainMapsConfig: IMainMapsConfig;
  const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList;
  const ACoordConverterFactory: ICoordConverterFactory;
  const ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
  const AProjectionFactory: IProjectionInfoFactory;
  const AVectorItmesFactory: IVectorItmesFactory;
  const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
  const ATileNameGenerator: ITileFileNameGeneratorsList
);
var
  VExportProvider: TExportProviderAbstract;
begin
  VExportProvider :=
    TExportProviderIPhone.Create(
      ALanguageManager,
      AAppClosingNotifier,
      ATimerNoifier,
      AMainMapsConfig,
      AFullMapsSet,
      AGUIConfigList,
      ACoordConverterFactory,
      ALocalConverterFactory,
      AProjectionFactory,
      AVectorItmesFactory,
      ABitmapTileSaveLoadFactory,
      True
    );
  CBFormat.Items.AddObject(VExportProvider.GetCaption, VExportProvider);

  VExportProvider :=
    TExportProviderIPhone.Create(
      ALanguageManager,
      AAppClosingNotifier,
      ATimerNoifier,
      AMainMapsConfig,
      AFullMapsSet,
      AGUIConfigList,
      ACoordConverterFactory,
      ALocalConverterFactory,
      AProjectionFactory,
      AVectorItmesFactory,
      ABitmapTileSaveLoadFactory,
      False
    );
  CBFormat.Items.AddObject(VExportProvider.GetCaption, VExportProvider);

  VExportProvider :=
    TExportProviderGEKml.Create(
      ALanguageManager,
      AAppClosingNotifier,
      ATimerNoifier,
      AMainMapsConfig,
      AFullMapsSet,
      AGUIConfigList,
      AProjectionFactory,
      AVectorItmesFactory
    );
  CBFormat.Items.AddObject(VExportProvider.GetCaption, VExportProvider);

  VExportProvider :=
    TExportProviderYaMobileV3.Create(
      ALanguageManager,
      AAppClosingNotifier,
      ATimerNoifier,
      AMainMapsConfig,
      AFullMapsSet,
      AGUIConfigList,
      AProjectionFactory,
      AVectorItmesFactory,
      ABitmapTileSaveLoadFactory,
      ALocalConverterFactory,
      ACoordConverterFactory
    );
  CBFormat.Items.AddObject(VExportProvider.GetCaption, VExportProvider);

  VExportProvider :=
    TExportProviderYaMobileV4.Create(
      ALanguageManager,
      AAppClosingNotifier,
      ATimerNoifier,
      AMainMapsConfig,
      AFullMapsSet,
      AGUIConfigList,
      AProjectionFactory,
      AVectorItmesFactory,
      ABitmapTileSaveLoadFactory,
      ALocalConverterFactory,
      ACoordConverterFactory
    );
  CBFormat.Items.AddObject(VExportProvider.GetCaption, VExportProvider);

  VExportProvider :=
    TExportProviderAUX.Create(
      ALanguageManager,
      AAppClosingNotifier,
      ATimerNoifier,
      AMainMapsConfig,
      AFullMapsSet,
      AGUIConfigList,
      AProjectionFactory,
      AVectorItmesFactory
    );
  CBFormat.Items.AddObject(VExportProvider.GetCaption, VExportProvider);

  VExportProvider :=
    TExportProviderZip.Create(
      ALanguageManager,
      AAppClosingNotifier,
      ATimerNoifier,
      AMainMapsConfig,
      AFullMapsSet,
      AGUIConfigList,
      AProjectionFactory,
      AVectorItmesFactory,
      ATileNameGenerator
    );
  CBFormat.Items.AddObject(VExportProvider.GetCaption, VExportProvider);

  VExportProvider :=
    TExportProviderTar.Create(
      ALanguageManager,
      AAppClosingNotifier,
      ATimerNoifier,
      AMainMapsConfig,
      AFullMapsSet,
      AGUIConfigList,
      AProjectionFactory,
      AVectorItmesFactory,
      ATileNameGenerator
    );
  CBFormat.Items.AddObject(VExportProvider.GetCaption, VExportProvider);

  VExportProvider :=
    TExportProviderJNX.Create(
      ALanguageManager,
      AAppClosingNotifier,
      ATimerNoifier,
      AMainMapsConfig,
      AFullMapsSet,
      AGUIConfigList,
      AProjectionFactory,
      AVectorItmesFactory,
      ABitmapTileSaveLoadFactory,
      ACoordConverterFactory
    );
  CBFormat.Items.AddObject(VExportProvider.GetCaption, VExportProvider);

    VExportProvider :=
    TExportProviderOgf2.Create(
      ALanguageManager,
      AAppClosingNotifier,
      ATimerNoifier,
      AMainMapsConfig,
      AFullMapsSet,
      AGUIConfigList,
      AProjectionFactory,
      AVectorItmesFactory,
      ABitmapTileSaveLoadFactory,
      ALocalConverterFactory,
      ACoordConverterFactory
    );
  CBFormat.Items.AddObject(VExportProvider.GetCaption, VExportProvider);

  VExportProvider :=
    TExportProviderCE.Create(
      ALanguageManager,
      AAppClosingNotifier,
      ATimerNoifier,
      AMainMapsConfig,
      AFullMapsSet,
      AGUIConfigList,
      AProjectionFactory,
      AVectorItmesFactory,
      ACoordConverterFactory
    );
  CBFormat.Items.AddObject(VExportProvider.GetCaption, VExportProvider);

  CBFormat.ItemIndex := 0;
end;

procedure TfrmRegionProcess.scleitRECT(const APolyLL: ILonLatPolygon);
begin
  FProviderMapCombine.StartProcess(APolyLL);
end;


procedure TfrmRegionProcess.Button1Click(Sender: TObject);
begin
 case PageControl1.ActivePage.Tag of
  0: LoadRegion(FPolygonLL);
  1: scleitRECT(FPolygonLL);
  2: genbacksatREG(FPolygonLL);
  3: delRegion(FPolygonLL);
  4: ExportREG(FPolygonLL);
  5: savefilesREG(FPolygonLL);
 end;
  if CBCloseWithStart.Checked then begin
    close;
  end;
end;

procedure TfrmRegionProcess.Show_(Azoom:byte; const APolygon: ILonLatPolygon);
var
  i:integer;
  VExportProvider: TExportProviderAbstract;
begin
  FZoom_rect:=Azoom;
  FPolygonLL := APolygon;

  FLastSelectionInfo.SetPolygon(APolygon, FZoom_rect);
  for i := 0 to CBFormat.Items.Count - 1 do begin
    VExportProvider := TExportProviderAbstract(CBFormat.Items.Objects[i]);
    if VExportProvider <> nil then begin
      VExportProvider.Show(pnlExport, FZoom_rect, FPolygonLL);
    end;
  end;
  Self.Show;
end;


procedure TfrmRegionProcess.FormShow(Sender: TObject);
begin
  CBFormatChange(CBFormat);
  FProviderTilesDelte.Show(TabSheet4, FZoom_rect, FPolygonLL);
  FProviderTilesGenPrev.Show(TabSheet3, FZoom_rect, FPolygonLL);
  FProviderTilesCopy.Show(TabSheet6, FZoom_rect, FPolygonLL);
  FProviderTilesDownload.Show(TabSheet1, FZoom_rect, FPolygonLL);
  FProviderMapCombine.Show(TabSheet2, FZoom_rect, FPolygonLL);

  PageControl1.ActivePageIndex:=0;
end;

procedure TfrmRegionProcess.Button3Click(Sender: TObject);
begin
  close;
end;

procedure TfrmRegionProcess.SpeedButton1Click(Sender: TObject);
var
  Ini: Tinifile;
  i:integer;
  VZoom: Byte;
  VPolygon: ILonLatPolygon;
  VEnum: IEnumDoublePoint;
  VPoint: TDoublePoint;
begin
  if (SaveSelDialog.Execute)and(SaveSelDialog.FileName<>'') then begin
    If FileExists(SaveSelDialog.FileName) then DeleteFile(SaveSelDialog.FileName);
    FLastSelectionInfo.LockRead;
    try
      VZoom := FLastSelectionInfo.Zoom;
      VPolygon := FLastSelectionInfo.Polygon;
    finally
      FLastSelectionInfo.UnlockRead;
    end;
    Ini:=TiniFile.Create(SaveSelDialog.FileName);
    try
      if VPolygon.Count > 0 then begin
        Ini.WriteInteger('HIGHLIGHTING','zoom',VZoom + 1);
        VEnum := VPolygon.GetEnum;
        i := 1;
        while VEnum.Next(VPoint) do begin
          Ini.WriteFloat('HIGHLIGHTING','PointLon_'+inttostr(i), VPoint.x);
          Ini.WriteFloat('HIGHLIGHTING','PointLat_'+inttostr(i), VPoint.y);
          Inc(i);
        end;
      end;
    finally
      ini.Free;
    end;
  end;
end;

procedure TfrmRegionProcess.StartSlsFromFile(const AFileName: string);
begin
  FProviderTilesDownload.StartBySLS(AFileName);
end;

procedure TfrmRegionProcess.CBFormatChange(Sender: TObject);
var
  VExportProvider: TExportProviderAbstract;
  i: Integer;
begin
  for i := 0 to CBFormat.Items.Count - 1 do begin
    VExportProvider := TExportProviderAbstract(CBFormat.Items.Objects[i]);
    if VExportProvider <> nil then begin
      if i = CBFormat.ItemIndex then begin
        VExportProvider.Show(pnlExport, FZoom_rect, FPolygonLL);
      end else begin
        VExportProvider.Hide;
      end;
    end;
  end;
end;

end.


