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
  ComCtrls,
  TB2Item,
  TB2Dock,
  TB2Toolbar,
  TBX,
  i_InternalPerformanceCounter,
  i_NotifierTime,
  i_NotifierOperation,
  i_MapViewGoto,
  i_LanguageManager,
  i_LastSelectionInfo,
  i_ProjectionSetFactory,
  i_ProjectionSetList,
  i_ProjectionSetChangeable,
  i_ContentTypeManager,
  i_GlobalViewMainConfig,
  i_GeometryLonLat,
  i_GeometryLonLatFactory,
  i_GeometryProjectedFactory,
  i_GeometryProjectedProvider,
  i_VectorItemSubsetBuilder,
  i_ImageResamplerFactory,
  i_ImageResamplerConfig,
  i_Bitmap32BufferFactory,
  i_BitmapTileSaveLoadFactory,
  i_ArchiveReadWriteFactory,
  i_BitmapPostProcessing,
  i_HashFunction,
  i_GlobalDownloadConfig,
  i_DownloadInfoSimple,
  i_UseTilePrevZoomConfig,
  i_UsedMarksConfig,
  i_MarksDrawConfig,
  i_MarkSystem,
  i_MapTypeListChangeable,
  i_MapTypeSet,
  i_MapTypeListBuilder,
  i_RegionProcess,
  i_ActiveMapsConfig,
  i_MapCalibration,
  i_TileFileNameGeneratorsList,
  i_TileStorageTypeList,
  i_LocalCoordConverterChangeable,
  i_MapType,
  i_FillingMapLayerConfig,
  i_FillingMapPolygon,
  i_MapLayerGridsConfig,
  i_CoordToStringConverter,
  i_ValueToStringConverter,
  i_InterfaceListStatic,
  i_MapTypeGUIConfigList,
  i_GlobalBerkeleyDBHelper,
  i_RegionProcessProgressInfoInternalFactory,
  i_RegionProcessProvider,
  i_TileIteratorFactory,
  u_CommonFormAndFrameParents,
  u_ProviderTilesDownload,
  u_MarkDbGUIHelper,
  fr_MapSelect;

type
  TfrmRegionProcess = class(TFormWitghLanguageManager, IRegionProcess, IRegionProcessFromFile)
    Button1: TButton;
    Button3: TButton;
    SaveSelDialog: TSaveDialog;
    pnlBottomButtons: TPanel;
    TBXOperationsToolbar: TTBXToolbar;
    tbtmMark: TTBItem;
    tbtmZoom: TTBItem;
    tbtmSave: TTBItem;
    tbtmCopyBbox: TTBItem;
    TBXDontClose: TTBXToolbar;
    tbtmDontClose: TTBItem;
    pnlContent: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tbtmSaveClick(Sender: TObject);
    procedure tbtmZoomClick(Sender: TObject);
    procedure tbtmMarkClick(Sender: TObject);
    procedure tbtmCopyBboxClick(Sender: TObject);
  private
    FTileIteratorFactory: ITileIteratorFactory;
    FVectorGeometryLonLatFactory: IGeometryLonLatFactory;
    FVectorGeometryProjectedFactory: IGeometryProjectedFactory;
    FLastSelectionInfo: ILastSelectionInfo;
    FZoom_rect: byte;
    FPolygonLL: IGeometryLonLatPolygon;
    FProviderAll: IRegionProcessProvider;
    FProviderTilesDownload: IRegionProcessProviderDownload;
    FMapGoto: IMapViewGoto;
    FMarkDBGUI: TMarkDbGUIHelper;
    FPosition: ILocalCoordConverterChangeable;

    function PrepareProviders(
      const ALanguageManager: ILanguageManager;
      const ACounterList: IInternalPerformanceCounterList;
      const AAppClosingNotifier: INotifierOneOperation;
      const ATimerNoifier: INotifierTime;
      const ALastSelectionInfo: ILastSelectionInfo;
      const AMainMapConfig: IActiveMapConfig;
      const AMainLayersConfig: IActiveLayersConfig;
      const AActiveBitmapLayersList: IMapTypeListChangeable;
      const AMapTypeListBuilderFactory: IMapTypeListBuilderFactory;
      const AGlobalBerkeleyDBHelper: IGlobalBerkeleyDBHelper;
      const APosition: ILocalCoordConverterChangeable;
      const AProjectionSet: IProjectionSetChangeable;
      const AFullMapsSet: IMapTypeSet;
      const AGUIConfigList: IMapTypeGUIConfigList;
      const AContentTypeManager: IContentTypeManager;
      const AProjectionSetFactory: IProjectionSetFactory;
      const ATileStorageTypeList: ITileStorageTypeListStatic;
      const ATileNameGenerator: ITileFileNameGeneratorsList;
      const AViewConfig: IGlobalViewMainConfig;
      const AUseTilePrevZoomConfig: IUseTilePrevZoomConfig;
      const AImageResamplerFactoryList: IImageResamplerFactoryList;
      const AImageResamplerConfig: IImageResamplerConfig;
      const ATileReprojectResamplerConfig: IImageResamplerConfig;
      const AMarksShowConfig: IUsedMarksConfig;
      const AMarksDrawConfig: IMarksDrawConfig;
      const AMarksDB: IMarkSystem;
      const AHashFunction: IHashFunction;
      const ABitmapPostProcessing: IBitmapPostProcessingChangeable;
      const AProjectionSetList: IProjectionSetList;
      const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
      const AProjectedGeometryProvider: IGeometryProjectedProvider;
      const AVectorSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
      const ABitmapFactory: IBitmap32StaticFactory;
      const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
      const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
      const AMapCalibrationList: IMapCalibrationList;
      const ADownloadConfig: IGlobalDownloadConfig;
      const ADownloadInfo: IDownloadInfoSimple;
      const AFillingMapConfig: IFillingMapLayerConfig;
      const AFillingMapType: IMapTypeChangeable;
      const AFillingMapPolygon: IFillingMapPolygon;
      const AGridsConfig: IMapLayerGridsConfig;
      const ACoordToStringConverter: ICoordToStringConverterChangeable;
      const AValueToStringConverter: IValueToStringConverterChangeable;
      const AMapGoto: IMapViewGoto;
      const AMarkDBGUI: TMarkDbGUIHelper
    ): IInterfaceListStatic;

    function PrepareCombineProviders(
      const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
      const ALanguageManager: ILanguageManager;
      const ACounterList: IInternalPerformanceCounterList;
      const AMapSelectFrameBuilder: IMapSelectFrameBuilder;
      const AActiveMapsSet: IMapTypeListChangeable;
      const AViewConfig: IGlobalViewMainConfig;
      const AUseTilePrevZoomConfig: IUseTilePrevZoomConfig;
      const AProjectionSet: IProjectionSetChangeable;
      const AProjectionSetList: IProjectionSetList;
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
      const AProjectedGeometryProvider: IGeometryProjectedProvider;
      const AVectorSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
      const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
      const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
      const AMarksShowConfig: IUsedMarksConfig;
      const AMarksDrawConfig: IMarksDrawConfig;
      const AMarksDB: IMarkSystem;
      const AHashFunction: IHashFunction;
      const ABitmapFactory: IBitmap32StaticFactory;
      const ABitmapPostProcessing: IBitmapPostProcessingChangeable;
      const AFillingMapConfig: IFillingMapLayerConfig;
      const AFillingMapType: IMapTypeChangeable;
      const AFillingMapPolygon: IFillingMapPolygon;
      const AGridsConfig: IMapLayerGridsConfig;
      const ACoordToStringConverter: ICoordToStringConverterChangeable;
      const AMapCalibrationList: IMapCalibrationList
    ): IInterfaceListStatic;
    function PrepareDeleteProviders(
      const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
      const ALanguageManager: ILanguageManager;
      const AMapSelectFrameBuilder: IMapSelectFrameBuilder;
      const APosition: ILocalCoordConverterChangeable;
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
      const AMarkSystem: IMarkSystem
    ): IInterfaceListStatic;
    function PrepareExportProviders(
      const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
      const ALanguageManager: ILanguageManager;
      const AMapSelectFrameBuilder: IMapSelectFrameBuilder;
      const AActiveMapsList: IMapTypeListChangeable;
      const AProjectionSetFactory: IProjectionSetFactory;
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
      const ABitmap32StaticFactory: IBitmap32StaticFactory;
      const ABitmapPostProcessing: IBitmapPostProcessingChangeable;
      const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
      const AImageResamplerFactoryList: IImageResamplerFactoryList;
      const ATileReprojectResamplerConfig: IImageResamplerConfig;
      const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
      const ATileStorageTypeList: ITileStorageTypeListStatic;
      const ATileNameGenerator: ITileFileNameGeneratorsList
    ): IInterfaceListStatic;
  private
    procedure ProcessPolygon(
      const APolygon: IGeometryLonLatPolygon
    );
    procedure ProcessPolygonWithZoom(
      const AZoom: Byte;
      const APolygon: IGeometryLonLatPolygon
    );
  private
    procedure LoadSelFromFile(
      const AFileName: string;
      out APolygon: IGeometryLonLatPolygon
    );
    procedure StartSlsFromFile(
      const AFileName: string;
      const AStartPaused: Boolean
    );
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const ACounterList: IInternalPerformanceCounterList;
      const AAppClosingNotifier: INotifierOneOperation;
      const ATimerNoifier: INotifierTime;
      const ALastSelectionInfo: ILastSelectionInfo;
      const AMainMapConfig: IActiveMapConfig;
      const AMainLayersConfig: IActiveLayersConfig;
      const AActiveBitmapLayersList: IMapTypeListChangeable;
      const AMapTypeListBuilderFactory: IMapTypeListBuilderFactory;
      const AGlobalBerkeleyDBHelper: IGlobalBerkeleyDBHelper;
      const APosition: ILocalCoordConverterChangeable;
      const AProjectionSet: IProjectionSetChangeable;
      const AFullMapsSet: IMapTypeSet;
      const AGUIConfigList: IMapTypeGUIConfigList;
      const AContentTypeManager: IContentTypeManager;
      const AProjectionSetFactory: IProjectionSetFactory;
      const ATileStorageTypeList: ITileStorageTypeListStatic;
      const ATileNameGenerator: ITileFileNameGeneratorsList;
      const AViewConfig: IGlobalViewMainConfig;
      const AUseTilePrevZoomConfig: IUseTilePrevZoomConfig;
      const AImageResamplerFactoryList: IImageResamplerFactoryList;
      const AImageResamplerConfig: IImageResamplerConfig;
      const ATileReprojectResamplerConfig: IImageResamplerConfig;
      const AMarksShowConfig: IUsedMarksConfig;
      const AMarksDrawConfig: IMarksDrawConfig;
      const AMarksDB: IMarkSystem;
      const AHashFunction: IHashFunction;
      const ABitmapPostProcessing: IBitmapPostProcessingChangeable;
      const AProjectionSetList: IProjectionSetList;
      const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
      const AProjectedGeometryProvider: IGeometryProjectedProvider;
      const AVectorSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
      const ABitmapFactory: IBitmap32StaticFactory;
      const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
      const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
      const AMapCalibrationList: IMapCalibrationList;
      const ADownloadConfig: IGlobalDownloadConfig;
      const ADownloadInfo: IDownloadInfoSimple;
      const AFillingMapConfig: IFillingMapLayerConfig;
      const AFillingMapType: IMapTypeChangeable;
      const AFillingMapPolygon: IFillingMapPolygon;
      const AGridsConfig: IMapLayerGridsConfig;
      const ACoordToStringConverter: ICoordToStringConverterChangeable;
      const AValueToStringConverter: IValueToStringConverterChangeable;
      const AMapGoto: IMapViewGoto;
      const AMarkDBGUI: TMarkDbGUIHelper
    ); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  IniFiles,
  {$IFNDef UNICODE}
  CompatibilityIniFiles,
  {$ENDIF}
  gnugettext,
  i_InterfaceListSimple,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  u_TileIteratorFactory,
  u_ConfigDataProviderByIniFile,
  u_ConfigDataWriteProviderByIniFile,
  u_ConfigProviderHelpers,
  u_ClipboardFunc,
  u_GeoToStrFunc,
  u_InterfaceListSimple,
  u_RegionProcessProgressInfoInternalFactory,
  u_ProviderTilesGenPrev,
  u_ProviderTilesCopy,
  u_RegionProcessProviderComplex,
  u_ProviderDeleteTiles,
  u_ProviderDeleteMarks,
  u_ProviderMapCombine,
  u_ExportProviderRMP,
  u_ExportProviderMBTiles,
  u_ExportProviderRMapsSQLite,
  u_ExportProviderOruxMapsSQLite,
  u_ExportProviderYaMobileV3,
  u_ExportProviderYaMobileV4,
  u_ExportProviderGEKml,
  u_ExportProviderIPhone,
  u_ExportProviderAUX,
  u_ExportProviderZip,
  u_ExportProviderTar,
  u_ExportProviderJNX,
  u_ExportProviderIMG,
  u_ExportProviderOgf2,
  u_ExportProviderCE,
  u_BitmapMapCombinerBMP,
  u_BitmapMapCombinerJPG,
  u_BitmapMapCombinerPNG,
  u_BitmapMapCombinerKMZ,
  u_BitmapMapCombinerECWJP2,
  u_BitmapMapCombinerRAW,
  u_BitmapMapCombinerGeoTIFF;

{$R *.dfm}

constructor TfrmRegionProcess.Create(
  const ALanguageManager: ILanguageManager;
  const ACounterList: IInternalPerformanceCounterList;
  const AAppClosingNotifier: INotifierOneOperation;
  const ATimerNoifier: INotifierTime;
  const ALastSelectionInfo: ILastSelectionInfo;
  const AMainMapConfig: IActiveMapConfig;
  const AMainLayersConfig: IActiveLayersConfig;
  const AActiveBitmapLayersList: IMapTypeListChangeable;
  const AMapTypeListBuilderFactory: IMapTypeListBuilderFactory;
  const AGlobalBerkeleyDBHelper: IGlobalBerkeleyDBHelper;
  const APosition: ILocalCoordConverterChangeable;
  const AProjectionSet: IProjectionSetChangeable;
  const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList;
  const AContentTypeManager: IContentTypeManager;
  const AProjectionSetFactory: IProjectionSetFactory;
  const ATileStorageTypeList: ITileStorageTypeListStatic;
  const ATileNameGenerator: ITileFileNameGeneratorsList;
  const AViewConfig: IGlobalViewMainConfig;
  const AUseTilePrevZoomConfig: IUseTilePrevZoomConfig;
  const AImageResamplerFactoryList: IImageResamplerFactoryList;
  const AImageResamplerConfig: IImageResamplerConfig;
  const ATileReprojectResamplerConfig: IImageResamplerConfig;
  const AMarksShowConfig: IUsedMarksConfig;
  const AMarksDrawConfig: IMarksDrawConfig;
  const AMarksDB: IMarkSystem;
  const AHashFunction: IHashFunction;
  const ABitmapPostProcessing: IBitmapPostProcessingChangeable;
  const AProjectionSetList: IProjectionSetList;
  const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  const AProjectedGeometryProvider: IGeometryProjectedProvider;
  const AVectorSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const ABitmapFactory: IBitmap32StaticFactory;
  const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
  const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
  const AMapCalibrationList: IMapCalibrationList;
  const ADownloadConfig: IGlobalDownloadConfig;
  const ADownloadInfo: IDownloadInfoSimple;
  const AFillingMapConfig: IFillingMapLayerConfig;
  const AFillingMapType: IMapTypeChangeable;
  const AFillingMapPolygon: IFillingMapPolygon;
  const AGridsConfig: IMapLayerGridsConfig;
  const ACoordToStringConverter: ICoordToStringConverterChangeable;
  const AValueToStringConverter: IValueToStringConverterChangeable;
  const AMapGoto: IMapViewGoto;
  const AMarkDBGUI: TMarkDbGUIHelper
);
begin
  inherited Create(ALanguageManager);
  FLastSelectionInfo := ALastSelectionInfo;
  FPosition := APosition;
  FVectorGeometryLonLatFactory := AVectorGeometryLonLatFactory;
  FVectorGeometryProjectedFactory := AVectorGeometryProjectedFactory;
  FMapGoto := AMapGoto;
  FMarkDBGUI := AMarkDBGUI;

  FTileIteratorFactory :=
    TTileIteratorFactory.Create(AVectorGeometryProjectedFactory);

  FProviderAll :=
    TRegionProcessProviderComplex.Create(
      ALanguageManager,
      PrepareProviders(
        ALanguageManager,
        ACounterList,
        AAppClosingNotifier,
        ATimerNoifier,
        ALastSelectionInfo,
        AMainMapConfig,
        AMainLayersConfig,
        AActiveBitmapLayersList,
        AMapTypeListBuilderFactory,
        AGlobalBerkeleyDBHelper,
        APosition,
        AProjectionSet,
        AFullMapsSet,
        AGUIConfigList,
        AContentTypeManager,
        AProjectionSetFactory,
        ATileStorageTypeList,
        ATileNameGenerator,
        AViewConfig,
        AUseTilePrevZoomConfig,
        AImageResamplerFactoryList,
        AImageResamplerConfig,
        ATileReprojectResamplerConfig,
        AMarksShowConfig,
        AMarksDrawConfig,
        AMarksDB,
        AHashFunction,
        ABitmapPostProcessing,
        AProjectionSetList,
        AVectorGeometryLonLatFactory,
        AVectorGeometryProjectedFactory,
        AProjectedGeometryProvider,
        AVectorSubsetBuilderFactory,
        ABitmapFactory,
        ABitmapTileSaveLoadFactory,
        AArchiveReadWriteFactory,
        AMapCalibrationList,
        ADownloadConfig,
        ADownloadInfo,
        AFillingMapConfig,
        AFillingMapType,
        AFillingMapPolygon,
        AGridsConfig,
        ACoordToStringConverter,
        AValueToStringConverter,
        AMapGoto,
        AMarkDBGUI
      ),
      True,
      '',
      '',
      ''
    );

end;

destructor TfrmRegionProcess.Destroy;
begin
  FProviderTilesDownload := nil;
  FProviderAll := nil;
  inherited;
end;

function TfrmRegionProcess.PrepareProviders(
  const ALanguageManager: ILanguageManager;
  const ACounterList: IInternalPerformanceCounterList;
  const AAppClosingNotifier: INotifierOneOperation;
  const ATimerNoifier: INotifierTime;
  const ALastSelectionInfo: ILastSelectionInfo;
  const AMainMapConfig: IActiveMapConfig;
  const AMainLayersConfig: IActiveLayersConfig;
  const AActiveBitmapLayersList: IMapTypeListChangeable;
  const AMapTypeListBuilderFactory: IMapTypeListBuilderFactory;
  const AGlobalBerkeleyDBHelper: IGlobalBerkeleyDBHelper;
  const APosition: ILocalCoordConverterChangeable;
  const AProjectionSet: IProjectionSetChangeable;
  const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList;
  const AContentTypeManager: IContentTypeManager;
  const AProjectionSetFactory: IProjectionSetFactory;
  const ATileStorageTypeList: ITileStorageTypeListStatic;
  const ATileNameGenerator: ITileFileNameGeneratorsList;
  const AViewConfig: IGlobalViewMainConfig;
  const AUseTilePrevZoomConfig: IUseTilePrevZoomConfig;
  const AImageResamplerFactoryList: IImageResamplerFactoryList;
  const AImageResamplerConfig: IImageResamplerConfig;
  const ATileReprojectResamplerConfig: IImageResamplerConfig;
  const AMarksShowConfig: IUsedMarksConfig;
  const AMarksDrawConfig: IMarksDrawConfig;
  const AMarksDB: IMarkSystem;
  const AHashFunction: IHashFunction;
  const ABitmapPostProcessing: IBitmapPostProcessingChangeable;
  const AProjectionSetList: IProjectionSetList;
  const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  const AProjectedGeometryProvider: IGeometryProjectedProvider;
  const AVectorSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const ABitmapFactory: IBitmap32StaticFactory;
  const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
  const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
  const AMapCalibrationList: IMapCalibrationList;
  const ADownloadConfig: IGlobalDownloadConfig;
  const ADownloadInfo: IDownloadInfoSimple;
  const AFillingMapConfig: IFillingMapLayerConfig;
  const AFillingMapType: IMapTypeChangeable;
  const AFillingMapPolygon: IFillingMapPolygon;
  const AGridsConfig: IMapLayerGridsConfig;
  const ACoordToStringConverter: ICoordToStringConverterChangeable;
  const AValueToStringConverter: IValueToStringConverterChangeable;
  const AMapGoto: IMapViewGoto;
  const AMarkDBGUI: TMarkDbGUIHelper
): IInterfaceListStatic;
var
  VProgressFactory: IRegionProcessProgressInfoInternalFactory;
  VMapSelectFrameBuilder: IMapSelectFrameBuilder;
  VExportProvider: IRegionProcessProvider;
  VList: IInterfaceListSimple;
begin
  VList := TInterfaceListSimple.Create;

  VMapSelectFrameBuilder :=
    TMapSelectFrameBuilder.Create(
      ALanguageManager,
      AMainMapConfig,
      AMainLayersConfig,
      AGUIConfigList,
      AFullMapsSet
    );
  VProgressFactory :=
    TRegionProcessProgressInfoInternalFactory.Create(
      AAppClosingNotifier,
      ATimerNoifier,
      Self,
      FMapGoto
    );

  FProviderTilesDownload :=
    TProviderTilesDownload.Create(
      AAppClosingNotifier,
      VProgressFactory,
      FTileIteratorFactory,
      ALanguageManager,
      AValueToStringConverter,
      VMapSelectFrameBuilder,
      AFullMapsSet,
      AVectorGeometryLonLatFactory,
      AVectorGeometryProjectedFactory,
      ADownloadConfig,
      ADownloadInfo,
      Self,
      FMapGoto,
      FMarkDBGUI,
      AMainMapConfig
    );
  VExportProvider := FProviderTilesDownload;
  VList.Add(VExportProvider);

  VExportProvider :=
    TRegionProcessProviderComplex.Create(
      ALanguageManager,
      PrepareCombineProviders(
        VProgressFactory,
        ALanguageManager,
        ACounterList.CreateAndAddNewSubList('Combine'),
        VMapSelectFrameBuilder,
        AActiveBitmapLayersList,
        AViewConfig,
        AUseTilePrevZoomConfig,
        AProjectionSet,
        AProjectionSetList,
        AVectorGeometryProjectedFactory,
        AProjectedGeometryProvider,
        AVectorSubsetBuilderFactory,
        ABitmapTileSaveLoadFactory,
        AArchiveReadWriteFactory,
        AMarksShowConfig,
        AMarksDrawConfig,
        AMarksDB,
        AHashFunction,
        ABitmapFactory,
        ABitmapPostProcessing,
        AFillingMapConfig,
        AFillingMapType,
        AFillingMapPolygon,
        AGridsConfig,
        ACoordToStringConverter,
        AMapCalibrationList
      ),
      False,
      gettext_NoOp('Stitch'),
      gettext_NoOp('Stitch selection'),
      gettext_NoOp('Output format:')
    );
  VList.Add(VExportProvider);

  VExportProvider :=
    TProviderTilesGenPrev.Create(
      VProgressFactory,
      FTileIteratorFactory,
      ALanguageManager,
      VMapSelectFrameBuilder,
      AViewConfig,
      ABitmapFactory,
      AImageResamplerFactoryList,
      AImageResamplerConfig
    );
  VList.Add(VExportProvider);

  VExportProvider :=
    TRegionProcessProviderComplex.Create(
      ALanguageManager,
      PrepareDeleteProviders(
        VProgressFactory,
        ALanguageManager,
        VMapSelectFrameBuilder,
        APosition,
        AVectorGeometryProjectedFactory,
        AMarkDBGUI.MarksDb
      ),
      True,
      gettext_NoOp('Delete'),
      '',
      ''
    );
  VList.Add(VExportProvider);

  VExportProvider :=
    TRegionProcessProviderComplex.Create(
      ALanguageManager,
      PrepareExportProviders(
        VProgressFactory,
        ALanguageManager,
        VMapSelectFrameBuilder,
        AActiveBitmapLayersList,
        AProjectionSetFactory,
        AVectorGeometryProjectedFactory,
        ABitmapFactory,
        ABitmapPostProcessing,
        ABitmapTileSaveLoadFactory,
        AImageResamplerFactoryList,
        ATileReprojectResamplerConfig,
        AArchiveReadWriteFactory,
        ATileStorageTypeList,
        ATileNameGenerator
      ),
      False,
      gettext_NoOp('Export'),
      '',
      gettext_NoOp('Export selection to format')
    );
  VList.Add(VExportProvider);

  VExportProvider :=
    TProviderTilesCopy.Create(
      ATimerNoifier,
      VProgressFactory,
      ALanguageManager,
      VMapSelectFrameBuilder,
      AActiveBitmapLayersList,
      AMainMapConfig,
      AGlobalBerkeleyDBHelper,
      AFullMapsSet,
      AGUIConfigList,
      AMapTypeListBuilderFactory,
      AContentTypeManager,
      FTileIteratorFactory,
      ATileStorageTypeList,
      ABitmapFactory,
      ABitmapTileSaveLoadFactory
    );
  VList.Add(VExportProvider);

  Result := VList.MakeStaticAndClear;
end;

function TfrmRegionProcess.PrepareCombineProviders(
  const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
  const ALanguageManager: ILanguageManager;
  const ACounterList: IInternalPerformanceCounterList;
  const AMapSelectFrameBuilder: IMapSelectFrameBuilder;
  const AActiveMapsSet: IMapTypeListChangeable;
  const AViewConfig: IGlobalViewMainConfig;
  const AUseTilePrevZoomConfig: IUseTilePrevZoomConfig;
  const AProjectionSet: IProjectionSetChangeable;
  const AProjectionSetList: IProjectionSetList;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  const AProjectedGeometryProvider: IGeometryProjectedProvider;
  const AVectorSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
  const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
  const AMarksShowConfig: IUsedMarksConfig;
  const AMarksDrawConfig: IMarksDrawConfig;
  const AMarksDB: IMarkSystem;
  const AHashFunction: IHashFunction;
  const ABitmapFactory: IBitmap32StaticFactory;
  const ABitmapPostProcessing: IBitmapPostProcessingChangeable;
  const AFillingMapConfig: IFillingMapLayerConfig;
  const AFillingMapType: IMapTypeChangeable;
  const AFillingMapPolygon: IFillingMapPolygon;
  const AGridsConfig: IMapLayerGridsConfig;
  const ACoordToStringConverter: ICoordToStringConverterChangeable;
  const AMapCalibrationList: IMapCalibrationList
): IInterfaceListStatic;
var
  VExportProvider: IRegionProcessProvider;
  VList: IInterfaceListSimple;
begin
  VList := TInterfaceListSimple.Create;

  VExportProvider :=
    TProviderMapCombine.Create(
      TBitmapMapCombinerFactoryJPG.Create(ACounterList),
      AProgressFactory,
      ALanguageManager,
      AMapSelectFrameBuilder,
      AActiveMapsSet,
      AViewConfig,
      AUseTilePrevZoomConfig,
      AProjectionSet,
      AProjectionSetList,
      AVectorGeometryProjectedFactory,
      AProjectedGeometryProvider,
      AVectorSubsetBuilderFactory,
      AMarksShowConfig,
      AMarksDrawConfig,
      AMarksDB,
      AHashFunction,
      ABitmapFactory,
      ABitmapPostProcessing,
      AFillingMapConfig,
      AFillingMapType,
      AFillingMapPolygon,
      AGridsConfig,
      ACoordToStringConverter,
      AMapCalibrationList
    );
  VList.Add(VExportProvider);

  VExportProvider :=
    TProviderMapCombine.Create(
      TBitmapMapCombinerFactoryPNG.Create(ACounterList),
      AProgressFactory,
      ALanguageManager,
      AMapSelectFrameBuilder,
      AActiveMapsSet,
      AViewConfig,
      AUseTilePrevZoomConfig,
      AProjectionSet,
      AProjectionSetList,
      AVectorGeometryProjectedFactory,
      AProjectedGeometryProvider,
      AVectorSubsetBuilderFactory,
      AMarksShowConfig,
      AMarksDrawConfig,
      AMarksDB,
      AHashFunction,
      ABitmapFactory,
      ABitmapPostProcessing,
      AFillingMapConfig,
      AFillingMapType,
      AFillingMapPolygon,
      AGridsConfig,
      ACoordToStringConverter,
      AMapCalibrationList
    );
  VList.Add(VExportProvider);

  VExportProvider :=
    TProviderMapCombine.Create(
      TBitmapMapCombinerFactoryBMP.Create(ACounterList),
      AProgressFactory,
      ALanguageManager,
      AMapSelectFrameBuilder,
      AActiveMapsSet,
      AViewConfig,
      AUseTilePrevZoomConfig,
      AProjectionSet,
      AProjectionSetList,
      AVectorGeometryProjectedFactory,
      AProjectedGeometryProvider,
      AVectorSubsetBuilderFactory,
      AMarksShowConfig,
      AMarksDrawConfig,
      AMarksDB,
      AHashFunction,
      ABitmapFactory,
      ABitmapPostProcessing,
      AFillingMapConfig,
      AFillingMapType,
      AFillingMapPolygon,
      AGridsConfig,
      ACoordToStringConverter,
      AMapCalibrationList
    );
  VList.Add(VExportProvider);

  VExportProvider :=
    TProviderMapCombine.Create(
      TBitmapMapCombinerFactoryECW.Create(ACounterList),
      AProgressFactory,
      ALanguageManager,
      AMapSelectFrameBuilder,
      AActiveMapsSet,
      AViewConfig,
      AUseTilePrevZoomConfig,
      AProjectionSet,
      AProjectionSetList,
      AVectorGeometryProjectedFactory,
      AProjectedGeometryProvider,
      AVectorSubsetBuilderFactory,
      AMarksShowConfig,
      AMarksDrawConfig,
      AMarksDB,
      AHashFunction,
      ABitmapFactory,
      ABitmapPostProcessing,
      AFillingMapConfig,
      AFillingMapType,
      AFillingMapPolygon,
      AGridsConfig,
      ACoordToStringConverter,
      AMapCalibrationList
    );
  VList.Add(VExportProvider);

  VExportProvider :=
    TProviderMapCombine.Create(
      TBitmapMapCombinerFactoryJP2.Create(ACounterList, False),
      AProgressFactory,
      ALanguageManager,
      AMapSelectFrameBuilder,
      AActiveMapsSet,
      AViewConfig,
      AUseTilePrevZoomConfig,
      AProjectionSet,
      AProjectionSetList,
      AVectorGeometryProjectedFactory,
      AProjectedGeometryProvider,
      AVectorSubsetBuilderFactory,
      AMarksShowConfig,
      AMarksDrawConfig,
      AMarksDB,
      AHashFunction,
      ABitmapFactory,
      ABitmapPostProcessing,
      AFillingMapConfig,
      AFillingMapType,
      AFillingMapPolygon,
      AGridsConfig,
      ACoordToStringConverter,
      AMapCalibrationList
    );
  VList.Add(VExportProvider);

  VExportProvider :=
    TProviderMapCombine.Create(
      TBitmapMapCombinerFactoryJP2.Create(ACounterList, True),
      AProgressFactory,
      ALanguageManager,
      AMapSelectFrameBuilder,
      AActiveMapsSet,
      AViewConfig,
      AUseTilePrevZoomConfig,
      AProjectionSet,
      AProjectionSetList,
      AVectorGeometryProjectedFactory,
      AProjectedGeometryProvider,
      AVectorSubsetBuilderFactory,
      AMarksShowConfig,
      AMarksDrawConfig,
      AMarksDB,
      AHashFunction,
      ABitmapFactory,
      ABitmapPostProcessing,
      AFillingMapConfig,
      AFillingMapType,
      AFillingMapPolygon,
      AGridsConfig,
      ACoordToStringConverter,
      AMapCalibrationList
    );
  VList.Add(VExportProvider);

  VExportProvider :=
    TProviderMapCombine.Create(
      TBitmapMapCombinerFactoryKMZ.Create(ABitmapTileSaveLoadFactory, AArchiveReadWriteFactory, ABitmapFactory),
      AProgressFactory,
      ALanguageManager,
      AMapSelectFrameBuilder,
      AActiveMapsSet,
      AViewConfig,
      AUseTilePrevZoomConfig,
      AProjectionSet,
      AProjectionSetList,
      AVectorGeometryProjectedFactory,
      AProjectedGeometryProvider,
      AVectorSubsetBuilderFactory,
      AMarksShowConfig,
      AMarksDrawConfig,
      AMarksDB,
      AHashFunction,
      ABitmapFactory,
      ABitmapPostProcessing,
      AFillingMapConfig,
      AFillingMapType,
      AFillingMapPolygon,
      AGridsConfig,
      ACoordToStringConverter,
      AMapCalibrationList
    );
  VList.Add(VExportProvider);

  VExportProvider :=
    TProviderMapCombine.Create(
      TBitmapMapCombinerFactoryRAW.Create(ACounterList),
      AProgressFactory,
      ALanguageManager,
      AMapSelectFrameBuilder,
      AActiveMapsSet,
      AViewConfig,
      AUseTilePrevZoomConfig,
      AProjectionSet,
      AProjectionSetList,
      AVectorGeometryProjectedFactory,
      AProjectedGeometryProvider,
      AVectorSubsetBuilderFactory,
      AMarksShowConfig,
      AMarksDrawConfig,
      AMarksDB,
      AHashFunction,
      ABitmapFactory,
      ABitmapPostProcessing,
      AFillingMapConfig,
      AFillingMapType,
      AFillingMapPolygon,
      AGridsConfig,
      ACoordToStringConverter,
      AMapCalibrationList
    );
  VList.Add(VExportProvider);

  VExportProvider :=
    TProviderMapCombine.Create(
      TBitmapMapCombinerFactoryGeoTIFF.Create(ACounterList),
      AProgressFactory,
      ALanguageManager,
      AMapSelectFrameBuilder,
      AActiveMapsSet,
      AViewConfig,
      AUseTilePrevZoomConfig,
      AProjectionSet,
      AProjectionSetList,
      AVectorGeometryProjectedFactory,
      AProjectedGeometryProvider,
      AVectorSubsetBuilderFactory,
      AMarksShowConfig,
      AMarksDrawConfig,
      AMarksDB,
      AHashFunction,
      ABitmapFactory,
      ABitmapPostProcessing,
      AFillingMapConfig,
      AFillingMapType,
      AFillingMapPolygon,
      AGridsConfig,
      ACoordToStringConverter,
      AMapCalibrationList
    );
  VList.Add(VExportProvider);
  Result := VList.MakeStaticAndClear;
end;

function TfrmRegionProcess.PrepareDeleteProviders(
  const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
  const ALanguageManager: ILanguageManager;
  const AMapSelectFrameBuilder: IMapSelectFrameBuilder;
  const APosition: ILocalCoordConverterChangeable;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  const AMarkSystem: IMarkSystem
): IInterfaceListStatic;
var
  VExportProvider: IRegionProcessProvider;
  VList: IInterfaceListSimple;
begin
  VList := TInterfaceListSimple.Create;

  VExportProvider :=
    TProviderDeleteTiles.Create(
      AProgressFactory,
      ALanguageManager,
      AMapSelectFrameBuilder,
      FTileIteratorFactory
    );

  VList.Add(VExportProvider);
  VExportProvider :=
    TProviderDeleteMarks.Create(
      AProgressFactory,
      ALanguageManager,
      AMapSelectFrameBuilder,
      APosition,
      AVectorGeometryProjectedFactory,
      AMarkSystem
    );
  VList.Add(VExportProvider);

  Result := VList.MakeStaticAndClear;
end;

function TfrmRegionProcess.PrepareExportProviders(
  const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
  const ALanguageManager: ILanguageManager;
  const AMapSelectFrameBuilder: IMapSelectFrameBuilder;
  const AActiveMapsList: IMapTypeListChangeable;
  const AProjectionSetFactory: IProjectionSetFactory;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  const ABitmap32StaticFactory: IBitmap32StaticFactory;
  const ABitmapPostProcessing: IBitmapPostProcessingChangeable;
  const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
  const AImageResamplerFactoryList: IImageResamplerFactoryList;
  const ATileReprojectResamplerConfig: IImageResamplerConfig;
  const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
  const ATileStorageTypeList: ITileStorageTypeListStatic;
  const ATileNameGenerator: ITileFileNameGeneratorsList
): IInterfaceListStatic;
var
  VExportProvider: IRegionProcessProvider;
  VList: IInterfaceListSimple;
begin
  VList := TInterfaceListSimple.Create;

  VExportProvider :=
    TExportProviderIPhone.Create(
      AProgressFactory,
      ALanguageManager,
      AMapSelectFrameBuilder,
      AProjectionSetFactory,
      FTileIteratorFactory,
      ABitmap32StaticFactory,
      ABitmapTileSaveLoadFactory,
      True
    );
  VList.Add(VExportProvider);

  VExportProvider :=
    TExportProviderIPhone.Create(
      AProgressFactory,
      ALanguageManager,
      AMapSelectFrameBuilder,
      AProjectionSetFactory,
      FTileIteratorFactory,
      ABitmap32StaticFactory,
      ABitmapTileSaveLoadFactory,
      False
    );
  VList.Add(VExportProvider);

  VExportProvider :=
    TExportProviderGEKml.Create(
      AProgressFactory,
      ALanguageManager,
      AMapSelectFrameBuilder,
      AVectorGeometryProjectedFactory
    );
  VList.Add(VExportProvider);

  VExportProvider :=
    TExportProviderYaMobileV3.Create(
      AProgressFactory,
      ALanguageManager,
      AMapSelectFrameBuilder,
      AVectorGeometryProjectedFactory,
      ABitmap32StaticFactory,
      ABitmapTileSaveLoadFactory,
      AProjectionSetFactory
    );
  VList.Add(VExportProvider);

  VExportProvider :=
    TExportProviderYaMobileV4.Create(
      AProgressFactory,
      ALanguageManager,
      AMapSelectFrameBuilder,
      AVectorGeometryProjectedFactory,
      ABitmap32StaticFactory,
      ABitmapTileSaveLoadFactory,
      AProjectionSetFactory
    );
  VList.Add(VExportProvider);

  VExportProvider :=
    TExportProviderAUX.Create(
      AProgressFactory,
      ALanguageManager,
      AMapSelectFrameBuilder,
      FTileIteratorFactory
    );
  VList.Add(VExportProvider);

  VExportProvider :=
    TExportProviderZip.Create(
      AProgressFactory,
      ALanguageManager,
      AMapSelectFrameBuilder,
      FTileIteratorFactory,
      AArchiveReadWriteFactory,
      ATileStorageTypeList,
      ATileNameGenerator
    );
  VList.Add(VExportProvider);

  VExportProvider :=
    TExportProviderTar.Create(
      AProgressFactory,
      ALanguageManager,
      AMapSelectFrameBuilder,
      FTileIteratorFactory,
      AArchiveReadWriteFactory,
      ATileStorageTypeList,
      ATileNameGenerator
    );
  VList.Add(VExportProvider);

  VExportProvider :=
    TExportProviderJNX.Create(
      AProgressFactory,
      ALanguageManager,
      AMapSelectFrameBuilder,
      AVectorGeometryProjectedFactory,
      ABitmapTileSaveLoadFactory,
      ABitmapPostProcessing
    );
  VList.Add(VExportProvider);

  VExportProvider :=
    TExportProviderIMG.Create(
      AProgressFactory,
      ALanguageManager,
      AMapSelectFrameBuilder,
      FTileIteratorFactory,
      ABitmapTileSaveLoadFactory,
      ABitmapPostProcessing
    );
  VList.Add(VExportProvider);

  VExportProvider :=
    TExportProviderOgf2.Create(
      AProgressFactory,
      ALanguageManager,
      AMapSelectFrameBuilder,
      AVectorGeometryProjectedFactory,
      ABitmap32StaticFactory,
      ABitmapTileSaveLoadFactory,
      AProjectionSetFactory
    );
  VList.Add(VExportProvider);

  VExportProvider :=
    TExportProviderCE.Create(
      AProgressFactory,
      ALanguageManager,
      AMapSelectFrameBuilder,
      FTileIteratorFactory
    );
  VList.Add(VExportProvider);

  VExportProvider :=
    TExportProviderRMapsSQLite.Create(
      AProgressFactory,
      ALanguageManager,
      AMapSelectFrameBuilder,
      AActiveMapsList,
      AVectorGeometryProjectedFactory,
      ABitmap32StaticFactory,
      ABitmapTileSaveLoadFactory,
      AProjectionSetFactory
    );
  VList.Add(VExportProvider);

  VExportProvider :=
    TExportProviderOruxMapsSQLite.Create(
      AProgressFactory,
      ALanguageManager,
      AMapSelectFrameBuilder,
      AActiveMapsList,
      AVectorGeometryProjectedFactory,
      ABitmap32StaticFactory,
      ABitmapTileSaveLoadFactory,
      AProjectionSetFactory
    );
  VList.Add(VExportProvider);

  VExportProvider :=
    TExportProviderMBTiles.Create(
      AProgressFactory,
      ALanguageManager,
      AMapSelectFrameBuilder,
      AActiveMapsList,
      AVectorGeometryProjectedFactory,
      ABitmap32StaticFactory,
      ABitmapTileSaveLoadFactory,
      AProjectionSetFactory
    );
  VList.Add(VExportProvider);

  VExportProvider :=
    TExportProviderRMP.Create(
      AProgressFactory,
      ALanguageManager,
      AMapSelectFrameBuilder,
      AActiveMapsList,
      AVectorGeometryProjectedFactory,
      ABitmap32StaticFactory,
      ABitmapTileSaveLoadFactory,
      AImageResamplerFactoryList,
      ATileReprojectResamplerConfig,
      AProjectionSetFactory
    );
  VList.Add(VExportProvider);

  Result := VList.MakeStaticAndClear;
end;

procedure TfrmRegionProcess.LoadSelFromFile(
  const AFileName: string;
  out APolygon: IGeometryLonLatPolygon
);
var
  VIniFile: TMemIniFile;
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
      if Assigned(APolygon) then begin
        VZoom := VPolygonSection.ReadInteger('zoom', 1) - 1;
        Self.ProcessPolygonWithZoom(VZoom, APolygon);
      end;
    end;
  end else begin
    ShowMessageFmt(_('Can''t open file: %s'), [AFileName]);
  end;
end;

procedure TfrmRegionProcess.ProcessPolygon(const APolygon: IGeometryLonLatPolygon);
begin
  FZoom_rect := FPosition.GetStatic.Projection.Zoom;
  FPolygonLL := APolygon;
  FLastSelectionInfo.SetPolygon(APolygon, FZoom_rect);
  Self.Show;
  if Self.WindowState = wsMinimized then begin
    Self.WindowState := wsNormal;
  end;
end;

procedure TfrmRegionProcess.ProcessPolygonWithZoom(
  const AZoom: Byte;
  const APolygon: IGeometryLonLatPolygon
);
begin
  FZoom_rect := AZoom;
  FPolygonLL := APolygon;
  FLastSelectionInfo.SetPolygon(APolygon, FZoom_rect);
  Self.Show;
  if Self.WindowState = wsMinimized then begin
    Self.WindowState := wsNormal;
  end;
end;

procedure TfrmRegionProcess.Button1Click(Sender: TObject);
var
  VResult: Boolean;
begin
  VResult := FProviderAll.Validate(FPolygonLL);
  if VResult then begin
    FProviderAll.StartProcess(FPolygonLL);
  end;
  if VResult then begin
    if not tbtmDontClose.Checked then begin
      close;
    end;
  end;
end;

procedure TfrmRegionProcess.FormShow(Sender: TObject);
begin
  FProviderAll.Show(pnlContent, FZoom_rect, FPolygonLL);
end;

procedure TfrmRegionProcess.Button3Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmRegionProcess.tbtmMarkClick(Sender: TObject);
var
  VIniFile: TMemIniFile;
  VZoom: Byte;
  VPolygon: IGeometryLonLatPolygon;
  VHLGData: IConfigDataWriteProvider;
  VPolygonSection: IConfigDataWriteProvider;
begin
  if (SaveSelDialog.Execute) and (SaveSelDialog.FileName <> '') then begin
    If FileExists(SaveSelDialog.FileName) then begin
      DeleteFile(SaveSelDialog.FileName);
    end;
    VZoom := FLastSelectionInfo.Zoom;
    VPolygon := FLastSelectionInfo.Polygon;
    if VPolygon <> nil then begin
      VIniFile := TMemIniFile.Create(SaveSelDialog.FileName);
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
  VPolygon: IGeometryLonLatPolygon;
begin
  VPolygon := FLastSelectionInfo.Polygon;
  if (VPolygon <> nil) then begin
    FMapGoto.FitRectToScreen(VPolygon.Bounds.Rect);
  end;
end;

procedure TfrmRegionProcess.tbtmSaveClick(Sender: TObject);
begin
  if (FLastSelectionInfo.Polygon <> nil) then begin
    FMarkDBGUI.SaveMarkModal(nil, FLastSelectionInfo.Polygon);
  end;
end;

procedure TfrmRegionProcess.StartSlsFromFile(
  const AFileName: string;
  const AStartPaused: Boolean
);
begin
  if FileExists(AFileName) then begin
    FProviderTilesDownload.StartBySLS(AFileName, AStartPaused);
  end else begin
    ShowMessageFmt(_('Can''t open file: %s'), [AFileName]);
  end;
end;

procedure TfrmRegionProcess.tbtmCopyBboxClick(Sender: TObject);
VAR
  VStr: string;
begin
  VStr := '*[bbox=' +
   (RoundEx(FLastSelectionInfo.Polygon.Bounds.Left,6)) + ',' +
   (RoundEx(FLastSelectionInfo.Polygon.Bounds.Bottom,6)) + ',' +
   (RoundEx(FLastSelectionInfo.Polygon.Bounds.Right,6)) + ',' +
   (RoundEx(FLastSelectionInfo.Polygon.Bounds.Top,6)) + ']';
  CopyStringToClipboard(Handle, VStr);
end;

end.
