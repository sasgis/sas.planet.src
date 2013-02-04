unit u_ExportProviderZip;

interface

uses
  Forms,
  i_NotifierTime,
  i_NotifierOperation,
  i_LanguageManager,
  i_VectorItemLonLat,
  i_MapTypes,
  i_MapViewGoto,
  i_ActiveMapsConfig,
  i_CoordConverterFactory,
  i_VectorItemsFactory,
  i_ArchiveReadWriteFactory,
  i_MapTypeGUIConfigList,
  i_TileFileNameGeneratorsList,
  u_ExportProviderAbstract,
  fr_ExportToFileCont;

type
  TExportProviderZip = class(TExportProviderAbstract)
  private
    FProjectionFactory: IProjectionInfoFactory;
    FVectorItemsFactory: IVectorItemsFactory;
    FArchiveReadWriteFactory: IArchiveReadWriteFactory;
    FTileNameGenerator: ITileFileNameGeneratorsList;
    FAppClosingNotifier: INotifierOneOperation;
    FTimerNoifier: INotifierTime;
  protected
    function CreateFrame: TFrame; override;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AAppClosingNotifier: INotifierOneOperation;
      const ATimerNoifier: INotifierTime;
      const AMainMapsConfig: IMainMapsConfig;
      const AFullMapsSet: IMapTypeSet;
      const AGUIConfigList: IMapTypeGUIConfigList;
      const AProjectionFactory: IProjectionInfoFactory;
      const AVectorItemsFactory: IVectorItemsFactory;
      const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
      const ATileNameGenerator: ITileFileNameGeneratorsList
    );
    function GetCaption: string; override;
    procedure StartProcess(const APolygon: ILonLatPolygon; const AMapGoto: IMapViewGoto ); override;
  end;


implementation

uses
  Types,
  SysUtils,
  i_RegionProcessParamsFrame,
  u_Notifier,
  u_NotifierOperation,
  u_RegionProcessProgressInfo,
  i_TileFileNameGenerator,
  u_ThreadExportToArchive,
  u_ResStrings,
  u_MapType,
  frm_ProgressSimple;

{ TExportProviderKml }

constructor TExportProviderZip.Create(
  const ALanguageManager: ILanguageManager;
  const AAppClosingNotifier: INotifierOneOperation;
  const ATimerNoifier: INotifierTime;
  const AMainMapsConfig: IMainMapsConfig;
  const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList;
  const AProjectionFactory: IProjectionInfoFactory;
  const AVectorItemsFactory: IVectorItemsFactory;
  const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
  const ATileNameGenerator: ITileFileNameGeneratorsList
);
begin
  inherited Create(
    ALanguageManager,
    AMainMapsConfig,
    AFullMapsSet,
    AGUIConfigList
  );
  FProjectionFactory := AProjectionFactory;
  FVectorItemsFactory := AVectorItemsFactory;
  FArchiveReadWriteFactory := AArchiveReadWriteFactory;
  FTileNameGenerator := ATileNameGenerator;
  FAppClosingNotifier := AAppClosingNotifier;
  FTimerNoifier := ATimerNoifier;
end;

function TExportProviderZip.CreateFrame: TFrame;
begin
  Result :=
    TfrExportToFileCont.Create(
      Self.LanguageManager,
      Self.MainMapsConfig,
      Self.FullMapsSet,
      Self.GUIConfigList,
      FTileNameGenerator,
      'Zip |*.zip',
      'zip'
    );
  Assert(Supports(Result, IRegionProcessParamsFrameZoomArray));
  Assert(Supports(Result, IRegionProcessParamsFrameOneMap));
  Assert(Supports(Result, IRegionProcessParamsFrameTargetPath));
  Assert(Supports(Result, IRegionProcessParamsFrameExportToFileCont));
end;

function TExportProviderZip.GetCaption: string;
begin
  Result := SAS_STR_ExportZipPackCaption;
end;

procedure TExportProviderZip.StartProcess(const APolygon: ILonLatPolygon; const AMapGoto: IMapViewGoto );
var
  VPath: string;
  Zoomarr: TByteDynArray;
  VMapType: TMapType;
  VNameGenerator: ITileFileNameGenerator;
  VCancelNotifierInternal: INotifierOperationInternal;
  VOperationID: Integer;
  VProgressInfo: TRegionProcessProgressInfo;
begin
  inherited;
  Zoomarr := (ParamsFrame as IRegionProcessParamsFrameZoomArray).ZoomArray;
  VPath := (ParamsFrame as IRegionProcessParamsFrameTargetPath).Path;
  VMapType := (ParamsFrame as IRegionProcessParamsFrameOneMap).MapType;
  VNameGenerator := (ParamsFrame as IRegionProcessParamsFrameExportToFileCont).NameGenerator;

  VCancelNotifierInternal := TNotifierOperation.Create(TNotifierBase.Create);
  VOperationID := VCancelNotifierInternal.CurrentOperation;
  VProgressInfo := TRegionProcessProgressInfo.Create(VCancelNotifierInternal, VOperationID);

  TfrmProgressSimple.Create(
    Application,
    FAppClosingNotifier,
    FTimerNoifier,
    VCancelNotifierInternal,
    VProgressInfo,
    AMapGoto,
    APolygon
  );

  TThreadExportToArchive.Create(
    VProgressInfo,
    FArchiveReadWriteFactory.CreateZipWriterByName(VPath),
    FProjectionFactory,
    FVectorItemsFactory,
    APolygon,
    Zoomarr,
    VMapType,
    VNameGenerator
  );
end;

end.


