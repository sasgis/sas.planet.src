unit u_ProviderTilesCopy;

interface

uses
  Forms,
  i_NotifierTime,
  i_LanguageManager,
  i_VectorItemLonLat,
  i_MapTypes,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  i_CoordConverterFactory,
  i_VectorItemsFactory,
  i_TileFileNameGeneratorsList,
  i_GlobalBerkeleyDBHelper,
  i_RegionProcessProgressInfoInternalFactory,
  u_ExportProviderAbstract,
  fr_TilesCopy;

type
  TProviderTilesCopy = class(TExportProviderAbstract)
  private
    FTimerNoifier: INotifierTime;
    FGlobalBerkeleyDBHelper: IGlobalBerkeleyDBHelper;
    FProjectionFactory: IProjectionInfoFactory;
    FVectorItemsFactory: IVectorItemsFactory;
    FTileNameGenerator: ITileFileNameGeneratorsList;
  protected
    function CreateFrame: TFrame; override;
  public
    constructor Create(
      const ATimerNoifier: INotifierTime;
      const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
      const ALanguageManager: ILanguageManager;
      const AMainMapsConfig: IMainMapsConfig;
      const AGlobalBerkeleyDBHelper: IGlobalBerkeleyDBHelper;
      const AFullMapsSet: IMapTypeSet;
      const AGUIConfigList: IMapTypeGUIConfigList;
      const AProjectionFactory: IProjectionInfoFactory;
      const AVectorItemsFactory: IVectorItemsFactory;
      const ATileNameGenerator: ITileFileNameGeneratorsList
    );
    function GetCaption: string; override;
    procedure StartProcess(const APolygon: ILonLatPolygon); override;
  end;


implementation

uses
  Types,
  SysUtils,
  c_CacheTypeCodes, // for cache types
  i_RegionProcessParamsFrame,
  i_RegionProcessProgressInfo,
  u_ThreadExportToFileSystem,
  u_ThreadExportToBerkeleyDB,
  u_ThreadExportToStorage,
  u_ResStrings;

{ TProviderTilesCopy }

constructor TProviderTilesCopy.Create(
  const ATimerNoifier: INotifierTime;
  const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
  const ALanguageManager: ILanguageManager;
  const AMainMapsConfig: IMainMapsConfig;
  const AGlobalBerkeleyDBHelper: IGlobalBerkeleyDBHelper;
  const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList;
  const AProjectionFactory: IProjectionInfoFactory;
  const AVectorItemsFactory: IVectorItemsFactory;
  const ATileNameGenerator: ITileFileNameGeneratorsList
);
begin
  inherited Create(
    AProgressFactory,
    ALanguageManager,
    AMainMapsConfig,
    AFullMapsSet,
    AGUIConfigList
  );
  FTimerNoifier := ATimerNoifier;
  FGlobalBerkeleyDBHelper := AGlobalBerkeleyDBHelper;
  FProjectionFactory := AProjectionFactory;
  FVectorItemsFactory := AVectorItemsFactory;
  FTileNameGenerator := ATileNameGenerator;
end;

function TProviderTilesCopy.CreateFrame: TFrame;
begin
  Result :=
    TfrTilesCopy.Create(
      Self.LanguageManager,
      Self.MainMapsConfig,
      Self.FullMapsSet,
      Self.GUIConfigList
    );
  Assert(Supports(Result, IRegionProcessParamsFrameZoomArray));
  Assert(Supports(Result, IRegionProcessParamsFrameTargetPath));
  Assert(Supports(Result, IRegionProcessParamsFrameTilesCopy));
end;

function TProviderTilesCopy.GetCaption: string;
begin
  Result := SAS_STR_OperationTilesCopyCaption;
end;

procedure TProviderTilesCopy.StartProcess(const APolygon: ILonLatPolygon);
var
  VPath: string;
  VZoomArr: TByteDynArray;
  VReplace: Boolean;
  VDeleteSource: Boolean;
  VProgressInfo: IRegionProcessProgressInfoInternal;
  VCacheType: Byte;
  VSetTargetVersionEnabled: Boolean;
  VSetTargetVersionValue: String;
  VMaps: IMapTypeListStatic;
begin
  VZoomArr := (ParamsFrame as IRegionProcessParamsFrameZoomArray).ZoomArray;
  VPath := (ParamsFrame as IRegionProcessParamsFrameTargetPath).Path;
  VMaps := (ParamsFrame as IRegionProcessParamsFrameTilesCopy).MapTypeList;
  VReplace := (ParamsFrame as IRegionProcessParamsFrameTilesCopy).ReplaseTarget;
  VDeleteSource := (ParamsFrame as IRegionProcessParamsFrameTilesCopy).DeleteSource;
  VCacheType := (ParamsFrame as IRegionProcessParamsFrameTilesCopy).TargetCacheType;

  VProgressInfo := ProgressFactory.Build(APolygon);

  if VCacheType = c_File_Cache_Id_DBMS then begin
    // set version options
    VSetTargetVersionEnabled := (ParamsFrame as IRegionProcessParamsFrameTilesCopy).SetTargetVersionEnabled;
    if VSetTargetVersionEnabled then
      VSetTargetVersionValue := (ParamsFrame as IRegionProcessParamsFrameTilesCopy).SetTargetVersionValue
    else
      VSetTargetVersionValue := '';

    TThreadExportToDBMS.Create(
      VProgressInfo,
      '', // allow empty value here (if path completely defined)
      VPath,
      FProjectionFactory,
      FVectorItemsFactory,
      APolygon,
      VZoomArr,
      VMaps,
      VSetTargetVersionEnabled,
      VSetTargetVersionValue,
      VDeleteSource,
      VReplace
    );
  end else if VCacheType = c_File_Cache_Id_BDB then begin
    TThreadExportToBerkeleyDB.Create(
      FTimerNoifier,
      FGlobalBerkeleyDBHelper,
      VProgressInfo,
      VPath,
      FProjectionFactory,
      FVectorItemsFactory,
      APolygon,
      VZoomArr,
      VMaps,
      VDeleteSource,
      VReplace
    );
  end else begin
    TThreadExportToFileSystem.Create(
      VProgressInfo,
      VPath,
      FProjectionFactory,
      FVectorItemsFactory,
      APolygon,
      VZoomArr,
      VMaps,
      VDeleteSource,
      VReplace,
      FTileNameGenerator.GetGenerator(VCacheType)
    );
  end;
end;

end.


