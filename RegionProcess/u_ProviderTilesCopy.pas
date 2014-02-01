unit u_ProviderTilesCopy;

interface

uses
  Forms,
  i_NotifierTime,
  i_LanguageManager,
  i_GeometryLonLat,
  i_MapTypeSet,
  i_MapTypeListBuilder,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  i_ContentTypeManager,
  i_CoordConverterFactory,
  i_GeometryProjectedFactory,
  i_TileFileNameGeneratorsList,
  i_GlobalBerkeleyDBHelper,
  i_RegionProcessProgressInfoInternalFactory,
  u_ExportProviderAbstract,
  fr_TilesCopy;

type
  TProviderTilesCopy = class(TExportProviderAbstract)
  private
    FMapTypeListBuilderFactory: IMapTypeListBuilderFactory;
    FTimerNoifier: INotifierTime;
    FGlobalBerkeleyDBHelper: IGlobalBerkeleyDBHelper;
    FProjectionFactory: IProjectionInfoFactory;
    FVectorGeometryProjectedFactory: IGeometryProjectedFactory;
    FTileNameGenerator: ITileFileNameGeneratorsList;
    FContentTypeManager: IContentTypeManager;
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
      const AMapTypeListBuilderFactory: IMapTypeListBuilderFactory;
      const AContentTypeManager: IContentTypeManager;
      const AProjectionFactory: IProjectionInfoFactory;
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
      const ATileNameGenerator: ITileFileNameGeneratorsList
    );
    function GetCaption: string; override;
    procedure StartProcess(const APolygon: IGeometryLonLatMultiPolygon); override;
  end;


implementation

uses
  Types,
  Classes,
  SysUtils,
  c_CacheTypeCodes, // for cache types
  i_MapTypeListStatic,
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
  const AMapTypeListBuilderFactory: IMapTypeListBuilderFactory;
  const AContentTypeManager: IContentTypeManager;
  const AProjectionFactory: IProjectionInfoFactory;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
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
  FMapTypeListBuilderFactory := AMapTypeListBuilderFactory;
  FTimerNoifier := ATimerNoifier;
  FGlobalBerkeleyDBHelper := AGlobalBerkeleyDBHelper;
  FContentTypeManager := AContentTypeManager;
  FProjectionFactory := AProjectionFactory;
  FVectorGeometryProjectedFactory := AVectorGeometryProjectedFactory;
  FTileNameGenerator := ATileNameGenerator;
end;

function TProviderTilesCopy.CreateFrame: TFrame;
begin
  Result :=
    TfrTilesCopy.Create(
      Self.LanguageManager,
      FMapTypeListBuilderFactory,
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

procedure TProviderTilesCopy.StartProcess(const APolygon: IGeometryLonLatMultiPolygon);
var
  VPath: string;
  VZoomArr: TByteDynArray;
  VReplace: Boolean;
  VDeleteSource: Boolean;
  VProgressInfo: IRegionProcessProgressInfoInternal;
  VCacheType: Byte;
  VPlaceInSubFolder: Boolean;
  VSetTargetVersionEnabled: Boolean;
  VSetTargetVersionValue: String;
  VMaps: IMapTypeListStatic;
  VThread: TThread;
begin
  VZoomArr := (ParamsFrame as IRegionProcessParamsFrameZoomArray).ZoomArray;
  VPath := (ParamsFrame as IRegionProcessParamsFrameTargetPath).Path;
  VMaps := (ParamsFrame as IRegionProcessParamsFrameTilesCopy).MapTypeList;
  VReplace := (ParamsFrame as IRegionProcessParamsFrameTilesCopy).ReplaseTarget;
  VPlaceInSubFolder := (ParamsFrame as IRegionProcessParamsFrameTilesCopy).PlaceInNameSubFolder;
  VDeleteSource := (ParamsFrame as IRegionProcessParamsFrameTilesCopy).DeleteSource;
  VCacheType := (ParamsFrame as IRegionProcessParamsFrameTilesCopy).TargetCacheType;

  VProgressInfo := ProgressFactory.Build(APolygon);

  // set version options
  VSetTargetVersionEnabled := (ParamsFrame as IRegionProcessParamsFrameTilesCopy).SetTargetVersionEnabled;
  if VSetTargetVersionEnabled then begin
    VSetTargetVersionValue := (ParamsFrame as IRegionProcessParamsFrameTilesCopy).SetTargetVersionValue
  end else begin
    VSetTargetVersionValue := '';
  end;

  if VCacheType = c_File_Cache_Id_DBMS then begin
    VThread :=
      TThreadExportToDBMS.Create(
        VProgressInfo,
        '', // allow empty value here (if path completely defined)
        VPath,
        FContentTypeManager,
        FProjectionFactory,
        FVectorGeometryProjectedFactory,
        APolygon,
        VZoomArr,
        VMaps,
        VSetTargetVersionEnabled,
        VSetTargetVersionValue,
        VDeleteSource,
        VReplace
      );
  end else if VCacheType in [c_File_Cache_Id_BDB, c_File_Cache_Id_BDB_Versioned] then begin
    VThread :=
      TThreadExportToBerkeleyDB.Create(
        FTimerNoifier,
        FGlobalBerkeleyDBHelper,
        VProgressInfo,
        VPath,
        (VPlaceInSubFolder or (VMaps.Count > 1)),
        (VCacheType = c_File_Cache_Id_BDB_Versioned),
        FContentTypeManager,
        FProjectionFactory,
        FVectorGeometryProjectedFactory,
        APolygon,
        VZoomArr,
        VMaps,
        VSetTargetVersionEnabled,
        VSetTargetVersionValue,
        VDeleteSource,
        VReplace
      );
  end else begin
    VThread :=
      TThreadExportToFileSystem.Create(
        VProgressInfo,
        VPath,
        (VPlaceInSubFolder or (VMaps.Count > 1)),
        FProjectionFactory,
        FVectorGeometryProjectedFactory,
        APolygon,
        VZoomArr,
        VMaps,
        VDeleteSource,
        VReplace,
        FTileNameGenerator.GetGenerator(VCacheType)
      );
  end;
  if Assigned(VThread) then begin
    VThread.Resume;
  end;
end;

end.


