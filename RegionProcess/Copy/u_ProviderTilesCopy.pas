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
  i_TileFileNameParsersList,
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
    FFileNameParsersList: ITileFileNameParsersList;
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
      const AFileNameParsersList: ITileFileNameParsersList;
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
  i_MapTypes,
  i_MapTypeListStatic,
  i_RegionProcessParamsFrame,
  i_RegionProcessProgressInfo,
  u_TileStorageDBMS,
  u_TileStorageBerkeleyDB,
  u_TileStorageFileSystem,
  u_ThreadCopyFromStorageToStorage,
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
  const AFileNameParsersList: ITileFileNameParsersList;
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
  FFileNameParsersList := AFileNameParsersList;
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
  VTasks: TCopyTaskArray;
  i: Integer;
  VTargetStoragePath: string;
  VMapType: IMapType;
begin
  VZoomArr := (ParamsFrame as IRegionProcessParamsFrameZoomArray).ZoomArray;
  VPath := (ParamsFrame as IRegionProcessParamsFrameTargetPath).Path;
  VMaps := (ParamsFrame as IRegionProcessParamsFrameTilesCopy).MapTypeList;
  VReplace := (ParamsFrame as IRegionProcessParamsFrameTilesCopy).ReplaseTarget;
  VPlaceInSubFolder := (ParamsFrame as IRegionProcessParamsFrameTilesCopy).PlaceInNameSubFolder;
  VDeleteSource := (ParamsFrame as IRegionProcessParamsFrameTilesCopy).DeleteSource;
  VCacheType := (ParamsFrame as IRegionProcessParamsFrameTilesCopy).TargetCacheType;
  if VMaps.Count > 1 then begin
    VPlaceInSubFolder := True;
  end;

  // set version options
  VSetTargetVersionEnabled := (ParamsFrame as IRegionProcessParamsFrameTilesCopy).SetTargetVersionEnabled;
  if VSetTargetVersionEnabled then begin
    VSetTargetVersionValue := (ParamsFrame as IRegionProcessParamsFrameTilesCopy).SetTargetVersionValue
  end else begin
    VSetTargetVersionValue := '';
  end;

  SetLength(VTasks, VMaps.Count);
  for i := 0 to VMaps.Count - 1 do begin
    VMapType := VMaps.Items[i];
    VTasks[i].FSource := VMapType.TileStorage;
    VTasks[i].FSourceVersion := VMapType.VersionRequestConfig.GetStatic;
    VTargetStoragePath := IncludeTrailingPathDelimiter(VPath);
    if VPlaceInSubFolder then begin
      VTargetStoragePath := IncludeTrailingPathDelimiter(VPath + VMapType.GetShortFolderName);
    end;
    if VCacheType = c_File_Cache_Id_DBMS then begin
      VTasks[i].FTarget :=
        TTileStorageDBMS.Create(
          VTasks[i].FSource.CoordConverter,
          '',
          VTargetStoragePath,
          nil,
          nil,
          FContentTypeManager,
          VMapType.VersionRequestConfig.VersionFactory.GetStatic,
          VMapType.ContentType
        );
    end else if VCacheType in [c_File_Cache_Id_BDB, c_File_Cache_Id_BDB_Versioned] then begin
      VTasks[i].FTarget :=
        TTileStorageBerkeleyDB.Create(
          FGlobalBerkeleyDBHelper,
          VTasks[i].FSource.CoordConverter,
          VTargetStoragePath,
          (VCacheType = c_File_Cache_Id_BDB_Versioned),
          FTimerNoifier,
          nil, // MemCache - not needed here
          FContentTypeManager,
          VMapType.VersionRequestConfig.VersionFactory.GetStatic,
          VMapType.ContentType
        );
    end else begin
      VTasks[i].FTarget :=
        TTileStorageFileSystem.Create(
          VTasks[i].FSource.CoordConverter,
          VTargetStoragePath,
          VMapType.ContentType,
          VMapType.VersionRequestConfig.VersionFactory.GetStatic,
          FTileNameGenerator.GetGenerator(VCacheType),
          FFileNameParsersList.GetParser(VCacheType)
        );
    end;
    if VSetTargetVersionEnabled then begin
      VTasks[i].FTargetVersionForce := VMapType.VersionRequestConfig.VersionFactory.GetStatic.CreateByStoreString(VSetTargetVersionValue);
    end else begin
      VTasks[i].FTargetVersionForce := nil;
    end;
  end;

  VProgressInfo := ProgressFactory.Build(APolygon);
  VThread :=
    TThreadCopyFromStorageToStorage.Create(
      VProgressInfo,
      FProjectionFactory,
      FVectorGeometryProjectedFactory,
      APolygon,
      VTasks,
      VZoomArr,
      True,
      VDeleteSource,
      VReplace
    );
  if Assigned(VThread) then begin
    VThread.Resume;
  end;
end;

end.


