unit u_ExportProviderTar;

interface

uses
  Forms,
  i_Notify,
  i_LanguageManager,
  i_VectorItemLonLat,
  i_MapTypes,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  i_CoordConverterFactory,
  i_VectorItmesFactory,
  i_TileFileNameGeneratorsList,
  u_ExportProviderAbstract,
  fr_ExportToFileCont;

type
  TExportProviderTar = class(TExportProviderAbstract)
  private
    FProjectionFactory: IProjectionInfoFactory;
    FVectorItmesFactory: IVectorItmesFactory;
    FTileNameGenerator: ITileFileNameGeneratorsList;
    FAppClosingNotifier: INotifier;
    FTimerNoifier: INotifier;
  protected
    function CreateFrame: TFrame; override;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AAppClosingNotifier: INotifier;
      const ATimerNoifier: INotifier;
      const AMainMapsConfig: IMainMapsConfig;
      const AFullMapsSet: IMapTypeSet;
      const AGUIConfigList: IMapTypeGUIConfigList;
      const AProjectionFactory: IProjectionInfoFactory;
      const AVectorItmesFactory: IVectorItmesFactory;
      const ATileNameGenerator: ITileFileNameGeneratorsList
    );
    function GetCaption: string; override;
    procedure StartProcess(const APolygon: ILonLatPolygon); override;
  end;

implementation

uses
  Types,
  SysUtils,
  u_OperationNotifier,
  u_RegionProcessProgressInfo,
  i_RegionProcessParamsFrame,
  i_TileFileNameGenerator,
  u_ThreadExportToTar,
  u_ResStrings,
  u_MapType,
  frm_ProgressSimple;

{ TExportProviderTar }

constructor TExportProviderTar.Create(
  const ALanguageManager: ILanguageManager;
  const AAppClosingNotifier: INotifier;
  const ATimerNoifier: INotifier;
  const AMainMapsConfig: IMainMapsConfig;
  const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList;
  const AProjectionFactory: IProjectionInfoFactory;
  const AVectorItmesFactory: IVectorItmesFactory;
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
  FVectorItmesFactory := AVectorItmesFactory;
  FTileNameGenerator := ATileNameGenerator;
  FAppClosingNotifier := AAppClosingNotifier;
  FTimerNoifier := ATimerNoifier;
end;

function TExportProviderTar.CreateFrame: TFrame;
begin
  Result :=
    TfrExportToFileCont.Create(
      Self.LanguageManager,
      Self.MainMapsConfig,
      Self.FullMapsSet,
      Self.GUIConfigList,
      FTileNameGenerator,
      'Tar |*.tar',
      'tar'
    );
  Assert(Supports(Result, IRegionProcessParamsFrameZoomArray));
  Assert(Supports(Result, IRegionProcessParamsFrameOneMap));
  Assert(Supports(Result, IRegionProcessParamsFrameTargetPath));
  Assert(Supports(Result, IRegionProcessParamsFrameExportToFileCont));
end;

function TExportProviderTar.GetCaption: string;
begin
  Result := SAS_STR_ExportTarPackCaption;
end;

procedure TExportProviderTar.StartProcess(const APolygon: ILonLatPolygon);
var
  path: string;
  Zoomarr: TByteDynArray;
  VMapType: TMapType;
  VNameGenerator: ITileFileNameGenerator;
  VCancelNotifierInternal: INotifierOperationInternal;
  VOperationID: Integer;
  VProgressInfo: TRegionProcessProgressInfo;
begin
  inherited;
  Zoomarr := (ParamsFrame as IRegionProcessParamsFrameZoomArray).ZoomArray;
  path := (ParamsFrame as IRegionProcessParamsFrameTargetPath).Path;
  VMapType := (ParamsFrame as IRegionProcessParamsFrameOneMap).MapType;
  VNameGenerator := (ParamsFrame as IRegionProcessParamsFrameExportToFileCont).NameGenerator;

  VCancelNotifierInternal := TNotifierOperation.Create;
  VOperationID := VCancelNotifierInternal.CurrentOperation;
  VProgressInfo := TRegionProcessProgressInfo.Create;

  TfrmProgressSimple.Create(
    Application,
    FAppClosingNotifier,
    FTimerNoifier,
    VCancelNotifierInternal,
    VProgressInfo
  );

  TThreadExportToTar.Create(
    VCancelNotifierInternal,
    VOperationID,
    VProgressInfo,
    path,
    FProjectionFactory,
    FVectorItmesFactory,
    APolygon,
    Zoomarr,
    VMapType,
    VNameGenerator
  );
end;

end.

