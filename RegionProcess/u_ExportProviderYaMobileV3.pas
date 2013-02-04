unit u_ExportProviderYaMobileV3;

interface

uses
  Forms,
  i_NotifierTime,
  i_NotifierOperation,
  i_LanguageManager,
  i_MapTypes,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  i_CoordConverterFactory,
  i_MapViewGoto,
  i_LocalCoordConverterFactorySimpe,
  i_Bitmap32StaticFactory,
  i_VectorItemsFactory,
  i_VectorItemLonLat,
  i_RegionProcessProgressInfoInternalFactory,
  i_BitmapTileSaveLoadFactory,
  u_ExportProviderAbstract,
  fr_ExportYaMobileV3;

type
  TExportProviderYaMobileV3 = class(TExportProviderAbstract)
  private
    FFrame: TfrExportYaMobileV3;
    FCoordConverterFactory: ICoordConverterFactory;
    FLocalConverterFactory: ILocalCoordConverterFactorySimpe;
    FProjectionFactory: IProjectionInfoFactory;
    FVectorItemsFactory: IVectorItemsFactory;
    FBitmapFactory: IBitmap32StaticFactory;
    FBitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
    FAppClosingNotifier: INotifierOneOperation;
    FTimerNoifier: INotifierTime;
  protected
    function CreateFrame: TFrame; override;
  public
    constructor Create(
      const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
      const ALanguageManager: ILanguageManager;
      const AAppClosingNotifier: INotifierOneOperation;
      const ATimerNoifier: INotifierTime;
      const AMainMapsConfig: IMainMapsConfig;
      const AFullMapsSet: IMapTypeSet;
      const AGUIConfigList: IMapTypeGUIConfigList;
      const AProjectionFactory: IProjectionInfoFactory;
      const AVectorItemsFactory: IVectorItemsFactory;
      const ABitmapFactory: IBitmap32StaticFactory;
      const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
      const ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
      const ACoordConverterFactory: ICoordConverterFactory
    );
    function GetCaption: string; override;
//    procedure StartProcess(const APolygon: ILonLatPolygon); override;
    procedure StartProcess(
      const APolygon: ILonLatPolygon;
      const AMapGoto: IMapViewGoto
    ); override;

  end;


implementation

uses
  Types,
  SysUtils,
  i_RegionProcessParamsFrame,
  i_RegionProcessProgressInfo,
  u_ThreadExportYaMobileV3,
  u_ResStrings,
  u_MapType;

{ TExportProviderYaMobileV3 }

constructor TExportProviderYaMobileV3.Create(
  const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
  const ALanguageManager: ILanguageManager;
  const AAppClosingNotifier: INotifierOneOperation;
  const ATimerNoifier: INotifierTime;
  const AMainMapsConfig: IMainMapsConfig;
  const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList;
  const AProjectionFactory: IProjectionInfoFactory;
  const AVectorItemsFactory: IVectorItemsFactory;
  const ABitmapFactory: IBitmap32StaticFactory;
  const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
  const ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
  const ACoordConverterFactory: ICoordConverterFactory
);
begin
  inherited Create(
    AProgressFactory,
    ALanguageManager,
    AMainMapsConfig,
    AFullMapsSet,
    AGUIConfigList
  );
  FProjectionFactory := AProjectionFactory;
  FVectorItemsFactory := AVectorItemsFactory;
  FBitmapFactory := ABitmapFactory;
  FBitmapTileSaveLoadFactory := ABitmapTileSaveLoadFactory;
  FCoordConverterFactory := ACoordConverterFactory;
  FLocalConverterFactory := ALocalConverterFactory;
  FAppClosingNotifier := AAppClosingNotifier;
  FTimerNoifier := ATimerNoifier;
end;

function TExportProviderYaMobileV3.CreateFrame: TFrame;
begin
  FFrame :=
    TfrExportYaMobileV3.Create(
      Self.LanguageManager,
      Self.MainMapsConfig,
      Self.FullMapsSet,
      Self.GUIConfigList
    );
  Result := FFrame;
  Assert(Supports(Result, IRegionProcessParamsFrameZoomArray));
  Assert(Supports(Result, IRegionProcessParamsFrameTargetPath));
end;

function TExportProviderYaMobileV3.GetCaption: string;
begin
  Result := SAS_STR_ExportYaMobileV3Caption;
end;

procedure TExportProviderYaMobileV3.StartProcess(const APolygon: ILonLatPolygon; const AMapGoto: IMapViewGoto );

var
  VPath: string;
  VZoomArr: TByteDynArray;
  typemaparr: array of TMapType;
  comprSat, comprMap: byte;
  Replace: boolean;
  VProgressInfo: IRegionProcessProgressInfoInternal;
begin
  inherited;
  VZoomArr := (ParamsFrame as IRegionProcessParamsFrameZoomArray).ZoomArray;
  VPath := (ParamsFrame as IRegionProcessParamsFrameTargetPath).Path;

  setlength(typemaparr, 3);
  typemaparr[0] := TMapType(FFrame.cbbSat.Items.Objects[FFrame.cbbSat.ItemIndex]);
  typemaparr[1] := TMapType(FFrame.cbbMap.Items.Objects[FFrame.cbbMap.ItemIndex]);
  typemaparr[2] := TMapType(FFrame.cbbHybr.Items.Objects[FFrame.cbbHybr.ItemIndex]);
  comprSat := FFrame.seSatCompress.Value;
  comprMap := FFrame.seMapCompress.Value;
  Replace := FFrame.chkReplaseTiles.Checked;

  VProgressInfo := ProgressFactory.Build(APolygon);

  TThreadExportYaMobileV3.Create(
    VProgressInfo,
    FCoordConverterFactory,
    FLocalConverterFactory,
    FProjectionFactory,
    FVectorItemsFactory,
    FBitmapFactory,
    FBitmapTileSaveLoadFactory,
    VPath,
    APolygon,
    VZoomArr,
    typemaparr,
    Replace,
    comprSat,
    comprMap
  );
end;

end.


