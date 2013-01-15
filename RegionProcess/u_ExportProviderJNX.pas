unit u_ExportProviderJNX;

interface

uses
  Forms,
  i_NotifierTime,
  i_NotifierOperation,
  i_VectorItemLonLat,
  i_CoordConverterFactory,
  i_VectorItemsFactory,
  i_StringListStatic,
  i_LanguageManager,
  i_MapTypes,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  i_BitmapTileSaveLoadFactory,
  u_ExportProviderAbstract,
  fr_ExportToJNX;

type
  TExportProviderJNX = class(TExportProviderAbstract)
  private
    FCoordConverterFactory: ICoordConverterFactory;
    FProjectionFactory: IProjectionInfoFactory;
    FVectorItemsFactory: IVectorItemsFactory;
    FBitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
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
      const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
      const ACoordConverterFactory: ICoordConverterFactory
    );
    function GetCaption: string; override;
    procedure StartProcess(const APolygon: ILonLatPolygon); override;
  end;

implementation

uses
  Types,
  SysUtils,
  i_RegionProcessParamsFrame,
  u_Notifier,
  u_NotifierOperation,
  u_RegionProcessProgressInfo,
  u_ThreadExportToJNX,
  u_ResStrings,
  frm_ProgressSimple;


{ TExportProviderJNX }

constructor TExportProviderJNX.Create(
  const ALanguageManager: ILanguageManager;
  const AAppClosingNotifier: INotifierOneOperation;
  const ATimerNoifier: INotifierTime;
  const AMainMapsConfig: IMainMapsConfig;
  const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList;
  const AProjectionFactory: IProjectionInfoFactory;
  const AVectorItemsFactory: IVectorItemsFactory;
  const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
  const ACoordConverterFactory: ICoordConverterFactory
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
  FBitmapTileSaveLoadFactory := ABitmapTileSaveLoadFactory;
  FCoordConverterFactory := ACoordConverterFactory;
  FAppClosingNotifier := AAppClosingNotifier;
  FTimerNoifier := ATimerNoifier;
end;

function TExportProviderJNX.CreateFrame: TFrame;
begin
  Result :=
    TfrExportToJNX.Create(
      Self.LanguageManager,
      Self.MainMapsConfig,
      Self.FullMapsSet,
      Self.GUIConfigList,
      'JNX |*.jnx',
      'jnx'
    );
  Assert(Supports(Result, IRegionProcessParamsFrameZoomArray));
  Assert(Supports(Result, IRegionProcessParamsFrameTargetPath));
  Assert(Supports(Result, IRegionProcessParamsFrameExportToJNX));
end;

function TExportProviderJNX.GetCaption: string;
begin
  Result := SAS_STR_ExportJNXPackCaption;
end;

procedure TExportProviderJNX.StartProcess(const APolygon: ILonLatPolygon);
var
  VPath: string;
  Zoomarr: TByteDynArray;
  VProductName: string;
  VMapName: string;
  VJNXVersion: integer;
  VZorder: integer;
  VProductID: integer;
  VJpgQuality: IStringListStatic;
  VCancelNotifierInternal: INotifierOperationInternal;
  VOperationID: Integer;
  VProgressInfo: TRegionProcessProgressInfo;
  VLevelsDesc: IStringListStatic;
  VMapList: IMapTypeListStatic;
  VLayerList: IMapTypeListStatic;
  VScaleArr: TByteDynArray;
begin
  inherited;

  Zoomarr := (ParamsFrame as IRegionProcessParamsFrameZoomArray).ZoomArray;
  VPath := (ParamsFrame as IRegionProcessParamsFrameTargetPath).Path;

  VLevelsDesc := (ParamsFrame as IRegionProcessParamsFrameExportToJNX).LevelsDesc;
  VProductName := (ParamsFrame as IRegionProcessParamsFrameExportToJNX).ProductName;
  VMapName := (ParamsFrame as IRegionProcessParamsFrameExportToJNX).MapName;
  VJpgQuality := (ParamsFrame as IRegionProcessParamsFrameExportToJNX).JpgQuality;
  VJNXVersion := (ParamsFrame as IRegionProcessParamsFrameExportToJNX).JNXVersion;
  VZorder := (ParamsFrame as IRegionProcessParamsFrameExportToJNX).ZOrder;
  VProductID := (ParamsFrame as IRegionProcessParamsFrameExportToJNX).ProductID;
  VScaleArr := (ParamsFrame as IRegionProcessParamsFrameExportToJNX).ScaleArray;
  VMapList := (ParamsFrame as IRegionProcessParamsFrameExportToJNX).MapList;

  VCancelNotifierInternal := TNotifierOperation.Create(TNotifierBase.Create);
  VOperationID := VCancelNotifierInternal.CurrentOperation;
  VProgressInfo := TRegionProcessProgressInfo.Create;

  TfrmProgressSimple.Create(
    Application,
    FAppClosingNotifier,
    FTimerNoifier,
    VCancelNotifierInternal,
    VProgressInfo
  );

  TThreadExportToJnx.Create(
    VCancelNotifierInternal,
    VOperationID,
    VProgressInfo,
    FCoordConverterFactory,
    FProjectionFactory,
    FVectorItemsFactory,
    FBitmapTileSaveLoadFactory,
    VPath,
    APolygon,
    Zoomarr,
    VProductName,
    VMapName,
    VJNXVersion,
    VZorder,
    VProductID,
    VJpgQuality,
    VLevelsDesc,
    VMapList,
    VLayerList,
    VScaleArr
  );
end;

end.


