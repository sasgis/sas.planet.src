unit u_ExportProviderOgf2;

interface

uses
  Controls,
  i_JclNotify,
  i_VectorItemLonLat,
  i_CoordConverterFactory,
  i_LocalCoordConverterFactorySimpe,
  i_VectorItmesFactory,
  i_LanguageManager,
  i_MapTypes,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  u_ExportProviderAbstract,
  fr_ExportToOgf2;

type
  TExportProviderOgf2 = class(TExportProviderAbstract)
  private
    FFrame: TfrExportToOgf2;
    FCoordConverterFactory: ICoordConverterFactory;
    FLocalConverterFactory: ILocalCoordConverterFactorySimpe;
    FProjectionFactory: IProjectionInfoFactory;
    FVectorItmesFactory: IVectorItmesFactory;
    FAppClosingNotifier: IJclNotifier;
    FTimerNoifier: IJclNotifier;
  public
    constructor Create(
      AParent: TWinControl;
      const ALanguageManager: ILanguageManager;
      const AAppClosingNotifier: IJclNotifier;
      const ATimerNoifier: IJclNotifier;
      const AMainMapsConfig: IMainMapsConfig;
      const AFullMapsSet: IMapTypeSet;
      const AGUIConfigList: IMapTypeGUIConfigList;
      const AProjectionFactory: IProjectionInfoFactory;
      const AVectorItmesFactory: IVectorItmesFactory;
      const ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
      const ACoordConverterFactory: ICoordConverterFactory
    );
    function GetCaption: string; override;
    procedure InitFrame(
      AZoom: Byte;
      const APolygon: ILonLatPolygon
    ); override;
    procedure StartProcess(const APolygon: ILonLatPolygon); override;
  end;

implementation

uses
  Forms,
  SysUtils,
  Classes,
  i_RegionProcessProgressInfo,
  u_OperationNotifier,
  u_RegionProcessProgressInfo,
  u_ThreadExportToOgf2,
  u_ResStrings,
  u_MapType,
  frm_ProgressSimple;

{ TExportProviderOgf2 }

constructor TExportProviderOgf2.Create(
  AParent: TWinControl;
  const ALanguageManager: ILanguageManager;
  const AAppClosingNotifier: IJclNotifier;
  const ATimerNoifier: IJclNotifier;
  const AMainMapsConfig: IMainMapsConfig;
  const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList;
  const AProjectionFactory: IProjectionInfoFactory;
  const AVectorItmesFactory: IVectorItmesFactory;
  const ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
  const ACoordConverterFactory: ICoordConverterFactory
);
begin
  inherited Create(AParent, ALanguageManager, AMainMapsConfig, AFullMapsSet, AGUIConfigList);
  FProjectionFactory := AProjectionFactory;
  FVectorItmesFactory := AVectorItmesFactory;
  FCoordConverterFactory := ACoordConverterFactory;
  FLocalConverterFactory := ALocalConverterFactory;
  FAppClosingNotifier := AAppClosingNotifier;
  FTimerNoifier := ATimerNoifier;
end;

function TExportProviderOgf2.GetCaption: string;
begin
  Result := SAS_STR_ExportOgf2PackCaption;
end;

procedure TExportProviderOgf2.InitFrame(
  AZoom: Byte;
  const APolygon: ILonLatPolygon
);
begin
  if FFrame = nil then begin
    FFrame := TfrExportToOgf2.CreateForFileType(
      nil,
      FProjectionFactory,
      FVectorItmesFactory,
      Self.MainMapsConfig,
      Self.FullMapsSet,
      Self.GUIConfigList,
      'OGF2 (*.ogf2) |*.ogf2',
      'ogf2'
    );
    SetFrame(FFrame);
  end;
  FFrame.Init(AZoom, APolygon);
end;

procedure TExportProviderOgf2.StartProcess(const APolygon: ILonLatPolygon);
var
  I: Integer;
  VTargetFile: string;
  VMapType: TMapType;
  VOverlay: TMapType;
  VUsePrevZoom: Boolean;
  VOgf2TileResolution: TOgf2TileResolution;
  VOgf2TileFormat: TOgf2TileFormat;
  VJpegQuality: Byte;
  VZoomArr: array [0..23] of Boolean;
  VCancelNotifierInternal: IOperationNotifierInternal;
  VOperationID: Integer;
  VProgressInfo: IRegionProcessProgressInfo;
begin
  inherited;

  VTargetFile := FFrame.edtTargetFile.Text;

  VMapType := TMapType(FFrame.cbbMap.Items.Objects[FFrame.cbbMap.ItemIndex]);
  VOverlay := TMapType(FFrame.cbbHyb.Items.Objects[FFrame.cbbHyb.ItemIndex]);

  for I := 0 to 23 do begin
    VZoomArr[I] := False;
  end;
  VZoomArr[FFrame.cbbZoom.ItemIndex] := True;

  VUsePrevZoom := FFrame.chkUsePrevZoom.Checked;

  VOgf2TileResolution := TOgf2TileResolution(FFrame.cbbTileRes.ItemIndex);
  VOgf2TileFormat := TOgf2TileFormat(FFrame.cbbImageFormat);
  VJpegQuality := FFrame.seJpgQuality.Value;

  VCancelNotifierInternal := TOperationNotifier.Create;
  VOperationID := VCancelNotifierInternal.CurrentOperation;
  VProgressInfo := TRegionProcessProgressInfo.Create;

  TfrmProgressSimple.Create(
    Application,
    FAppClosingNotifier,
    FTimerNoifier,
    VCancelNotifierInternal,
    VProgressInfo
  );

  TThreadExportToOgf2.Create(
    VCancelNotifierInternal,
    VOperationID,
    VProgressInfo,
    FCoordConverterFactory,
    FLocalConverterFactory,
    FProjectionFactory,
    FVectorItmesFactory,
    VTargetFile,
    APolygon,
    VZoomArr,
    VMapType,
    VOverlay,
    VUsePrevZoom,
    VOgf2TileResolution,
    VOgf2TileFormat,
    VJpegQuality
  );
end;

end.
