unit u_ProviderTilesGenPrev;

interface

uses
  Controls,
  Forms,
  i_JclNotify,
  i_LanguageManager,
  i_VectorItemLonLat,
  i_MapTypes,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  i_ImageResamplerConfig,
  i_CoordConverterFactory,
  i_VectorItmesFactory,
  i_GlobalViewMainConfig,
  u_ExportProviderAbstract,
  fr_TilesGenPrev;

type
  TProviderTilesGenPrev = class(TExportProviderAbstract)
  private
    FFrame: TfrTilesGenPrev;
    FProjectionFactory: IProjectionInfoFactory;
    FVectorItmesFactory: IVectorItmesFactory;
    FImageResamplerConfig: IImageResamplerConfig;
    FViewConfig: IGlobalViewMainConfig;
    FAppClosingNotifier: IJclNotifier;
    FTimerNoifier: IJclNotifier;
  protected
    function CreateFrame: TFrame; override;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AAppClosingNotifier: IJclNotifier;
      const ATimerNoifier: IJclNotifier;
      const AMainMapsConfig: IMainMapsConfig;
      const AFullMapsSet: IMapTypeSet;
      const AGUIConfigList: IMapTypeGUIConfigList;
      const AViewConfig: IGlobalViewMainConfig;
      const AProjectionFactory: IProjectionInfoFactory;
      const AVectorItmesFactory: IVectorItmesFactory;
      const AImageResamplerConfig: IImageResamplerConfig
    );
    function GetCaption: string; override;
    procedure StartProcess(const APolygon: ILonLatPolygon); override;
  end;


implementation

uses
  SysUtils,
  GR32,
  i_ImageResamplerFactory,
  i_RegionProcessProgressInfo,
  u_OperationNotifier,
  u_RegionProcessProgressInfo,
  u_ThreadGenPrevZoom,
  u_ResStrings,
  u_MapType,
  frm_ProgressSimple;

{ TProviderTilesGenPrev }

constructor TProviderTilesGenPrev.Create(
  const ALanguageManager: ILanguageManager;
  const AAppClosingNotifier: IJclNotifier;
  const ATimerNoifier: IJclNotifier;
  const AMainMapsConfig: IMainMapsConfig;
  const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList;
  const AViewConfig: IGlobalViewMainConfig;
  const AProjectionFactory: IProjectionInfoFactory;
  const AVectorItmesFactory: IVectorItmesFactory;
  const AImageResamplerConfig: IImageResamplerConfig
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
  FViewConfig := AViewConfig;
  FImageResamplerConfig := AImageResamplerConfig;
  FAppClosingNotifier := AAppClosingNotifier;
  FTimerNoifier := ATimerNoifier;
end;

function TProviderTilesGenPrev.CreateFrame: TFrame;
begin
  FFrame :=
    TfrTilesGenPrev.Create(
      Self.LanguageManager,
      Self.MainMapsConfig,
      Self.FullMapsSet,
      Self.GUIConfigList,
      FImageResamplerConfig
    );
  Result := FFrame;
end;

function TProviderTilesGenPrev.GetCaption: string;
begin
  Result := SAS_STR_OperationGenPrevCaption;
end;

procedure TProviderTilesGenPrev.StartProcess(const APolygon: ILonLatPolygon);
var
  i: integer;
  VInZooms: TArrayOfByte;
  VMapType: TMapType;
  VZoomsCount: Integer;
  VFromZoom: Byte;
  VResampler: IImageResamplerFactory;
  VCancelNotifierInternal: IOperationNotifierInternal;
  VOperationID: Integer;
  VProgressInfo: IRegionProcessProgressInfo;
  VBgColor: TColor32;
begin
  inherited;
  VMapType := TMapType(FFrame.cbbMap.Items.Objects[FFrame.cbbMap.ItemIndex]);
  VFromZoom := FFrame.cbbFromZoom.ItemIndex + 1;
  VZoomsCount := 0;
  for i := 0 to FFrame.cbbFromZoom.ItemIndex do begin
    if FFrame.chklstZooms.ItemEnabled[i] then begin
      if FFrame.chklstZooms.Checked[i] then begin
        SetLength(VInZooms, VZoomsCount + 1);
        VInZooms[VZoomsCount] := FFrame.cbbFromZoom.ItemIndex - i;
        Inc(VZoomsCount);
      end;
    end;
  end;
  try
    if FFrame.cbbResampler.ItemIndex >= 0 then begin
      VResampler := FImageResamplerConfig.GetList.Items[FFrame.cbbResampler.ItemIndex];
    end else begin
      VResampler := FImageResamplerConfig.GetActiveFactory;
    end;
  except
    VResampler := FImageResamplerConfig.GetActiveFactory;
  end;
  if VMapType.IsHybridLayer then begin
    VBgColor := 0;
  end else begin
    VBgColor := Color32(FViewConfig.BackGroundColor);
  end;

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

  TThreadGenPrevZoom.Create(
    VCancelNotifierInternal,
    VOperationID,
    VProgressInfo,
    FProjectionFactory,
    FVectorItmesFactory,
    VFromZoom,
    VInZooms,
    APolygon,
    VMapType,
    FFrame.chkReplace.Checked,
    FFrame.chkSaveFullOnly.Checked,
    FFrame.chkFromPrevZoom.Checked,
    FFrame.chkUsePrevTiles.Checked,
    VBgColor,
    VResampler
  );
end;

end.
