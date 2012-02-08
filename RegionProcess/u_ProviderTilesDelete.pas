unit u_ProviderTilesDelete;

interface

uses
  Windows,
  Controls,
  i_JclNotify,
  i_LanguageManager,
  i_MapTypes,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  i_CoordConverterFactory,
  i_VectorItmesFactory,
  i_VectorItemLonLat,
  u_ExportProviderAbstract,
  fr_TilesDelete;

type
  TProviderTilesDelete = class(TExportProviderAbstract)
  private
    FFrame: TfrTilesDelete;
    FProjectionFactory: IProjectionInfoFactory;
    FVectorItmesFactory: IVectorItmesFactory;
    FAppClosingNotifier: IJclNotifier;
    FTimerNoifier: IJclNotifier;
  public
    constructor Create(
      AParent: TWinControl;
      ALanguageManager: ILanguageManager;
      AAppClosingNotifier: IJclNotifier;
      ATimerNoifier: IJclNotifier;
      AMainMapsConfig: IMainMapsConfig;
      AFullMapsSet: IMapTypeSet;
      AGUIConfigList: IMapTypeGUIConfigList;
      AProjectionFactory: IProjectionInfoFactory;
      AVectorItmesFactory: IVectorItmesFactory
    );
    destructor Destroy; override;
    function GetCaption: string; override;
    procedure InitFrame(Azoom: byte; APolygon: ILonLatPolygon); override;
    procedure Show; override;
    procedure Hide; override;
    procedure RefreshTranslation; override;
    procedure StartProcess(APolygon: ILonLatPolygon); override;
  end;


implementation

uses
  Forms,
  SysUtils,
  i_RegionProcessProgressInfo,
  u_OperationNotifier,
  u_RegionProcessProgressInfo,
  i_VectorItemProjected,
  u_ThreadDeleteTiles,
  u_ResStrings,
  u_MapType,
  frm_ProgressSimple;

{ TProviderTilesDelete }

constructor TProviderTilesDelete.Create(
  AParent: TWinControl;
  ALanguageManager: ILanguageManager;
  AAppClosingNotifier: IJclNotifier;
  ATimerNoifier: IJclNotifier;
  AMainMapsConfig: IMainMapsConfig;
  AFullMapsSet: IMapTypeSet;
  AGUIConfigList: IMapTypeGUIConfigList;
  AProjectionFactory: IProjectionInfoFactory;
  AVectorItmesFactory: IVectorItmesFactory
);
begin
  inherited Create(AParent, ALanguageManager, AMainMapsConfig, AFullMapsSet,  AGUIConfigList);
  FProjectionFactory := AProjectionFactory;
  FVectorItmesFactory := AVectorItmesFactory;
  FAppClosingNotifier := AAppClosingNotifier;
  FTimerNoifier := ATimerNoifier;
end;

destructor TProviderTilesDelete.Destroy;
begin
  FreeAndNil(FFrame);
  inherited;
end;

function TProviderTilesDelete.GetCaption: string;
begin
  Result := SAS_STR_OperationDeleteCaption;
end;

procedure TProviderTilesDelete.InitFrame(Azoom: byte; APolygon: ILonLatPolygon);
begin
  if FFrame = nil then begin
    FFrame := TfrTilesDelete.Create(
      nil,
      Self.MainMapsConfig,
      Self.FullMapsSet,
      Self.GUIConfigList
    );
    FFrame.Visible := False;
    FFrame.Parent := Self.Parent;
  end;
  FFrame.Init(Azoom);
end;

procedure TProviderTilesDelete.RefreshTranslation;
begin
  inherited;
  if FFrame <> nil then begin
    FFrame.RefreshTranslation;
  end;
end;

procedure TProviderTilesDelete.Hide;
begin
  inherited;
  if FFrame <> nil then begin
    if FFrame.Visible then begin
      FFrame.Hide;
    end;
  end;
end;

procedure TProviderTilesDelete.Show;
begin
  inherited;
  if FFrame <> nil then begin
    if not FFrame.Visible then begin
      FFrame.Show;
    end;
  end;
end;

procedure TProviderTilesDelete.StartProcess(APolygon: ILonLatPolygon);
var
  VMapType: TMapType;
  VDelBySize: Boolean;
  VDelSize: Cardinal;
  VZoom: byte;
  VProjectedPolygon: IProjectedPolygon;
  VCancelNotifierInternal: IOperationNotifierInternal;
  VOperationID: Integer;
  VProgressInfo: IRegionProcessProgressInfo;
begin
  inherited;
  if (MessageBox(FFrame.handle,pchar(SAS_MSG_youasure),pchar(SAS_MSG_coution),36)=IDYES) then begin
    VMapType:=TMapType(FFrame.cbbMap.Items.Objects[FFrame.cbbMap.ItemIndex]);
    if FFrame.cbbZoom.ItemIndex < 0 then begin
      FFrame.cbbZoom.ItemIndex := 0;
    end;
    VDelSize := 0;
    VDelBySize := FFrame.chkDelBySize.Checked;
    if VDelBySize then begin
      VDelSize := FFrame.seDelSize.Value;
    end;
    VZoom := FFrame.cbbZoom.ItemIndex;

    VProjectedPolygon :=
      FVectorItmesFactory.CreateProjectedPolygonByLonLatPolygon(
        FProjectionFactory.GetByConverterAndZoom(
          VMapType.GeoConvert,
          VZoom
        ),
        APolygon
      );

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

    TThreadDeleteTiles.Create(
      VCancelNotifierInternal,
      VOperationID,
      VProgressInfo,
      APolygon,
      VProjectedPolygon,
      FFrame.cbbZoom.ItemIndex,
      VMapType,
      VDelBySize,
      VDelSize
    );
  end;
end;

end.

