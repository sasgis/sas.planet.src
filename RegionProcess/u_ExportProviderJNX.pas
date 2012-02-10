unit u_ExportProviderJNX;

interface

uses
  Controls,
  i_JclNotify,
  i_VectorItemLonLat,
  i_CoordConverterFactory,
  i_VectorItmesFactory,
  i_LanguageManager,
  i_MapTypes,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  u_ExportProviderAbstract,
  fr_ExportToJNX;

type
  TExportProviderJNX = class(TExportProviderAbstract)
  private
    FFrame: TfrExportToJNX;
    FCoordConverterFactory: ICoordConverterFactory;
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
      AVectorItmesFactory: IVectorItmesFactory;
      ACoordConverterFactory: ICoordConverterFactory
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
  StrUtils,
  i_RegionProcessProgressInfo,
  u_OperationNotifier,
  u_RegionProcessProgressInfo,
  u_ThreadExportToJNX,
  u_ResStrings,
  u_MapType,
  frm_ProgressSimple,
  RegExpr;


{ TExportProviderJNX }

constructor TExportProviderJNX.Create(
  AParent: TWinControl;
  ALanguageManager: ILanguageManager;
  AAppClosingNotifier: IJclNotifier;
  ATimerNoifier: IJclNotifier;
  AMainMapsConfig: IMainMapsConfig;
  AFullMapsSet: IMapTypeSet;
  AGUIConfigList: IMapTypeGUIConfigList;
  AProjectionFactory: IProjectionInfoFactory;
  AVectorItmesFactory: IVectorItmesFactory;
  ACoordConverterFactory: ICoordConverterFactory
);
begin
  inherited Create(AParent, ALanguageManager, AMainMapsConfig, AFullMapsSet, AGUIConfigList);
  FProjectionFactory := AProjectionFactory;
  FVectorItmesFactory := AVectorItmesFactory;
  FCoordConverterFactory := ACoordConverterFactory;
  FAppClosingNotifier := AAppClosingNotifier;
  FTimerNoifier := ATimerNoifier;
end;

destructor TExportProviderJNX.Destroy;
begin
  FreeAndNil(FFrame);
  inherited;
end;

function TExportProviderJNX.GetCaption: string;
begin
  Result := SAS_STR_ExportJNXPackCaption;
end;

procedure TExportProviderJNX.InitFrame(Azoom: byte; APolygon: ILonLatPolygon);
begin
  if FFrame = nil then begin
    FFrame := TfrExportToJNX.CreateForFileType(
      nil,
      Self.MainMapsConfig,
      Self.FullMapsSet,
      Self.GUIConfigList,
      'JNX |*.jnx',
      'jnx'
    );
    FFrame.Visible := False;
    FFrame.Parent := Self.Parent;
  end;
  FFrame.Init;
end;

procedure TExportProviderJNX.RefreshTranslation;
begin
  inherited;
  if FFrame <> nil then begin
    FFrame.RefreshTranslation;
  end;
end;

procedure TExportProviderJNX.Hide;
begin
  inherited;
  if FFrame <> nil then begin
    if FFrame.Visible then begin
      FFrame.Hide;
    end;
  end;
end;

procedure TExportProviderJNX.Show;
begin
  inherited;
  if FFrame <> nil then begin
    if not FFrame.Visible then begin
      FFrame.Show;
    end;
  end;
end;

function RegExprGetMatchSubStr(const AStr, AMatchExpr: string; AMatchID: Integer): string;
var
  VRegExpr: TRegExpr;
begin
    VRegExpr  := TRegExpr.Create;
  try
    VRegExpr.Expression := AMatchExpr;
    if VRegExpr.Exec(AStr) then
    begin
      if (AMatchID <= VRegExpr.SubExprMatchCount) and (AMatchID >= 0) then
        Result := VRegExpr.Match[AMatchID]
      else
        Result := '';
    end
    else
      Result := '';
  finally
    FreeAndNil(VRegExpr);
  end;
end;

procedure TExportProviderJNX.StartProcess(APolygon: ILonLatPolygon);
var
  i:integer;
  path:string;
  Zoomarr:array [0..23] of boolean;
  VMapType: TMapType;
  VProductName : string;
  VMapName : string;
  VJNXVersion : integer;
  VZorder : integer;
  VProductID : integer;
  VJpgQuality : byte;
  VCancelNotifierInternal: IOperationNotifierInternal;
  VOperationID: Integer;
  VProgressInfo: IRegionProcessProgressInfo;
begin
  inherited;
  for i:=0 to 23 do begin
    ZoomArr[i]:= FFrame.chklstZooms.Checked[i];
  end;
  VMapType:=TMapType(FFrame.cbbMap.Items.Objects[FFrame.cbbMap.ItemIndex]);
  path:=FFrame.edtTargetFile.Text;
  VProductName := FFrame.EProductName.Text;
  VMapName := FFrame.EmapName.Text;
  VJpgQuality := FFrame.EJpgQuality.Value;
  if FFrame.v3.checked then begin
      VJNXVersion := 3;
      VZorder := 0;
  end else begin
      VJNXVersion := 4;
      VZorder := FFrame.EZorder.Value;
  end;
  try
  if (RegExprGetMatchSubStr(FFrame.EProductID.text,'[0-9]+',0)<>'' ) then
   VProductID := StrToInt(RegExprGetMatchSubStr(FFrame.EProductID.text,'[0-9]+',0));
   else VProductID := 0;
  except
   VProductID := 0;
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

  TThreadExportToJNX.Create(
    VCancelNotifierInternal,
    VOperationID,
    VProgressInfo,
    FCoordConverterFactory,
    FProjectionFactory,
    FVectorItmesFactory,
    path,
    APolygon,
    Zoomarr,
    VMapType,
    VProductName,
    VMapName,
    VJNXVersion,
    VZorder,
    VProductID,
    VJpgQuality
  );
end;

end.
