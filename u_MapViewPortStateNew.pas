unit u_MapViewPortStateNew;

interface

uses
  Types,
  i_JclNotify,
  t_GeoTypes,
  i_ICoordConverter,
  i_ILocalCoordConverter,
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider,
  i_IViewPortState,
  i_IActiveMapsConfig,
  i_ILocalCoordConverterFactorySimpe,
  u_ConfigDataElementBase;

type
  TMapViewPortStateNew = class(TConfigDataElementBase, IViewPortState)
  private
    FScaleChangeNotifier: IJclNotifier;
    FMainCoordConverter: ICoordConverter;
    FVisibleCoordConverter: ILocalCoordConverter;
    FVisibleCoordConverterFactory: ILocalCoordConverterFactorySimpe;
    FMainMapConfig: IMainActiveMap;

    FActiveCoordConverter: ICoordConverter;
    FCenterPos: TPoint;
    FZoom: Byte;
    FViewSize: TPoint;

    FVisibleMove: TDoublePoint;
    FBaseScale: TDoublePoint;
    FMapScale: TDoublePoint;

    FMainMapChangeListener: IJclListener;
    procedure SetActiveCoordConverter;
    procedure CreateVisibleCoordConverter;
    procedure OnMainMapChange(Sender: TObject);
    procedure ResetScaleAndMove;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetMainCoordConverter: ICoordConverter;
    procedure SetMainCoordConverter(AValue: ICoordConverter);
    property MainCoordConverter: ICoordConverter read GetMainCoordConverter write SetMainCoordConverter;

    function GetCurrentCoordConverter: ICoordConverter;
    function GetCurrentZoom: Byte;

    function GetVisualCoordConverter: ILocalCoordConverter;

    procedure ChangeViewSize(ANewSize: TPoint);
    procedure ChangeMapPixelByDelta(ADelta: TDoublePoint);
    procedure ChangeMapPixelToVisualPoint(AVisualPoint: TPoint);
    procedure ChangeZoomWithFreezeAtVisualPoint(AZoom: Byte; AFreezePoint: TPoint);
    procedure ChangeZoomWithFreezeAtCenter(AZoom: Byte);

    procedure ChangeLonLat(ALonLat: TDoublePoint);

    procedure MoveTo(Pnt: TPoint);
    procedure ScaleTo(AScale: Double; ACenterPoint: TPoint); overload;
    procedure ScaleTo(AScale: Double); overload;

    function GetScaleChangeNotifier: IJclNotifier;
  public
    constructor Create(AMainMapConfig: IMainMapsConfig);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  u_JclNotify,
  u_NotifyEventListener,
  u_LocalCoordConverterFactorySimpe;

{ TMapViewPortStateNew }

constructor TMapViewPortStateNew.Create(AMainMapConfig: IMainMapsConfig);
begin
  inherited Create;
  FScaleChangeNotifier := TJclBaseNotifier.Create;
  FVisibleCoordConverterFactory := TLocalCoordConverterFactorySimpe.Create;
  FMainMapConfig := AMainMapConfig;
  FMainCoordConverter := nil;
  FCenterPos := Point(0, 0);
  FZoom := 0;
  FViewSize := Point(1024, 768);
  FBaseScale.X := 1;
  FBaseScale.Y := 1;
  ResetScaleAndMove;
  SetActiveCoordConverter;
  CreateVisibleCoordConverter;
  FMainMapChangeListener := TNotifyEventListener.Create(Self.OnMainMapChange);
  FMainMapConfig.GetChangeNotifier.Add(FMainMapChangeListener);
end;

destructor TMapViewPortStateNew.Destroy;
begin
  FMainMapConfig.GetChangeNotifier.Remove(FMainMapChangeListener);
  FMainMapChangeListener := nil;
  FMainMapConfig := nil;
  FScaleChangeNotifier := nil;
  FVisibleCoordConverterFactory := nil;
  inherited;
end;

procedure TMapViewPortStateNew.ChangeLonLat(ALonLat: TDoublePoint);
var
  VLonLat: TDoublePoint;
  VPixelPos: TPoint;
  VPosChanged: Boolean;
begin
  LockWrite;
  try
    VLonLat := ALonLat;
    FActiveCoordConverter.CheckLonLatPos(VLonLat);
    VPixelPos := FActiveCoordConverter.LonLat2PixelPos(VLonLat, FZoom);
    VPosChanged := (FCenterPos.X <> VPixelPos.X) or (FCenterPos.Y <> VPixelPos.Y);
    FCenterPos := VPixelPos;
    ResetScaleAndMove;
    if VPosChanged then begin
      CreateVisibleCoordConverter;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMapViewPortStateNew.ChangeMapPixelByDelta(ADelta: TDoublePoint);
var
  VNewPos: TPoint;
  VZoom: Byte;
  VChanged: Boolean;
begin
  LockWrite;
  try
    VZoom := FZoom;
    VNewPos.X := Trunc(FCenterPos.X + ADelta.X / FBaseScale.X);
    VNewPos.Y := Trunc(FCenterPos.Y + ADelta.Y / FBaseScale.Y);
    ResetScaleAndMove;
    FActiveCoordConverter.CheckPixelPosStrict(VNewPos, VZoom, True);
    VChanged := (FCenterPos.X <> VNewPos.X) or (FCenterPos.Y <> VNewPos.Y);
    FCenterPos := VNewPos;
    if VChanged then begin
      CreateVisibleCoordConverter;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMapViewPortStateNew.ChangeMapPixelToVisualPoint(
  AVisualPoint: TPoint);
var
  VNewPos: TPoint;
  VZoom: Byte;
  VChanged: Boolean;
begin
  LockWrite;
  try
    VZoom := FZoom;
    VNewPos := FVisibleCoordConverter.LocalPixel2MapPixel(AVisualPoint);
    FActiveCoordConverter.CheckPixelPosStrict(VNewPos, VZoom, True);
    VChanged := (FCenterPos.X <> VNewPos.X) or (FCenterPos.Y <> VNewPos.Y);
    ResetScaleAndMove;
    FCenterPos := VNewPos;
    if VChanged then begin
      CreateVisibleCoordConverter;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMapViewPortStateNew.ChangeViewSize(ANewSize: TPoint);
var
  VChanged: Boolean;
begin
  if ANewSize.X <= 0 then begin
    raise Exception.Create('Ошибочный размер отображаемой карты');
  end;
  if ANewSize.X > 4096 then begin
    raise Exception.Create('Ошибочный размер отображаемой карты');
  end;
  if ANewSize.Y <= 0 then begin
    raise Exception.Create('Ошибочный размер отображаемой карты');
  end;
  if ANewSize.Y > 4096 then begin
    raise Exception.Create('Ошибочный размер отображаемой карты');
  end;
  LockWrite;
  try
    VChanged := (FViewSize.X <> ANewSize.X) or (FViewSize.Y <> ANewSize.Y);
    ResetScaleAndMove;
    FViewSize := ANewSize;
    if VChanged then begin
      CreateVisibleCoordConverter;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMapViewPortStateNew.ChangeZoomWithFreezeAtCenter(AZoom: Byte);
var
  VRelativePoint: TDoublePoint;
  VZoom: Byte;
  VZoomOld: Byte;
  VChanged: Boolean;
begin
  VChanged := False;
  LockWrite;
  try
    VZoom := AZoom;
    FActiveCoordConverter.CheckZoom(VZoom);
    if FZoom <> VZoom then begin
      VChanged := True;
      VZoomOld := FZoom;
      ResetScaleAndMove;
      VRelativePoint := FActiveCoordConverter.PixelPos2Relative(FCenterPos, VZoomOld);
      FCenterPos := FActiveCoordConverter.Relative2Pixel(VRelativePoint, VZoom);
      FZoom := VZoom;
    end;
    if VChanged then begin
      CreateVisibleCoordConverter;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMapViewPortStateNew.ChangeZoomWithFreezeAtVisualPoint(AZoom: Byte;
  AFreezePoint: TPoint);
var
  VZoom: Byte;
  VZoomOld: Byte;
  VMapFreezePoint: TDoublePoint;
  VRelativeFreezePoint: TDoublePoint;
  VMapFreezPointAtNewZoom: TDoublePoint;
  VNewCenterPos: TPoint;
  VFreezeDelta: TDoublePoint;
  VChanged: Boolean;
  VViewCenter: TPoint;
  VTempMapPoint: TPoint;
begin
  VChanged := False;
  LockWrite;
  try
    VZoom := AZoom;
    FActiveCoordConverter.CheckZoom(VZoom);
    if FZoom <> VZoom then begin
      VChanged := True;
      VZoomOld := FZoom;
      VMapFreezePoint := FVisibleCoordConverter.LocalPixel2MapPixelFloat(AFreezePoint);
      FActiveCoordConverter.CheckPixelPosFloat(VMapFreezePoint, VZoomOld, False);
      VRelativeFreezePoint := FActiveCoordConverter.PixelPosFloat2Relative(VMapFreezePoint, VZoomOld);
      VMapFreezPointAtNewZoom := FActiveCoordConverter.Relative2PixelPosFloat(VRelativeFreezePoint, VZoom);
      VViewCenter := Point(FViewSize.X div 2, FViewSize.Y div 2);

      VNewCenterPos.X := Trunc(VMapFreezPointAtNewZoom.X - (AFreezePoint.X - VViewCenter.X) / FBaseScale.X);
      VNewCenterPos.Y := Trunc(VMapFreezPointAtNewZoom.Y - (AFreezePoint.Y - VViewCenter.Y) / FBaseScale.Y);
      ResetScaleAndMove;
      FZoom := VZoom;

      FActiveCoordConverter.CheckPixelPosStrict(VNewCenterPos, VZoom, False);
      FCenterPos := VNewCenterPos;
    end;
    if VChanged then begin
      CreateVisibleCoordConverter;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMapViewPortStateNew.CreateVisibleCoordConverter;
var
  VViewCenter: TPoint;
  VLocalTopLeftAtMap: TDoublePoint;
begin
  VViewCenter := Point(FViewSize.X div 2, FViewSize.Y div 2);
  VLocalTopLeftAtMap.X := (-VViewCenter.X + FVisibleMove.X) / FMapScale.X + FCenterPos.X;
  VLocalTopLeftAtMap.Y := (-VViewCenter.Y + FVisibleMove.Y) / FMapScale.Y + FCenterPos.Y;

  FVisibleCoordConverter := FVisibleCoordConverterFactory.CreateConverter(
    Rect(0, 0, FViewSize.X, FViewSize.Y),
    FZoom,
    FActiveCoordConverter,
    FMapScale,
    VLocalTopLeftAtMap
  );
end;

procedure TMapViewPortStateNew.DoReadConfig(AConfigData: IConfigDataProvider);
begin
  inherited;

end;

procedure TMapViewPortStateNew.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  inherited;

end;

function TMapViewPortStateNew.GetCurrentCoordConverter: ICoordConverter;
begin
  LockRead;
  try
    Result := FActiveCoordConverter;
  finally
    UnlockRead;
  end;
end;

function TMapViewPortStateNew.GetCurrentZoom: Byte;
begin
  LockRead;
  try
    Result := FZoom;
  finally
    UnlockRead;
  end;
end;

function TMapViewPortStateNew.GetMainCoordConverter: ICoordConverter;
begin
  LockRead;
  try
    Result := FMainCoordConverter;
  finally
    UnlockRead;
  end;
end;

function TMapViewPortStateNew.GetScaleChangeNotifier: IJclNotifier;
begin
  Result := FScaleChangeNotifier;
end;

function TMapViewPortStateNew.GetVisualCoordConverter: ILocalCoordConverter;
begin
  LockRead;
  try
    Result := FVisibleCoordConverter;
  finally
    UnlockRead;
  end;
end;

procedure TMapViewPortStateNew.MoveTo(Pnt: TPoint);
begin

end;

procedure TMapViewPortStateNew.OnMainMapChange(Sender: TObject);
begin

end;

procedure TMapViewPortStateNew.ResetScaleAndMove;
begin
  FMapScale := FBaseScale;
  FVisibleMove.X := 0;
  FVisibleMove.Y := 0;
end;

procedure TMapViewPortStateNew.ScaleTo(AScale: Double; ACenterPoint: TPoint);
begin

end;

procedure TMapViewPortStateNew.ScaleTo(AScale: Double);
begin

end;

procedure TMapViewPortStateNew.SetActiveCoordConverter;
begin

end;

procedure TMapViewPortStateNew.SetMainCoordConverter(AValue: ICoordConverter);
begin

end;

end.
