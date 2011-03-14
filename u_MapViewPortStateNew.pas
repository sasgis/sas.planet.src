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
    procedure NotifyChangeScale;
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
  i_MapTypes,
  u_NotifyEventListener,
  u_LocalCoordConverterFactorySimpe,
  Ugeofun;

{ TMapViewPortStateNew }

constructor TMapViewPortStateNew.Create(AMainMapConfig: IMainMapsConfig);
begin
  inherited Create;
  FScaleChangeNotifier := TJclBaseNotifier.Create;
  FVisibleCoordConverterFactory := TLocalCoordConverterFactorySimpe.Create;
  FMainMapConfig := AMainMapConfig;
  FMainCoordConverter := nil;
  FCenterPos := Point(128, 128);
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
  VChanged: Boolean;
  VViewCenter: TPoint;
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
var
  VLonLat: TDoublePoint;
  VZoom: Byte;
begin
  inherited;
  if AConfigData <> nil then begin
    VZoom := AConfigData.ReadInteger('Zoom', FZoom);
    FActiveCoordConverter.CheckZoom(VZoom);
    VLonLat := FVisibleCoordConverter.GetCenterLonLat;
    VLonLat.X := AConfigData.ReadFloat('X', VLonLat.X);
    VLonLat.Y := AConfigData.ReadFloat('Y', VLonLat.Y);
    FActiveCoordConverter.CheckLonLatPos(VLonLat);
    if FZoom <> VZoom then begin
      FZoom := VZoom;
      SetChanged;
    end;
    ChangeLonLat(VLonLat);
  end;
end;

procedure TMapViewPortStateNew.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
var
  VLonLat: TDoublePoint;
begin
  inherited;
  VLonLat := FVisibleCoordConverter.GetCenterLonLat;
  AConfigData.WriteInteger('Zoom', FZoom);
  AConfigData.WriteFloat('X', VLonLat.X);
  AConfigData.WriteFloat('Y', VLonLat.Y);
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
var
  VChanged: Boolean;
  VVisibleMove: TDoublePoint;
begin
  VChanged := False;
  LockWrite;
  try
    if not compare2EP(FMapScale, FBaseScale) then begin
      FMapScale := FBaseScale;
      VChanged := True;
    end;
    VVisibleMove.X := Pnt.X;
    VVisibleMove.Y := Pnt.Y;
    if not compare2EP(FVisibleMove, VVisibleMove) then begin
      FVisibleMove := VVisibleMove;
      VChanged := True;
    end;

    if VChanged then begin
      CreateVisibleCoordConverter;
    end;
  finally
    UnlockWrite;
  end;
  if VChanged then begin
    NotifyChangeScale;
  end;
end;

procedure TMapViewPortStateNew.NotifyChangeScale;
begin
  FScaleChangeNotifier.Notify(nil);
end;

procedure TMapViewPortStateNew.OnMainMapChange(Sender: TObject);
begin
  SetActiveCoordConverter;
end;

procedure TMapViewPortStateNew.ResetScaleAndMove;
begin
  FMapScale := FBaseScale;
  FVisibleMove.X := 0;
  FVisibleMove.Y := 0;
end;

procedure TMapViewPortStateNew.ScaleTo(AScale: Double; ACenterPoint: TPoint);
var
  VVisiblePointFixed: TDoublePoint;
  VMapPointFixed: TDoublePoint;
  VNewVisualPoint: TDoublePoint;
  VNewMapScale: TDoublePoint;
  VNewVisibleMove: TDoublePoint;
  VChanged: Boolean;
  VViewCenter: TPoint;
begin
  VChanged := False;
  VVisiblePointFixed.X := ACenterPoint.X;
  VVisiblePointFixed.Y := ACenterPoint.Y;
  LockWrite;
  try
    if not compare2EP(FVisibleMove, DoublePoint(0,0)) then begin
      FVisibleMove.X := 0;
      FVisibleMove.Y := 0;
      VChanged := True;
    end;

    VNewMapScale.X := FBaseScale.X * AScale;
    VNewMapScale.Y := FBaseScale.X * AScale;
    if not compare2EP(FMapScale, VNewMapScale) then begin
      FMapScale := VNewMapScale;
      VChanged := True;
    end;
    VMapPointFixed := FVisibleCoordConverter.LocalPixelFloat2MapPixelFloat(VVisiblePointFixed);
    VViewCenter := Point(FViewSize.X div 2, FViewSize.Y div 2);
    VNewVisualPoint.X := (VMapPointFixed.X - FCenterPos.X) * FMapScale.X + VViewCenter.X;
    VNewVisualPoint.Y := (VMapPointFixed.Y - FCenterPos.Y) * FMapScale.Y + VViewCenter.Y;

    VNewVisibleMove.X := VNewVisualPoint.X - VVisiblePointFixed.X;
    VNewVisibleMove.Y := VNewVisualPoint.Y - VVisiblePointFixed.Y;
    if not compare2EP(FVisibleMove, VNewVisibleMove) then begin
      FVisibleMove := VNewVisibleMove;
      VChanged := True;
    end;
    if VChanged then begin
      CreateVisibleCoordConverter;
    end;
  finally
    UnlockWrite;
  end;
  if VChanged then begin
    NotifyChangeScale;
  end;
end;

procedure TMapViewPortStateNew.ScaleTo(AScale: Double);
var
  VVisiblePointFixed: TDoublePoint;
  VMapPointFixed: TDoublePoint;
  VNewVisualPoint: TDoublePoint;
  VViewCenter: TPoint;
  VNewMapScale: TDoublePoint;
  VNewVisibleMove: TDoublePoint;
  VChanged: Boolean;
begin
  VChanged := False;
  LockWrite;
  try
    VViewCenter := Point(FViewSize.X div 2, FViewSize.Y div 2);
    VVisiblePointFixed.X := VViewCenter.X;
    VVisiblePointFixed.Y := VViewCenter.Y;
    VMapPointFixed := FVisibleCoordConverter.LocalPixelFloat2MapPixelFloat(VVisiblePointFixed);
    if not compare2EP(FVisibleMove, DoublePoint(0,0)) then begin
      FVisibleMove.X := 0;
      FVisibleMove.Y := 0;
      VChanged := True;
    end;

    VNewMapScale.X := FBaseScale.X * AScale;
    VNewMapScale.Y := FBaseScale.X * AScale;
    if not compare2EP(FMapScale, VNewMapScale) then begin
      FMapScale := VNewMapScale;
      VChanged := True;
    end;

    VNewVisualPoint.X := (VMapPointFixed.X - FCenterPos.X) * FMapScale.X + VViewCenter.X;
    VNewVisualPoint.Y := (VMapPointFixed.Y - FCenterPos.Y) * FMapScale.Y + VViewCenter.Y;
    VNewVisibleMove.X := VNewVisualPoint.X - VVisiblePointFixed.X;
    VNewVisibleMove.Y := VNewVisualPoint.Y - VVisiblePointFixed.Y;
    if not compare2EP(FVisibleMove, VNewVisibleMove) then begin
      FVisibleMove := VNewVisibleMove;
      VChanged := True;
    end;
    if VChanged then begin
      CreateVisibleCoordConverter;
    end;
  finally
    UnlockWrite;
  end;
  if VChanged then begin
    NotifyChangeScale;
  end;
end;

procedure TMapViewPortStateNew.SetActiveCoordConverter;
var
  VNewConverter: ICoordConverter;
  VGUID: TGUID;
  VMap: IMapType;
  VCenterLonLat: TDoublePoint;
  VChanged: Boolean;
begin
  VChanged := False;
  LockWrite;
  try
    if FMainCoordConverter <> nil then begin
      VNewConverter := FMainCoordConverter;
    end else begin
      VGUID := FMainMapConfig.GetActiveMap.GetSelectedGUID;
      VMap := FMainMapConfig.GetActiveMap.GetMapsList.GetMapTypeByGUID(VGUID);
      if VMap <> nil then begin
        VNewConverter := VMap.MapType.MainGeoConvert;
      end;
    end;
    if VNewConverter <> nil then begin
      if FActiveCoordConverter <> nil then begin
        if not FActiveCoordConverter.IsSameConverter(VNewConverter) then begin
          VCenterLonLat := FActiveCoordConverter.PixelPos2LonLat(FCenterPos, FZoom);
          VNewConverter.CheckLonLatPos(VCenterLonLat);
          FCenterPos := VNewConverter.LonLat2PixelPos(VCenterLonLat, FZoom);
          FActiveCoordConverter := VNewConverter;
          VChanged := True;
        end;
      end else begin
        FActiveCoordConverter := VNewConverter;
        VChanged := True;
      end;
    end;
    if VChanged then begin
      CreateVisibleCoordConverter;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMapViewPortStateNew.SetMainCoordConverter(AValue: ICoordConverter);
begin
  LockWrite;
  try
    if FMainCoordConverter <> AValue then begin
      FMainCoordConverter := AValue;
      SetActiveCoordConverter;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
