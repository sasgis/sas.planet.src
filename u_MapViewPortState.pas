unit u_MapViewPortState;

interface

uses
  Types,
  SysUtils,
  i_JclNotify,
  t_GeoTypes,
  i_ICoordConverter,
  i_IActiveMapsConfig,
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider,
  i_ActiveMapsConfigSaveLoad,
  i_MapTypes,
  i_ILocalCoordConverter,
  i_ILocalCoordConverterFactorySimpe,
  UMapType;

type
  TMapViewPortState = class
  private
    FVisibleCoordConverter: ILocalCoordConverter;
    FVisibleCoordConverterFactory: ILocalCoordConverterFactorySimpe;

    FActiveMaps: IActiveMapWithHybrConfig;
    FCenterPos: TPoint;
    FZoom: Byte;
    FViewSize: TPoint;

    FVisibleMove: TDoublePoint;
    FBaseScale: TDoublePoint;
    FMapScale: TDoublePoint;

    FSync: TMultiReadExclusiveWriteSynchronizer;
    FWriteLocked: Boolean;
    FPosChangeNotifier: IJclNotifier;
    FViewSizeChangeNotifier: IJclNotifier;
    FScaleChangeNotifier: IJclNotifier;
    procedure NotifyChangePos;
    procedure NotifyChangeViewSize;
    procedure NotifyChangeScale;
    function GetHybrChangeNotifier: IJclNotifier;
    function GetHybrList: IMapTypeList;
    function GetMapsList: IMapTypeList;
  protected
    function GetMapChangeNotifier: IJclNotifier;
    function InternalGetCurrentMap: TMapType;
    function InternalGetCurrentCoordConverter: ICoordConverter;
    procedure CreateVisibleCoordConverter;
  public
    constructor Create(
      AMapsList: IMapTypeList;
      ALayersList: IMapTypeList;
      AScreenSize: TPoint
    );
    destructor Destroy; override;

    procedure LockRead;
    procedure LockWrite;
    procedure UnLockRead;
    procedure UnLockWrite;
    procedure ChangeMapPixelByDelta(ADelta: TPoint);
    procedure ChangeMapPixelToVisualPoint(AVisualPoint: TPoint);
    procedure ChangeZoomWithFreezeAtVisualPoint(AZoom: Byte; AFreezePoint: TPoint);
    procedure ChangeZoomWithFreezeAtCenter(AZoom: Byte);
    procedure ChangeMainMapAtCurrentPoint(AMainMap: TMapType);
    procedure ChangeViewSize(ANewSize: TPoint);

    procedure ChangeMapPixelPosAndUnlock(ANewPos: TPoint);
    procedure ChangeLonLatAndUnlock(ALonLat: TDoublePoint); overload;
    procedure ChangeZoomAndUnlock(ANewZoom: Byte; ANewPos: TPoint); overload;
    procedure ChangeZoomAndUnlock(ANewZoom: Byte; ANewPos: TDoublePoint); overload;

    procedure MoveTo(Pnt: TPoint); virtual;
    procedure ScaleTo(AScale: Double; ACenterPoint: TPoint); overload; virtual;
    procedure ScaleTo(AScale: Double); overload; virtual;

    function GetCenterMapPixel: TPoint;
    function GetCenterLonLat: TDoublePoint;
    function GetCurrentZoom: Byte;
    function GetCurrentMap: TMapType;
    function GetCurrentCoordConverter: ICoordConverter;
    function GetViewRectInMapPixel: TRect;
    function GetViewRectInVisualPixel: TRect;
    function GetViewSizeInMapPixel: TPoint;
    function GetViewSizeInVisiblePixel: TPoint;
    function GetViewLonLatRect: TDoubleRect;
    function GetViewCenterInVisualPixel: TPoint;
    function GetViewTopLeftInMapPixel: TPoint;
    function GetVisualCoordConverter: ILocalCoordConverter;

    function VisiblePixel2MapPixel(Pnt: TPoint): TPoint; overload;
    function VisiblePixel2MapPixel(Pnt: TDoublePoint): TDoublePoint; overload;
    function MapPixel2VisiblePixel(Pnt: TPoint): TPoint; overload;
    function MapPixel2VisiblePixel(Pnt: TDoublePoint): TDoublePoint; overload;

    function VisibleRect2MapRect(ARect: TRect): TRect; overload;
    function VisibleRect2MapRect(ARect: TDoubleRect): TDoubleRect; overload;
    function MapRect2VisibleRect(ARect: TRect): TRect; overload;
    function MapRect2VisibleRect(ARect: TDoubleRect): TDoubleRect; overload;

    function VisiblePixel2LonLat(Pnt: TPoint): TDoublePoint; overload;
    function VisiblePixel2LonLat(Pnt: TDoublePoint): TDoublePoint; overload;
    function LonLat2VisiblePixel(Pnt: TDoublePoint): TDoublePoint;

    procedure SelectHybrByGUID(AMapGUID: TGUID);
    procedure UnSelectHybrByGUID(AMapGUID: TGUID);
    function IsHybrGUIDSelected(AMapGUID: TGUID): Boolean;
    procedure ChangeSelectHybrByGUID(AMapGUID: TGUID);

    property MapChangeNotifier: IJclNotifier read GetMapChangeNotifier;
    property PosChangeNotifier: IJclNotifier read FPosChangeNotifier;
    property ScaleChangeNotifier: IJclNotifier read FScaleChangeNotifier;
    property ViewSizeChangeNotifier: IJclNotifier read FViewSizeChangeNotifier;
    property MapsList: IMapTypeList read GetMapsList;
    property HybrList: IMapTypeList read GetHybrList;
    property HybrChangeNotifier: IJclNotifier read GetHybrChangeNotifier;

    procedure SaveViewPortState(AProvider: IConfigDataWriteProvider);
    procedure LoadViewPortState(AProvider: IConfigDataProvider);
  end;

implementation

uses
  u_JclNotify,
  i_IPosChangeMessage,
  u_ActiveMapWithHybrConfig,
  u_MapsConfigByConfigDataProvider,
  u_LocalCoordConverterFactorySimpe,
  Ugeofun,
  u_PosChangeMessage;

{ TMapViewPortState }

constructor TMapViewPortState.Create(
  AMapsList: IMapTypeList;
  ALayersList: IMapTypeList;
  AScreenSize: TPoint
);
var
  VConverter: ICoordConverter;
begin
  FVisibleCoordConverterFactory := TLocalCoordConverterFactorySimpe.Create;

  FActiveMaps := TActiveMapWithHybrConfig.Create(False, AMapsList, ALayersList);

  FPosChangeNotifier := TJclBaseNotifier.Create;
  FScaleChangeNotifier := TJclBaseNotifier.Create;
  FViewSizeChangeNotifier := TJclBaseNotifier.Create;

  VConverter := InternalGetCurrentCoordConverter;
  if FViewSize.X <= 0 then begin
    FViewSize.X := 1024;
  end;
  if FViewSize.X >= 4096 then begin
    FViewSize.X := 1024;
  end;
  if FViewSize.Y <= 0 then begin
    FViewSize.Y := 768;
  end;
  if FViewSize.Y >= 4096 then begin
    FViewSize.Y := 768;
  end;

  FVisibleMove.X := 0;
  FVisibleMove.Y := 0;
  FBaseScale.X := 1;
  FBaseScale.Y := 1;
  FMapScale := FBaseScale;
  FSync := TMultiReadExclusiveWriteSynchronizer.Create;
  CreateVisibleCoordConverter;
  FWriteLocked := False;
end;

procedure TMapViewPortState.CreateVisibleCoordConverter;
var
  VViewCenter: TPoint;
  VLocalTopLeftAtMap: TDoublePoint;
begin
  VViewCenter := GetViewCenterInVisualPixel;
  VLocalTopLeftAtMap.X := (-VViewCenter.X + FVisibleMove.X) / FMapScale.X + FCenterPos.X;
  VLocalTopLeftAtMap.Y := (-VViewCenter.Y + FVisibleMove.Y) / FMapScale.Y + FCenterPos.Y;

  FVisibleCoordConverter := FVisibleCoordConverterFactory.CreateConverter(
    Rect(0, 0, FViewSize.X, FViewSize.Y),
    FZoom,
    InternalGetCurrentCoordConverter,
    FMapScale,
    VLocalTopLeftAtMap
  );
end;

destructor TMapViewPortState.Destroy;
begin
  FSync.BeginWrite;
  FreeAndNil(FSync);
  FActiveMaps := nil;
  FPosChangeNotifier := nil;
  FScaleChangeNotifier := nil;
  FViewSizeChangeNotifier := nil;
  FVisibleCoordConverter := nil;
  FVisibleCoordConverterFactory := nil;
  inherited;
end;

procedure TMapViewPortState.ChangeLonLatAndUnlock(ALonLat: TDoublePoint);
var
  VLonLat: TDoublePoint;
  VConverter: ICoordConverter;
  VPixelPos: TPoint;
  VPosChanged: Boolean;
begin
  VPosChanged := false;
  FSync.BeginWrite;
  try
    if FWriteLocked then begin
      try
        VConverter := InternalGetCurrentCoordConverter;
        VLonLat := ALonLat;
        VConverter.CheckLonLatPos(VLonLat);
        VPixelPos := VConverter.LonLat2PixelPos(VLonLat, FZoom);
        VPosChanged := (FCenterPos.X <> VPixelPos.X) or (FCenterPos.Y <> VPixelPos.Y);
        FCenterPos := VPixelPos;
        if VPosChanged then begin
          CreateVisibleCoordConverter;
        end;
      finally
        FWriteLocked := False;
        FSync.EndWrite;
      end;
    end else begin
      raise Exception.Create('Настройки состояния не были заблокированы');
    end;
  finally
    FSync.EndWrite;
  end;
  if VPosChanged then begin
    NotifyChangePos;
  end;
end;

procedure TMapViewPortState.ChangeMainMapAtCurrentPoint(AMainMap: TMapType);
var
  VLonLat: TDoublePoint;
  VConverterOld: ICoordConverter;
  VConverterNew: ICoordConverter;
  VNewPos: TPoint;
  VPosChanged: Boolean;
begin
  if AMainMap = nil then begin
    raise Exception.Create('Нужно обязательно указывать активную карту');
  end;
  VPosChanged := false;
  FSync.BeginWrite;
  try
    if not IsEqualGUID(FActiveMaps.SelectedMapGUID, AMainMap.GUID) then begin
      VPosChanged := True;
      VConverterOld := InternalGetCurrentCoordConverter;
      VLonLat := VConverterOld.PixelPos2LonLat(FCenterPos, FZoom);
      VConverterNew := AMainMap.GeoConvert;
      VNewPos := VConverterNew.LonLat2PixelPos(VLonLat, FZoom);
      FCenterPos := VNewPos;
    end;
    if VPosChanged then begin
      CreateVisibleCoordConverter;
    end;
  finally
    FSync.EndWrite;
  end;
  if VPosChanged then begin
    FActiveMaps.SelectMapByGUID(AMainMap.GUID);
    NotifyChangePos;
  end;
end;

procedure TMapViewPortState.ChangeMapPixelPosAndUnlock(ANewPos: TPoint);
var
  VPixelPos: TPoint;
  VPosChanged: Boolean;
  VConverter: ICoordConverter;
begin
  VPosChanged := false;
  FSync.BeginWrite;
  try
    if FWriteLocked then begin
      try
        VPixelPos := ANewPos;
        VConverter := InternalGetCurrentCoordConverter;
        VConverter.CheckPixelPosStrict(VPixelPos, FZoom, True);
        VPosChanged := (FCenterPos.X <> VPixelPos.X) or (FCenterPos.Y <> VPixelPos.Y);
        FCenterPos := VPixelPos;
        if VPosChanged then begin
          CreateVisibleCoordConverter;
        end;
      finally
        FWriteLocked := False;
        FSync.EndWrite;
      end;
    end else begin
      raise Exception.Create('Настройки состояния не были заблокированы');
    end;
  finally
    FSync.EndWrite;
  end;
  if VPosChanged then begin
    NotifyChangePos;
  end;
end;

procedure TMapViewPortState.ChangeViewSize(ANewSize: TPoint);
var
  VChanged: Boolean;
begin
  if ANewSize.X <= 0 then begin
    raise Exception.Create('Ошибочный размер отображаемой карты');
  end;
  if ANewSize.X >= 4096 then begin
    raise Exception.Create('Ошибочный размер отображаемой карты');
  end;
  if ANewSize.Y <= 0 then begin
    raise Exception.Create('Ошибочный размер отображаемой карты');
  end;
  if ANewSize.Y >= 4096 then begin
    raise Exception.Create('Ошибочный размер отображаемой карты');
  end;
  FSync.BeginWrite;
  try
    VChanged := (FViewSize.X <> ANewSize.X) or (FViewSize.Y <> ANewSize.Y);
    FViewSize := ANewSize;
    if VChanged then begin
      CreateVisibleCoordConverter;
    end;
  finally
    FSync.EndWrite;
  end;
  if VChanged then begin
    NotifyChangePos;
    NotifyChangeViewSize;
  end;
end;

procedure TMapViewPortState.ChangeZoomAndUnlock(ANewZoom: Byte;
  ANewPos: TPoint);
var
  VZoom: Byte;
  VNewPos: TPoint;
  VConverter: ICoordConverter;
  VChanged: Boolean;
begin
  VChanged := False;
  FSync.BeginWrite;
  try
    if FWriteLocked then begin
      try
        VZoom := ANewZoom;
        VNewPos := ANewPos;
        VConverter := InternalGetCurrentCoordConverter;
        VConverter.CheckZoom(VZoom);
        VConverter.CheckPixelPos(VNewPos, VZoom, True);
        VChanged := (FZoom <> VZoom) or (FCenterPos.X <> VNewPos.X) or (FCenterPos.Y <> VNewPos.Y);
        FZoom := VZoom;
        FCenterPos := VNewPos;
        if VChanged then begin
          CreateVisibleCoordConverter;
        end;
      finally
        FWriteLocked := False;
        FSync.EndWrite;
      end;
    end else begin
      raise Exception.Create('Настройки состояния не были заблокированы');
    end;
  finally
    FSync.EndWrite;
  end;
  if VChanged then begin
    NotifyChangePos;
  end;
end;

procedure TMapViewPortState.ChangeZoomAndUnlock(ANewZoom: Byte;
  ANewPos: TDoublePoint);
var
  VZoom: Byte;
  VNewPos: TDoublePoint;
  VConverter: ICoordConverter;
  VPixelPos: TPoint;
  VChanged: Boolean;
begin
  VChanged := False;
  FSync.BeginWrite;
  try
    if FWriteLocked then begin
      try
        VZoom := ANewZoom;
        VNewPos := ANewPos;
        VConverter := InternalGetCurrentCoordConverter;
        VConverter.CheckZoom(VZoom);
        VConverter.CheckLonLatPos(VNewPos);
        VPixelPos := VConverter.LonLat2PixelPos(VNewPos, VZoom);
        VChanged := (FZoom <> VZoom) or (FCenterPos.X <> VPixelPos.X) or (FCenterPos.Y <> VPixelPos.Y);
        FZoom := VZoom;
        FCenterPos := VPixelPos;
        if VChanged then begin
          CreateVisibleCoordConverter;
        end;
      finally
        FWriteLocked := False;
        FSync.EndWrite;
      end;
    end else begin
      raise Exception.Create('Настройки состояния не были заблокированы');
    end;
  finally
    FSync.EndWrite;
  end;
  if VChanged then begin
    NotifyChangePos;
  end;
end;

function TMapViewPortState.GetCenterLonLat: TDoublePoint;
var
  VConverter: ICoordConverter;
begin
  FSync.BeginRead;
  try
    VConverter := InternalGetCurrentCoordConverter;
    Result := VConverter.PixelPos2LonLat(FCenterPos, FZoom);
  finally
    FSync.EndRead;
  end;
end;

function TMapViewPortState.GetCenterMapPixel: TPoint;
begin
  FSync.BeginRead;
  try
    Result := FCenterPos;
  finally
    FSync.EndRead;
  end;
end;

function TMapViewPortState.GetCurrentCoordConverter: ICoordConverter;
begin
  FSync.BeginRead;
  try
    Result := InternalGetCurrentCoordConverter;
  finally
    FSync.EndRead;
  end;
end;

function TMapViewPortState.GetCurrentMap: TMapType;
begin
  FSync.BeginRead;
  try
    Result := InternalGetCurrentMap;
  finally
    FSync.EndRead;
  end;
end;

function TMapViewPortState.GetCurrentZoom: Byte;
begin
  FSync.BeginRead;
  try
    Result := FZoom;
  finally
    FSync.EndRead;
  end;
end;

function TMapViewPortState.GetViewRectInMapPixel: TRect;
var
  VViewRect: TRect;
begin
  FSync.BeginRead;
  try
    VViewRect := GetViewRectInVisualPixel;
    Dec(VViewRect.Right);
    Dec(VViewRect.Bottom);
    Result.TopLeft := VisiblePixel2MapPixel(VViewRect.TopLeft);
    Result.BottomRight := VisiblePixel2MapPixel(VViewRect.BottomRight);
  finally
    FSync.EndRead;
  end;
end;

function TMapViewPortState.GetViewRectInVisualPixel: TRect;
begin
  FSync.BeginRead;
  try
    Result.TopLeft := Point(0, 0);
    Result.BottomRight := FViewSize;
  finally
    FSync.EndRead;
  end;
end;

function TMapViewPortState.GetViewCenterInVisualPixel: TPoint;
begin
  FSync.BeginRead;
  try
    Result := Point(FViewSize.X div 2, FViewSize.Y div 2);
  finally
    FSync.EndRead;
  end;
end;

function TMapViewPortState.GetViewLonLatRect: TDoubleRect;
var
  VMapRect: TRect;
begin
  FSync.BeginRead;
  try
    VMapRect := GetViewRectInMapPixel;
    InternalGetCurrentCoordConverter.CheckPixelRect(VMapRect, FZoom);
    Result := InternalGetCurrentCoordConverter.PixelRect2LonLatRect(VMapRect, FZoom);
  finally
    FSync.EndRead;
  end;
end;

function TMapViewPortState.GetViewSizeInMapPixel: TPoint;
var
  VRect: TRect;
begin
  VRect := GetViewRectInMapPixel;
  Result.X := VRect.Right - VRect.Left + 1;
  Result.Y := VRect.Bottom - VRect.Top + 1;
end;

function TMapViewPortState.GetViewSizeInVisiblePixel: TPoint;
begin
  FSync.BeginRead;
  try
    Result := FViewSize;
  finally
    FSync.EndRead;
  end;
end;

function TMapViewPortState.GetViewTopLeftInMapPixel: TPoint;
begin
  Result := VisiblePixel2MapPixel(Point(0,0));
end;

function TMapViewPortState.GetVisualCoordConverter: ILocalCoordConverter;
begin
  FSync.BeginRead;
  try
    Result := FVisibleCoordConverter;
  finally
    FSync.EndRead;
  end;
end;

procedure TMapViewPortState.LockRead;
begin
  FSync.BeginRead;
end;

procedure TMapViewPortState.LockWrite;
begin
  FSync.BeginWrite;
  if FWriteLocked then begin
    raise Exception.Create('Повторная блокировка настроек отображаемого окна');
  end else begin
    FWriteLocked := True;
  end;
end;

function TMapViewPortState.LonLat2VisiblePixel(
  Pnt: TDoublePoint): TDoublePoint;
var
  VMapPixel: TDoublePoint;
  VConverter: ICoordConverter;
begin
  FSync.BeginRead;
  try
    VConverter := InternalGetCurrentCoordConverter;
    VConverter.CheckLonLatPos(Pnt);
    VMapPixel := VConverter.LonLat2PixelPosFloat(Pnt, FZoom);
    Result := MapPixel2VisiblePixel(VMapPixel);
  finally
    FSync.EndRead;
  end;
end;

function TMapViewPortState.MapPixel2VisiblePixel(
  Pnt: TDoublePoint): TDoublePoint;
var
  VViewCenter: TPoint;
begin
  FSync.BeginRead;
  try
    VViewCenter := GetViewCenterInVisualPixel;
    Result.X := (Pnt.X - FCenterPos.X) * FMapScale.X + VViewCenter.X - FVisibleMove.X;
    Result.Y := (Pnt.Y - FCenterPos.Y) * FMapScale.Y + VViewCenter.Y - FVisibleMove.Y;
  finally
    FSync.EndRead;
  end;
end;

function TMapViewPortState.MapRect2VisibleRect(ARect: TRect): TRect;
begin
  LockRead;
  try
    Result.TopLeft := MapPixel2VisiblePixel(ARect.TopLeft);
    Result.BottomRight := MapPixel2VisiblePixel(ARect.BottomRight);
  finally
    UnLockRead;
  end;
end;

function TMapViewPortState.MapRect2VisibleRect(
  ARect: TDoubleRect): TDoubleRect;
begin
  LockRead;
  try
    Result.TopLeft := MapPixel2VisiblePixel(ARect.TopLeft);
    Result.BottomRight := MapPixel2VisiblePixel(ARect.BottomRight);
  finally
    UnLockRead;
  end;
end;

function TMapViewPortState.MapPixel2VisiblePixel(Pnt: TPoint): TPoint;
var
  VSourcePoint: TDoublePoint;
  VResultPoint: TDoublePoint;
begin
  VSourcePoint.X := Pnt.X;
  VSourcePoint.Y := Pnt.Y;
  VResultPoint := MapPixel2VisiblePixel(VSourcePoint);
  Result := Point(Trunc(VResultPoint.X), Trunc(VResultPoint.Y));
end;

function TMapViewPortState.VisiblePixel2MapPixel(
  Pnt: TDoublePoint): TDoublePoint;
var
  VViewCenter: TPoint;
begin
  FSync.BeginRead;
  try
    VViewCenter := GetViewCenterInVisualPixel;
    Result.X := (Pnt.X - VViewCenter.X + FVisibleMove.X) / FMapScale.X + FCenterPos.X;
    Result.Y := (Pnt.Y - VViewCenter.Y + FVisibleMove.Y) / FMapScale.Y + FCenterPos.Y;
  finally
    FSync.EndRead;
  end;
end;

function TMapViewPortState.VisibleRect2MapRect(ARect: TRect): TRect;
begin
  LockRead;
  try
    Result.TopLeft := VisiblePixel2MapPixel(ARect.TopLeft);
    Result.BottomRight := VisiblePixel2MapPixel(ARect.BottomRight);
  finally
    UnLockRead;
  end;
end;

function TMapViewPortState.VisibleRect2MapRect(
  ARect: TDoubleRect): TDoubleRect;
begin
  LockRead;
  try
    Result.TopLeft := VisiblePixel2MapPixel(ARect.TopLeft);
    Result.BottomRight := VisiblePixel2MapPixel(ARect.BottomRight);
  finally
    UnLockRead;
  end;
end;

function TMapViewPortState.VisiblePixel2MapPixel(Pnt: TPoint): TPoint;
var
  VSourcePoint: TDoublePoint;
  VResultPoint: TDoublePoint;
begin
  VSourcePoint.X := Pnt.X;
  VSourcePoint.Y := Pnt.Y;
  VResultPoint := VisiblePixel2MapPixel(VSourcePoint);
  Result := Point(Trunc(VResultPoint.X), Trunc(VResultPoint.Y));
end;

function TMapViewPortState.VisiblePixel2LonLat(
  Pnt: TPoint): TDoublePoint;
var
  VZoom: Byte;
  VMapPixel: TPoint;
  VConverter: ICoordConverter;
begin
  FSync.BeginRead;
  try
    VZoom := FZoom;
    VMapPixel := VisiblePixel2MapPixel(Pnt);
    VConverter := InternalGetCurrentCoordConverter;
    VConverter.CheckPixelPos(VMapPixel, VZoom, False);
    Result := VConverter.PixelPos2LonLat(VMapPixel, VZoom);
  finally
    FSync.EndRead;
  end;
end;

function TMapViewPortState.VisiblePixel2LonLat(
  Pnt: TDoublePoint): TDoublePoint;
var
  VZoom: Byte;
  VMapPixel: TDoublePoint;
  VConverter: ICoordConverter;
begin
  FSync.BeginRead;
  try
    VZoom := FZoom;
    VMapPixel := VisiblePixel2MapPixel(Pnt);
    VConverter := InternalGetCurrentCoordConverter;
    VConverter.CheckPixelPosFloat(VMapPixel, VZoom, False);
    Result := VConverter.PixelPosFloat2LonLat(VMapPixel, VZoom);
  finally
    FSync.EndRead;
  end;
end;

procedure TMapViewPortState.UnLockRead;
begin
  FSync.EndRead;
end;

procedure TMapViewPortState.UnLockWrite;
begin
  FSync.BeginWrite;
  try
    if FWriteLocked then begin
      FWriteLocked := False;
      FSync.EndWrite;
    end else begin
      raise Exception.Create('Настройки состояния не были заблокированы');
    end;
  finally
    FSync.EndWrite;
  end;
end;

procedure TMapViewPortState.ChangeMapPixelByDelta(ADelta: TPoint);
var
  VNewPos: TPoint;
  VConverter: ICoordConverter;
  VZoom: Byte;
  VChanged: Boolean;
begin
  if (ADelta.X <> 0) or (ADelta.Y <> 0) then begin
    FSync.BeginWrite;
    try
      VConverter := InternalGetCurrentCoordConverter;
      VZoom := FZoom;
      VNewPos.X := FCenterPos.X + Trunc(ADelta.X / FBaseScale.X);
      VNewPos.Y := FCenterPos.Y + Trunc(ADelta.Y / FBaseScale.Y);
      FVisibleMove.X := 0;
      FVisibleMove.Y := 0;
      FMapScale := FBaseScale;
      VConverter.CheckPixelPosStrict(VNewPos, VZoom, True);
      VChanged := (FCenterPos.X <> VNewPos.X) or (FCenterPos.Y <> VNewPos.Y);
      FCenterPos := VNewPos;
      if VChanged then begin
        CreateVisibleCoordConverter;
      end;
    finally
      FSync.EndWrite;
    end;
    if VChanged then begin
      NotifyChangePos;
    end;
  end;
end;

procedure TMapViewPortState.ChangeMapPixelToVisualPoint(
  AVisualPoint: TPoint);
var
  VNewPos: TPoint;
  VConverter: ICoordConverter;
  VZoom: Byte;
  VChanged: Boolean;
begin
  FSync.BeginWrite;
  try
    VConverter := InternalGetCurrentCoordConverter;
    VZoom := FZoom;
    VNewPos := VisiblePixel2MapPixel(AVisualPoint);
    VConverter.CheckPixelPosStrict(VNewPos, VZoom, True);
    VChanged := (FCenterPos.X <> VNewPos.X) or (FCenterPos.Y <> VNewPos.Y);
    FVisibleMove.X := 0;
    FVisibleMove.Y := 0;
    FMapScale := FBaseScale;
    FCenterPos := VNewPos;
    if VChanged then begin
      CreateVisibleCoordConverter;
    end;
  finally
    FSync.EndWrite;
  end;
  if VChanged then begin
    NotifyChangePos;
  end;
end;

procedure TMapViewPortState.ChangeZoomWithFreezeAtVisualPoint(AZoom: Byte;
  AFreezePoint: TPoint);
var
  VConverter: ICoordConverter;
  VZoom: Byte;
  VZoomOld: Byte;
  VMapFreezePoint: TPoint;
  VRelativeFreezePoint: TDoublePoint;
  VMapFreezPointAtNewZoom: TPoint;
  VNewCenterPos: TPoint;
  VFreezeDelta: TPoint;
  VChanged: Boolean;
  VTempMapPoint: TPoint;
begin
  VChanged := False;
  FSync.BeginWrite;
  try
    VConverter := InternalGetCurrentCoordConverter;
    VZoom := AZoom;
    VConverter.CheckZoom(VZoom);
    if FZoom <> VZoom then begin
      VChanged := True;
      VZoomOld := FZoom;
      VMapFreezePoint := VisiblePixel2MapPixel(AFreezePoint);
      VConverter.CheckPixelPos(VMapFreezePoint, VZoomOld, False);
      VRelativeFreezePoint := VConverter.PixelPos2Relative(VMapFreezePoint, VZoomOld);
      VMapFreezPointAtNewZoom := VConverter.Relative2Pixel(VRelativeFreezePoint, VZoom);

      FVisibleMove.X := 0;
      FVisibleMove.Y := 0;
      FMapScale := FBaseScale;
      FCenterPos := VMapFreezPointAtNewZoom;
      FZoom := VZoom;

      VTempMapPoint := VisiblePixel2MapPixel(AFreezePoint);

      VFreezeDelta.X := VTempMapPoint.X - FCenterPos.X;
      VFreezeDelta.Y := VTempMapPoint.Y - FCenterPos.Y;


      VNewCenterPos.X := FCenterPos.X - VFreezeDelta.X;
      VNewCenterPos.Y := FCenterPos.Y - VFreezeDelta.Y;

      VConverter.CheckPixelPosStrict(VNewCenterPos, VZoom, False);
      FCenterPos := VNewCenterPos;
    end;
    if VChanged then begin
      CreateVisibleCoordConverter;
    end;
  finally
    FSync.EndWrite;
  end;
  if VChanged then begin
    NotifyChangePos;
  end;
end;

procedure TMapViewPortState.ChangeZoomWithFreezeAtCenter(AZoom: Byte);
var
  VConverter: ICoordConverter;
  VRelativePoint: TDoublePoint;
  VZoom: Byte;
  VZoomOld: Byte;
  VChanged: Boolean;
begin
  VChanged := False;
  FSync.BeginWrite;
  try
    VConverter := InternalGetCurrentCoordConverter;
    VZoom := AZoom;
    VConverter.CheckZoom(VZoom);
    if FZoom <> VZoom then begin
      VChanged := True;
      VZoomOld := FZoom;
      FVisibleMove.X := 0;
      FVisibleMove.Y := 0;
      FMapScale := FBaseScale;
      VRelativePoint := VConverter.PixelPos2Relative(FCenterPos, VZoomOld);
      FCenterPos := VConverter.Relative2Pixel(VRelativePoint, VZoom);
      FZoom := VZoom;
    end;
    if VChanged then begin
      CreateVisibleCoordConverter;
    end;
  finally
    FSync.EndWrite;
  end;
  if VChanged then begin
    NotifyChangePos;
  end;
end;

procedure TMapViewPortState.NotifyChangePos;
var
  VMessage: IPosChangeMessage;
begin
  FSync.BeginRead;
  try
    VMessage := TPosChangeMessage.Create(FVisibleCoordConverter);
  finally
    FSync.EndRead;
  end;
  FPosChangeNotifier.Notify(VMessage);
end;

procedure TMapViewPortState.NotifyChangeScale;
begin
  FScaleChangeNotifier.Notify(nil);
end;

procedure TMapViewPortState.NotifyChangeViewSize;
begin
  FViewSizeChangeNotifier.Notify(nil);
end;

function TMapViewPortState.GetMapChangeNotifier: IJclNotifier;
begin
  Result := FActiveMaps.MapChangeNotifier;
end;

function TMapViewPortState.InternalGetCurrentCoordConverter: ICoordConverter;
begin
  Result := FActiveMaps.MapsList.GetMapTypeByGUID(FActiveMaps.SelectedMapGUID).MapType.GeoConvert;
end;

function TMapViewPortState.InternalGetCurrentMap: TMapType;
begin
  Result := FActiveMaps.MapsList.GetMapTypeByGUID(FActiveMaps.SelectedMapGUID).MapType;
end;

function TMapViewPortState.GetHybrChangeNotifier: IJclNotifier;
begin
  Result := FActiveMaps.HybrChangeNotifier;
end;

function TMapViewPortState.GetHybrList: IMapTypeList;
begin
  Result := FActiveMaps.HybrList;
end;

function TMapViewPortState.GetMapsList: IMapTypeList;
begin
  Result := FActiveMaps.MapsList;
end;

function TMapViewPortState.IsHybrGUIDSelected(AMapGUID: TGUID): Boolean;
begin
  Result := FActiveMaps.IsHybrGUIDSelected(AMapGUID);
end;

procedure TMapViewPortState.SelectHybrByGUID(AMapGUID: TGUID);
begin
  FActiveMaps.SelectHybrByGUID(AMapGUID);
end;

procedure TMapViewPortState.UnSelectHybrByGUID(AMapGUID: TGUID);
begin
  FActiveMaps.UnSelectHybrByGUID(AMapGUID);
end;

procedure TMapViewPortState.ChangeSelectHybrByGUID(AMapGUID: TGUID);
begin
  if FActiveMaps.IsHybrGUIDSelected(AMapGUID) then begin
    FActiveMaps.UnSelectHybrByGUID(AMapGUID);
  end else begin
    FActiveMaps.SelectHybrByGUID(AMapGUID);
  end;
end;

procedure TMapViewPortState.LoadViewPortState(AProvider: IConfigDataProvider);
var
  VMapConfigLoader: IActiveMapsConfigLoader;
begin
  VMapConfigLoader := TMapsConfigLoaderByConfigDataProvider.Create(AProvider.GetSubItem('MainViewMaps'));
  try
    VMapConfigLoader.Load(FActiveMaps);
  finally
    VMapConfigLoader := nil;
  end;
end;

procedure TMapViewPortState.SaveViewPortState(AProvider: IConfigDataWriteProvider);
var
  VMapConfigSaver: IActiveMapsConfigSaver;
begin
  VMapConfigSaver := TMapsConfigSaverByConfigDataProvider.Create(AProvider.GetOrCreateSubItem('MainViewMaps'));
  try
    VMapConfigSaver.Save(FActiveMaps);
  finally
    VMapConfigSaver := nil;
  end;
end;

procedure TMapViewPortState.MoveTo(Pnt: TPoint);
var
  VChanged: Boolean;
  VVisibleMove: TDoublePoint;
begin
  VChanged := False;
  FSync.BeginWrite;
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
    FSync.EndWrite;
  end;
  if VChanged then begin
    NotifyChangeScale;
  end;
end;

procedure TMapViewPortState.ScaleTo(AScale: Double; ACenterPoint: TPoint);
var
  VVisiblePointFixed: TDoublePoint;
  VMapPointFixed: TDoublePoint;
  VNewVisualPoint: TDoublePoint;
  VNewMapScale: TDoublePoint;
  VNewVisibleMove: TDoublePoint;
  VChanged: Boolean;
begin
  VChanged := False;
  VVisiblePointFixed.X := ACenterPoint.X;
  VVisiblePointFixed.Y := ACenterPoint.Y;
  FSync.BeginWrite;
  try
    VMapPointFixed := VisiblePixel2MapPixel(VVisiblePointFixed);
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
    VNewVisualPoint := MapPixel2VisiblePixel(VMapPointFixed);
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
    FSync.EndWrite;
  end;
  if VChanged then begin
    NotifyChangeScale;
  end;
end;

procedure TMapViewPortState.ScaleTo(AScale: Double);
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
  FSync.BeginWrite;
  try
    VViewCenter := GetViewCenterInVisualPixel;
    VVisiblePointFixed.X := VViewCenter.X;
    VVisiblePointFixed.Y := VViewCenter.Y;
    VMapPointFixed := VisiblePixel2MapPixel(VVisiblePointFixed);
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

    VNewVisualPoint := MapPixel2VisiblePixel(VMapPointFixed);
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
    FSync.EndWrite;
  end;
  if VChanged then begin
    NotifyChangeScale;
  end;
end;

end.


