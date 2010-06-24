unit u_MapViewPortState;

interface

uses
  Types,
  SysUtils,
  i_JclNotify,
  t_GeoTypes,
  i_ICoordConverter,
  UMapType;

type
  TMapViewPortState = class
  private
    FMainMap: TMapType;
    FCenterPos: TPoint;
    FZoom: Byte;
    FViewSize: TPoint;
    FSync: TMultiReadExclusiveWriteSynchronizer;
    FWriteLocked: Boolean;
    FMainMapChangeNotifier: IJclNotifier;
    FPosChangeNotifier: IJclNotifier;
    FViewSizeChangeNotifier: IJclNotifier;
    procedure NotifyChangePos;
    procedure NotifyChangeMainMap(AOldSelected, ANewSelected: TMapType);
    procedure NotifyChangeViewSize;
  public
    constructor Create(AMainMap: TMapType; AZoom: Byte; ACenterPos: TPoint; AScreenSize: TPoint);
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
    procedure ChangeLonLatAndUnlock(ALonLat: TExtendedPoint); overload;
    procedure ChangeZoomAndUnlock(ANewZoom: Byte; ANewPos: TPoint); overload;
    procedure ChangeZoomAndUnlock(ANewZoom: Byte; ANewPos: TDoublePoint); overload;
    procedure ChangeZoomAndUnlock(ANewZoom: Byte; ANewPos: TExtendedPoint); overload;

    function GetCenterMapPixel: TPoint;
    function GetCenterLonLat: TExtendedPoint;
    function GetCenterLonLatDlb: TDoublePoint;
    function GetCurrentZoom: Byte;
    function GetCurrentMap: TMapType;
    function GetCurrentCoordConverter: ICoordConverter;
    function GetViewMapRect: TRect;
    function GetViewMapSize: TPoint;
    function GetViewLonLatRect: TExtendedRect;

    function VisiblePixel2MapPixel(Pnt: TPoint): TPoint; overload;
    function VisiblePixel2MapPixel(Pnt: TExtendedPoint): TExtendedPoint; overload;
    function MapPixel2VisiblePixel(Pnt: TPoint): TPoint; overload;
    function MapPixel2VisiblePixel(Pnt: TExtendedPoint): TExtendedPoint; overload;
    function VisiblePixel2LonLat(Pnt: TPoint): TExtendedPoint; overload;
    function GetVisiblePixelRect: TRect;
    function GetVisibleTopLeft: TPoint;
    function GetVisibleSizeInPixel: TPoint;

    property MainMapChangeNotifier: IJclNotifier read FMainMapChangeNotifier;
    property PosChangeNotifier: IJclNotifier read FPosChangeNotifier;
    property ViewSizeChangeNotifier: IJclNotifier read FViewSizeChangeNotifier;
  end;

implementation

uses
  u_JclNotify,
  u_PosChangeMessage,
  u_MapChangeMessage;

{ TMapViewPortState }

constructor TMapViewPortState.Create(AMainMap: TMapType; AZoom: Byte; ACenterPos: TPoint; AScreenSize: TPoint);
var
  VConverter: ICoordConverter;
begin
  if AMainMap = nil then begin
    raise Exception.Create('Нужно обязательно указывать активную карту');
  end;
  FMainMapChangeNotifier := TJclBaseNotifier.Create;
  FPosChangeNotifier := TJclBaseNotifier.Create;
  FViewSizeChangeNotifier := TJclBaseNotifier.Create;
  FMainMap := AMainMap;
  VConverter := FMainMap.GeoConvert;
  FZoom := AZoom;
  VConverter.CheckZoom(FZoom);
  FCenterPos := ACenterPos;
  VConverter.CheckPixelPosStrict(FCenterPos, FZoom, True);
  FViewSize := AScreenSize;
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

  FSync := TMultiReadExclusiveWriteSynchronizer.Create;
  FWriteLocked := False;
end;

destructor TMapViewPortState.Destroy;
begin
  FSync.BeginWrite;
  FreeAndNil(FSync);
  FMainMapChangeNotifier := nil;
  FPosChangeNotifier := nil;
  FViewSizeChangeNotifier := nil;
  inherited;
end;

procedure TMapViewPortState.ChangeLonLatAndUnlock(ALonLat: TDoublePoint);
var
  VLonLat: TExtendedPoint;
  VConverter: ICoordConverter;
  VPixelPos: TPoint;
  VPosChanged: Boolean;
begin
  VPosChanged := false;
  FSync.BeginWrite;
  try
    if FWriteLocked then begin
      try
        VConverter := FMainMap.GeoConvert;
        VLonLat.X := ALonLat.X;
        VLonLat.Y := ALonLat.Y;
        VConverter.CheckLonLatPos(VLonLat);
        VPixelPos := VConverter.LonLat2PixelPos(VLonLat, FZoom);
        VPosChanged := (FCenterPos.X <> VPixelPos.X) or (FCenterPos.Y <> VPixelPos.Y);
        FCenterPos := VPixelPos;
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

procedure TMapViewPortState.ChangeLonLatAndUnlock(ALonLat: TExtendedPoint);
var
  VLonLat: TExtendedPoint;
  VConverter: ICoordConverter;
  VPixelPos: TPoint;
  VPosChanged: Boolean;
begin
  VPosChanged := false;
  FSync.BeginWrite;
  try
    if FWriteLocked then begin
      try
        VConverter := FMainMap.GeoConvert;
        VLonLat := ALonLat;
        VConverter.CheckLonLatPos(VLonLat);
        VPixelPos := VConverter.LonLat2PixelPos(VLonLat, FZoom);
        VPosChanged := (FCenterPos.X <> VPixelPos.X) or (FCenterPos.Y <> VPixelPos.Y);
        FCenterPos := VPixelPos;
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
  VLonLat: TExtendedPoint;
  VConverterOld: ICoordConverter;
  VConverterNew: ICoordConverter;
  VNewPos: TPoint;
  VOldSelected: TMapType;
  VPosChanged: Boolean;
begin
  if AMainMap = nil then begin
    raise Exception.Create('Нужно обязательно указывать активную карту');
  end;
  VPosChanged := false;
  VOldSelected := nil;
  FSync.BeginWrite;
  try
    if FMainMap <> AMainMap then begin
      VPosChanged := True;
      VOldSelected := FMainMap;
      VConverterOld := FMainMap.GeoConvert;
      VLonLat := VConverterOld.PixelPos2LonLat(FCenterPos, FZoom);
      VConverterNew := AMainMap.GeoConvert;
      VNewPos := VConverterNew.LonLat2PixelPos(VLonLat, FZoom);
      FMainMap := AMainMap;
      FCenterPos := VNewPos;
    end;
  finally
    FSync.EndWrite;
  end;
  if VPosChanged then begin
    NotifyChangeMainMap(VOldSelected, AMainMap);
    NotifyChangePos;
  end;
end;

procedure TMapViewPortState.ChangeMapPixelPosAndUnlock(ANewPos: TPoint);
var
  VPixelPos: TPoint;
  VPosChanged: Boolean;
begin
  VPosChanged := false;
  FSync.BeginWrite;
  try
    if FWriteLocked then begin
      try
        VPixelPos := ANewPos;
        FMainMap.GeoConvert.CheckPixelPosStrict(VPixelPos, FZoom, True);
        VPosChanged := (FCenterPos.X <> VPixelPos.X) or (FCenterPos.Y <> VPixelPos.Y);
        FCenterPos := VPixelPos;
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
  VChanged := False;
  if FViewSize.X <= 0 then begin
    raise Exception.Create('Ошибочный размер отображаемой карты');
  end;
  if FViewSize.X >= 4096 then begin
    raise Exception.Create('Ошибочный размер отображаемой карты');
  end;
  if FViewSize.Y <= 0 then begin
    raise Exception.Create('Ошибочный размер отображаемой карты');
  end;
  if FViewSize.Y >= 4096 then begin
    raise Exception.Create('Ошибочный размер отображаемой карты');
  end;
  FSync.BeginWrite;
  try
    VChanged := (FViewSize.X <> ANewSize.X) or (FViewSize.Y <> ANewSize.Y);
    FViewSize := ANewSize;
  finally
    FSync.EndWrite;
  end;
  if VChanged then begin
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
        VConverter := FMainMap.GeoConvert;
        VConverter.CheckZoom(VZoom);
        VConverter.CheckPixelPos(VNewPos, VZoom, True);
        VChanged := (FZoom <> VZoom) or (FCenterPos.X <> VNewPos.X) or (FCenterPos.Y <> VNewPos.Y);
        FZoom := VZoom;
        FCenterPos := VNewPos;
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
  VNewPos: TExtendedPoint;
begin
  VNewPos.X := ANewPos.X;
  VNewPos.Y := ANewPos.Y;
  ChangeZoomAndUnlock(ANewZoom, VNewPos);
end;

procedure TMapViewPortState.ChangeZoomAndUnlock(ANewZoom: Byte;
  ANewPos: TExtendedPoint);
var
  VZoom: Byte;
  VNewPos: TExtendedPoint;
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
        VConverter := FMainMap.GeoConvert;
        VConverter.CheckZoom(VZoom);
        VConverter.CheckLonLatPos(VNewPos);
        VPixelPos := VConverter.LonLat2PixelPos(VNewPos, VZoom);
        VChanged := (FZoom <> VZoom) or (FCenterPos.X <> VPixelPos.X) or (FCenterPos.Y <> VPixelPos.Y);
        FZoom := VZoom;
        FCenterPos := VPixelPos;
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

function TMapViewPortState.GetCenterLonLat: TExtendedPoint;
begin
  FSync.BeginRead;
  try
    Result := FMainMap.GeoConvert.PixelPos2LonLat(FCenterPos, FZoom);
  finally
    FSync.EndRead;
  end;
end;

function TMapViewPortState.GetCenterLonLatDlb: TDoublePoint;
var
  VLonLat: TExtendedPoint;
begin
  VLonLat := GetCenterLonLat;
  Result.X := VLonLat.X;
  Result.Y := VLonLat.Y;
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
    Result := FMainMap.GeoConvert;
  finally
    FSync.EndRead;
  end;
end;

function TMapViewPortState.GetCurrentMap: TMapType;
begin
  FSync.BeginRead;
  try
    Result := FMainMap;
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

function TMapViewPortState.GetViewMapRect: TRect;
begin
  FSync.BeginRead;
  try
    Result.Left := FCenterPos.X - FViewSize.X div 2;
    Result.Top := FCenterPos.Y - FViewSize.Y div 2;
    Result.Right := Result.Left + FViewSize.X;
    Result.Bottom := Result.Top + FViewSize.Y;
    FMainMap.GeoConvert.CheckPixelRect(Result, FZoom, False);
  finally
    FSync.EndRead;
  end;
end;

function TMapViewPortState.GetViewLonLatRect: TExtendedRect;
begin
  FSync.BeginRead;
  try
    Result := FMainMap.GeoConvert.PixelRect2LonLatRect(GetViewMapRect, FZoom);
  finally
    FSync.EndRead;
  end;
end;

function TMapViewPortState.GetViewMapSize: TPoint;
var
  VRect: TRect;
begin
  FSync.BeginRead;
  try
    VRect.Left := FCenterPos.X - FViewSize.X div 2;
    VRect.Top := FCenterPos.Y - FViewSize.Y div 2;
    VRect.Right := VRect.Left + FViewSize.X;
    VRect.Bottom := VRect.Top + FViewSize.Y;
    FMainMap.GeoConvert.CheckPixelRect(VRect, FZoom, False);
    Result.X := VRect.Right - VRect.Left + 1;
    Result.Y := VRect.Bottom - VRect.Top + 1;
  finally
    FSync.EndRead;
  end;
end;

function TMapViewPortState.GetVisiblePixelRect: TRect;
begin
  FSync.BeginRead;
  try
    Result.Left := FCenterPos.X - FViewSize.X div 2;
    Result.Top := FCenterPos.Y - FViewSize.Y div 2;
    Result.Right := Result.Left + FViewSize.X;
    Result.Bottom := Result.Top + FViewSize.Y;
  finally
    FSync.EndRead;
  end;
end;

function TMapViewPortState.GetVisibleSizeInPixel: TPoint;
begin
  FSync.BeginRead;
  try
    Result := FViewSize;
  finally
    FSync.EndRead;
  end;
end;

function TMapViewPortState.GetVisibleTopLeft: TPoint;
begin
  FSync.BeginRead;
  try
    Result.X := FCenterPos.X - FViewSize.X div 2;
    Result.Y := FCenterPos.Y - FViewSize.Y div 2;
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

function TMapViewPortState.MapPixel2VisiblePixel(
  Pnt: TExtendedPoint): TExtendedPoint;
begin
  FSync.BeginRead;
  try
    Result.X := Pnt.X - FCenterPos.X + FViewSize.X div 2;
    Result.Y := Pnt.Y - FCenterPos.Y + FViewSize.Y div 2;
  finally
    FSync.EndRead;
  end;
end;

function TMapViewPortState.MapPixel2VisiblePixel(Pnt: TPoint): TPoint;
begin
  FSync.BeginRead;
  try
    Result.X := Pnt.X - FCenterPos.X + FViewSize.X div 2;
    Result.Y := Pnt.Y - FCenterPos.Y + FViewSize.Y div 2;
  finally
    FSync.EndRead;
  end;
end;

function TMapViewPortState.VisiblePixel2MapPixel(Pnt: TPoint): TPoint;
begin
  FSync.BeginRead;
  try
    Result.X := Pnt.X + FCenterPos.X - FViewSize.X div 2;
    Result.Y := Pnt.Y + FCenterPos.Y - FViewSize.Y div 2;
  finally
    FSync.EndRead;
  end;
end;

function TMapViewPortState.VisiblePixel2MapPixel(
  Pnt: TExtendedPoint): TExtendedPoint;
begin
  FSync.BeginRead;
  try
    Result.X := Pnt.X + FCenterPos.X - FViewSize.X div 2;
    Result.Y := Pnt.Y + FCenterPos.Y - FViewSize.Y div 2;
  finally
    FSync.EndRead;
  end;
end;

function TMapViewPortState.VisiblePixel2LonLat(
  Pnt: TPoint): TExtendedPoint;
var
  VZoom: Byte;
  VMapPixel: TPoint;
begin
  FSync.BeginRead;
  try
    VZoom := FZoom;
    VMapPixel := VisiblePixel2MapPixel(Pnt);
    FMainMap.GeoConvert.CheckPixelPos(VMapPixel, VZoom, False);
    Result := FMainMap.GeoConvert.PixelPos2LonLat(VMapPixel, VZoom);
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
  FSync.EndWrite;
end;

procedure TMapViewPortState.ChangeMapPixelByDelta(ADelta: TPoint);
var
  VNewPos: TPoint;
  VConverter: ICoordConverter;
  VZoom: Byte;
  VChanged: Boolean;
begin
  if (ADelta.X <> 0) or (ADelta.Y <> 0)then begin
    FSync.BeginWrite;
    try
      VConverter := FMainMap.GeoConvert;
      VZoom := FZoom;
      VNewPos.X := FCenterPos.X + ADelta.X;
      VNewPos.Y := FCenterPos.Y + ADelta.Y;
      VConverter.CheckPixelPosStrict(VNewPos, VZoom, True);
      VChanged := (FCenterPos.X <> VNewPos.X) or (FCenterPos.Y <> VNewPos.Y);
      FCenterPos := VNewPos;
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
    VConverter := FMainMap.GeoConvert;
    VZoom := FZoom;
    VNewPos := VisiblePixel2MapPixel(AVisualPoint);
    VConverter.CheckPixelPosStrict(VNewPos, VZoom, True);
    VChanged := (FCenterPos.X <> VNewPos.X) or (FCenterPos.Y <> VNewPos.Y);
    FCenterPos := VNewPos;
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
  VRelativeFreezePoint: TExtendedPoint;
  VMapFreezPointAtNewZoom: TPoint;
  VNewCenterPos: TPoint;
  VFreezeDelta: TPoint;
  VChanged: Boolean;
begin
  VChanged := False;
  FSync.BeginWrite;
  try
    VConverter := FMainMap.GeoConvert;
    VZoom := AZoom;
    VConverter.CheckZoom(VZoom);
    if FZoom <> VZoom then begin
      VChanged := True;
      VZoomOld := FZoom;
      VMapFreezePoint := VisiblePixel2MapPixel(AFreezePoint);
      VConverter.CheckPixelPos(VMapFreezePoint, VZoomOld, False);
      VFreezeDelta.X := FCenterPos.X - VMapFreezePoint.X;
      VFreezeDelta.Y := FCenterPos.Y - VMapFreezePoint.Y;

      VRelativeFreezePoint := VConverter.PixelPos2Relative(VMapFreezePoint, VZoomOld);
      VMapFreezPointAtNewZoom := VConverter.Relative2Pixel(VRelativeFreezePoint, VZoom);

      VNewCenterPos.X := VMapFreezPointAtNewZoom.X + VFreezeDelta.X;
      VNewCenterPos.Y := VMapFreezPointAtNewZoom.Y + VFreezeDelta.Y;

      VConverter.CheckPixelPosStrict(VNewCenterPos, VZoom, False);
      FCenterPos := VNewCenterPos;
      FZoom := VZoom;
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
  VRelativePoint: TExtendedPoint;
  VZoom: Byte;
  VZoomOld: Byte;
  VChanged: Boolean;
begin
  VChanged := False;
  FSync.BeginWrite;
  try
    VConverter := FMainMap.GeoConvert;
    VZoom := AZoom;
    VConverter.CheckZoom(VZoom);
    if FZoom <> VZoom then begin
      VChanged := True;
      VZoomOld := FZoom;
      VRelativePoint := VConverter.PixelPos2Relative(FCenterPos, VZoomOld);
      FCenterPos := VConverter.Relative2Pixel(VRelativePoint, VZoom);
      FZoom := VZoom;
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
  VMessage: IJclNotificationMessage;
begin
  FSync.BeginRead;
  try
    VMessage := TPosChangeMessage.Create(FMainMap, FZoom, FCenterPos);
  finally
    FSync.EndRead;
  end;
  FPosChangeNotifier.Notify(VMessage);
end;

procedure TMapViewPortState.NotifyChangeMainMap(AOldSelected, ANewSelected: TMapType);
var
  VMessage: IJclNotificationMessage;
begin
  VMessage := TMapChangeMessage.Create(AOldSelected, ANewSelected);
  FMainMapChangeNotifier.Notify(VMessage);
  VMessage := nil;
end;

procedure TMapViewPortState.NotifyChangeViewSize;
begin
  FViewSizeChangeNotifier.Notify(nil);
end;

end.

