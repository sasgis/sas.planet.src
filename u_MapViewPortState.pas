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
    procedure NotyfyChangePos;
  public
    constructor Create(AMainMap: TMapType; AZoom: Byte; ACenterPos: TPoint; AScreenSize: TPoint);
    destructor Destroy; override;

    procedure LockRead;
    procedure LockWrite;
    procedure UnLockRead;
    procedure UnLockWrite;
    procedure ChangeMapPixelByDelta(ADelta: TPoint);
    procedure ChangeZoomWithFreezeAtVisualPoint(AFreezePoint: TPoint);
    procedure ChangeMainMapAtCurrentPoint(AMainMap: TMapType);

    procedure ChangeMapPixelPosAndUnlock(ANewPos: TPoint);
    procedure ChangeLonLatAndUnlock(ANewPos: TDoublePoint); overload;
    procedure ChangeLonLatAndUnlock(ANewPos: TExtendedPoint); overload;
    procedure ChangeZoomAndUnlock(ANewZoom: Byte; ANewPos: TPoint); overload;
    procedure ChangeZoomAndUnlock(ANewZoom: Byte; ANewPos: TDoublePoint); overload;
    procedure ChangeZoomAndUnlock(ANewZoom: Byte; ANewPos: TExtendedPoint); overload;
    procedure ChangeViewSizeAndUnlock(ANewSize: TPoint);

    function GetCenterMapPixel: TPoint;
    function GetCenterLonLat: TExtendedPoint;
    function GetCurrentZoom: Byte;
    function GetCurrentMap: TMapType;
    function GetCurrentCoordConverter: ICoordConverter;
    function GetViewMapRect: TRect;
    function GetViewMapSize: TPoint;

    function VisiblePixel2MapPixel(Pnt: TPoint): TPoint; overload;
    function VisiblePixel2MapPixel(Pnt: TExtendedPoint): TExtendedPoint; overload;
    function MapPixel2VisiblePixel(Pnt: TPoint): TPoint; overload;
    function MapPixel2VisiblePixel(Pnt: TExtendedPoint): TExtendedPoint; overload;
    function GetVisiblePixelRect: TRect;
    function GetVisibleTopLeft: TPoint;
    function GetVisibleSizeInPixel: TPoint;

    property MainMapChangeNotifier: IJclNotifier read FMainMapChangeNotifier;
    property PosChangeNotifier: IJclNotifier read FPosChangeNotifier;
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
  if FViewSize.Y <= 4096 then begin
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
  inherited;
end;

procedure TMapViewPortState.ChangeLonLatAndUnlock(ANewPos: TDoublePoint);
var
  VLonLat: TExtendedPoint;
  VConverter: ICoordConverter;
begin
  FSync.BeginWrite;
  try
    if FWriteLocked then begin
      try
        VConverter := FMainMap.GeoConvert;
        VLonLat.X := ANewPos.X;
        VLonLat.Y := ANewPos.Y;
        VConverter.CheckLonLatPos(VLonLat);
        FCenterPos := VConverter.LonLat2Pos(VLonLat, FZoom);
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
end;

procedure TMapViewPortState.ChangeLonLatAndUnlock(ANewPos: TExtendedPoint);
var
  VLonLat: TExtendedPoint;
  VConverter: ICoordConverter;
begin
  FSync.BeginWrite;
  try
    if FWriteLocked then begin
      try
        VConverter := FMainMap.GeoConvert;
        VLonLat := ANewPos;
        VConverter.CheckLonLatPos(VLonLat);
        FCenterPos := VConverter.LonLat2Pos(VLonLat, FZoom);
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
end;

procedure TMapViewPortState.ChangeMainMapAtCurrentPoint(AMainMap: TMapType);
var
  VLonLat: TExtendedPoint;
  VConverterOld: ICoordConverter;
  VConverterNew: ICoordConverter;
  VNewPos: TPoint;
  VMessage: IJclNotificationMessage;
  VOldSelected: TMapType;
begin
  if AMainMap = nil then begin
    raise Exception.Create('Нужно обязательно указывать активную карту');
  end;
  FSync.BeginWrite;
  try
    VConverterOld := FMainMap.GeoConvert;
    VLonLat := VConverterOld.PixelPos2LonLat(FCenterPos, FZoom);
    VConverterNew := AMainMap.GeoConvert;
    VNewPos := VConverterNew.LonLat2PixelPos(VLonLat, FZoom);
    FMainMap := AMainMap;
    FCenterPos := VNewPos;
    if VOldSelected <> FMainMap then begin
      VMessage := TMapChangeMessage.Create(VOldSelected, FMainMap);
      FMainMapChangeNotifier.Notify(VMessage);
      VMessage := nil;
    end;
  finally
    FSync.EndWrite;
  end;
end;

procedure TMapViewPortState.ChangeMapPixelPosAndUnlock(ANewPos: TPoint);
var
  VNewPos: TPoint;
begin
  FSync.BeginWrite;
  try
    if FWriteLocked then begin
      try
        VNewPos := ANewPos;
        FMainMap.GeoConvert.CheckPixelPosStrict(VNewPos, FZoom, True);
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
end;

procedure TMapViewPortState.ChangeViewSizeAndUnlock(ANewSize: TPoint);
begin
  if FViewSize.X <= 0 then begin
    raise Exception.Create('Ошибочный размер отображаемой карты');
  end;
  if FViewSize.X >= 4096 then begin
    raise Exception.Create('Ошибочный размер отображаемой карты');
  end;
  if FViewSize.Y <= 0 then begin
    raise Exception.Create('Ошибочный размер отображаемой карты');
  end;
  if FViewSize.Y <= 4096 then begin
    raise Exception.Create('Ошибочный размер отображаемой карты');
  end;
  FSync.BeginWrite;
  try
    if FWriteLocked then begin
      try
        FViewSize := ANewSize;
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
end;

procedure TMapViewPortState.ChangeZoomAndUnlock(ANewZoom: Byte;
  ANewPos: TPoint);
var
  VZoom: Byte;
  VNewPos: TPoint;
  VConverter: ICoordConverter;
begin
  FSync.BeginWrite;
  try
    if FWriteLocked then begin
      try
        VZoom := ANewZoom;
        VNewPos := ANewPos;
        VConverter := FMainMap.GeoConvert;
        VConverter.CheckZoom(VZoom);
        VConverter.CheckPixelPos(VNewPos, VZoom, True);
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
begin
  FSync.BeginWrite;
  try
    if FWriteLocked then begin
      try
        VZoom := ANewZoom;
        VNewPos := ANewPos;
        VConverter := FMainMap.GeoConvert;
        VConverter.CheckZoom(VZoom);
        VConverter.CheckLonLatPos(VNewPos);
        FZoom := VZoom;
        FCenterPos := VConverter.LonLat2PixelPos(VNewPos, FZoom);
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
begin
  FSync.BeginWrite;
  try
    VConverter := FMainMap.GeoConvert;
    VZoom := FZoom;
    VNewPos.X := FCenterPos.X + ADelta.X;
    VNewPos.Y := FCenterPos.Y + ADelta.Y;
    VConverter.CheckPixelPosStrict(VNewPos, VZoom, True);
    FCenterPos := VNewPos;
  finally
    FSync.EndWrite;
  end;
end;

procedure TMapViewPortState.ChangeZoomWithFreezeAtVisualPoint(
  AFreezePoint: TPoint);
begin

end;

procedure TMapViewPortState.NotyfyChangePos;
var
  VMessage: IJclNotificationMessage;
begin
  VMessage := TPosChangeMessage.Create(FMainMap, FZoom, FCenterPos);
  FPosChangeNotifier.Notify(VMessage);
end;

end.

