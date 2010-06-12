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
    FScreenSize: TPoint;
    FSync: TMultiReadExclusiveWriteSynchronizer;
    FWriteLocked: Boolean;
  public
    constructor Create(AMainMap: TMapType; AZoom: Byte; ACenterPos: TPoint; AScreenSize: TPoint);
    destructor Destroy; override;

    procedure LockRead;
    procedure LockWrite;
    procedure UnLockRead;
    procedure UnLockWrite;
    procedure ChangeMapPixelPosAndUnlock(ANewPos: TPoint);
    procedure ChangeLonLatAndUnlock(ANewPos: TDoublePoint); overload;
    procedure ChangeLonLatAndUnlock(ANewPos: TExtendedPoint); overload;
    procedure ChangeZoomAndUnlock(ANewZoom: Byte; ANewPos: TPoint); overload;
    procedure ChangeZoomAndUnlock(ANewZoom: Byte; ANewPos: TDoublePoint); overload;
    procedure ChangeZoomAndUnlock(ANewZoom: Byte; ANewPos: TExtendedPoint); overload;
    procedure ChangeMainMapAndUnlock(AMainMap: TMapType);
    procedure ChangeScreenSizeAndUnlock(ANewSize: TPoint);

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
  end;

implementation

{ TMapViewPortState }

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

procedure TMapViewPortState.ChangeMainMapAndUnlock(AMainMap: TMapType);
begin
  if AMainMap = nil then begin
    raise Exception.Create();
  end;
  FSync.BeginWrite;
  try
    if FWriteLocked then begin
      try
        FMainMap := AMainMap;
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

procedure TMapViewPortState.ChangeScreenSizeAndUnlock(ANewSize: TPoint);
begin
  if FScreenSize.X <= 0 then begin
    raise Exception.Create('Ошибочный размер отображаемой карты');
  end;
  if FScreenSize.X >= 4096 then begin
    raise Exception.Create('Ошибочный размер отображаемой карты');
  end;
  if FScreenSize.Y <= 0 then begin
    raise Exception.Create('Ошибочный размер отображаемой карты');
  end;
  if FScreenSize.Y <= 4096 then begin
    raise Exception.Create('Ошибочный размер отображаемой карты');
  end;
  FSync.BeginWrite;
  try
    if FWriteLocked then begin
      try
        FScreenSize := ANewSize;
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

constructor TMapViewPortState.Create(AMainMap: TMapType; AZoom: Byte; ACenterPos: TPoint; AScreenSize: TPoint);
var
  VConverter: ICoordConverter;
begin
  if AMainMap = nil then begin
    raise Exception.Create('Нужно обязательно указывать активную карту');
  end;
  FMainMap := AMainMap;
  VConverter := FMainMap.GeoConvert;
  FZoom := AZoom;
  VConverter.CheckZoom(FZoom);
  FCenterPos := ACenterPos;
  VConverter.CheckPixelPosStrict(FCenterPos, FZoom, True);
  FScreenSize := AScreenSize;
  if FScreenSize.X <= 0 then begin
    FScreenSize.X := 1024;
  end;
  if FScreenSize.X >= 4096 then begin
    FScreenSize.X := 1024;
  end;
  if FScreenSize.Y <= 0 then begin
    FScreenSize.Y := 768;
  end;
  if FScreenSize.Y <= 4096 then begin
    FScreenSize.Y := 768;
  end;

  FSync := TMultiReadExclusiveWriteSynchronizer.Create;
  FWriteLocked := False;
end;

destructor TMapViewPortState.Destroy;
begin
  FSync.BeginWrite;
  FreeAndNil(FSync);
  inherited;
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
    Result.Left := FCenterPos.X - FScreenSize.X div 2;
    Result.Top := FCenterPos.Y - FScreenSize.Y div 2;
    Result.Right := Result.Left + FScreenSize.X;
    Result.Bottom := Result.Top + FScreenSize.Y;
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
    VRect.Left := FCenterPos.X - FScreenSize.X div 2;
    VRect.Top := FCenterPos.Y - FScreenSize.Y div 2;
    VRect.Right := VRect.Left + FScreenSize.X;
    VRect.Bottom := VRect.Top + FScreenSize.Y;
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
    Result.Left := FCenterPos.X - FScreenSize.X div 2;
    Result.Top := FCenterPos.Y - FScreenSize.Y div 2;
    Result.Right := Result.Left + FScreenSize.X;
    Result.Bottom := Result.Top + FScreenSize.Y;
  finally
    FSync.EndRead;
  end;
end;

function TMapViewPortState.GetVisibleSizeInPixel: TPoint;
begin
  FSync.BeginRead;
  try
    Result := FScreenSize;
  finally
    FSync.EndRead;
  end;
end;

function TMapViewPortState.GetVisibleTopLeft: TPoint;
begin
  FSync.BeginRead;
  try
    Result.X := FCenterPos.X - FScreenSize.X div 2;
    Result.Y := FCenterPos.Y - FScreenSize.Y div 2;
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
    Result.X := Pnt.X - FCenterPos.X + FScreenSize.X div 2;
    Result.Y := Pnt.Y - FCenterPos.Y + FScreenSize.Y div 2;
  finally
    FSync.EndRead;
  end;
end;

function TMapViewPortState.MapPixel2VisiblePixel(Pnt: TPoint): TPoint;
begin
  FSync.BeginRead;
  try
    Result.X := Pnt.X - FCenterPos.X + FScreenSize.X div 2;
    Result.Y := Pnt.Y - FCenterPos.Y + FScreenSize.Y div 2;
  finally
    FSync.EndRead;
  end;
end;

function TMapViewPortState.VisiblePixel2MapPixel(Pnt: TPoint): TPoint;
begin
  FSync.BeginRead;
  try
    Result.X := Pnt.X + FCenterPos.X - FScreenSize.X div 2;
    Result.Y := Pnt.Y + FCenterPos.Y - FScreenSize.Y div 2;
  finally
    FSync.EndRead;
  end;
end;

function TMapViewPortState.VisiblePixel2MapPixel(
  Pnt: TExtendedPoint): TExtendedPoint;
begin
  FSync.BeginRead;
  try
    Result.X := Pnt.X + FCenterPos.X - FScreenSize.X div 2;
    Result.Y := Pnt.Y + FCenterPos.Y - FScreenSize.Y div 2;
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

end.

