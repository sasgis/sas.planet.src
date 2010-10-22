unit u_GPSModuleAbstract;

interface

uses
  Classes,
  SyncObjs,
  i_JclNotify,
  t_GeoTypes,
  i_GPS,
  i_IGPSModule;

type
  TGPSModuleAbstract = class(TInterfacedObject, IGPSModule)
  private
    FCS: TCriticalSection;
    FPosChanged: Boolean;
    FSatellitesChanged: Boolean;
    FLastStaticPosition: IGPSPosition;
    FEmptyDataPosition: IGPSPosition;

    FPosition: TExtendedPoint;
    FAltitude: Extended;
    FSpeed_KMH: Extended;
    FHeading: Extended;
    FUTCDateTime: TDateTime;
    FLocalDateTime: TDateTime;
    FIsFix: Word;
    FHDOP: Extended;
    FVDOP: Extended;
    FPDOP: Extended;
    FFixCount: Integer;
    FSatellites: IInterfaceList;

    FDataReciveNotifier: IJclNotifier;
    FConnectErrorNotifier: IJclNotifier;
    FConnectNotifier: IJclNotifier;
    FDisconnectNotifier: IJclNotifier;
    FTimeOutNotifier: IJclNotifier;
  protected
    function _GetSatellitesCopy: IInterfaceList;
    procedure _UpdatePosition(
      APosition: TExtendedPoint;
      AAltitude: Extended;
      ASpeed_KMH: Extended;
      AHeading: Extended;
      AUTCDateTime: TDateTime;
      ALocalDateTime: TDateTime;
      AIsFix: Word;
      AHDOP: Extended;
      AVDOP: Extended;
      APDOP: Extended
    );
    procedure _UpdateSatellitesCount(
      AFixCount: Integer;
      ACount: Integer
    );
    procedure _UpdateSattelite(
      AIndex: Integer;
      APseudoRandomCode: Integer;
      AElevation: Integer;
      AAzimuth: Integer;
      ASignalToNoiseRatio: Integer;
      AIsFix: Boolean
    );
    procedure _UpdateToEmptyPosition;
    procedure Lock;
    procedure UnLock;
  protected
    procedure Connect; virtual; safecall; abstract;
    procedure Disconnect; virtual; safecall; abstract;
    function GetIsConnected: Boolean; virtual; safecall; abstract;
    function GetPosition: IGPSPosition; virtual; safecall;

    function GetDataReciveNotifier: IJclNotifier; virtual; safecall;
    function GetConnectErrorNotifier: IJclNotifier; virtual; safecall;
    function GetConnectNotifier: IJclNotifier; virtual; safecall;
    function GetDisconnectNotifier: IJclNotifier; virtual; safecall;
    function GetTimeOutNotifier: IJclNotifier; virtual; safecall;
  public
    constructor Create();
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  u_JclNotify,
  u_GPSPositionStatic,
  u_GPSSatellitesInView,
  u_GPSSatelliteInfo;

{ TGPSPositionUpdatable }

constructor TGPSModuleAbstract.Create;
var
  VPoint: TExtendedPoint;
begin
  FCS := TCriticalSection.Create;
  FSatellites := TInterfaceList.Create;
  FSatellites.Capacity := 32;
  FPosChanged := True;
  FSatellitesChanged := True;
  FLastStaticPosition := nil;
  FConnectErrorNotifier := TJclBaseNotifier.Create;
  FConnectNotifier := TJclBaseNotifier.Create;
  FDataReciveNotifier := TJclBaseNotifier.Create;
  FTimeOutNotifier := TJclBaseNotifier.Create;
  FDisconnectNotifier := TJclBaseNotifier.Create;
  VPoint.X := 0;
  VPoint.Y := 0;
  FEmptyDataPosition := TGPSPositionStatic.Create(
    VPoint, 0, 0, 0, 0, 0, 0, 0, 0, 0, TGPSSatellitesInView.Create(0, nil)
  );
end;

destructor TGPSModuleAbstract.Destroy;
begin
  FreeAndNil(FCS);
  FSatellites := nil;
  FLastStaticPosition := nil;
  FEmptyDataPosition := nil;
  FConnectErrorNotifier := nil;
  FConnectNotifier := nil;
  FDataReciveNotifier := nil;
  FTimeOutNotifier := nil;
  FDisconnectNotifier := nil;
  inherited;
end;

function TGPSModuleAbstract.GetConnectErrorNotifier: IJclNotifier;
begin
  Result := FConnectErrorNotifier;
end;

function TGPSModuleAbstract.GetConnectNotifier: IJclNotifier;
begin
  Result := FConnectNotifier;
end;

function TGPSModuleAbstract.GetDataReciveNotifier: IJclNotifier;
begin
  Result := FDataReciveNotifier;
end;

function TGPSModuleAbstract.GetDisconnectNotifier: IJclNotifier;
begin
  Result := FDisconnectNotifier;
end;

function TGPSModuleAbstract.GetPosition: IGPSPosition;
begin
  Lock;
  try
    if FSatellitesChanged or FPosChanged then begin
      if not FSatellitesChanged then begin
        Result := TGPSPositionStatic.Create(
          FPosition,
          FAltitude,
          FSpeed_KMH,
          FHeading,
          FUTCDateTime,
          FLocalDateTime,
          FIsFix,
          FHDOP,
          FVDOP,
          FPDOP,
          FLastStaticPosition.Satellites
        );
      end else begin
        Result := TGPSPositionStatic.Create(
          FPosition,
          FAltitude,
          FSpeed_KMH,
          FHeading,
          FUTCDateTime,
          FLocalDateTime,
          FIsFix,
          FHDOP,
          FVDOP,
          FPDOP,
          TGPSSatellitesInView.Create(
            FFixCount,
            _GetSatellitesCopy
          )
        );
      end;
      FLastStaticPosition := Result;
    end else begin
      Result := FLastStaticPosition;
    end;
  finally
    UnLock;
  end;
end;

function TGPSModuleAbstract.GetTimeOutNotifier: IJclNotifier;
begin
  Result := FTimeOutNotifier;
end;

procedure TGPSModuleAbstract.Lock;
begin
  FCS.Acquire;
end;

procedure TGPSModuleAbstract.UnLock;
begin
  FCS.Release;
end;

function TGPSModuleAbstract._GetSatellitesCopy: IInterfaceList;
var
  i: Integer;
begin
  Result := TInterfaceList.Create;
  Result.Capacity := FSatellites.Count;
  for i := 0 to FSatellites.Count - 1 do begin
    Result.Add(FSatellites[i]);
  end;
end;

procedure TGPSModuleAbstract._UpdatePosition(APosition: TExtendedPoint;
  AAltitude, ASpeed_KMH, AHeading: Extended; AUTCDateTime,
  ALocalDateTime: TDateTime; AIsFix: Word; AHDOP, AVDOP, APDOP: Extended);
begin
  if FPosition.X <> APosition.X then begin
    FPosition.X := APosition.X;
    FPosChanged := True;
  end;
  if FPosition.Y <> APosition.Y then begin
    FPosition.Y := APosition.Y;
    FPosChanged := True;
  end;

  if FAltitude <> AAltitude then begin
    FAltitude := AAltitude;
    FPosChanged := True;
  end;

  if FSpeed_KMH <> ASpeed_KMH then begin
    FSpeed_KMH := ASpeed_KMH;
    FPosChanged := True;
  end;

  if FHeading <> AHeading then begin
    FHeading := AHeading;
    FPosChanged := True;
  end;

  if FUTCDateTime <> AUTCDateTime then begin
    FUTCDateTime := AUTCDateTime;
    FPosChanged := True;
  end;

  if FLocalDateTime <> ALocalDateTime then begin
    FLocalDateTime := ALocalDateTime;
    FPosChanged := True;
  end;

  if FIsFix <> AIsFix then begin
    FIsFix := AIsFix;
    FPosChanged := True;
  end;

  if FHDOP <> AHDOP then begin
    FHDOP := AHDOP;
    FPosChanged := True;
  end;

  if FVDOP <> AVDOP then begin
    FVDOP := AVDOP;
    FPosChanged := True;
  end;

  if FPDOP <> APDOP then begin
    FPDOP := APDOP;
    FPosChanged := True;
  end;
end;

procedure TGPSModuleAbstract._UpdateSatellitesCount(AFixCount,
  ACount: Integer);
begin
  if FFixCount <> AFixCount then begin
    FFixCount := AFixCount;
    FSatellitesChanged := True;
  end;
  if FSatellites.Count <> ACount then begin
    FSatellites.Count := ACount;
    FSatellitesChanged := True;
  end;
end;

procedure TGPSModuleAbstract._UpdateSattelite(AIndex, APseudoRandomCode,
  AElevation, AAzimuth, ASignalToNoiseRatio: Integer; AIsFix: Boolean);
var
  VSattelite: IGPSSatelliteInfo;
  VSatteliteChanged: Boolean;
begin
  VSattelite := IGPSSatelliteInfo(FSatellites[AIndex]);
  VSatteliteChanged := False;
  if VSattelite <> nil then begin
    if VSattelite.PseudoRandomCode <> APseudoRandomCode then begin
      VSatteliteChanged := True;
    end;
    if VSattelite.Elevation <> AElevation then begin
      VSatteliteChanged := True;
    end;
    if VSattelite.Azimuth <> AAzimuth then begin
      VSatteliteChanged := True;
    end;
    if VSattelite.SignalToNoiseRatio <> ASignalToNoiseRatio then begin
      VSatteliteChanged := True;
    end;
    if VSattelite.IsFix <> AIsFix then begin
      VSatteliteChanged := True;
    end;
  end else begin
    VSatteliteChanged := True;
  end;

  if VSatteliteChanged then begin
    VSattelite := TGPSSatelliteInfo.Create(
      APseudoRandomCode,
      AElevation,
      AAzimuth,
      ASignalToNoiseRatio,
      AIsFix
    );
    FSatellites[AIndex] := VSattelite;
    FSatellitesChanged := True;
  end;
end;

procedure TGPSModuleAbstract._UpdateToEmptyPosition;
begin
  FPosChanged := False;
  FSatellitesChanged := False;
  FLastStaticPosition := FEmptyDataPosition;
end;

end.
