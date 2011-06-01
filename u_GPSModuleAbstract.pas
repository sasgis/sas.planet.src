unit u_GPSModuleAbstract;

interface

uses
  Classes,
  SyncObjs,
  i_JclNotify,
  t_GeoTypes,
  i_GPS,
  i_GPSModule;

type
  TGPSModuleAbstract = class(TInterfacedObject, IGPSModule)
  private
    FCS: TCriticalSection;
    FPosChanged: Boolean;
    FSatellitesChanged: Boolean;
    FLastStaticPosition: IGPSPosition;
    FEmptyDataPosition: IGPSPosition;

    FPosition: TDoublePoint;
    FAltitude: Double;
    FSpeed_KMH: Double;
    FHeading: Double;
    FUTCDateTime: TDateTime;
    FLocalDateTime: TDateTime;
    FIsFix: Word;
    FHDOP: Double;
    FVDOP: Double;
    FPDOP: Double;
    FFixCount: Integer;
    FSatellites: IInterfaceList;

    FDataReciveNotifier: IJclNotifier;

    FConnectingNotifier: IJclNotifier;
    FConnectedNotifier: IJclNotifier;
    FDisconnectingNotifier: IJclNotifier;
    FDisconnectedNotifier: IJclNotifier;

    FConnectErrorNotifier: IJclNotifier;
    FTimeOutNotifier: IJclNotifier;
  protected
    procedure _UpdatePosition(
      APosition: TDoublePoint;
      AAltitude: Double;
      ASpeed_KMH: Double;
      AHeading: Double;
      AUTCDateTime: TDateTime;
      ALocalDateTime: TDateTime;
      AIsFix: Word;
      AHDOP: Double;
      AVDOP: Double;
      APDOP: Double
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
    function GetPosition: IGPSPosition; virtual; safecall;

    function GetDataReciveNotifier: IJclNotifier; virtual; safecall;

    function GetConnectingNotifier: IJclNotifier; virtual; safecall;
    function GetConnectedNotifier: IJclNotifier; virtual; safecall;
    function GetDisconnectingNotifier: IJclNotifier; virtual; safecall;
    function GetDisconnectedNotifier: IJclNotifier; virtual; safecall;

    function GetConnectErrorNotifier: IJclNotifier; virtual; safecall;
    function GetTimeOutNotifier: IJclNotifier; virtual; safecall;
  public
    constructor Create();
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  u_JclNotify,
  u_EnumUnknown,
  u_GPSPositionStatic,
  u_GPSSatellitesInView,
  u_GPSSatelliteInfo;

{ TGPSPositionUpdatable }

constructor TGPSModuleAbstract.Create;
var
  VPoint: TDoublePoint;
begin
  FCS := TCriticalSection.Create;
  FSatellites := TInterfaceList.Create;
  FSatellites.Capacity := 32;
  FPosChanged := True;
  FSatellitesChanged := True;
  FLastStaticPosition := nil;
  FConnectErrorNotifier := TJclBaseNotifier.Create;

  FConnectingNotifier := TJclBaseNotifier.Create;
  FConnectedNotifier := TJclBaseNotifier.Create;
  FDisconnectingNotifier := TJclBaseNotifier.Create;
  FDisconnectedNotifier := TJclBaseNotifier.Create;

  FDataReciveNotifier := TJclBaseNotifier.Create;
  FTimeOutNotifier := TJclBaseNotifier.Create;
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

  FConnectingNotifier := nil;
  FConnectedNotifier := nil;
  FDisconnectingNotifier := nil;
  FDisconnectedNotifier := nil;

  FConnectErrorNotifier := nil;
  FDataReciveNotifier := nil;
  FTimeOutNotifier := nil;
  inherited;
end;

function TGPSModuleAbstract.GetConnectErrorNotifier: IJclNotifier;
begin
  Result := FConnectErrorNotifier;
end;

function TGPSModuleAbstract.GetConnectingNotifier: IJclNotifier;
begin
  Result := FConnectingNotifier;
end;

function TGPSModuleAbstract.GetConnectedNotifier: IJclNotifier;
begin
  Result := FConnectedNotifier;
end;

function TGPSModuleAbstract.GetDisconnectingNotifier: IJclNotifier;
begin
  Result := FDisconnectingNotifier;
end;

function TGPSModuleAbstract.GetDisconnectedNotifier: IJclNotifier;
begin
  Result := FDisconnectedNotifier;
end;

function TGPSModuleAbstract.GetDataReciveNotifier: IJclNotifier;
begin
  Result := FDataReciveNotifier;
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
            TEnumUnknown.Create(FSatellites)
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

procedure TGPSModuleAbstract._UpdatePosition(APosition: TDoublePoint;
  AAltitude, ASpeed_KMH, AHeading: Double; AUTCDateTime,
  ALocalDateTime: TDateTime; AIsFix: Word; AHDOP, AVDOP, APDOP: Double);
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
