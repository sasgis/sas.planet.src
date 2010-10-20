unit u_GPSPositionUpdatable;

interface

uses
  Classes,
  SyncObjs,
  t_GeoTypes,
  i_GPS;

type
  TGPSPositionUpdatable = class
  private
    FCS: TCriticalSection;
    FPosChanged: Boolean;
    FSatellitesChanged: Boolean;
    FLastStaticPosition: IGPSPosition;

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
    procedure Lock;
    procedure UnLock;
  public
    constructor Create();
    destructor Destroy; override;
    function GetPosition: IGPSPosition;
  end;

implementation

uses
  SysUtils,
  u_GPSPositionStatic,
  u_GPSSatellitesInView,
  u_GPSSatelliteInfo;

{ TGPSPositionUpdatable }

constructor TGPSPositionUpdatable.Create;
begin
  FCS := TCriticalSection.Create;
  FSatellites := TInterfaceList.Create;
  FSatellites.Capacity := 32;
  FPosChanged := True;
  FSatellitesChanged := True;
  FLastStaticPosition := nil;
end;

destructor TGPSPositionUpdatable.Destroy;
begin
  FreeAndNil(FCS);
  FSatellites := nil;
  FLastStaticPosition := nil;
  inherited;
end;

function TGPSPositionUpdatable.GetPosition: IGPSPosition;
begin
  FCS.Acquire;
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
    FCS.Release;
  end;
end;

procedure TGPSPositionUpdatable.Lock;
begin
  FCS.Acquire;
end;

procedure TGPSPositionUpdatable.UnLock;
begin
  FCS.Release;
end;

function TGPSPositionUpdatable._GetSatellitesCopy: IInterfaceList;
var
  i: Integer;
begin
  Result := TInterfaceList.Create;
  Result.Capacity := FSatellites.Count;
  for i := 0 to FSatellites.Count - 1 do begin
    Result.Add(FSatellites[i]);
  end;
end;

procedure TGPSPositionUpdatable._UpdatePosition(APosition: TExtendedPoint;
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

procedure TGPSPositionUpdatable._UpdateSatellitesCount(AFixCount,
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

procedure TGPSPositionUpdatable._UpdateSattelite(AIndex, APseudoRandomCode,
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
    FSatellites[AIndex] := TGPSSatelliteInfo.Create(
      APseudoRandomCode,
      AElevation,
      AAzimuth,
      ASignalToNoiseRatio,
      AIsFix
    );
    FSatellitesChanged := True;
  end;
end;

end.
