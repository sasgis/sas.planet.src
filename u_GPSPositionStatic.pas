unit u_GPSPositionStatic;

interface

uses
  SysUtils,
  t_GeoTypes,
  i_GPS;

type
  TGPSPositionStatic = class(TInterfacedObject, IGPSPosition)
  private
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
    FSatellites: IGPSSatellitesInView;
  protected
    function GetPosition: TExtendedPoint; stdcall;
    function GetAltitude: Extended; stdcall;
    function GetSpeed_KMH: Extended; stdcall;
    function GetHeading: Extended; stdcall;
    function GetUTCDateTime: TDateTime; stdcall;
    function GetLocalDateTime: TDateTime; stdcall;
    function GetIsFix: Word; stdcall;
    function GetHDOP: Extended; stdcall;
    function GetVDOP: Extended; stdcall;
    function GetPDOP: Extended; stdcall;
    function GetSatellites: IGPSSatellitesInView; stdcall;
  public
    constructor Create(
      APosition: TExtendedPoint;
      AAltitude: Extended;
      ASpeed_KMH: Extended;
      AHeading: Extended;
      AUTCDateTime: TDateTime;
      ALocalDateTime: TDateTime;
      AIsFix: Word;
      AHDOP: Extended;
      AVDOP: Extended;
      APDOP: Extended;
      ASatellites: IGPSSatellitesInView
    );
    destructor Destroy; override;
  end;

implementation

{ TGPSPosition }

constructor TGPSPositionStatic.Create(
  APosition: TExtendedPoint; AAltitude, ASpeed_KMH, AHeading: Extended;
  AUTCDateTime, ALocalDateTime: TDateTime; AIsFix: Word; AHDOP, AVDOP,
  APDOP: Extended; ASatellites: IGPSSatellitesInView);
begin
  FPosition := APosition;
  FAltitude := AAltitude;
  FSpeed_KMH := ASpeed_KMH;
  FHeading := AHeading;
  FUTCDateTime := AUTCDateTime;
  FLocalDateTime := ALocalDateTime;
  FIsFix := AIsFix;
  FHDOP := AHDOP;
  FVDOP := AVDOP;
  FPDOP := APDOP;
  FSatellites := ASatellites;
end;

destructor TGPSPositionStatic.Destroy;
begin
  FSatellites := nil;
  inherited;
end;

function TGPSPositionStatic.GetAltitude: Extended;
begin
  Result := FAltitude;
end;

function TGPSPositionStatic.GetHDOP: Extended;
begin
  Result := FHDOP;
end;

function TGPSPositionStatic.GetHeading: Extended;
begin
  Result := FHeading;
end;

function TGPSPositionStatic.GetIsFix: Word;
begin
  Result := FIsFix;
end;

function TGPSPositionStatic.GetLocalDateTime: TDateTime;
begin
  Result := FLocalDateTime;
end;

function TGPSPositionStatic.GetPDOP: Extended;
begin
  Result := FPDOP;
end;

function TGPSPositionStatic.GetPosition: TExtendedPoint;
begin
  Result := FPosition;
end;

function TGPSPositionStatic.GetSatellites: IGPSSatellitesInView;
begin
  Result := FSatellites;
end;

function TGPSPositionStatic.GetSpeed_KMH: Extended;
begin
  Result := FSpeed_KMH;
end;

function TGPSPositionStatic.GetUTCDateTime: TDateTime;
begin
  Result := FUTCDateTime;
end;

function TGPSPositionStatic.GetVDOP: Extended;
begin
  Result := FVDOP;
end;

end.
