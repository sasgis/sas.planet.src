unit u_GPSPositionStatic;

interface

uses
  SysUtils,
  t_GeoTypes,
  i_GPS;

type
  TGPSPositionStatic = class(TInterfacedObject, IGPSPosition)
  private
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
    FSatellites: IGPSSatellitesInView;
  protected
    function GetPosition: TDoublePoint; stdcall;
    function GetAltitude: Double; stdcall;
    function GetSpeed_KMH: Double; stdcall;
    function GetHeading: Double; stdcall;
    function GetUTCDateTime: TDateTime; stdcall;
    function GetLocalDateTime: TDateTime; stdcall;
    function GetIsFix: Word; stdcall;
    function GetHDOP: Double; stdcall;
    function GetVDOP: Double; stdcall;
    function GetPDOP: Double; stdcall;
    function GetSatellites: IGPSSatellitesInView; stdcall;
  public
    constructor Create(
      APosition: TDoublePoint;
      AAltitude: Double;
      ASpeed_KMH: Double;
      AHeading: Double;
      AUTCDateTime: TDateTime;
      ALocalDateTime: TDateTime;
      AIsFix: Word;
      AHDOP: Double;
      AVDOP: Double;
      APDOP: Double;
      ASatellites: IGPSSatellitesInView
    );
    destructor Destroy; override;
  end;

implementation

{ TGPSPosition }

constructor TGPSPositionStatic.Create(
  APosition: TDoublePoint; AAltitude, ASpeed_KMH, AHeading: Double;
  AUTCDateTime, ALocalDateTime: TDateTime; AIsFix: Word; AHDOP, AVDOP,
  APDOP: Double; ASatellites: IGPSSatellitesInView);
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

function TGPSPositionStatic.GetAltitude: Double;
begin
  Result := FAltitude;
end;

function TGPSPositionStatic.GetHDOP: Double;
begin
  Result := FHDOP;
end;

function TGPSPositionStatic.GetHeading: Double;
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

function TGPSPositionStatic.GetPDOP: Double;
begin
  Result := FPDOP;
end;

function TGPSPositionStatic.GetPosition: TDoublePoint;
begin
  Result := FPosition;
end;

function TGPSPositionStatic.GetSatellites: IGPSSatellitesInView;
begin
  Result := FSatellites;
end;

function TGPSPositionStatic.GetSpeed_KMH: Double;
begin
  Result := FSpeed_KMH;
end;

function TGPSPositionStatic.GetUTCDateTime: TDateTime;
begin
  Result := FUTCDateTime;
end;

function TGPSPositionStatic.GetVDOP: Double;
begin
  Result := FVDOP;
end;

end.
