unit u_GPSPositionFactory;

interface

uses
  ActiveX,
  t_GeoTypes,
  i_GPS;

type
  TGPSPositionFactory = class(TInterfacedObject, IGPSPositionFactory)
  private
    FSatellitesInViewEmpty: IGPSSatellitesInView;
    FPositionEmpty: IGPSPosition;
  protected
    function BuildSatelliteInfo(
      APseudoRandomCode: Integer;
      AElevation: Integer;
      AAzimuth: Integer;
      ASignalToNoiseRatio: Integer;
      AIsFix: Boolean
    ): IGPSSatelliteInfo;

    function BuildSatellitesInViewEmpty: IGPSSatellitesInView;
    function BuildSatellitesInView(
      AFixCount: Integer;
      AItemsCount: Integer;
      AItems: PUnknownList
    ): IGPSSatellitesInView;

    function BuildPositionEmpty: IGPSPosition;
    function BuildPosition(
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
    ): IGPSPosition;
  public
    constructor Create;
  end;

implementation

uses
  u_GPSPositionStatic,
  u_GPSSatellitesInView,
  u_GPSSatelliteInfo;

{ TGPSPositionFactory }

constructor TGPSPositionFactory.Create;
var
  VPoint: TDoublePoint;
begin
  FSatellitesInViewEmpty := TGPSSatellitesInView.Create(0, 0, nil);

  VPoint.X := 0;
  VPoint.Y := 0;
  FPositionEmpty :=
    TGPSPositionStatic.Create(
      VPoint, 0, 0, 0, 0, 0, 0, 0, 0, 0, FSatellitesInViewEmpty
    );
end;

function TGPSPositionFactory.BuildPosition(APosition: TDoublePoint; AAltitude,
  ASpeed_KMH, AHeading: Double; AUTCDateTime, ALocalDateTime: TDateTime;
  AIsFix: Word; AHDOP, AVDOP, APDOP: Double;
  ASatellites: IGPSSatellitesInView): IGPSPosition;
begin
  Result :=
    TGPSPositionStatic.Create(
      APosition,
      AAltitude,
      ASpeed_KMH,
      AHeading,
      AUTCDateTime,
      ALocalDateTime,
      AIsFix,
      AHDOP,
      AVDOP,
      APDOP,
      ASatellites
    );
end;

function TGPSPositionFactory.BuildPositionEmpty: IGPSPosition;
begin
  Result := FPositionEmpty;
end;

function TGPSPositionFactory.BuildSatelliteInfo(
  APseudoRandomCode, AElevation, AAzimuth, ASignalToNoiseRatio: Integer;
  AIsFix: Boolean
): IGPSSatelliteInfo;
begin
  Result :=
    TGPSSatelliteInfo.Create(
      APseudoRandomCode,
      AElevation,
      AAzimuth,
      ASignalToNoiseRatio,
      AIsFix
    );
end;

function TGPSPositionFactory.BuildSatellitesInView(AFixCount,
  AItemsCount: Integer; AItems: PUnknownList): IGPSSatellitesInView;
begin
  Result :=
    TGPSSatellitesInView.Create(
      AFixCount,
      AItemsCount,
      AItems
    )
end;

function TGPSPositionFactory.BuildSatellitesInViewEmpty: IGPSSatellitesInView;
begin
  Result := FSatellitesInViewEmpty;
end;

end.
