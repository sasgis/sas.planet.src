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
      const APseudoRandomCode: Integer;
      const AElevation: Integer;
      const AAzimuth: Integer;
      const ASignalToNoiseRatio: Integer;
      const AIsFix: Boolean
    ): IGPSSatelliteInfo;

    function BuildSatellitesInViewEmpty: IGPSSatellitesInView;
    function BuildSatellitesInView(
      const AFixCount: Integer;
      const AItemsCount: Integer;
      const AItems: PUnknownList
    ): IGPSSatellitesInView;

    function BuildPositionEmpty: IGPSPosition;
    function BuildPosition(
      const APosition: TDoublePoint;
      const AAltitude: Double;
      const ASpeed_KMH: Double;
      const AHeading: Double;
      const AUTCDateTime: TDateTime;
      const ALocalDateTime: TDateTime;
      const AIsFix: Word;
      const AHDOP: Double;
      const AVDOP: Double;
      const APDOP: Double;
      const ASatellites: IGPSSatellitesInView
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

function TGPSPositionFactory.BuildPosition(
  const APosition: TDoublePoint;
  const AAltitude, ASpeed_KMH, AHeading: Double;
  const AUTCDateTime, ALocalDateTime: TDateTime;
  const AIsFix: Word;
  const AHDOP, AVDOP, APDOP: Double;
  const ASatellites: IGPSSatellitesInView
): IGPSPosition;
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
  const APseudoRandomCode, AElevation, AAzimuth, ASignalToNoiseRatio: Integer;
  const AIsFix: Boolean
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

function TGPSPositionFactory.BuildSatellitesInView(
  const AFixCount, AItemsCount: Integer;
  const AItems: PUnknownList
): IGPSSatellitesInView;
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
