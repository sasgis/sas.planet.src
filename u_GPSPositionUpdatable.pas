unit u_GPSPositionUpdatable;

interface

uses
  t_GeoTypes,
  i_GPS,
  u_GPSSatellitesInView;

type
  TGPSPositionUpdatable = class
  protected
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
    FSatellites: TArrayOfIGPSSatelliteInfo;
  public
    function GetPosition: IGPSPosition;
  end;

implementation

{ TGPSPositionUpdatable }

function TGPSPositionUpdatable.GetPosition: IGPSPosition;
begin

end;

end.
