unit i_GPS;

interface

uses
  t_GeoTypes;

type
  IGPSSatelliteInfo = interface
    ['{38C3C77F-DAC8-4187-B243-0F7001A7DF9B}']
    function GetPseudoRandomCode: Integer; stdcall;
    function GetElevation: Integer; stdcall;
    function GetAzimuth: Integer; stdcall;
    function GetSignalToNoiseRatio: Integer; stdcall;
    function GetIsFix: Boolean; stdcall;

    property PseudoRandomCode: Integer read GetPseudoRandomCode;
    property Elevation: Integer read GetElevation;
    property Azimuth: Integer read GetAzimuth;
    property SignalToNoiseRatio: Integer read GetSignalToNoiseRatio;
    property IsFix: Boolean read GetIsFix;
  end;

  IGPSSatellitesInView = interface
    ['{D8744967-74EB-47A1-A8FD-4626B5CD2B20}']
    function GetCount: Integer; stdcall;
    function GetFixCount: Integer; stdcall;
    function GetItem(AIndex: Integer): IGPSSatelliteInfo; stdcall;

    property Count: Integer read GetCount;
    property FixCount: Integer read GetFixCount;
    property Item[Idx: Integer]: IGPSSatelliteInfo read GetItem;
  end;

  IGPSPosition = interface
    ['{B2422759-9B8B-4CC5-AAA5-46A7240759D0}']
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

    property Position: TExtendedPoint read GetPosition;
    property Altitude: Extended read GetAltitude;
    property Speed_KMH: Extended read GetSpeed_KMH;
    property Heading: Extended read GetHeading;
    property UTCDateTime: TDateTime read GetUTCDateTime;
    property LocalDateTime: TDateTime read GetLocalDateTime;
    property IsFix: Word read GetIsFix;
    property HDOP: Extended read GetHDOP;
    property VDOP: Extended read GetVDOP;
    property PDOP: Extended read GetPDOP;
    property Satellites: IGPSSatellitesInView  read GetSatellites;
  end;

implementation

end.
