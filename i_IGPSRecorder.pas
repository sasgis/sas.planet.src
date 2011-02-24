unit i_IGPSRecorder;

interface

uses
  t_GeoTypes,
  i_GPS,
  i_IConfigDataElement;

type
  TGPSTrackPoint = record
    Point: TDoublePoint;
    Speed: Double;
  end;

  TGPSTrackPointArray = array of TGPSTrackPoint;

  IGPSRecorder = interface(IConfigDataElement)
    ['{E8525CFD-243B-4454-82AA-C66108A74B8F}']
    procedure ClearTrack;
    function IsEmpty: Boolean;
    function LastPoints(ACount: Integer): TGPSTrackPointArray;
    function GetAllPoints: TDoublePointArray;
    function GetAllTracPoints: TGPSTrackPointArray;

    function GetOdometer1: Double;
    procedure ResetOdometer1;
    function GetOdometer2: Double;
    procedure ResetOdometer2;
    function GetDist: Double;
    procedure ResetDist;
    function GetMaxSpeed: Double;
    procedure ResetMaxSpeed;
    function GetAvgSpeed: Double;
    procedure ResetAvgSpeed;
    function GetLastSpeed: Double;
    function GetLastAltitude: Double;
    function GetLastHeading: Double;
    function GetLastPosition: TDoublePoint;
  end;

implementation

end.
