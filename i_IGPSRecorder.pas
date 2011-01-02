unit i_IGPSRecorder;

interface

uses
  t_GeoTypes;

type
  TGPSTrackPoint = record
    Point: TDoublePoint;
    Speed: Double;
  end;

  TGPSTrackPointArray = array of TGPSTrackPoint;

  IGPSRecorder = interface
    ['{E8525CFD-243B-4454-82AA-C66108A74B8F}']
    procedure AddPoint(APoint: TGPSTrackPoint);
    procedure ClearTrack;
    function IsEmpty: Boolean;
    function GetLastPoint: TDoublePoint;
    function GetTwoLastPoints(var APointLast, APointPrev: TDoublePoint): Boolean;
    function LastPoints(ACount: Integer): TGPSTrackPointArray;
    function GetAllPoints: TDoublePointArray;
    function GetAllTracPoints: TGPSTrackPointArray;
  end;

implementation

end.
