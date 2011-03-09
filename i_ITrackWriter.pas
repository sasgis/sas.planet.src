unit i_ITrackWriter;

interface

uses
  i_GPS;

type
  ITrackWriter = interface
    ['{1F62E0F1-F6BD-4DCF-A022-E898920B1B9B}']
    procedure StartWrite;
    procedure AddPoint(APosition: IGPSPosition);
    procedure CloseLog;
  end;

implementation

end.
