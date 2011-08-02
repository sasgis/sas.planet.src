unit i_MapVersionInfo;

interface

type
  IMapVersionInfo = interface
    ['{CC157D46-11DA-4035-963B-2F0BEAEA265A}']
    function GetVersion: Variant;
    property Version: Variant read GetVersion;
  end;

implementation

end.
