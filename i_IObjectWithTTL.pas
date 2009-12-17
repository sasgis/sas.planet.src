unit i_IObjectWithTTL;

interface

uses
  Types;

type
  IObjectWithTTL = interface
  ['{1DA8EB6F-499D-4FB7-9E3F-5AC865E7D044}']
    function GetNextCheckTime: Cardinal;
    procedure TrimByTTL;
  end;

implementation

end.
