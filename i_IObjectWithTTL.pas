unit i_IObjectWithTTL;

interface

uses
  Types;

type
  IObjectWithTTL = interface
  ['']
    function GetNextCheckTime: TDateTime;
    procedure TrimByTTL;
  end;
implementation

end.
