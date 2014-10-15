unit i_AppEnum;

interface

type
  IAppEnum = interface
    ['{FDDF70EC-23AA-4770-B160-CA57B5BAB9E7}']
    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetCurrentID: Integer;
    property CurrentID: Integer read GetCurrentID;
  end;

implementation

end.
