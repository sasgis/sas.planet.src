unit i_BinaryData;

interface

type
  IBinaryData = interface
    ['{C83111F6-4983-4CF7-9FEB-A3EAAF60B5A3}']
    function GetBuffer: Pointer;
    property Buffer: Pointer read GetBuffer;

    function GetSize: Integer;
    property Size: Integer read GetSize;
  end;

implementation

end.
