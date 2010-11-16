unit i_IMapChangeMessage;

interface

uses
  UMapType;

type
  IMapChangeMessage = interface
    ['{DF7B8E36-4893-4B7F-8824-F011993E9E73}']
    function GetSorurceMap: TMapType; stdcall;
    function GetNewMap: TMapType; stdcall;
  end;

implementation

end.
 