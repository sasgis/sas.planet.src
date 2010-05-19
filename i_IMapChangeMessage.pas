unit i_IMapChangeMessage;

interface

uses
  UMapType;

type
  IMapChangeMessage = interface
    ['{D804ACF7-73BF-4FD3-9907-6FF1F1334D01}']
    function GetSorurceMap: TMapType; stdcall;
    function GetNewMap: TMapType; stdcall;
  end;

implementation

end.
 