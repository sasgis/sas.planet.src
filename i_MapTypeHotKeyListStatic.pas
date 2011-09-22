unit i_MapTypeHotKeyListStatic;

interface

uses
  Classes,
  i_MapTypes;

type
  IMapTypeHotKeyListStatic = interface
    ['{A12F7B6B-D5A8-4858-8672-21121C45A6EB}']
    function GetMapTypeGUIDByHotKey(AHotKey: TShortCut): IMapType;
  end;

implementation

end.
