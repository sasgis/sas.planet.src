unit i_IPoolOfObjects;

interface

uses
  Types;

type
  IPoolOfObjects = interface
  ['']
    function TryGetPoolElement(ATimeOut: Cardianl): IPoolElement;
    function GetPoolSize: Cardianl;
    function GetPoolMaxSize: Cardianl;
    procedure SetPoolMaxSize(ASize: Cardinal);
  end;
implementation

end.
