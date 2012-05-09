unit i_IdCacheSimple;

interface

type
  IIdCacheSimple = interface
    ['{3411787C-DE34-4CD6-A087-8A60A64BF7DD}']
    function GetByID(AID: Integer): IInterface;
    procedure Clear;
    procedure Add(
      AID: Integer;
      const AInterface: IInterface
    );
  end;

implementation

end.
