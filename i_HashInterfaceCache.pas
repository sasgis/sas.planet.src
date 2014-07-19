unit i_HashInterfaceCache;

interface

uses
  t_Hash;

type
  IHashInterfaceCache = interface
    ['{8597D0BA-BC26-4C92-9603-BB842671FE1C}']
    function GetOrCreateItem(
      const AKey: THashValue;
      const AData: Pointer
    ): IInterface;
    procedure DeleteItem(const AKey: THashValue);
    procedure Clear;
  end;

implementation

end.
