unit i_HashFunctionImpl;

interface

uses
  t_Hash;

type
  IHashFunctionImpl = interface
    ['{79D1F243-3578-4AD9-9E2C-8F28A22D7B1A}']
    function CalcHash(
      ABuffer: Pointer;
      ASize: Integer
    ): THashValue;
    function CalcHashWithSeed(
      ABuffer: Pointer;
      ASize: Integer;
      const ASeed: THashValue
    ): THashValue;
  end;



implementation

end.
