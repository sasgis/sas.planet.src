unit i_HashFunction;

interface

uses
  t_Hash;

type
  IHashFunction = interface
    ['{5960ED76-146C-4172-80F7-ECBDF1270DDF}']
    function CalcHash(
      ABuffer: Pointer;
      ASize: Integer
    ): THashValue;
    function CalcHashWithSeed(
      ABuffer: Pointer;
      ASize: Integer;
      const ASeed: THashValue
    ): THashValue;
    function CalcHashOfTwoHash(
      const AHash1: THashValue;
      const AHash2: THashValue
    ): THashValue;
  end;

implementation

end.
