unit i_HashFunction;

interface

type
  THashValue = UInt64;

  IHashFunction = interface
    ['{5960ED76-146C-4172-80F7-ECBDF1270DDF}']
    function CalcHash(
      ABuffer: Pointer;
      ASize: Integer
    ): THashValue;
    function CalcHashWithSeed(
      ABuffer: Pointer;
      ASize: Integer;
      ASeed: THashValue
    ): THashValue;
  end;

implementation

end.
