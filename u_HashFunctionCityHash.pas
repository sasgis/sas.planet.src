unit u_HashFunctionCityHash;

interface

uses
  t_Hash,
  i_HashFunction,
  u_BaseInterfacedObject;

type
  THashFunctionCityHash = class(TBaseInterfacedObject, IHashFunction)
  private
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

uses
  CityHash;

{ THashFunctionCityHash }

function THashFunctionCityHash.CalcHash(
  ABuffer: Pointer;
  ASize: Integer
): THashValue;
begin
  Result := CityHash64(ABuffer, ASize);
end;

function THashFunctionCityHash.CalcHashOfTwoHash(const AHash1,
  AHash2: THashValue): THashValue;
begin
  Result := CalcHashWithSeed(@AHash2, SizeOf(THashValue), AHash1);
end;

function THashFunctionCityHash.CalcHashWithSeed(
  ABuffer: Pointer;
  ASize: Integer;
  const ASeed: THashValue
): THashValue;
begin
  Result := CityHash64WithSeed(ABuffer, ASize, ASeed);
end;

end.
