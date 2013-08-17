unit u_HashFunctionCityHash;

interface

uses
  t_Hash,
  i_HashFunctionImpl,
  u_BaseInterfacedObject;

type
  THashFunctionCityHash = class(TBaseInterfacedObject, IHashFunctionImpl)
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

function THashFunctionCityHash.CalcHashWithSeed(
  ABuffer: Pointer;
  ASize: Integer;
  const ASeed: THashValue
): THashValue;
begin
  Result := CityHash64WithSeed(ABuffer, ASize, ASeed);
end;

end.
