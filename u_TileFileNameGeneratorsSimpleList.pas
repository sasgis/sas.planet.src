{
  Ёто плохой пример юнита. “олько временное решение перед переходом на плагины.
}
unit u_TileFileNameGeneratorsSimpleList;

interface

uses
  i_ITileFileNameGenerator,
  i_ITileFileNameGeneratorsList;

type
  TTileFileNameGeneratorsSimpleList = class(TInterfacedObject, ITileFileNameGeneratorsList)
  private
    FItems: array of ITileFileNameGenerator;
  public
    constructor Create;
    destructor Destroy; override;
    function GetGenerator(CacheType: Byte): ITileFileNameGenerator;
  end;
implementation

uses
  u_TileFileNameSAS,
  u_TileFileNameGMV,
  u_TileFileNameES,
  u_TileFileNameGM1,
  u_TileFileNameGM2,
  u_GlobalState;

{ TTileFileNameGeneratorsSimpleList }

constructor TTileFileNameGeneratorsSimpleList.Create;
begin
  SetLength(FItems, 5);
  FItems[0] := TTileFileNameGMV.Create;
  FItems[1] := TTileFileNameSAS.Create;
  FItems[2] := TTileFileNameES.Create;
  FItems[3] := TTileFileNameGM1.Create;
  FItems[4] := TTileFileNameGM2.Create;
end;

destructor TTileFileNameGeneratorsSimpleList.Destroy;
var
  i: integer;
begin
  for i := 0 to Length(FItems) - 1 do begin
    FItems[i] := nil;
  end;
  FItems := nil;
  inherited;
end;

function TTileFileNameGeneratorsSimpleList.GetGenerator(
  CacheType: Byte): ITileFileNameGenerator;
var
  VCacheType: Byte;
begin
  if CacheType = 0 then begin
    VCacheType := GState.DefCache;
  end else begin
    VCacheType := CacheType;
  end;
  case VCacheType of
    1: Result := FItems[0];
    2: Result := FItems[1];
    3: Result := FItems[2];
    4: Result := FItems[3];
    41: Result := FItems[4];
  else
    Result := FItems[3];
  end;
end;

end.
 