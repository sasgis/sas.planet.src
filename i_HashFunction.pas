unit i_HashFunction;

interface

uses
  Types,
  t_Hash,
  t_GeoTypes;

type
  IHashFunction = interface
    ['{5960ED76-146C-4172-80F7-ECBDF1270DDF}']
    function CalcHashByString(const AValue: string): THashValue;
    function CalcHashByAnsiString(const AValue: AnsiString): THashValue;
    function CalcHashByGUID(const AValue: TGUID): THashValue;
    function CalcHashByDouble(const AValue: Double): THashValue;
    function CalcHashByInteger(const AValue: Integer): THashValue;
    function CalcHashByPoint(const AValue: TPoint): THashValue;
    function CalcHashByDoublePoint(const AValue: TDoublePoint): THashValue;
    function CalcHashByRect(const AValue: TRect): THashValue;
    function CalcHashByDoubleRect(const AValue: TDoubleRect): THashValue;
    function CalcHashByBuffer(ABuffer: Pointer; ASize: Integer): THashValue;

    procedure UpdateHashByString(var AHash: THashValue; const AValue: string);
    procedure UpdateHashByAnsiString(var AHash: THashValue; const AValue: AnsiString);
    procedure UpdateHashByGUID(var AHash: THashValue; const AValue: TGUID);
    procedure UpdateHashByDouble(var AHash: THashValue; const AValue: Double);
    procedure UpdateHashByInteger(var AHash: THashValue; const AValue: Integer);
    procedure UpdateHashByPoint(var AHash: THashValue; const AValue: TPoint);
    procedure UpdateHashByDoublePoint(var AHash: THashValue; const AValue: TDoublePoint);
    procedure UpdateHashByRect(var AHash: THashValue; const AValue: TRect);
    procedure UpdateHashByDoubleRect(var AHash: THashValue; const AValue: TDoubleRect);
    procedure UpdateHashByBuffer(var AHash: THashValue; ABuffer: Pointer; ASize: Integer);
    procedure UpdateHashByHash(var AHash: THashValue; AValue: THashValue);
  end;

implementation

end.
