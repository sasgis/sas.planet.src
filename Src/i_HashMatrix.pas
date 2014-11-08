unit i_HashMatrix;

interface

uses
  Types,
  t_Hash;

type
  IHashMatrix = interface
    ['{30ADFD38-9B4C-49EC-B6D4-5902B5696E7B}']
    procedure Reset(const ARect: TRect);
    procedure ChangeRect(const ARect: TRect);
    procedure SetHash(const APos: TPoint; const AHash: THashValue);
    function GetHash(const APos: TPoint): THashValue;
  end;

implementation

end.
