unit u_Bitmap32Static;

interface

uses
  Types,
  t_Bitmap32,
  t_Hash,
  i_Bitmap32Static,
  u_BaseInterfacedObject;

type
  TBitmap32Static = class(TBaseInterfacedObject, IBitmap32Static)
  private
    FBuffer: IBitmap32Buffer;
    FData: PColor32Array;
    FSize: TPoint;
    FHash: THashValue;
  private
    function GetSize: TPoint;
    function GetData: PColor32Array;
    function GetHash: THashValue;
  public
    constructor Create(
      const AHash: THashValue;
      const ABuffer: IBitmap32Buffer
    );
  end;

implementation

{ TBitmap32Static }

constructor TBitmap32Static.Create(
  const AHash: THashValue;
  const ABuffer: IBitmap32Buffer
);
begin
  Assert(Assigned(ABuffer));
  inherited Create;
  FBuffer := ABuffer;
  FHash := AHash;
  FData := FBuffer.Data;
  FSize := FBuffer.Size;
end;

function TBitmap32Static.GetData: PColor32Array;
begin
  Result := FData;
end;

function TBitmap32Static.GetHash: THashValue;
begin
  Result := FHash;
end;

function TBitmap32Static.GetSize: TPoint;
begin
  Result := FSize;
end;

end.
