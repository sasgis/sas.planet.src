unit u_Bitmap32StaticFactory;

interface

uses
  Types,
  t_Hash,
  t_Bitmap32,
  i_Bitmap32Static,
  i_HashFunction,
  i_Bitmap32BufferFactory,
  u_BaseInterfacedObject;

type
  TBitmap32StaticFactory = class(TBaseInterfacedObject, IBitmap32StaticFactory)
  private
    FHashFunction: IHashFunction;
    FBitmapFactory: IBitmap32BufferFactory;
  private
    function GetBufferFactory: IBitmap32BufferFactory;
    function BuildWithOwnBuffer(const ABuffer: IBitmap32Buffer): IBitmap32Static;
    function Build(
      const ASize: TPoint;
      const AData: PColor32Array
    ): IBitmap32Static;
  public
    constructor Create(
      const AHashFunction: IHashFunction;
      const ABitmapFactory: IBitmap32BufferFactory
    );
  end;

implementation

uses
  u_Bitmap32Static;

{ TBitmap32StaticFactory }

constructor TBitmap32StaticFactory.Create(
  const AHashFunction: IHashFunction;
  const ABitmapFactory: IBitmap32BufferFactory
);
begin
  Assert(Assigned(ABitmapFactory));
  Assert(Assigned(AHashFunction));
  inherited Create;
  FHashFunction := AHashFunction;
  FBitmapFactory := ABitmapFactory;
end;

function TBitmap32StaticFactory.GetBufferFactory: IBitmap32BufferFactory;
begin
  Result := FBitmapFactory;
end;

function TBitmap32StaticFactory.Build(
  const ASize: TPoint;
  const AData: PColor32Array
): IBitmap32Static;
var
  VBuffer: IBitmap32Buffer;
begin
  Assert(Assigned(AData));
  Result := nil;
  VBuffer := FBitmapFactory.Build(ASize, AData);
  Assert(Assigned(VBuffer));
  if Assigned(VBuffer) then begin
    Result := BuildWithOwnBuffer(VBuffer);
  end;
end;

function TBitmap32StaticFactory.BuildWithOwnBuffer(
  const ABuffer: IBitmap32Buffer
): IBitmap32Static;
var
  VHash: THashValue;
  VSize: TPoint;
begin
  Assert(Assigned(ABuffer));
  if Assigned(ABuffer) then begin
    VSize := ABuffer.Size;
//    VHash := FHashFunction.CalcHashByBuffer(ABuffer.Data, VSize.X * VSize.Y * SizeOf(TColor32));
    VHash := 0;
    Result := TBitmap32Static.Create(VHash, ABuffer);
  end;
end;

end.
