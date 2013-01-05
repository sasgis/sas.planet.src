unit u_ObjectPoolBitmap32Standart;

interface

uses
  Types,
  i_Bitmap32Static,
  i_Bitmap32StaticFactory,
  u_ObjectFromPoolAbstract,
  u_ObjectPoolBase;

type
  TObjectPoolBitmap32Standart = class(TObjectPoolBase, IObjectPoolBitmap32Standart)
  private
    function Build: IBitmap32Static;
    function GetSize: TPoint;
  protected
    function BuildNewObject(const AFreeProcedure: IFreeObjectProcedure): TObjectFromPoolAbstract; override;
  end;

implementation

uses
  GR32,
  u_ObjectFromPoolBase;

const
  CStandartSize = 256;

type
  TBitmap32StaticStandartSize = class(TObjectFromPoolBase, IBitmap32Static)
  private
    FBits: array [0..(CStandartSize * CStandartSize - 1)] of TColor32;
  private
    function GetSize: TPoint;
    function GetData: PColor32Array;
  end;

{ TBitmap32StaticStandartSize }

function TBitmap32StaticStandartSize.GetData: PColor32Array;
begin
  Result := PColor32Array(@FBits[0]);
end;

function TBitmap32StaticStandartSize.GetSize: TPoint;
begin
  Result := Point(CStandartSize, CStandartSize);
end;

{ TObjectPoolBitmap32Standart }

function TObjectPoolBitmap32Standart.Build: IBitmap32Static;
var
  VObject: TObjectFromPoolAbstract;
begin
  VObject := PullOrCreateObject;
  Result := VObject as IBitmap32Static;
end;

function TObjectPoolBitmap32Standart.BuildNewObject(
  const AFreeProcedure: IFreeObjectProcedure): TObjectFromPoolAbstract;
begin
  Result := TBitmap32StaticStandartSize.Create(AFreeProcedure);
end;

function TObjectPoolBitmap32Standart.GetSize: TPoint;
begin
  Result := Point(CStandartSize, CStandartSize);
end;

end.
