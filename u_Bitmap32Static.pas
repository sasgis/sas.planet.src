unit u_Bitmap32Static;

interface

uses
  GR32,
  i_Bitmap32Static,
  u_BaseInterfacedObject;

type
  TBitmap32Static = class(TBaseInterfacedObject, IBitmap32Static)
  private
    FSize: TPoint;
    FBitmap: TCustomBitmap32;
  private
    function GetSize: TPoint;
    function GetData: PColor32Array;
  public
    constructor CreateWithOwn(ABitmap: TCustomBitmap32);
    destructor Destroy; override;
  end;

implementation

uses
  Types,
  SysUtils;

{ TBitmap32Static }

constructor TBitmap32Static.CreateWithOwn(ABitmap: TCustomBitmap32);
begin
  inherited Create;
  FBitmap := ABitmap;
  FSize := Point(FBitmap.Width, FBitmap.Height);
end;

destructor TBitmap32Static.Destroy;
begin
  FreeAndNil(FBitmap);
  inherited;
end;

function TBitmap32Static.GetData: PColor32Array;
begin
  Result := FBitmap.Bits;
end;

function TBitmap32Static.GetSize: TPoint;
begin
  Result := FSize;
end;

end.
