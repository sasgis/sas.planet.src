unit u_Bitmap32Static;

interface

uses
  GR32,
  i_Bitmap32Static;

type
  TBitmap32Static = class(TInterfacedObject, IBitmap32Static)
  private
    FBitmap: TCustomBitmap32;
  private
    function GetBitmap: TCustomBitmap32;
  public
    constructor CreateWithCopy(ABitmap: TCustomBitmap32);
    constructor CreateWithOwn(ABitmap: TCustomBitmap32);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils;

{ TBitmap32Static }

constructor TBitmap32Static.CreateWithCopy(ABitmap: TCustomBitmap32);
var
  VBitmap: TCustomBitmap32;
begin
  VBitmap := TCustomBitmap32.Create;
  VBitmap.Assign(ABitmap);
  CreateWithOwn(VBitmap);
end;

constructor TBitmap32Static.CreateWithOwn(ABitmap: TCustomBitmap32);
begin
  FBitmap := ABitmap;
end;

destructor TBitmap32Static.Destroy;
begin
  FreeAndNil(FBitmap);
  inherited;
end;

function TBitmap32Static.GetBitmap: TCustomBitmap32;
begin
  Result := FBitmap;
end;

end.
