unit u_MarkPictureSimple;

interface

uses
  Types,
  Classes,
  GR32,
  i_IMarkPicture;

type
  TMarkPictureSimple = class(TInterfacedObject, IMarkPicture)
    FBitmap: TCustomBitmap32;
    FBitmapSize: TPoint;
  protected
    procedure LoadBitmap(ABmp: TCustomBitmap32);
    function GetPointInPicture: TPoint;
    function GetTextAlignment: TAlignment;
    function GetTextVerticalAlignment: TVerticalAlignment;
  public
    constructor Create(ABitmap: TCustomBitmap32);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  GR32_LowLevel;

{ TMarkPictureSimple }

constructor TMarkPictureSimple.Create(ABitmap: TCustomBitmap32);
begin
  FBitmap := TCustomBitmap32.Create;
  FBitmap.Assign(ABitmap);
  FBitmapSize := Point(FBitmap.Width, FBitmap.Height);
end;

destructor TMarkPictureSimple.Destroy;
begin
  FreeAndNil(FBitmap);
  inherited;
end;

function TMarkPictureSimple.GetPointInPicture: TPoint;
begin
  Result.X := FBitmapSize.X div 2;
  Result.Y := FBitmapSize.Y;
end;

function TMarkPictureSimple.GetTextAlignment: TAlignment;
begin
  Result := taRightJustify;
end;

function TMarkPictureSimple.GetTextVerticalAlignment: TVerticalAlignment;
begin
  Result := taVerticalCenter;
end;

procedure TMarkPictureSimple.LoadBitmap(ABmp: TCustomBitmap32);
begin
  ABmp.SetSize(FBitmapSize.X, FBitmapSize.Y);
  if not FBitmap.Empty then
    MoveLongword(FBitmap.Bits[0], ABmp.Bits[0], FBitmapSize.X * FBitmapSize.Y);
end;

end.
