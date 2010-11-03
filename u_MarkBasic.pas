unit u_MarkBasic;

interface

uses
  t_GeoTypes,
  i_Marks;
type
  TMarkBasic = class(TInterfacedObject, IMarkBasic)
  public
    FId: integer;
    FName: string;
    FDescr: string;
    FScale1: integer;
    FScale2: integer;
    FLonLatArr: TDoublePointArray;
    FRect: TDoubleRect;
    FColor1: integer;
    FColor2: integer;
    FVisible: boolean;
    FPicName: string;
    FCategoryId: integer;
    function GetId: integer; stdcall;
    function GetCategoryId: integer; virtual; stdcall;
    function GetColor1: integer; virtual; stdcall;
    function GetColor2: integer; virtual; stdcall;
    function GetDescr: string; virtual; stdcall;
    function GetLonLatArr: TDoublePointArray; virtual; stdcall;
    function GetName: string; virtual; stdcall;
    function GetPicName: string; virtual; stdcall;
    function GetRect: TDoubleRect; virtual; stdcall;
    function GetScale1: integer; virtual; stdcall;
    function GetScale2: integer; virtual; stdcall;
    function GetVisible: boolean; virtual; stdcall;

    constructor Create(AMark: IMarkBasic);
    destructor Destroy; override;
    property Name: string read GetName;
    property Descr: string read GetDescr;
    property Scale1: integer read GetScale1;
    property Scale2: integer read GetScale2;
    property LonLatArr: TDoublePointArray read GetLonLatArr;
    property Rect: TDoubleRect read GetRect;
    property Color1: integer read GetColor1;
    property Color2: integer read GetColor2;
    property Visible: boolean read GetVisible;
    property PicName: string read GetPicName;
    property CategoryId: integer read GetCategoryId;
  end;


implementation

{ TMarkBasic }

constructor TMarkBasic.Create(AMark: IMarkBasic);
begin
  FId := AMark.Id;
  FName := AMark.Name;
  FDescr := AMark.Descr;
  FScale1 := AMark.Scale1;
  FScale2 := AMark.Scale2;
  FLonLatArr := AMark.LonLatArr;
  FRect := AMark.Rect;
  FColor1 := AMark.Color1;
  FColor2 := AMark.Color2;
  FVisible := AMark.Visible;
  FPicName := AMark.PicName;
  FCategoryId := AMark.CategoryId;
end;

destructor TMarkBasic.Destroy;
begin
  FLonLatArr := nil;
  inherited;
end;

function TMarkBasic.GetCategoryId: integer;
begin
  Result := FCategoryId;
end;

function TMarkBasic.GetColor1: integer;
begin
  Result := FColor1;
end;

function TMarkBasic.GetColor2: integer;
begin
  Result := FColor2;
end;

function TMarkBasic.GetDescr: string;
begin
  Result := FDescr;
end;

function TMarkBasic.GetId: integer;
begin
  Result := FId;
end;

function TMarkBasic.GetLonLatArr: TDoublePointArray;
begin
  Result := FLonLatArr;
end;

function TMarkBasic.GetName: string;
begin
  Result := FName;
end;

function TMarkBasic.GetPicName: string;
begin
  Result := FPicName;
end;

function TMarkBasic.GetRect: TDoubleRect;
begin
  Result := FRect;
end;

function TMarkBasic.GetScale1: integer;
begin
  Result := FScale1;
end;

function TMarkBasic.GetScale2: integer;
begin
  Result := FScale2;
end;

function TMarkBasic.GetVisible: boolean;
begin
  Result := FVisible;
end;
end.
