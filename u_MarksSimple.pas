unit u_MarksSimple;

interface

uses
  GR32,
  t_GeoTypes;

type
  TCategoryId = class
   id:integer;
   name:string;
   AfterScale: Integer;
   BeforeScale: Integer;
   visible:boolean;
  end;

  TMarkId = class
   name:string;
   id:integer;
   visible:boolean;
  end;

  TMarkFull = class(TMarkId)
    CategoryId: Integer;
    Desc: string;
    LLRect: TExtendedRect;
    Points: TExtendedPointArray;
    PicName: string;
    Color1: TColor32;
    Color2: TColor32;
    Scale1: Integer;
    Scale2: Integer;
    function IsEmpty: Boolean;
    function IsPoint: Boolean;
    function IsLine: Boolean;
    function IsPoly: Boolean;
    procedure ClosePoly;
    function GetGoToLonLat: TExtendedPoint;
  end;

implementation

{ TMarkFull }

procedure TMarkFull.ClosePoly;
var
  VPointCount: Integer;
begin
  VPointCount := Length(Points);
  if VPointCount > 1 then begin
    if (Points[0].X <> Points[VPointCount - 1].X) or
       (Points[0].Y <> Points[VPointCount - 1].Y) then begin
      SetLength(Points, VPointCount + 1);
      Points[VPointCount] := Points[0];
    end;
  end;
end;

function TMarkFull.GetGoToLonLat: TExtendedPoint;
begin
  Result.X := 0;
  Result.Y := 0;
  if IsPoint then begin
    Result := Points[0];
  end else if IsPoly then begin
    Result.X:= (LLRect.Left + LLRect.Right)/2 ;
    Result.Y:= (LLRect.Top + LLRect.Bottom)/2 ;
  end else if IsLine then begin
    Result := Points[0];
  end;
end;

function TMarkFull.IsEmpty: Boolean;
begin
  Result := Length(Points) = 0;
end;

function TMarkFull.IsLine: Boolean;
var
  VPointCount: Integer;
begin
  VPointCount := Length(Points);
  if VPointCount > 1 then begin
    Result := (Points[0].X <> Points[VPointCount - 1].X) or
      (Points[0].Y <> Points[VPointCount - 1].Y);
  end else begin
    Result := False;
  end;
end;

function TMarkFull.IsPoint: Boolean;
begin
  Result := Length(Points) = 1;
end;

function TMarkFull.IsPoly: Boolean;
var
  VPointCount: Integer;
begin
  VPointCount := Length(Points);
  if VPointCount > 1 then begin
    Result := (Points[0].X = Points[VPointCount - 1].X) and
      (Points[0].Y = Points[VPointCount - 1].Y);
  end else begin
    Result := False;
  end;
end;

end.
