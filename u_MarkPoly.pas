unit u_MarkPoly;

interface

uses
  GR32,
  t_GeoTypes,
  i_MarksSimple,
  i_MarkCategory,
  i_MarkPicture,
  u_MarkId;

type
  TMarkPoly = class(TMarkId, IMarkFull)
  private
    FDesc: string;
    FLLRect: TDoubleRect;
    FPoints: TArrayOfDoublePoint;
    FColor1: TColor32;
    FColor2: TColor32;
    FScale1: Integer;
  protected
    function GetDesc: string;
    function GetLLRect: TDoubleRect;
    function GetPoints: TArrayOfDoublePoint;
    function GetColor1: TColor32;
    function GetColor2: TColor32;
    function GetScale1: Integer;
    function GetScale2: Integer;
    function GetPicName: string;
    function GetPic: IMarkPicture;
    function IsEmpty: Boolean;
    function IsPoint: Boolean;
    function IsLine: Boolean;
    function IsPoly: Boolean;
    function GetGoToLonLat: TDoublePoint;
  public
    constructor Create(
      AName: string;
      AId: Integer;
      AVisible: Boolean;
      ACategory: IMarkCategory;
      ADesc: string;
      ALLRect: TDoubleRect;
      APoints: TArrayOfDoublePoint;
      AColor1: TColor32;
      AColor2: TColor32;
      AScale1: Integer
    );
  end;

implementation

{ TMarkFull }

constructor TMarkPoly.Create(
  AName: string;
  AId: Integer;
  AVisible: Boolean;
  ACategory: IMarkCategory;
  ADesc: string;
  ALLRect: TDoubleRect;
  APoints: TArrayOfDoublePoint;
  AColor1, AColor2: TColor32;
  AScale1: Integer
);
begin
  inherited Create(AName, AId, ACategory, AVisible);
  FDesc := ADesc;
  FLLRect := ALLRect;
  FPoints := APoints;
  FColor1 := AColor1;
  FColor2 := AColor2;
  FScale1 := AScale1;
end;

function TMarkPoly.GetColor1: TColor32;
begin
  Result := FColor1;
end;

function TMarkPoly.GetColor2: TColor32;
begin
  Result := FColor2;
end;

function TMarkPoly.GetDesc: string;
begin
  Result := FDesc;
end;

function TMarkPoly.GetGoToLonLat: TDoublePoint;
begin
  Result.X := (FLLRect.Left + FLLRect.Right) / 2;
  Result.Y := (FLLRect.Top + FLLRect.Bottom) / 2;
end;

function TMarkPoly.GetLLRect: TDoubleRect;
begin
  Result := FLLRect;
end;

function TMarkPoly.GetPic: IMarkPicture;
begin
  Result := nil;
end;

function TMarkPoly.GetPicName: string;
begin
  Result := '';
end;

function TMarkPoly.GetPoints: TArrayOfDoublePoint;
begin
  Result := FPoints;
end;

function TMarkPoly.GetScale1: Integer;
begin
  Result := FScale1;
end;

function TMarkPoly.GetScale2: Integer;
begin
  Result := 0;
end;

function TMarkPoly.IsEmpty: Boolean;
begin
  Result := False;
end;

function TMarkPoly.IsLine: Boolean;
begin
  Result := False;
end;

function TMarkPoly.IsPoint: Boolean;
begin
  Result := False;
end;

function TMarkPoly.IsPoly: Boolean;
begin
  Result := True;
end;

end.
