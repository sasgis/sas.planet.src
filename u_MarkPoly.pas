unit u_MarkPoly;

interface

uses
  GR32,
  t_GeoTypes,
  i_MarksSimple,
  i_MarkCategory,
  i_MarkPicture,
  i_HtmlToHintTextConverter,
  u_MarkFullBase;

type
  TMarkPoly = class(TMarkFullBase, IMarkPoly)
  private
    FLLRect: TDoubleRect;
    FPoints: TArrayOfDoublePoint;
    FBorderColor: TColor32;
    FColor2: TColor32;
    FScale1: Integer;
  protected
    function GetLLRect: TDoubleRect; override;
    function GetPoints: TArrayOfDoublePoint;
    function GetBorderColor: TColor32;
    function GetColor2: TColor32;
    function GetScale1: Integer;
    function GetScale2: Integer;
    function GetPicName: string;
    function GetPic: IMarkPicture;
    function IsEmpty: Boolean;
    function IsPoint: Boolean;
    function IsLine: Boolean;
    function IsPoly: Boolean;
    function GetGoToLonLat: TDoublePoint; override;
  public
    constructor Create(
      AHintConverter: IHtmlToHintTextConverter;
      ADbCode: Integer;
      AName: string;
      AId: Integer;
      AVisible: Boolean;
      ACategory: IMarkCategory;
      ADesc: string;
      ALLRect: TDoubleRect;
      APoints: TArrayOfDoublePoint;
      ABorderColor: TColor32;
      AColor2: TColor32;
      AScale1: Integer
    );
  end;

implementation

{ TMarkFull }

constructor TMarkPoly.Create(
  AHintConverter: IHtmlToHintTextConverter;
  ADbCode: Integer;
  AName: string;
  AId: Integer;
  AVisible: Boolean;
  ACategory: IMarkCategory;
  ADesc: string;
  ALLRect: TDoubleRect;
  APoints: TArrayOfDoublePoint;
  ABorderColor, AColor2: TColor32;
  AScale1: Integer
);
begin
  inherited Create(AHintConverter, ADbCode, AName, AId, ACategory, ADesc, AVisible);
  FLLRect := ALLRect;
  FPoints := APoints;
  FBorderColor := ABorderColor;
  FColor2 := AColor2;
  FScale1 := AScale1;
end;

function TMarkPoly.GetBorderColor: TColor32;
begin
  Result := FBorderColor;
end;

function TMarkPoly.GetColor2: TColor32;
begin
  Result := FColor2;
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
