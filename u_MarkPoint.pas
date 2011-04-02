unit u_MarkPoint;

interface

uses
  GR32,
  t_GeoTypes,
  i_MarksSimple,
  i_MarkPicture,
  u_MarkId;

type
  TMarkPoint = class(TMarkId, IMarkFull)
  private
    FPicName: string;
    FPic: IMarkPicture;
    FCategoryId: Integer;
    FDesc: string;
    FLLRect: TDoubleRect;
    FPoints: TArrayOfDoublePoint;
    FColor1: TColor32;
    FColor2: TColor32;
    FScale1: Integer;
    FScale2: Integer;
  protected
    function GetCategoryId: Integer;
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
      APicName: string;
      APic: IMarkPicture;
      ACategoryId: Integer;
      ADesc: string;
      ALLRect: TDoubleRect;
      APoint: TDoublePoint;
      AColor1: TColor32;
      AColor2: TColor32;
      AScale1: Integer;
      AScale2: Integer
    );
  end;

implementation

{ TMarkPoint }

constructor TMarkPoint.Create(
  AName: string;
  AId: Integer;
  AVisible: Boolean;
  APicName: string;
  APic: IMarkPicture;
  ACategoryId: Integer;
  ADesc: string;
  ALLRect: TDoubleRect;
  APoint: TDoublePoint;
  AColor1, AColor2: TColor32;
  AScale1, AScale2: Integer
);
begin
  inherited Create(AName, AId, AVisible);
  FPicName := APicName;
  FPic := APic;
  FCategoryId := ACategoryId;
  FDesc := ADesc;
  FLLRect := ALLRect;
  SetLength(FPoints, 1);
  FPoints[0] := APoint;
  FColor1 := AColor1;
  FColor2 := AColor2;
  FScale1 := AScale1;
  FScale2 := AScale2;
end;

function TMarkPoint.GetCategoryId: Integer;
begin
  Result := FCategoryId;
end;

function TMarkPoint.GetColor1: TColor32;
begin
  Result := FColor1;
end;

function TMarkPoint.GetColor2: TColor32;
begin
  Result := FColor2;
end;

function TMarkPoint.GetDesc: string;
begin
  Result := FDesc;
end;

function TMarkPoint.GetGoToLonLat: TDoublePoint;
begin
  Result := FPoints[0];
end;

function TMarkPoint.GetLLRect: TDoubleRect;
begin
  Result := FLLRect;
end;

function TMarkPoint.GetPic: IMarkPicture;
begin
  Result := FPic;
end;

function TMarkPoint.GetPicName: string;
begin
  Result := FPicName;
end;

function TMarkPoint.GetPoints: TArrayOfDoublePoint;
begin
  Result := FPoints;
end;

function TMarkPoint.GetScale1: Integer;
begin
  Result := FScale1;
end;

function TMarkPoint.GetScale2: Integer;
begin
  Result := FScale2;
end;

function TMarkPoint.IsEmpty: Boolean;
begin
  Result := False;
end;

function TMarkPoint.IsLine: Boolean;
begin
  Result := False;
end;

function TMarkPoint.IsPoint: Boolean;
begin
  Result := True;
end;

function TMarkPoint.IsPoly: Boolean;
begin
  Result := False;
end;

end.
