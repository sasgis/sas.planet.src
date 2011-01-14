unit u_MarksSimpleNew;

interface

uses
  GR32,
  t_GeoTypes,
  i_MarksSimple,
  i_IMarkPicture;

type
  TMarkId = class(TInterfacedObject, IMarkID, IMarkVisible)
  private
    FName: string;
    FId: Integer;
    FVisible: Boolean;
  protected
    function GetId: Integer;
    function GetName: string;
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);
  public
    constructor Create(
      AName: string;
      AId: Integer;
      AVisible: Boolean
    );
  end;

  TMarkFull = class(TMarkId, IMarkFull)
  private
    FPicName: string;
    FPic: IMarkPicture;
    FCategoryId: Integer;
    FDesc: string;
    FLLRect: TDoubleRect;
    FPoints: TDoublePointArray;
    FColor1: TColor32;
    FColor2: TColor32;
    FScale1: Integer;
    FScale2: Integer;
  protected
    function GetCategoryId: Integer;
    function GetDesc: string;
    function GetLLRect: TDoubleRect;
    function GetPoints: TDoublePointArray;
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
    constructor Create(ASource: IMarkFull); overload;
    constructor Create(
      AName: string;
      AId: Integer;
      AVisible: Boolean;
      APicName: string;
      APic: IMarkPicture;
      ACategoryId: Integer;
      ADesc: string;
      ALLRect: TDoubleRect;
      APoints: TDoublePointArray;
      AColor1: TColor32;
      AColor2: TColor32;
      AScale1: Integer;
      AScale2: Integer
    ); overload;
  end;

implementation

{ TMarkId }

constructor TMarkId.Create(AName: string; AId: Integer; AVisible: Boolean);
begin
  FName := AName;
  FId := AId;
  FVisible := AVisible;
end;

function TMarkId.GetId: Integer;
begin
  Result := FId;
end;

function TMarkId.GetName: string;
begin
  Result := FName;
end;

function TMarkId.GetVisible: Boolean;
begin
  Result := FVisible;
end;

procedure TMarkId.SetVisible(AValue: Boolean);
begin
  FVisible := AValue;
end;

{ TMarkFull }

constructor TMarkFull.Create(ASource: IMarkFull);
begin
  inherited Create(ASource.Name, ASource.Id, False);
  FPicName := ASource.PicName;
  FPic := ASource.Pic;
  FCategoryId := ASource.CategoryId;
  FDesc := ASource.Desc;
  FLLRect := ASource.LLRect;
  FPoints := Copy(ASource.Points);
  FColor1 := ASource.Color1;
  FColor2 := ASource.Color2;
  FScale1 := ASource.Scale1;
  FScale2 := ASource.Scale2;
end;

constructor TMarkFull.Create(AName: string; AId: Integer; AVisible: Boolean;
  APicName: string; APic: IMarkPicture; ACategoryId: Integer; ADesc: string;
  ALLRect: TDoubleRect; APoints: TDoublePointArray; AColor1, AColor2: TColor32;
  AScale1, AScale2: Integer);
begin
  inherited Create(AName, AId, AVisible);
  FPicName := APicName;
  FPic := APic;
  FCategoryId := ACategoryId;
  FDesc := ADesc;
  FLLRect := ALLRect;
  FPoints := Copy(APoints);
  FColor1 := AColor1;
  FColor2 := AColor2;
  FScale1 := AScale1;
  FScale2 := AScale2;
end;

function TMarkFull.GetCategoryId: Integer;
begin

end;

function TMarkFull.GetColor1: TColor32;
begin

end;

function TMarkFull.GetColor2: TColor32;
begin

end;

function TMarkFull.GetDesc: string;
begin

end;

function TMarkFull.GetGoToLonLat: TDoublePoint;
begin
  Result.X := 0;
  Result.Y := 0;
  if IsPoint then begin
    Result := FPoints[0];
  end else if IsPoly then begin
    Result.X := (FLLRect.Left + FLLRect.Right) / 2;
    Result.Y := (FLLRect.Top + FLLRect.Bottom) / 2;
  end else if IsLine then begin
    Result := FPoints[0];
  end;
end;

function TMarkFull.GetLLRect: TDoubleRect;
begin

end;

function TMarkFull.GetPic: IMarkPicture;
begin

end;

function TMarkFull.GetPicName: string;
begin

end;

function TMarkFull.GetPoints: TDoublePointArray;
begin

end;

function TMarkFull.GetScale1: Integer;
begin

end;

function TMarkFull.GetScale2: Integer;
begin

end;

function TMarkFull.IsEmpty: Boolean;
begin
  Result := Length(FPoints) = 0;
end;

function TMarkFull.IsLine: Boolean;
var
  VPointCount: Integer;
begin
  VPointCount := Length(FPoints);
  if VPointCount > 1 then begin
    Result := (FPoints[0].X <> FPoints[VPointCount - 1].X) or
      (FPoints[0].Y <> FPoints[VPointCount - 1].Y);
  end else begin
    Result := False;
  end;
end;

function TMarkFull.IsPoint: Boolean;
begin
  Result := Length(FPoints) = 1;
end;

function TMarkFull.IsPoly: Boolean;
var
  VPointCount: Integer;
begin
  VPointCount := Length(FPoints);
  if VPointCount > 1 then begin
    Result := (FPoints[0].X = FPoints[VPointCount - 1].X) and
      (FPoints[0].Y = FPoints[VPointCount - 1].Y);
  end else begin
    Result := False;
  end;
end;

end.
