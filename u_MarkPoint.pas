unit u_MarkPoint;

interface

uses
  GR32,
  t_GeoTypes,
  i_MarksSimple,
  i_MarkCategory,
  i_MarksDbSmlInternal,
  i_MarkPicture,
  i_HtmlToHintTextConverter,
  u_MarkFullBase;

type
  TMarkPoint = class(TMarkFullBase, IMarkPoint, IMarkPointSMLInternal)
  private
    FPicName: string;
    FPic: IMarkPicture;
    FPoint: TDoublePoint;
    FColor1: TColor32;
    FColor2: TColor32;
    FScale1: Integer;
    FScale2: Integer;
  protected
    function GetLLRect: TDoubleRect; override;
    function GetPoint: TDoublePoint;
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
    function GetGoToLonLat: TDoublePoint; override;
  public
    constructor Create(
      AHintConverter: IHtmlToHintTextConverter;
      ADbCode: Integer;
      const AName: string;
      AId: Integer;
      AVisible: Boolean;
      const APicName: string;
      APic: IMarkPicture;
      ACategory: IMarkCategory;
      const ADesc: string;
      const APoint: TDoublePoint;
      AColor1: TColor32;
      AColor2: TColor32;
      AScale1: Integer;
      AScale2: Integer
    );
  end;

implementation

{ TMarkPoint }

constructor TMarkPoint.Create(
  AHintConverter: IHtmlToHintTextConverter;
  ADbCode: Integer;
  const AName: string;
  AId: Integer;
  AVisible: Boolean;
  const APicName: string;
  APic: IMarkPicture;
  ACategory: IMarkCategory;
  const ADesc: string;
  const APoint: TDoublePoint;
  AColor1, AColor2: TColor32;
  AScale1, AScale2: Integer
);
begin
  inherited Create(AHintConverter, ADbCode, AName, AId, ACategory, ADesc, AVisible);
  FPicName := APicName;
  FPic := APic;
  FPoint := APoint;
  FColor1 := AColor1;
  FColor2 := AColor2;
  FScale1 := AScale1;
  FScale2 := AScale2;
end;

function TMarkPoint.GetColor1: TColor32;
begin
  Result := FColor1;
end;

function TMarkPoint.GetColor2: TColor32;
begin
  Result := FColor2;
end;

function TMarkPoint.GetGoToLonLat: TDoublePoint;
begin
  Result := FPoint;
end;

function TMarkPoint.GetLLRect: TDoubleRect;
begin
  Result.TopLeft := FPoint;
  Result.BottomRight := FPoint;
end;

function TMarkPoint.GetPic: IMarkPicture;
begin
  Result := FPic;
end;

function TMarkPoint.GetPicName: string;
begin
  Result := FPicName;
end;

function TMarkPoint.GetPoint: TDoublePoint;
begin
  Result := FPoint;
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
