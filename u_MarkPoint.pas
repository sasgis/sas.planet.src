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
    FTextColor: TColor32;
    FTextBgColor: TColor32;
    FFontSize: Integer;
    FMarkerSize: Integer;
  protected
    function GetLLRect: TDoubleRect; override;
    function GetPoint: TDoublePoint;
    function GetTextColor: TColor32;
    function GetTextBgColor: TColor32;
    function GetFontSize: Integer;
    function GetMarkerSize: Integer;
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
      ATextColor: TColor32;
      ATextBgColor: TColor32;
      AFontSize: Integer;
      AMarkerSize: Integer
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
  ATextColor, ATextBgColor: TColor32;
  AFontSize, AMarkerSize: Integer
);
begin
  inherited Create(AHintConverter, ADbCode, AName, AId, ACategory, ADesc, AVisible);
  FPicName := APicName;
  FPic := APic;
  FPoint := APoint;
  FTextColor := ATextColor;
  FTextBgColor := ATextBgColor;
  FFontSize := AFontSize;
  FMarkerSize := AMarkerSize;
end;

function TMarkPoint.GetTextColor: TColor32;
begin
  Result := FTextColor;
end;

function TMarkPoint.GetTextBgColor: TColor32;
begin
  Result := FTextBgColor;
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

function TMarkPoint.GetFontSize: Integer;
begin
  Result := FFontSize;
end;

function TMarkPoint.GetMarkerSize: Integer;
begin
  Result := FMarkerSize;
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
