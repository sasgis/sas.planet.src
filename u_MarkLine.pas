unit u_MarkLine;

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
  TMarkLine = class(TMarkFullBase, IMarkLine)
  private
    FLLRect: TDoubleRect;
    FPoints: TArrayOfDoublePoint;
    FLineColor: TColor32;
    FLineWidth: Integer;
  protected
    function GetLLRect: TDoubleRect; override;
    function GetPoints: TArrayOfDoublePoint;
    function GetLineColor: TColor32;
    function GetColor2: TColor32;
    function GetLineWidth: Integer;
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
      ALineColor: TColor32;
      ALineWidth: Integer
    );
  end;

implementation

{ TMarkFull }

constructor TMarkLine.Create(
  AHintConverter: IHtmlToHintTextConverter;
  ADbCode: Integer;
  AName: string;
  AId: Integer;
  AVisible: Boolean;
  ACategory: IMarkCategory;
  ADesc: string;
  ALLRect: TDoubleRect;
  APoints: TArrayOfDoublePoint;
  ALineColor: TColor32;
  ALineWidth: Integer
);
begin
  inherited Create(AHintConverter, ADbCode, AName, AId, ACategory, ADesc, AVisible);
  FLLRect := ALLRect;
  FPoints := APoints;
  FLineColor := ALineColor;
  FLineWidth := ALineWidth;
end;

function TMarkLine.GetLineColor: TColor32;
begin
  Result := FLineColor;
end;

function TMarkLine.GetColor2: TColor32;
begin
  Result := 0;
end;

function TMarkLine.GetGoToLonLat: TDoublePoint;
begin
  Result := FPoints[0];
end;

function TMarkLine.GetLLRect: TDoubleRect;
begin
  Result := FLLRect;
end;

function TMarkLine.GetPic: IMarkPicture;
begin
  Result := nil;
end;

function TMarkLine.GetPicName: string;
begin
  Result := '';
end;

function TMarkLine.GetPoints: TArrayOfDoublePoint;
begin
  Result := FPoints;
end;

function TMarkLine.GetLineWidth: Integer;
begin
  Result := FLineWidth;
end;

function TMarkLine.GetScale2: Integer;
begin
  Result := 0;
end;

function TMarkLine.IsEmpty: Boolean;
begin
  Result := False;
end;

function TMarkLine.IsLine: Boolean;
begin
  Result := True;
end;

function TMarkLine.IsPoint: Boolean;
begin
  Result := False;
end;

function TMarkLine.IsPoly: Boolean;
begin
  Result := False;
end;

end.

