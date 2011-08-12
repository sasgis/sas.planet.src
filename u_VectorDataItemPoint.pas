unit u_VectorDataItemPoint;

interface

uses
  t_GeoTypes,
  i_HtmlToHintTextConverter,
  u_VectorDataItemBase;

type
  TVectorDataItemPoint = class(TVectorDataItemBase)
  private
    FPoint: TDoublePoint;
  protected
    function IsPoint: Boolean; override;
    function IsLine: Boolean; override;
    function IsPoly: Boolean; override;
    function GetLLRect: TDoubleRect;  override;
    function GetPoints: TArrayOfDoublePoint;  override;
  public
    constructor Create(
      AHintConverter: IHtmlToHintTextConverter;
      AName: string;
      ADesc: string;
      APoint: TDoublePoint
    );
  end;

implementation

{ TVectorDataItemPoint }

constructor TVectorDataItemPoint.Create(
  AHintConverter: IHtmlToHintTextConverter;
  AName, ADesc: string;
  APoint: TDoublePoint
);
begin
  inherited Create(AHintConverter, AName, ADesc);
  FPoint := APoint;
end;

function TVectorDataItemPoint.GetLLRect: TDoubleRect;
begin
  Result.TopLeft := FPoint;
  Result.BottomRight := FPoint;
end;

function TVectorDataItemPoint.GetPoints: TArrayOfDoublePoint;
begin
  SetLength(Result, 1);
  Result[0] := FPoint;
end;

function TVectorDataItemPoint.IsLine: Boolean;
begin
  Result := False;
end;

function TVectorDataItemPoint.IsPoint: Boolean;
begin
  Result := True;
end;

function TVectorDataItemPoint.IsPoly: Boolean;
begin
  Result := False;
end;

end.
