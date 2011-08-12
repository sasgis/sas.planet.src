unit u_VectorDataItemPolygon;

interface

uses
  t_GeoTypes,
  i_HtmlToHintTextConverter,
  u_VectorDataItemBase;

type
  TVectorDataItemPolygon = class(TVectorDataItemBase)
  private
    FPoints: TArrayOfDoublePoint;
    FLLRect: TDoubleRect;
  protected
    function IsPoint: Boolean; override;
    function GetLLRect: TDoubleRect;  override;
    function GetPoints: TArrayOfDoublePoint;  override;
  public
    constructor Create(
      AHintConverter: IHtmlToHintTextConverter;
      AName: string;
      ADesc: string;
      APoints: TArrayOfDoublePoint;
      ALLRect: TDoubleRect
    );
  end;

  TVectorDataItemPath = class(TVectorDataItemPolygon)
  protected
    function IsLine: Boolean; override;
    function IsPoly: Boolean; override;
  end;

  TVectorDataItemPoly = class(TVectorDataItemPolygon)
  protected
    function IsLine: Boolean; override;
    function IsPoly: Boolean; override;
  end;


implementation

{ TVectorDataItemPolygon }

constructor TVectorDataItemPolygon.Create(
  AHintConverter: IHtmlToHintTextConverter;
  AName, ADesc: string;
  APoints: TArrayOfDoublePoint;
  ALLRect: TDoubleRect
);
begin
  inherited Create(AHintConverter, AName, ADesc);
  FLLRect := ALLRect;
  FPoints := APoints;
end;

function TVectorDataItemPolygon.GetLLRect: TDoubleRect;
begin
  Result := FLLRect;
end;

function TVectorDataItemPolygon.GetPoints: TArrayOfDoublePoint;
begin
  Result := FPoints;
end;

function TVectorDataItemPolygon.IsPoint: Boolean;
begin
  Result := False;
end;

{ TVectorDataItemPath }

function TVectorDataItemPath.IsLine: Boolean;
begin
  Result := True;
end;

function TVectorDataItemPath.IsPoly: Boolean;
begin
  Result := False;
end;

{ TVectorDataItemPoly }

function TVectorDataItemPoly.IsLine: Boolean;
begin
  Result := False;
end;

function TVectorDataItemPoly.IsPoly: Boolean;
begin
  Result := True;
end;

end.
