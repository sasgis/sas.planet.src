unit u_MarkFullBase;

interface

uses
  t_GeoTypes,
  i_HtmlToHintTextConverter,
  i_MarkCategory,
  i_MarksSimple,
  u_MarkId;

type
  TMarkFullBase = class(TMarkId, IMark)
  private
    FHintConverter: IHtmlToHintTextConverter;
    FDesc: string;
  protected
    function GetDesc: string;
    function GetLLRect: TDoubleRect; virtual; abstract;
    function GetHintText: string;
    function GetInfoHTML: string;
    function GetGoToLonLat: TDoublePoint; virtual; abstract;
  public
    constructor Create(
      AHintConverter: IHtmlToHintTextConverter;
      ADbCode: Integer;
      AName: string;
      AId: Integer;
      ACategory: IMarkCategory;
      ADesc: string;
      AVisible: Boolean
    );
  end;

implementation

{ TMarkFullBase }

constructor TMarkFullBase.Create(
  AHintConverter: IHtmlToHintTextConverter;
  ADbCode: Integer;
  AName: string;
  AId: Integer;
  ACategory: IMarkCategory;
  ADesc: string;
  AVisible: Boolean
);
begin
  inherited Create(ADbCode, AName, AId, ACategory, AVisible);
  FHintConverter := AHintConverter;
  FDesc := ADesc;
end;

function TMarkFullBase.GetDesc: string;
begin
  Result := FDesc;
end;

function TMarkFullBase.GetHintText: string;
begin
  Result := FHintConverter.Convert(GetName, FDesc);
end;

function TMarkFullBase.GetInfoHTML: string;
begin
  Result := '';
  if Fdesc <> '' then begin
    Result:='<HTML><BODY>';
    Result:=Result+Fdesc;
    Result:=Result+'</BODY></HTML>';
  end;
end;

end.
