unit u_VectorDataItemBase;

interface

uses
  t_GeoTypes,
  i_HtmlToHintTextConverter,
  i_VectorDataItemSimple;

type
  TVectorDataItemBase = class(TInterfacedObject, IVectorDataItemSimple)
  private
    FHintConverter: IHtmlToHintTextConverter;
    FName: string;
    FDesc: string;
  protected
    function GetName: string;
    function GetDesc: string;
    function GetLLRect: TDoubleRect;  virtual; abstract;
    function GetPoints: TArrayOfDoublePoint;  virtual; abstract;
    function GetHintText: string;
    function GetInfoCaption: string;
    function GetInfoHTML: string;

    function IsPoint: Boolean; virtual; abstract;
    function IsLine: Boolean; virtual; abstract;
    function IsPoly: Boolean; virtual; abstract;
  public
    constructor Create(
      AHintConverter: IHtmlToHintTextConverter;
      AName: string;
      ADesc: string
    );
  end;

implementation

uses
  SysUtils;

{ TVectorDataItemBase }

constructor TVectorDataItemBase.Create(
  AHintConverter: IHtmlToHintTextConverter;
  AName, ADesc: string
);
begin
  FHintConverter := AHintConverter;
  FName := AName;
  FDesc := ADesc;
end;

function TVectorDataItemBase.GetDesc: string;
begin
  Result := FDesc;
end;

function TVectorDataItemBase.GetHintText: string;
begin
  Result := FHintConverter.Convert(FName, FDesc);
end;

function TVectorDataItemBase.GetInfoCaption: string;
begin
  Result := FName;
end;

function TVectorDataItemBase.GetInfoHTML: string;
begin
  Result := '';
  if Fdesc <> '' then begin
    Result:='<HTML><BODY>';
    Result:=Result+Fdesc;
    Result:=Result+'</BODY></HTML>';
  end;
end;

function TVectorDataItemBase.GetName: string;
begin
  Result := FName;
end;

end.
