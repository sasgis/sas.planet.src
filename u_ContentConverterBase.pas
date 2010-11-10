unit u_ContentConverterBase;

interface

uses
  Classes,
  i_ContentTypeInfo,
  i_IContentConverter;

type
  TContentConverterBase = class(TInterfacedObject, IContentConverter)
  private
    FSource: IContentTypeInfoBasic;
    FTarget: IContentTypeInfoBasic;
  protected
    function GetSource: IContentTypeInfoBasic;
    function GetTarget: IContentTypeInfoBasic;
    procedure ConvertStream(ASource, ATarget: TStream); virtual; abstract;
  public
    constructor Create(
      ASource: IContentTypeInfoBasic;
      ATarget: IContentTypeInfoBasic
    );
    destructor Destroy; override;
  end;

implementation

{ TContentConverterBase }

constructor TContentConverterBase.Create(ASource,
  ATarget: IContentTypeInfoBasic);
begin
  FSource := ASource;
  FTarget := ATarget;
end;

destructor TContentConverterBase.Destroy;
begin
  FSource := nil;
  FTarget := nil;
  inherited;
end;

function TContentConverterBase.GetSource: IContentTypeInfoBasic;
begin
  Result := FSource;
end;

function TContentConverterBase.GetTarget: IContentTypeInfoBasic;
begin
  Result := FTarget;
end;

end.
