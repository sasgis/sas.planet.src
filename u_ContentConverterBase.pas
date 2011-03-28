unit u_ContentConverterBase;

interface

uses
  Classes,
  i_ContentTypeInfo,
  i_ContentConverter;

type

  TContentConverterAbstract = class(TInterfacedObject, IContentConverter)
  private
    FSource: IContentTypeInfoBasic;
    FTarget: IContentTypeInfoBasic;
  protected
    function GetSource: IContentTypeInfoBasic;
    function GetTarget: IContentTypeInfoBasic;
    function GetIsSimpleCopy: Boolean; virtual; abstract;
    procedure ConvertStream(ASource, ATarget: TStream); virtual; abstract;
  public
    constructor Create(
      ASource: IContentTypeInfoBasic;
      ATarget: IContentTypeInfoBasic
    );
    destructor Destroy; override;
  end;

  TContentConverterBase = class(TContentConverterAbstract)
  protected
    function GetIsSimpleCopy: Boolean; override;
  end;

  TContentConverterSimpleCopy = class(TContentConverterAbstract)
  protected
    function GetIsSimpleCopy: Boolean; override;
    procedure ConvertStream(ASource, ATarget: TStream); override;
  end;

implementation

{ TContentConverterAbstract }

constructor TContentConverterAbstract.Create(ASource,
  ATarget: IContentTypeInfoBasic);
begin
  FSource := ASource;
  FTarget := ATarget;
end;

destructor TContentConverterAbstract.Destroy;
begin
  FSource := nil;
  FTarget := nil;
  inherited;
end;

function TContentConverterAbstract.GetSource: IContentTypeInfoBasic;
begin
  Result := FSource;
end;

function TContentConverterAbstract.GetTarget: IContentTypeInfoBasic;
begin
  Result := FTarget;
end;

{ TContentConverterBase }

function TContentConverterBase.GetIsSimpleCopy: Boolean;
begin
  Result := False;
end;

{ TContentConverterSimpleCopy }

procedure TContentConverterSimpleCopy.ConvertStream(ASource, ATarget: TStream);
begin
  ATarget.CopyFrom(ASource, ASource.Size);
end;

function TContentConverterSimpleCopy.GetIsSimpleCopy: Boolean;
begin
  Result := True;
end;

end.
