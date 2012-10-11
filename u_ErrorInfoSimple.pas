unit u_ErrorInfoSimple;

interface

uses
  i_ErrorInfo;

type
  TErrorInfoSimple = class(TInterfacedObject, IErrorInfoSimple)
  private
    FText: string;
  private
    function GetErrorText: string;
  public
    constructor Create(
      const AText: string
    );
  end;

implementation

{ TErrorInfoSimple }

constructor TErrorInfoSimple.Create(const AText: string);
begin
  inherited Create;
  FText := AText;
end;

function TErrorInfoSimple.GetErrorText: string;
begin
  Result := FText;
end;

end.
