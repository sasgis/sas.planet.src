unit u_TextByVectorItemHTMLByDescription;

interface

uses
  i_VectorDataItemSimple,
  i_TextByVectorItem;

type
  TTextByVectorItemHTMLByDescription = class(TInterfacedObject, ITextByVectorItem)
  private
    function GetText(const AItem: IVectorDataItemSimple): string;
  end;

implementation

{ TTextByVectorItemHTMLByDescription }

function TTextByVectorItemHTMLByDescription.GetText(
  const AItem: IVectorDataItemSimple): string;
begin
  Result := AItem.Desc;
  if Result <> '' then begin
    Result := '<HTML><BODY>' + Result + '</BODY></HTML>';
  end;
end;

end.
