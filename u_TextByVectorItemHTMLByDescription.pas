unit u_TextByVectorItemHTMLByDescription;

interface

uses
  i_VectorDataItemSimple,
  i_TextByVectorItem,
  u_BaseInterfacedObject;

type
  TTextByVectorItemHTMLByDescription = class(TBaseInterfacedObject, ITextByVectorItem)
  private
    function GetText(const AItem: IVectorDataItemSimple): string;
  end;

implementation

{ TTextByVectorItemHTMLByDescription }

function TTextByVectorItemHTMLByDescription.GetText(
  const AItem: IVectorDataItemSimple): string;
begin
  Result :=
    '<html>'#13#10 +
      '<head>'#13#10 +
        '<title>' + AItem.GetInfoCaption + '</title>'#13#10 +
      '</head>'#13#10 +
      '<body>'#13#10 +
        AItem.Desc + #13#10 +
      '</body>'#13#10 +
      '</html>';
end;

end.
