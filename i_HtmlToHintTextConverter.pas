unit i_HtmlToHintTextConverter;

interface

type
  IHtmlToHintTextConverter = interface
    ['{693082C2-9346-4DB9-BCB0-EBA9C585A439}']
    function Convert(AName, ADescription: string): string;
  end;

implementation

end.
