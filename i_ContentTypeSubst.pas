unit i_ContentTypeSubst;

interface

type
  IContentTypeSubst = interface
    function GetContentType(const ASource: string): string;
  end;

implementation

end.
