unit i_ITileInfoBasic;

interface

uses
  i_ContentTypeInfo;

type
  ITileInfoBasic = interface
    ['{7916FA97-49F1-451E-B2C1-0669B9336291}']
    function GetIsExists: Boolean;
    function GetIsExistsTNE: Boolean;
    function GetLoadDate: TDateTime;
    function GetSize: Cardinal;
    function GetVersion: Variant;
    function GetContentType: IContentTypeInfoBasic;
  end;


implementation

end.
