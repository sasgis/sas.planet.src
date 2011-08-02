unit i_ContentTypeManager;

interface

uses
  i_ContentTypeInfo,
  i_ContentConverter;

type
  IContentTypeManager = interface
    ['{157D7F4C-BBBB-4617-A0D1-250D066B4C2C}']
    function GetInfo(const AType: WideString): IContentTypeInfoBasic;
    function GetInfoByExt(const AExt: WideString): IContentTypeInfoBasic;
    function GetIsBitmapType(const AType: WideString): Boolean;
    function GetIsBitmapExt(const AExt: WideString): Boolean;
    function GetIsKmlType(const AType: WideString): Boolean;
    function GetIsKmlExt(const AExt: WideString): Boolean;
    function GetConverter(const ATypeSource, ATypeTarget: WideString): IContentConverter;
  end;


implementation

end.
