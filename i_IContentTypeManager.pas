unit i_IContentTypeManager;

interface

uses
  i_ContentTypeInfo,
  i_IContentConverter;

type
  IContentTypeManager = interface
    ['{157D7F4C-BBBB-4617-A0D1-250D066B4C2C}']
    function GetInfo(AType: WideString): IContentTypeInfoBasic;
    function GetInfoByExt(AExt: WideString): IContentTypeInfoBasic;
    function GetIsBitmapType(AType: WideString): Boolean;
    function GetIsBitmapExt(AExt: WideString): Boolean;
    function GetIsKmlType(AType: WideString): Boolean;
    function GetIsKmlExt(AExt: WideString): Boolean;
    function GetConverter(ATypeSource, ATypeTarget: WideString): IContentConverter;
  end;


implementation

end.
