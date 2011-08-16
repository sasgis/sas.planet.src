unit i_ContentTypeInfo;

interface

uses
  i_BitmapTileSaveLoad,
  i_KmlInfoSimpleLoader;

type
  IContentTypeInfoBasic = interface
    ['{A2FC7C16-1B96-4AA2-BC70-1A353E4E1923}']
    function GetContentType: WideString;
    function GetDefaultExt: WideString;
  end;

  IContentTypeInfoBitmap = interface(IContentTypeInfoBasic)
    ['{DB6FAD7E-CACD-47C7-BA5E-9D1A0959FE88}']
    function GetLoader: IBitmapTileLoader;
    function GetSaver: IBitmapTileSaver;
  end;

  IContentTypeInfoVectorData = interface(IContentTypeInfoBasic)
    ['{62A84A15-D775-4F23-A12D-C25948182757}']
    function GetLoader: IVectorDataLoader;
  end;


implementation

end.
