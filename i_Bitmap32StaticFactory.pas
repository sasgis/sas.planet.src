unit i_Bitmap32StaticFactory;

interface

uses
  GR32,
  i_Bitmap32Static;

type
  IObjectPoolBitmap32Standart = interface
    ['{31803812-FF36-4275-A4F7-B2E8CD136A30}']
    function Build: IBitmap32Static;
    function GetSize: TPoint;
    property Size: TPoint read GetSize;
  end;

  IBitmap32StaticFactory = interface
    ['{F110B8E7-76D3-48C4-BE8B-B75E5F374355}']
    function Build(
      const ASize: TPoint;
      const AData: PColor32Array
    ): IBitmap32Static;
    function BuildEmpty(const ASize: TPoint): IBitmap32Static;
    function BuildEmptyClear(
      const ASize: TPoint;
      const AColor: TColor32
    ): IBitmap32Static;
  end;

implementation

end.
