unit i_Bitmap32StaticFactory;

interface

uses
  GR32,
  i_Bitmap32Static;

type
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
