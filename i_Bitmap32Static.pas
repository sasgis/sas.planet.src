unit i_Bitmap32Static;

interface

uses
  GR32,
  i_Changeable;

type
  IBitmap32Static = interface
    ['{CE710076-F0B6-43BF-A70F-15B40555DBFA}']
    function GetSize: TPoint;
    property Size: TPoint read GetSize;

    function GetData: PColor32Array;
    property Data: PColor32Array read GetData;
  end;

  IBitmapChangeable = interface(IChangeable)
    ['{D0735E64-ED1C-42B0-8892-ADFDF9C56BE4}']
    function GetStatic: IBitmap32Static;
  end;

implementation

end.
