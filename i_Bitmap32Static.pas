unit i_Bitmap32Static;

interface

uses
  GR32;

type
  IBitmap32Static = interface
    ['{CE710076-F0B6-43BF-A70F-15B40555DBFA}']
    function GetBitmap: TCustomBitmap32;
    property Bitmap: TCustomBitmap32 read GetBitmap;
  end;

implementation

end.
