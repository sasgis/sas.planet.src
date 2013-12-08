unit i_Bitmap32To8Converter;

interface

uses
  i_Bitmap8Static,
  i_Bitmap32Static;

type
  IBitmap32To8Converter = interface
    ['{15FE823D-6795-4F2A-8F44-96321C371C8E}']
    function Convert(const ABitmap32: IBitmap32Static): IBitmap8Static;
  end;

implementation

end.
