unit i_TextDrawerBasic;

interface

uses
  t_Bitmap32,
  i_Bitmap32Static;

type
  ITextDrawerBasic = interface
    ['{874AF838-4B3D-44AF-BF3E-96A72FE3D8CF}']
    function DrawText(
      const AText: string;
      const AFontSize: Integer;
      const ATextColor: TColor32;
      const ATextBgColor: TColor32;
      const ASolidBgDraw: Boolean
    ): IBitmap32Static;
  end;

implementation

end.
