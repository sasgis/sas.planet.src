unit i_IMarkPicture;

interface

uses
  Classes,
  GR32;

type
  IMarkPicture = interface
    ['{4F70C829-D49A-4019-AAF6-3AA9BCD2CCAE}']
    procedure LoadBitmap(ABmp: TCustomBitmap32);
    function GetPointInPicture: TPoint;
    function GetTextAlignment: TAlignment;
    function GetTextVerticalAlignment: TVerticalAlignment;
  end;

implementation

end.
