unit i_IMarkPicture;

interface

uses
  Classes,
  GR32,
  i_IConfigDataElement;

type
  IMarkPicture = interface
    ['{4F70C829-D49A-4019-AAF6-3AA9BCD2CCAE}']
    procedure LoadBitmap(ABmp: TCustomBitmap32);
    function GetBitmapSize: TPoint;
    function GetPointInPicture: TPoint;
    function GetTextAlignment: TAlignment;
    function GetTextVerticalAlignment: TVerticalAlignment;
  end;

  IMarkPictureList = interface(IConfigDataElement)
    ['{C080A087-C571-4654-8B3E-63D6E6A5542F}']
    function GetCount: Integer;
    property Count: Integer read GetCount;

    function Get(AIndex: Integer): IMarkPicture;
    function GetName(AIndex: Integer): string;
    function GetIndexByName(AValue: string): Integer;
    function GetPictureName(AValue: IMarkPicture): string;
  end;

implementation

end.
